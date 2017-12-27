(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

let strf = Printf.sprintf

(* A bit of randomness for functions that need unique filenames *)

let rand_gen = lazy (Random.State.make_self_init ())

(* Error handling *)

let failwithf fmt = Format.kasprintf (fun s -> raise_notrace (Failure s)) fmt
let uerror = Unix.error_message

let apply f x ~finally y =
  let result = try f x with
  | e -> try finally y; raise e with _ -> raise e
  in
  finally y;
  result

(* Environment variables *)

module Env = struct

  (* Variables *)

  let find ?(empty_is_absent = true) name = match Unix.getenv name with
  | "" when empty_is_absent -> None
  | v -> Some v
  | exception Not_found -> None

  let get ?(empty_is_absent = true) name ~absent = match Unix.getenv name with
  | "" when empty_is_absent -> absent
  | v -> v
  | exception Not_found -> absent

  let value ?empty_is_absent name conv ~absent =
    match find ?empty_is_absent name with
    | None -> Ok absent
    | Some v -> B0_conv.parse conv v

  let get_value ?(log = B0_log.Error) ?empty_is_absent name conv ~absent =
    (value ?empty_is_absent name conv ~absent)
    |>  B0_log.on_error_msg ~level:log ?header:None ~use:(fun _ -> absent)

  (* Environments *)

  type t = string B0_string.map
  let empty = B0_string.Map.empty

  let _of_assignments ?(init = B0_string.Map.empty) fold v =
    try
      let add acc assign = match B0_string.cut ~sep:"=" assign with
      | Some (var, value) -> B0_string.Map.add var value acc
      | None -> failwithf "%S: cannot parse env assignement" assign
      in
      Ok (fold add init v)
    with
    | Failure e -> R.error_msg e

  let assignments () = try Ok (Array.to_list @@ Unix.environment ()) with
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "process environment: %s" (Unix.error_message e)

  let of_assignments ?init l = _of_assignments ?init List.fold_left l
  let to_assignments env =
    let bind var v = Printf.sprintf "%s=%s" var v in
    let add var v acc = bind var v :: acc in
    B0_string.Map.fold add env []

  let current () =
    let err msg = R.error_msgf "could not get process environment: %s" msg in
    try
      let env = Unix.environment () in
      match _of_assignments Array.fold_left env with
      | Error (`Msg e) -> err e
      | Ok _ as v -> v
    with
    | Failure e -> err e
    | Unix.Unix_error (e, _, _) -> err (Unix.error_message e)

  let override env ~by =
    let lean_right _ l r = match r with
    | Some _ as v -> v
    | None -> match l with Some _ as v -> v | None -> assert false
    in
    B0_string.Map.merge lean_right env by
end

(* File operations *)

module File = struct

  (* Famous file paths *)

  let null = B0_fpath.v (if Sys.win32 then "NUL" else "/dev/null")
  let dash = B0_fpath.v "-"
  let is_dash = B0_fpath.equal dash

  (* Existence and deletion *)

  let rec exists file =
    try Ok (Unix.((stat @@ B0_fpath.to_string file).st_kind = S_REG)) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> exists file
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "file %a exists: %s" B0_fpath.pp file (uerror e)

  let rec must_exist file =
    try match Unix.((stat @@ B0_fpath.to_string file).st_kind) with
    | Unix.S_REG -> Ok file
    | _ -> R.error_msgf "%a: Not a file" B0_fpath.pp file
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        R.error_msgf "%a: No such file" B0_fpath.pp file
    | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist file
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "file %a must exist: %s" B0_fpath.pp file (uerror e)

  let delete ?(must_exist = false) file =
    let rec unlink file = try Ok (Unix.unlink @@ B0_fpath.to_string file) with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        if not must_exist then Ok () else
        R.error_msgf "delete file %a: No such file" B0_fpath.pp file
    | Unix.Unix_error (Unix.EINTR, _, _) -> unlink file
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "delete file %a: %s" B0_fpath.pp file (uerror e)
    in
    unlink file

  (* Hard links *)

  let rec link ~force ~target p =
    let rec unlink target p =
      try Ok (Unix.unlink (B0_fpath.to_string p)) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unlink target p
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "force link target %a to %a: %s"
            B0_fpath.pp target B0_fpath.pp p (uerror e)
    in
    try Ok (Unix.link (B0_fpath.to_string target) (B0_fpath.to_string p)) with
    | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
        unlink target p >>= fun () -> link ~force ~target p
    | Unix.Unix_error (Unix.EINTR, _, _) -> link ~force ~target p
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "link target %a to %a: %s"
          B0_fpath.pp target B0_fpath.pp p (uerror e)

  (* Executability *)

  let _is_executable file = match Unix.access file [Unix.X_OK] with
  | () -> true
  | exception (Unix.Unix_error _) -> false

  let is_executable file = _is_executable (B0_fpath.to_string file)

  (* Input *)

  let io_buffer_size = 65535
  let err_empty_buf = "buffer size can't be 0"
  let bytes_buf = function
  | None -> Bytes.create io_buffer_size
  | Some bytes ->
      if Bytes.length bytes <> 0 then bytes else
      invalid_arg err_empty_buf

  let with_ic file f v =
    try
      let ic = match is_dash file with
      | true -> stdin
      | false -> open_in_bin (B0_fpath.to_string file)
      in
      let close ic = if is_dash file then () else close_in ic in
      try Ok (apply (f ic) v ~finally:close ic) with
      | Sys_error e -> R.error_msgf "%a: %s" B0_fpath.pp file e
    with
    | End_of_file -> R.error_msgf "%a: unexpected end of file" B0_fpath.pp file
    | Sys_error e -> R.error_msg e

  let err_file_too_large file len =
    R.error_msgf "read %a: file too large (%d, max supported: %d)"
      B0_fpath.pp file len Sys.max_string_length

  let read file =
    let input_stdin () =
      let blen = io_buffer_size in
      let b = Bytes.create blen in
      let buf = Buffer.create blen in
      let rec loop () = match input stdin b 0 blen with
      | 0 -> Ok (Buffer.contents buf)
      | n -> Buffer.add_subbytes buf b 0 n; loop ()
      in
      loop ()
    in
    let input ic () = match ic == stdin with
    | true -> input_stdin ()
    | false ->
        let len = in_channel_length ic in
        match len <= Sys.max_string_length with
        | false -> err_file_too_large file len
        | true ->
            let s = Bytes.create len in
            (really_input ic s 0 len; Ok (Bytes.unsafe_to_string s))
    in
    match with_ic file input () with
    | Ok (Ok _ as v) -> v
    | Ok (Error _ as e) -> e
    | Error _ as e -> e

  let read_fd file fd =
    try
      let rec really_read fd b start len = match len <= 0 with
      | true -> Ok (Bytes.unsafe_to_string b)
      | false ->
          match Unix.read fd b start len with
          | 0 -> R.error_msgf "read %a: unexpected end of file" B0_fpath.pp file
          | r -> really_read fd b (start + r) (len - r)
          | exception Unix.Unix_error (Unix.EINTR, _, _) ->
              really_read fd b start len
      in
      let len = Unix.lseek fd 0 Unix.SEEK_END in
      match len <= Sys.max_string_length with
      | false -> err_file_too_large file len
      | true ->
          let b = Bytes.create len in
          ignore (Unix.lseek fd 0 Unix.SEEK_SET);
          really_read fd b 0 len
    with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "read %a: %s" B0_fpath.pp file (uerror e)

  (* Handling tmp files *)

  let rand_suff =
    fun () ->
      let rand = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFF in
      Printf.sprintf "-%06x.tmp" rand

  let rec unlink_tmp file = try Unix.unlink (B0_fpath.to_string file) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> unlink_tmp file
  | Unix.Unix_error (e, _, _) -> ()

  let tmps = ref B0_fpath.Set.empty
  let tmps_add file = tmps := B0_fpath.Set.add file !tmps
  let tmps_rem file = unlink_tmp file; tmps := B0_fpath.Set.remove file !tmps
  let unlink_tmps () = B0_fpath.Set.iter unlink_tmp !tmps
  let () = at_exit unlink_tmps

  let open_tmp
      ?(flags = Unix.[O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC])
      ?(mode = 0o600) p
    =
    let p = B0_fpath.to_string p in
    let rec loop n = match n with
    | 0 -> R.error_msgf "tmp file %s-X.tmp: too many attempts" p
    | n ->
        let file = p ^ rand_suff () in
        try
          let fd = Unix.openfile file flags mode in
          let file = B0_fpath.v file in
          (tmps_add file; Ok (file, fd))
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
        | Unix.Unix_error (e, _, _) ->
            R.error_msgf "open tmp file %s: %s" file (uerror e)
    in
    loop 10000

  let with_tmp_oc ?flags ?mode p f v =
    try
      open_tmp ?flags ?mode p >>= fun (file, fd) ->
      let oc = Unix.out_channel_of_descr fd in
      let delete_close oc = tmps_rem file; close_out oc in
      try Ok (apply (f file oc) v ~finally:delete_close oc) with
      | Sys_error e -> R.error_msgf "%a: %s" B0_fpath.pp file e
    with
    | Sys_error e -> R.error_msg e

  (* Output *)

  let default_mode = 0o644

  let rec rename src dst =
    try Ok (Unix.rename (B0_fpath.to_string src) (B0_fpath.to_string dst)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> rename src dst
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf
          "rename %a to %a: %s" B0_fpath.pp src B0_fpath.pp dst (uerror e)

  let with_oc ?(mode = default_mode) file f v = match is_dash file with
  | true -> Ok (apply (f stdout) v ~finally:(fun () -> ()) ())
  | false ->
      let do_write tmp tmp_oc v = match f tmp_oc v with
      | Error _ as v -> Ok v
      | Ok _ as v ->
          match rename tmp file with
          | Error _ as e -> e
          | Ok () -> Ok v
      in
      match with_tmp_oc file do_write v with
      | Ok (Ok _ as r) -> r
      | Ok (Error _ as e) -> e
      | Error _ as e -> e

  let write ?mode file contents =
    let write oc contents = output_string oc contents; Ok () in
    R.join @@ with_oc ?mode file write contents
end

(* Directory operations *)

module Dir = struct

  let rec exists dir =
    try Ok (Unix.((stat @@ B0_fpath.to_string dir).st_kind = S_DIR)) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "directory %a exists: %s" B0_fpath.pp dir (uerror e)

  let rec must_exist dir =
    try match Unix.((stat @@ B0_fpath.to_string dir).st_kind) with
    | Unix.S_DIR -> Ok dir
    | _ -> R.error_msgf "%a: Not a directory" B0_fpath.pp dir
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        R.error_msgf "%a: No such directory" B0_fpath.pp dir
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "directory  %a must exist: %s" B0_fpath.pp dir (uerror e)

  let create ?(path = true) ?(mode = 0o755) dir =
    let rec mkdir d mode = try Ok (Unix.mkdir (B0_fpath.to_string d) mode) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
    | Unix.Unix_error (e, _, _) ->
        if d = dir
        then R.error_msgf "create directory %a: %s" B0_fpath.pp d (uerror e)
        else R.error_msgf "create directory %a: %a: %s"
            B0_fpath.pp dir B0_fpath.pp d (uerror e)
    in
    exists dir >>= function
    | true -> Ok false
    | false ->
        match path with
        | false -> mkdir dir mode >>= fun () -> Ok false
        | true ->
            let rec dirs_to_create p acc = exists p >>= function
              | true -> Ok acc
              | false -> dirs_to_create (B0_fpath.parent p) (p :: acc)
            in
            let rec create_them dirs () = match dirs with
            | dir :: dirs -> mkdir dir mode >>= create_them dirs
            | [] -> Ok ()
            in
            dirs_to_create dir []
            >>= fun dirs -> create_them dirs ()
            >>= fun () -> Ok true

  let delete ?(must_exist = false) ~contents d =
    let err d msg = failwithf "delete %a: %s" B0_fpath.pp d msg in
    let try_unlink file = match Unix.unlink (B0_fpath.to_string file) with
    | () -> true
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.ENOENT -> true
        | Unix.EISDIR (* Linux *) | Unix.EPERM (* POSIX *) -> false
        | Unix.EACCES when Sys.win32 ->
            (* This is what Unix.unlink returns on directories on Windows. *)
            false
        | e -> err file (uerror e)
    in
    let rec delete_contents d dh todo = match Unix.readdir dh with
    | exception End_of_file -> d :: todo
    | ".." | "." -> delete_contents d dh todo
    | file ->
        let file = B0_fpath.(d / file) in
        if try_unlink file then delete_contents d dh todo else
        file :: d :: todo (* file is a dir we'll come back later for [d] *)
    in
    let rec try_delete d todo = match Unix.opendir (B0_fpath.to_string d) with
    | dh -> doit @@ apply (delete_contents d dh) todo ~finally:Unix.closedir dh
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.ENOENT | Unix.ENOTDIR -> doit todo
        | e -> err d (uerror e)
    and doit = function
    | [] -> ()
    | d :: ds ->
        match Unix.rmdir (B0_fpath.to_string d) with
        | () -> doit ds
        | exception Unix.Unix_error (e, _, _) ->
            match e with
            | Unix.ENOTEMPTY -> try_delete d ds
            | Unix.ENOENT | Unix.ENOTDIR -> doit ds
            | e -> err d (uerror e)
    in
    try match Unix.rmdir (B0_fpath.to_string d) with
    | () -> Ok ()
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.ENOTEMPTY when contents -> Ok (try_delete d [])
        | Unix.ENOENT when not must_exist -> Ok ()
        | Unix.ENOENT -> err d "No such directory"
        | e -> err d (uerror e)
    with
    | Failure msg -> R.error_msg msg

  let _contents ~filter ?(dotfiles = false) ?(rel = false) dir =
    let is_dot_file s = String.length s <> 0 && s.[0] = '.' in
    let rec readdir dh dir acc =
      match try Some (Unix.readdir dh) with End_of_file -> None with
      | None -> Ok acc
      | Some (".."| ".") -> readdir dh dir acc
      | Some f when is_dot_file f && not dotfiles -> readdir dh dir acc
      | Some f ->
          match B0_fpath.is_seg f with
          | true ->
              let full = B0_fpath.(dir / f) in
              let p = if rel then B0_fpath.v f else full in
              let acc = if filter full then p :: acc else acc in
              readdir dh dir acc
          | false ->
              R.error_msgf
                "contents of %a: cannot parse %S to a path" B0_fpath.pp dir f
    in
    try
      let dh = Unix.opendir (B0_fpath.to_string dir) in
      apply (readdir dh dir) [] ~finally:Unix.closedir dh
    with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "contents of %a: %s" B0_fpath.pp dir (uerror e)

  let contents ?dotfiles ?rel dir =
    let filter _ = true in
    _contents ~filter ?dotfiles ?rel dir

  let rec filter_kind kind file =
    try Unix.((stat @@ B0_fpath.to_string file).st_kind = kind) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> false
    | Unix.Unix_error (Unix.EINTR, _, _) -> filter_kind kind file
    | Unix.Unix_error (e, _, _) -> false

  let files ?dotfiles ?rel dir =
    let filter = filter_kind Unix.S_REG in
    _contents ~filter ?dotfiles ?rel dir

  let dirs ?dotfiles ?rel dir =
    let filter = filter_kind Unix.S_DIR in
    _contents ~filter ?dotfiles ?rel dir

  (* Current working directory *)

  let rec current () =
    try
      let p = Unix.getcwd () in
      match B0_fpath.of_string p with
      | Error _ -> R.error_msgf "get cwd: cannot parse %S to a path" p
      | Ok dir ->
          match B0_fpath.is_abs dir with
          | true -> Ok dir
          | false ->
              R.error_msgf
                "get cwd: getcwd(3) returned a relative path: (%a)"
                B0_fpath.pp dir
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "get cwd: %s" (uerror e)

  let rec set_current dir = try Ok (Unix.chdir (B0_fpath.to_string dir)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> set_current dir
  | Unix.Unix_error (e, _, _) ->
      R.error_msgf "set cwd to %a: %s" B0_fpath.pp dir (uerror e)

  let with_current dir f v =
    current () >>= fun old ->
    try
      set_current dir >>= fun () ->
      let ret = f v in
      set_current old >>= fun () -> Ok ret
    with
    | exn -> ignore (set_current old); raise exn

  (* Default temporary directory *)

  let default_tmp_init =
    let from_env var ~absent =
      match try Some (Sys.getenv var) with Not_found -> None with
      | None -> absent
      | Some v ->
          match B0_fpath.of_string v with
          | Error _ -> absent
          | Ok v -> v
    in
    if Sys.win32 then from_env "TEMP" ~absent:B0_fpath.(v "./") else
    from_env "TMPDIR" ~absent:(B0_fpath.v "/tmp")

  let default_tmp = ref default_tmp_init
  let set_default_tmp p = default_tmp := p
  let default_tmp () = !default_tmp
end

(* Executing commands *)

module Cmd = struct

  let err_empty_line = "no command, empty command line"

  (* Tool existence and search *)

  let default_path_sep = if Sys.win32 then ";" else ":"

  let exe_is_path t =
    try ignore (B0_string.index t B0_fpath.dir_sep_char); true with
    | Not_found -> false

  let tool_file ~dir tool = match dir.[String.length dir - 1] with
  | c when B0_fpath.char_is_dir_sep c -> dir ^ tool
  | _ -> String.concat B0_fpath.dir_sep [dir; tool]

  let search_in_path tool =
    let rec loop tool = function
    | "" -> None
    | p ->
        let dir, p = match B0_string.cut ~sep:default_path_sep p with
        | None -> p, ""
        | Some (dir, p) -> dir, p
        in
        if dir = "" then loop tool p else
        let tool_file = tool_file ~dir tool in
        match File._is_executable tool_file with
        | false -> loop tool p
        | true -> Some (B0_fpath.v tool_file)
    in
    match Unix.getenv "PATH" with
    | p -> loop tool p
    | exception Not_found -> None

  let search_in_dirs ~dirs tool =
    let rec loop tool = function
    | [] -> None
    | d :: dirs ->
        let tool_file = tool_file ~dir:(B0_fpath.to_string d) tool in
        match File._is_executable tool_file with
        | false -> loop tool dirs
        | trure -> Some (B0_fpath.v tool_file)
    in
    loop tool dirs

  let ensure_exe_suffix_if_win32 = match Sys.win32 with
  | false -> fun t -> t
  | true ->
      fun t -> match B0_string.is_suffix ~affix:".exe" t with
      | true -> t
      | false -> t ^ ".exe"

  let _find_tool ?search tool = match tool with
  | "" -> Ok None
  | tool ->
      let tool = ensure_exe_suffix_if_win32 tool in
      match exe_is_path tool with
      | true -> B0_fpath.of_string tool >>| fun t -> Some t
      | false ->
          match search with
          | None -> Ok (search_in_path tool)
          | Some dirs -> Ok (search_in_dirs ~dirs tool)

  let find_tool ?search cmd = match B0_cmd.to_list cmd with
  | [] -> Ok None
  | c :: _ -> _find_tool ?search c

  let err_not_found ?search cmd = match B0_cmd.is_empty cmd with
  | true -> R.error_msg err_empty_line
  | false ->
      let pp_search ppf = function
      | None -> B0_fmt.string ppf "PATH"
      | Some dirs ->
          let pp_dir ppf d =
            B0_fmt.string ppf (Filename.quote @@ B0_fpath.to_string d)
          in
          B0_fmt.(list ~sep:comma pp_dir) ppf dirs
      in
      let tool = List.hd @@ B0_cmd.to_list cmd in
      R.error_msgf "%s: no such command in %a" tool pp_search search

  let get_tool ?search cmd = match find_tool ?search cmd with
  | Ok (Some t) -> Ok t
  | Ok None -> err_not_found ?search cmd
  | Error _ as e -> e

  let exists ?search cmd = match find_tool ?search cmd with
  | Ok (Some _) -> Ok true
  | Ok None -> Ok false
  | Error _ as e -> e

  let must_exist ?search cmd = match find_tool ?search cmd with
  | Ok (Some _) -> Ok cmd
  | Ok None -> err_not_found ?search cmd
  | Error _ as e -> e

  let resolve ?search cmd = match find_tool ?search cmd with
  | Ok (Some t) ->
      let t = B0_fpath.to_string t in
      Ok (B0_cmd.of_list (t :: List.tl (B0_cmd.to_list cmd)))
  | Ok None -> err_not_found ?search cmd
  | Error _ as e -> e

  let search_path_dirs ?(sep = default_path_sep) path =
    let rec loop acc = function
    | ""  -> Ok (List.rev acc)
    | p ->
        let dir, p = match B0_string.cut ~sep p with
        | None -> p, ""
        | Some (dir, p) -> dir, p
        in
        if dir = "" then loop acc p else
        match B0_fpath.of_string dir with
        | Error (`Msg m) -> R.error_msgf "search path value %S: %s" path m
        | Ok d -> loop (d :: acc) p
    in
    loop [] path

  (* Process completion statuses *)

  type status = [ `Exited of int | `Signaled of int ]

  let status_of_unix_status = function
  | Unix.WEXITED n -> `Exited n
  | Unix.WSIGNALED n -> `Signaled n
  | Unix.WSTOPPED _ -> assert false

  let pp_status ppf = function
  | `Exited n -> B0_fmt.pf ppf "@[exited [%d]@]" n
  | `Signaled n -> B0_fmt.pf ppf "@[signaled [%d]@]" n

  let pp_cmd_status ppf (cmd, st) =
    B0_fmt.pf ppf "cmd [%s]: %a" (B0_cmd.to_string cmd) pp_status st

  (* Fd utils *)

  let rec openfile fn mode perm = try Unix.openfile fn mode perm with
  | Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

  let rec close fd = try Unix.close fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> close fd

  let close_no_err fd = try close fd with e -> ()

  module Fds = struct (* Maintains a set of fds to close. *)
    module Fd = struct
      type t = Unix.file_descr
      let compare : t -> t -> int = compare
    end
    module S = Set.Make (Fd)
    type t = S.t ref
    let empty () = ref S.empty
    let rem fd s = s := S.remove fd !s
    let add fd s = s := S.add fd !s
    let close_all s = S.iter close_no_err !s; s := S.empty
    let close fd s = if S.mem fd !s then (close_no_err fd; s := S.remove fd !s)
  end

  (* Process standard inputs and outputs *)

  type stdi =
  | In_string of string
  | In_file of B0_fpath.t
  | In_fd of { fd : Unix.file_descr; close : bool }

  type stdo =
  | Out_file of B0_fpath.t
  | Out_fd of { fd : Unix.file_descr; close : bool }

  let in_string s = In_string s
  let in_file f = In_file f
  let in_fd ~close fd = In_fd { fd; close }
  let in_stdin = In_fd { fd = Unix.stdin; close = false }
  let in_null = In_file File.null

  let out_file f = Out_file f
  let out_fd ~close fd = Out_fd { fd; close }
  let out_stdout = Out_fd { fd = Unix.stdout; close = false }
  let out_stderr = Out_fd { fd = Unix.stderr; close = false }
  let out_null = Out_file File.null

  let stdi_to_fd fds = function
  | In_fd { fd; close } -> if close then Fds.add fd fds; fd
  | In_string s ->
      begin try
        (* We write the input string to a temporary file. *)
        let base = B0_fpath.v "b0-sdtin" in
        let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC] in
        let f, fd = R.failwith_error_msg @@ File.open_tmp ~flags base in
        Fds.add fd fds;
        File.tmps_rem f; (* We don't need the actual file. *)
        ignore (Unix.write_substring fd s 0 (String.length s));
        ignore (Unix.lseek fd 0 Unix.SEEK_SET);
        fd
      with
      | Unix.Unix_error (e, _, _) ->
          failwithf "tmp file for stdin: %s" (uerror e)
      end
  | In_file f ->
      try
        let f = B0_fpath.to_string f in
        let fd = openfile f Unix.[O_RDONLY] 0o644 in
        Fds.add fd fds; fd
      with Unix.Unix_error (e, _, _) ->
        failwithf "open file %a for stdin: %s" B0_fpath.pp f (uerror e)

  let stdo_to_fd fds = function
  | Out_fd { fd; close } -> if close then Fds.add fd fds; fd
  | Out_file f ->
      try
        let f = B0_fpath.to_string f in
        let fd = openfile f Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Fds.add fd fds; fd
      with Unix.Unix_error (e, _, _) ->
        failwithf "open file %a for stdout: %s" B0_fpath.pp f (uerror e)

  (* Low-level command spawn *)

  let spawn_err cmd msg = match B0_cmd.is_empty cmd with
  | true -> R.error_msg msg
  | false -> R.error_msgf "cmd %s: %s" (B0_cmd.to_string cmd) msg

  let rec spawn_getcwd () = try Unix.getcwd () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> spawn_getcwd ()

  let rec spawn_chdir cwd = try Unix.chdir cwd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> spawn_chdir cwd
  | Unix.Unix_error (e, _, _) -> failwithf "chdir %s: %s" cwd (uerror e)

  let spawn_env = function
  | None -> Unix.environment ()
  | Some e -> Array.of_list e

  let spawn_cwd = function
  | None -> spawn_getcwd ()
  | Some cwd -> B0_fpath.to_string cwd

  let _spawn fds ~env ~cwd ~stdin ~stdout ~stderr cmd =
    if B0_cmd.is_empty cmd then failwith err_empty_line else
    let line = Array.of_list @@ B0_cmd.to_list cmd in
    let stdin = stdi_to_fd fds stdin in
    let stdout = stdo_to_fd fds stdout in
    let stderr = stdo_to_fd fds stderr in
    let old_cwd = spawn_getcwd () in
    let change_cwd = old_cwd <> cwd in
    if change_cwd then spawn_chdir cwd;
    let pid = Unix.create_process_env line.(0) line env stdin stdout stderr in
    if change_cwd then spawn_chdir old_cwd; (* Pid zombie on fail, oh well. *)
    Fds.close_all fds;
    pid

  (* Blocking command execution *)

  let rec run_collect pid cmd = match Unix.waitpid [] pid with
  | _, status ->
      B0_log.debug begin fun m ->
        let header = match status with
        | Unix.WEXITED 0 -> "EXEC"
        | Unix.WEXITED n -> strf "EXEC [%d]" n
        | Unix.WSIGNALED n -> strf "EXEC sig'd [%d]" n
        | Unix.WSTOPPED _ -> assert false
        in
        m ~header "[%s]" (B0_cmd.to_string cmd)
      end;
      status_of_unix_status status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> run_collect pid cmd
  | exception Unix.Unix_error (e, _, _) ->
      failwithf "waitpid %d: %s" pid (uerror e)

  let run_status
      ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
      ?(stderr = out_stderr) cmd
    =
    let fds = Fds.empty () in
    try
      let env = spawn_env env in
      let cwd = spawn_cwd cwd in
      let pid = _spawn fds ~env ~cwd ~stdin ~stdout ~stderr cmd in
      Ok (run_collect pid cmd)
    with
    | Failure e -> Fds.close_all fds; spawn_err cmd e
    | Unix.Unix_error (e, _, _) -> Fds.close_all fds; spawn_err cmd (uerror e)

  let run_status_out
      ?(trim = true) ?env ?cwd ?(stdin = in_stdin) ?(stderr = `Stdo out_stderr)
      cmd
    =
    let fds = Fds.empty () in
    try
      let env = spawn_env env in
      let cwd = spawn_cwd cwd in
      let base = B0_fpath.v "b0-sdtout" in
      let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC] in
      let tmpf, fd = R.failwith_error_msg @@ File.open_tmp ~flags base in
      let stdout = out_fd ~close:false fd in
      let stderr = match stderr with `Out -> stdout | `Stdo o -> o in
      let pid = _spawn fds ~env ~cwd ~stdin ~stdout ~stderr cmd in
      let status = run_collect pid cmd in
      let out = R.failwith_error_msg @@ File.read_fd tmpf fd in
      let out = if trim then String.trim out else out in
      ignore (File.tmps_rem tmpf);
      Ok (status, out)
    with
    | Failure e -> Fds.close_all fds; spawn_err cmd e
    | Unix.Unix_error (e, _, _) -> Fds.close_all fds; spawn_err cmd (uerror e)

  let run ?env ?cwd ?stdin ?stdout ?stderr cmd =
    match run_status ?env ?cwd ?stdin ?stdout ?stderr cmd with
    | Ok (`Exited 0) -> Ok ()
    | Ok st -> R.error_msgf "%a" pp_cmd_status (cmd, st)
    | Error _ as e -> e

  let run_out ?trim ?env ?cwd ?stdin ?stderr cmd =
    match run_status_out ?trim ?env ?cwd ?stdin ?stderr cmd with
    | Ok (`Exited 0, v) -> Ok v
    | Ok (st, _) -> R.error_msgf "%a" pp_cmd_status (cmd, st)
    | Error _ as e -> e

  (* Non-blocking command execution *)

  type pid = int
  let pid_to_int pid = pid

  let spawn_low ~env ~cwd ~stdin ~stdout ~stderr cmd =
    let fds = Fds.empty () in
    try
      let pid = _spawn fds ~env ~cwd ~stdin ~stdout ~stderr cmd in
      B0_log.debug begin fun m ->
        m ~header:(strf "SPAWN:%d" pid) "[%s]" (B0_cmd.to_string cmd)
      end;
      Ok pid
    with
    | Failure e -> Fds.close_all fds; spawn_err cmd e
    | Unix.Unix_error (e, _, _) -> Fds.close_all fds; spawn_err cmd (uerror e)

  let spawn
      ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
      ?(stderr = out_stderr) cmd
    =
    try
      spawn_low
        ~env:(spawn_env env) ~cwd:(spawn_cwd cwd) ~stdin ~stdout ~stderr cmd
    with
    | Unix.Unix_error (e, _, _) -> spawn_err cmd (uerror e)

  let rec collect ?(block = false) pid =
    let flags = if not block then Unix.[WNOHANG] else [] in
    match Unix.waitpid flags pid with
    | 0, _ -> Ok None
    | _, status ->
        let status = status_of_unix_status status in
        B0_log.debug begin fun m ->
          m ~header:(strf "COLLECT:%d" pid) "%a" pp_status status
        end;
        Ok (Some status)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> collect ~block pid
    | exception Unix.Unix_error (e, _, _) ->
        R.error_msgf "waitpid %d: %s" pid (uerror e)

  (* execv

     On Windows when Unix.execv[e] is invoked, control is returned to
     the controlling terminal when the child process starts (vs. child
     process terminates on POSIX). This entails all sort of weird
     behaviour. To workaround this, our execv[e] on Windows simply
     runs the program as a sub-process on which we waitpid(2) and then
     exit with the resulting status. *)

  let err_execv bin e = R.error_msgf "execv %s: %s" bin (uerror e)

  let _execv_win32 ~env bin args =
    let exit pid = match Unix.waitpid [] pid with
    | _, (Unix.WEXITED c) -> exit c
    | _, (Unix.WSIGNALED sg) ->
        Unix.(kill (getpid ()) sg);
        (* In case we don't get killed, exit with bash convention. *)
        exit (128 + sg)
    | _ -> assert false
    in
    try
      exit @@ match env with
      | None -> Unix.(create_process bin args stdin stdout stderr)
      | Some env ->
          let env = Array.of_list env in
          Unix.(create_process_env bin args env stdin stderr stderr)
    with
    | Unix.Unix_error (e, _, _) -> err_execv bin e

  let _execv_posix ~env bin args =
    try match env with
    | None -> Ok (Unix.execv bin args)
    | Some env ->
        let env = Array.of_list env in
        Ok (Unix.execve bin args env)
    with
    | Unix.Unix_error (e, _, _) -> err_execv bin e

  let _execv = if Sys.win32 then _execv_win32 else _execv_posix

  let execv ?env ?cwd f cmd =
    let cwd = match cwd with
    | None -> Ok ()
    | Some cwd -> Dir.set_current cwd
    in
    cwd >>= fun () ->
    _execv ~env (B0_fpath.to_string f) (Array.of_list @@ B0_cmd.to_list cmd)
end

(* B0 internals *)

module B0 = struct

  let trash_path p ~in_dir =
    let p = B0_fpath.to_string p in
    let dir = B0_fpath.(to_string @@ to_dir_path in_dir) in
    let rec loop n = match n with
    | 0 -> R.error_msgf "trash %s in %s: too many attempts" p dir
    | n ->
        let rand = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFFF in
        let dst = dir ^ Printf.sprintf "%07x" rand in
        try Ok (Unix.rename p dst) with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
        | Unix.Unix_error (e, _, _) ->
            R.error_msgf "trash %s in %s: %s" p dir (uerror e)
    in
    loop 10000

  let rm_rf p =
    let p = B0_fpath.to_string p in
    let rm_rf = match Sys.win32 with
    | true -> [| "cmd.exe"; "/c"; "rd"; "/s"; "/q"; p; |]
    | false -> [| "rm"; "-r"; "-f"; p |]
    in
    try
      let pid = Unix.(create_process rm_rf.(0) rm_rf stdin stdout stderr) in
      B0_log.debug begin fun m ->
        let cmd = B0_cmd.of_list @@ Array.to_list rm_rf in
        m ~header:(strf "SPAWN:%d" pid) "[%s]" (B0_cmd.to_string cmd)
      end;
      Ok pid
    with
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "remove file hierarchy: %s: %s" rm_rf.(0) (uerror e)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
