(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* A bit of randomness for functions that need unique filenames *)

let rand_gen = lazy (Random.State.make_self_init ())

(* String formatting *)

let strf = Format.asprintf
let failwithf fmt =
  Format.kasprintf (fun s -> raise_notrace (Failure s)) fmt

let uerror = Unix.error_message

(* Error handling *)

let apply f x ~finally y =
  let result = try f x with
  | e -> try finally y; raise e with _ -> raise e
  in
  finally y;
  result

module Env = struct

  (* Variables *)

  let var name = try Some (Unix.getenv name) with Not_found -> None
  let opt_var name ~absent = try Unix.getenv name with Not_found -> absent

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

module Path = struct
  let trash p ~in_dir =
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
end

module File = struct

  (* Famous file paths *)

  let null = B0_fpath.v (if Sys.os_type = "Win32" then "NUL" else "/dev/null")
  let dash = B0_fpath.v "-"
  let is_dash = B0_fpath.equal dash

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
        | true ->
            let s = Bytes.create len in
            (really_input ic s 0 len; Ok (Bytes.unsafe_to_string s))
        | false ->
            R.error_msgf
              "read %a: file too large (%d, max supported: %d)"
              B0_fpath.pp file len Sys.max_string_length
    in
    match with_ic file input () with
    | Ok (Ok _ as v) -> v
    | Ok (Error _ as e) -> e
    | Error _ as e -> e

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

  let rec open_tmp_path ?(mode = 0o600) p =
    let p = B0_fpath.to_string p in
    let rec loop n = match n with
    | 0 -> R.error_msgf "tmp file %s-X.tmp: too many attempts" p
    | n ->
        let file = p ^ rand_suff () in
        let flags =
          Unix.([O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC;])
        in
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

  let with_tmp_oc ?mode p f v =
    try
      open_tmp_path ?mode p >>= fun (file, fd) ->
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
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        R.error_msgf "%a: No such directory" B0_fpath.pp dir
    | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
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
    let err d msg =
      raise_notrace (Failure (strf "delete %a: %s" B0_fpath.pp d msg))
    in
    let try_unlink file = match Unix.unlink (B0_fpath.to_string file) with
    | () -> true
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.ENOENT -> true
        | Unix.EISDIR (* Linux *) | Unix.EPERM (* POSIX *) -> false
        | Unix.EACCES when Sys.win32 -> false (* FIXME need to check dir ? *)
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
      | Ok dir ->
          if B0_fpath.is_abs dir then Ok dir else
          R.error_msgf
            "get cwd: getcwd(3) returned a relative path: (%a)" B0_fpath.pp dir
      | Error _ ->
          R.error_msgf
            "get cwd: cannot parse %S to a path" p
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> current ()
    | Unix.Unix_error (e, _, _) ->
        R.error_msgf "get cwd: %s" (uerror e)

  let rec set_current dir =
    try Ok (Unix.chdir (B0_fpath.to_string dir)) with
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
    if Sys.os_type = "Win32" then from_env "TEMP" ~absent:B0_fpath.(v "./") else
    from_env "TMPDIR" ~absent:(B0_fpath.v "/tmp")

  let default_tmp = ref default_tmp_init
  let set_default_tmp p = default_tmp := p
  let default_tmp () = !default_tmp
end

module Cmd = struct

  (* which *)

  let default_path_sep = if Sys.win32 then ";" else ":"
  let path_dirs ?(sep = default_path_sep) path =
    B0_string.cuts ~empty:false ~sep path

  let which_lookup_needed =
    let tool_need_lookup_win32 cmd =
      B0_string.exists B0_fpath.char_is_dir_sep cmd
    in
    let tool_need_lookup_unix cmd =
      try ignore (B0_string.index cmd '/'); false with Not_found -> true
    in
    if Sys.win32 then tool_need_lookup_win32 else tool_need_lookup_unix

  let which_file ~dirs tool =
    let execable tool = match Unix.access tool [Unix.X_OK] with
    | () -> true
    | exception (Unix.Unix_error _) -> false
    in
    if tool = "" then None else
    match which_lookup_needed tool with
    | false -> if execable tool then Some tool else None
    | true ->
        let rec loop tool = function
        | [] -> None
        | d :: dirs ->
            let exec_path = match d.[String.length d - 1] with
            | c when B0_fpath.char_is_dir_sep c -> d ^ tool
            | _ -> Printf.sprintf "%s/%s" d tool in
            if execable exec_path then Some exec_path else loop tool dirs
        in
        loop tool dirs

  let which_raw tool =
    let dirs = match Unix.getenv "PATH" with
    | p -> path_dirs p
    | exception Not_found -> []
    in
    which_file ~dirs tool

  (* FIXME this was c&p from topkg and should not use Sys.command *)

  let err_empty_line = "no command, empty command line"

  let line ?stdout ?stderr cmd =
    let strf = Printf.sprintf in
    if B0_cmd.is_empty cmd then failwith err_empty_line else
    let cmd = List.rev_map Filename.quote (B0_cmd.to_rev_list cmd) in
    let cmd = String.concat " " cmd in
    let redirect fd f =
      strf " %d>%s" fd (Filename.quote @@ B0_fpath.to_string f)
    in
    let stdout = match stdout with None -> "" | Some f -> redirect 1 f in
    let stderr = match stderr with None -> "" | Some f -> redirect 2 f in
    let win_quote = if Sys.win32 then "\"" else "" in
    strf "%s%s%s%s%s" win_quote cmd stdout stderr win_quote

  type status = [ `Exited of int | `Signaled of int ]

  let exec ?stdout ?stderr cmd =
    try
      let line = line ?stdout ?stderr cmd in
      B0_log.debug (fun m -> m ~header:"EXEC" "@[<1>[%s]@]" line);
      Ok ((), (cmd, (`Exited (Sys.command line) :> status)))
    with Sys_error e | Failure e -> R.error_msg e

  let cmd_bin cmd =
    try List.hd (B0_cmd.to_list cmd) with
    | Failure _ -> failwith err_empty_line

  let exists cmd = match which_raw (cmd_bin cmd) with
  | Some _ -> Ok true
  | None -> Ok false
  | exception Failure e -> R.error_msg e

  let must_exist cmd = exists cmd >>= function
  | false -> R.error_msgf "%s: no such command" (cmd_bin cmd)
  | true -> Ok cmd

  let which cmd = match which_raw (cmd_bin cmd) with
  | Some v -> Ok (Some (B0_fpath.v v))
  | None -> Ok None
  | exception Failure e -> R.error_msg e

  (* Running commands *)

  type run_status = B0_cmd.t * status

  let success r = r >>= function
    | (v, (_, `Exited 0)) -> Ok v
    | (v, (cmd, `Exited c)) ->
        R.error_msgf "cmd [%s]: exited with %d" (B0_cmd.to_string cmd) c
    | (v, (_, `Signaled _)) -> assert false

  let run ?err:stderr cmd = exec ?stderr cmd |> success
  let run_status ?err:stderr cmd =
    exec ?stderr cmd >>= function ((), (_, st)) -> Ok st

  type run_out = { cmd : B0_cmd.t; err : B0_fpath.t option }

  let tmp () =
    try
      let f = Filename.temp_file (Filename.basename Sys.argv.(0)) "b0" in
      at_exit (fun () -> ignore (File.delete (B0_fpath.v f)));
      Ok (B0_fpath.v f)
    with Sys_error e -> R.error_msg e

  let out_string ?(trim = true) o =
    tmp ()
    >>= fun file -> exec ?stderr:o.err ~stdout:file o.cmd
    >>= fun ((), st) -> File.read file
    >>= fun out -> Ok ((if trim then String.trim out else out), st)

  let out_file stdout o = exec ?stderr:o.err ~stdout o.cmd
  let out_stdout o = exec ?stderr:o.err ?stdout:None o.cmd
  let to_string ?trim o = out_string ?trim o |> success
  let to_file stdout o = out_file stdout o |> success
  let to_null o = to_file File.null o
  let run_out ?err cmd = { cmd; err }

  (* Spawn *)

  let rec openfile fn mode perm = try Unix.openfile fn mode perm with
  | Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

  let rec close fd = try Unix.close fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> close fd

  let close_no_err fd = try close fd with e -> ()

  (* Fd utils *)

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

  type spawn_pid = int
  type spawn_stdio = [ `Fd of Unix.file_descr * bool | `File of B0_fpath.t ]

  let spawn_in fds = function
  | `Fd (fd, close) -> if close then Fds.add fd fds; fd
  | `File f ->
      try
        let f = B0_fpath.to_string f in
        let fd = openfile f Unix.[O_RDONLY] 0o644 in
        Fds.add fd fds; fd
      with Unix.Unix_error (e, _, _) ->
        failwithf "open input %a: %s" B0_fpath.pp f (uerror e)

  let spawn_out fds = function
  | `Fd (fd, close) -> if close then Fds.add fd fds; fd
  | `File f ->
      try
        let f = B0_fpath.to_string f in
        let fd = openfile f Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Fds.add fd fds; fd
      with Unix.Unix_error (e, _, _) ->
        failwithf "open output %a: %s" B0_fpath.pp f (uerror e)

  let rec spawn_getcwd () = try Unix.getcwd () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> spawn_getcwd ()

  let rec spawn_chdir cwd =
    try Unix.chdir cwd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> spawn_chdir cwd
    | Unix.Unix_error (e, _, _) ->
        failwithf "chdir %s: %s" cwd (uerror e)

  let log_spawn args =
    B0_log.debug (fun m ->
        args (fun pid cmd -> m ~header:(strf "SPAWN:%d" pid) "@[<1>[%s]@]" cmd))

  let spawn env ~cwd ~stdin ~stdout ~stderr cmd =
    let err cmd msg = R.error_msgf "cmd %s: %s" (B0_cmd.to_string cmd) msg in
    if B0_cmd.is_empty cmd then err cmd err_empty_line else
    let line = Array.of_list @@ B0_cmd.to_list cmd in
    let fds = Fds.empty () in
    try
      let stdin = spawn_in fds stdin in
      let stdout = spawn_out fds stdout in
      let stderr = spawn_out fds stderr in
      let cwd = B0_fpath.to_string cwd in
      let old_cwd = spawn_getcwd () in
      let change_cwd = old_cwd <> cwd in
      if change_cwd then spawn_chdir cwd;
      let pid = Unix.create_process_env line.(0) line env stdin stdout stderr in
      if change_cwd then spawn_chdir old_cwd; (* Pid zombie on fail, oh well. *)
      Fds.close_all fds;
      log_spawn (fun m -> m pid (B0_cmd.to_string cmd));
      Ok pid
    with
    | Failure e -> Fds.close_all fds; (* FIXME omit cmd (or not) *) err cmd e
    | Unix.Unix_error (e, _, _) -> Fds.close_all fds; err cmd (uerror e)

  let status_of_unix_status = function
  | Unix.WEXITED n -> `Exited n
  | Unix.WSIGNALED n -> `Signaled n
  | Unix.WSTOPPED _ -> assert false

  let pp_status ppf = function
  | `Exited n -> B0_fmt.pf ppf "@[exit [%d]@]" n
  | `Signaled n -> B0_fmt.pf ppf "@[signaled [%d]@]" n

  let rec collect ~block pid =
    let flags = if not block then Unix.[WNOHANG] else [] in
    match Unix.waitpid flags pid with
    | 0, _ -> Ok None
    | n, status ->
        let status = status_of_unix_status status in
        B0_log.debug
          (fun m -> m ~header:(strf "COLLECT:%d" pid) "%a" pp_status status);
        Ok (Some (n, status))
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.EINTR -> collect ~block pid
        | e -> R.error_msgf "wait %d: %s" pid (uerror e)

  let rm_rf p =
    let p = B0_fpath.to_string p in
    let rm_rf = match Sys.win32 with
    | true -> [| "rmdir"; p; "/S"; "/Q"; "/F" |]
    | false ->[| "rm"; "-r"; "-f"; p |]
    in
    try
      let pid = Unix.(create_process rm_rf.(0) rm_rf stdin stdout stderr) in
      log_spawn
        (fun m -> m pid (B0_cmd.(to_string @@ of_list (Array.to_list rm_rf))));
      Ok pid
    with
    | Unix.Unix_error (e, _, _) -> R.error_msgf "rm -rf %s: %s"  p (uerror e)


  (* exec *)

  let rec execv_raw bin args = try Ok (Unix.execv bin args) with
  | Unix.Unix_error (e, _, _) -> R.error_msgf "execv %s: %s" bin (uerror e)

  let rec execve_raw bin args ~env = try Ok (Unix.execve bin args env) with
  | Unix.Unix_error (e, _, _) -> R.error_msgf "execve %s: %s" bin (uerror e)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
