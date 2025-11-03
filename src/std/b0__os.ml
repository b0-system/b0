(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A bit of randomness for functions that need unique filenames *)

let rand_gen = lazy (Random.State.make_self_init ()) (* FIXME multicore *)

(* Error handling *)

let doing_exist_test = "Existence test:"
let err_seg_not_dir = "A segment of the path is not a directory"

let uerror = Unix.error_message

let err_op_path op path e = B0__fmt.str "%s %a: %s" op B0__fmt.code path e
let err_doing doing e = B0__fmt.str "%s: %s" doing e
let ferr file e = B0__fmt.error "%a: %s" B0__fpath.pp file e
let ffail file e = B0__fmt.failwith "%a: %s" B0__fpath.pp file e
let ffail_notrace file e =
  B0__fmt.failwith_notrace "%a: %s" B0__fpath.pp file e

module Mtime = struct
  external mtime_now_ns : unit -> Int64.t = "ocaml_b0_monotonic_now_ns"

  (* Monotonic clock *)

  let origin = mtime_now_ns ()
  let now () = B0__mtime.of_uint64_ns (mtime_now_ns ())
  let elapsed () =
    B0__mtime.Span.of_uint64_ns (Int64.sub (mtime_now_ns ()) origin)

  (* Monotonic time counter *)

  type counter = B0__mtime.t
  let counter () = B0__mtime.of_uint64_ns (mtime_now_ns ())
  let count c =
    B0__mtime.Span.of_uint64_ns
      (Int64.sub (mtime_now_ns ()) (B0__mtime.to_uint64_ns c))

  (* Sleep *)

  let sleep dur =
    let measure = counter () in
    let dur = B0__mtime.Span.to_float_ns dur *. 1e-9 in
    let () = try Unix.sleepf dur with Unix.Unix_error _ -> () in
    count measure
end

(* Handle the recursive dep with Log *)

let time ?(level = B0__log.Info) m f =
  let time = Mtime.counter () in
  let v = f () in
  let span = Mtime.count time in
  B0__log.kmsg (fun () -> v) level
    (fun w ->
       let header = Format.asprintf "%a" B0__mtime.Span.pp span in
       m v (w ~header))

let () = B0__log.set_time_func { time = time }

module Signal = struct
  let set_behaviour sg sg_behaviour = try Ok (Sys.signal sg sg_behaviour) with
  | Sys_error e -> Error e

  let set_behaviour_noerr sg sg_behaviour =
    try Sys.set_signal sg sg_behaviour with
    | Sys_error _ -> ()

  let with_behaviour sg b f = match set_behaviour sg b with
  | Error _ as e -> e
  | Ok prev ->
      let finally () = set_behaviour_noerr sg prev in
      Ok (Fun.protect ~finally f)
end

module Fd = struct
  let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)

  let rec openfile fn mode perm = try Unix.openfile fn mode perm with
  | Unix.Unix_error (EINTR, _, _) -> openfile fn mode perm

  let rec close fd = try Unix.close fd with
  | Unix.Unix_error (EINTR, _, _) -> close fd

  let rec close_noerr fd = try Unix.close fd with
  | Unix.Unix_error (EINTR, _, _) -> close_noerr fd
  | Unix.Unix_error (_, _, _) -> ()

  let apply ~close fd f =
    let close fd = try close fd with Unix.Unix_error _ -> () in
    match f fd with v -> close fd; v | exception e -> close fd; raise e

  let copy ?buf src ~dst =
    let rec unix_read fd b = try Unix.read fd b 0 (Bytes.length b) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd b
    in
    let rec unix_write fd s i l =
      let rec write fd s i l = try Unix.single_write fd s i l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s i l
      in
      let bc = write fd s i l in
      if bc < l then unix_write fd s (i + bc) (l - bc) else ()
    in
    let rec loop buf src dst = match unix_read src buf with
    | 0 -> ()
    | l -> unix_write dst buf 0 l; loop buf src dst
    in
    let buf = match buf with
    | Some b -> b
    | None -> Bytes.create unix_buffer_size
    in
    loop buf src dst

  let to_string fd =
    let b = Bytes.create unix_buffer_size in
    let acc = Buffer.create unix_buffer_size in
    let rec loop () = match Unix.read fd b 0 (Bytes.length b) with
    | 0 -> Buffer.contents acc
    | l -> Buffer.add_subbytes acc b 0 l; loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
    in
    loop ()

  let rec really_read fd b start len = match len <= 0 with
  | true -> ()
  | false ->
      match Unix.read fd b start len with
      | 0 -> failwith (err_doing "Reading" "Unexpected end of file")
      | r -> really_read fd b (start + r) (len - r)
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          really_read fd b start len

  let read_file file fd =
    try
      match Unix.lseek fd 0 Unix.SEEK_END with
      | exception Unix.Unix_error (Unix.ESPIPE, _, _) -> to_string fd
      | len when len > Sys.max_string_length ->
          B0__fmt.failwith_notrace
            "File to read too large: %d bytes, max supported: %d"
            len Sys.max_string_length
      | len ->
          let b = Bytes.create len in
          ignore (Unix.lseek fd 0 Unix.SEEK_SET);
          really_read fd b 0 len;
          Bytes.unsafe_to_string b
    with
    | Failure e -> B0__fmt.failwith_notrace "%s: %s" file e
    | Unix.Unix_error (e, _, _) ->
        B0__fmt.failwith_notrace
          "%s: %s" file (err_doing "Reading" (uerror e))

  module Set = struct (* Maintains a set of fds to close. *)
    module Fd = struct
      type t = Unix.file_descr
      let compare : t -> t -> int = compare
    end
    module S = Set.Make (Fd)
    type t = S.t ref
    let empty () = ref S.empty
    let rem fd s = s := S.remove fd !s
    let add fd s = s := S.add fd !s
    let close_all s = S.iter close_noerr !s; s := S.empty
    let close fd s =
      if S.mem fd !s then (close_noerr fd; s := S.remove fd !s)
  end
end

module Socket = struct
  let rec set_nonblock ~nonblock fd =
    try if nonblock then Unix.set_nonblock fd else Unix.clear_nonblock fd with
    | Unix.Unix_error (EINTR, _, _) -> set_nonblock ~nonblock fd

  let rec connect fd addr = match Unix.connect fd addr with
  | () -> Ok ()
  | exception Unix.Unix_error (EINTR, _, _) -> connect fd addr
  | exception Unix.Unix_error (e, _, _) ->
      B0__fmt.error "connect to %a: %s" B0__fmt.sockaddr addr (uerror e)

  let rec bind fd addr =
    try
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.bind fd addr;
      Ok ()
    with
    | Unix.Unix_error (EINTR, _, _) -> bind fd addr
    | Unix.Unix_error (e, _, _) ->
        B0__fmt.error "bind %a: %s" B0__fmt.sockaddr addr (uerror e)

  let rec listen ?(backlog = 128) fd = try Ok (Unix.listen fd backlog) with
  | Unix.Unix_error (EINTR, _, _) -> listen fd ~backlog
  | Unix.Unix_error (e, _, _) -> B0__fmt.error "listen: %s" (uerror e)

  let rec accept ~cloexec fd = try Ok (Unix.accept ~cloexec fd) with
  | Unix.Unix_error (EINTR, _, _) -> accept ~cloexec fd
  | Unix.Unix_error (e, _, _) -> B0__fmt.error "accept: %s" (uerror e)

  let rec for_endpoint ?(nonblock = false) ep stype = match ep with
  | `Fd fd ->
      begin match set_nonblock ~nonblock fd with
      | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
      | () -> Ok (fd, false, None)
      end
  | `Host (name, port) ->
      begin match Unix.gethostbyname name with
      | exception Not_found -> B0__fmt.error "%s: host not found" name
      | h ->
          let c = `Sockaddr (Unix.ADDR_INET (h.h_addr_list.(0), port)) in
          for_endpoint c stype
      end
  | `Sockaddr addr ->
      let domain = Unix.domain_of_sockaddr addr in
      match Unix.socket ~cloexec:true domain stype 0 with
      | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
      | fd ->
          match set_nonblock ~nonblock fd with
          | exception Unix.Unix_error (e, _, _) ->
              Fd.close_noerr fd; Error (uerror e)
          | () -> Ok (fd, true, Some addr)

  let connect_endpoint ?nonblock ep stype =
    Result.map_error (fun e -> B0__fmt.str "connect endpoint: %s" e) @@
    match for_endpoint ?nonblock ep stype with
    | Error _ as e -> e
    | Ok (fd, true, (Some addr)) ->
        begin match connect fd addr with
        | Ok () -> Ok (fd, true, addr)
        | Error _ as e -> Fd.close_noerr fd; e
        end;
    | Ok (fd, false, None) -> (* [ep] was (`Fd fd) check it is connected *)
        begin match Unix.getpeername fd with
        | addr -> Ok (fd, false, addr)
        | exception Unix.Unix_error (ENOTCONN, _, _) ->
            Error "provided file descriptor is not connected"
        | exception Unix.Unix_error (e, _, _) ->
            Error (uerror e)
        end
    | _ -> assert false

  let with_connected_endpoint ?nonblock ep stype f =
    Result.join @@ Signal.with_behaviour Sys.sigpipe Signal_ignore @@ fun () ->
    match connect_endpoint ?nonblock ep stype with
    | Error _ as err -> err
    | Ok (fd, close, addr) ->
        let finally () = if close then Fd.close_noerr fd in
        Ok (Fun.protect ~finally (fun () -> f fd addr))

  let listen_endpoint ?nonblock ?backlog ep stype =
    let maybe_listen ?backlog stype fd close addr = match stype with
    | Unix.SOCK_STREAM ->
        begin match listen ?backlog fd with
        | Error _ as e -> if close then Fd.close_noerr fd; e
        | Ok () -> Ok (fd, close, addr)
        end
    | _ -> Ok (fd, close, addr)
    in
    Result.map_error (fun e -> B0__fmt.str "listen endpoint: %s" e) @@
    match for_endpoint ?nonblock ep stype with
    | Error _ as e -> e
    | Ok (fd, true, (Some addr)) ->
        begin match bind fd addr with
        | Ok () ->
            (* In case [addr]'s port was 0 we get a dynamically allocated
               port. This retrieves the concrete address. *)
            begin match Unix.getsockname fd with
            | addr -> maybe_listen ?backlog stype fd true addr
            | exception Unix.Unix_error (e, _, _) ->
                Fd.close_noerr fd; Error (uerror e)
            end
        | Error _ as e -> e
        end;
    | Ok (fd, false, None) -> (* [ep] was (`Fd fd) check it is bound *)
        begin match Unix.getsockname fd with
        | ADDR_INET (_, 0) -> Error "provided file descriptor is not bound"
        | addr -> maybe_listen ?backlog stype fd false addr
        | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
        end
    | _ -> assert false

  let with_listening_endpoint ?nonblock ?backlog ep stype f =
    Result.join @@ Signal.with_behaviour Sys.sigpipe Signal_ignore @@ fun () ->
    match listen_endpoint ?nonblock ?backlog ep stype with
    | Error _ as err -> err
    | Ok (fd, close, addr) ->
        let finally () = if close then Fd.close_noerr fd in
        Ok (Fun.protect ~finally (fun () -> f fd addr))

  (* TODO we would like to have that in endpoint but it requires flying
     code around
     of code restruction. *)

  let err_wait ep e = B0__fmt.str "Wait on %a: %s" B0__net.Endpoint.pp ep e
  let endpoint_wait_connectable ?(socket_type = Unix.SOCK_STREAM) ~timeout ep =
    let open B0__result.Syntax in
    let relax = Mtime.sleep B0__mtime.Span.(1 * ms) in
    Result.map_error (err_wait ep) @@
    let rec loop ~deadline dur =
      let* fd, close, addr = for_endpoint ~nonblock:false ep socket_type in
      let finally () = if close then Fd.close_noerr fd in
      let* status = Fun.protect ~finally @@ fun () ->
        match addr with
        | None -> Error "no address to connect to"
        | Some addr ->
            match Unix.connect fd addr with
            | () -> Ok `Ready
            | exception Unix.(Unix_error (ECONNREFUSED, _, _)) -> Ok `Retry
            | exception Unix.Unix_error (e, _, _) ->
                Error (Unix.error_message e)
      in
      match status with
      | `Ready -> Ok `Ready
      | `Retry ->
          let count = Mtime.count dur in
          if B0__mtime.Span.is_shorter count ~than:deadline
          then (ignore (Mtime.sleep relax); loop ~deadline dur)
          else Ok `Timeout
    in
    loop ~deadline:timeout (Mtime.counter ())

  let endpoint_wait_connectable' ?socket_type ~timeout ep =
    match endpoint_wait_connectable ?socket_type ~timeout ep with
    | Error _ as e -> e
    | Ok `Ready -> Ok ()
    | Ok `Timeout ->
        let err =
          B0__fmt.str "timed out after %a" B0__mtime.Span.pp timeout
        in
        Error (err_wait ep err)

end

module Env = struct

  (* On Windows environment variables are case insensitive. Direct
     calls to [getenv] syscalls handle that. We need to care about
     it when we munge the results of [Unix.environment] into a
     string map. *)

  let normalize = if Sys.win32 then B0__string.Ascii.uppercase else Fun.id

  (* Variables *)

  type var_name = string

  let var ~empty_is_none name = match Unix.getenv name with
  | "" when empty_is_none -> None
  | v -> Some v
  | exception Not_found -> None

  let var' ~empty_is_none parse name = match var ~empty_is_none name with
  | None -> Ok None
  | Some v ->
      match parse v with
      | Error e -> B0__fmt.error "%s env: %s" name e
      | Ok v -> Ok (Some v)

  (* Assignements *)

  type assignments = string list

  let env_err e = B0__fmt.error "process environment: %s" e
  let current_assignments () =
    try Ok (Array.to_list (Unix.environment ())) with
    | Sys_error e -> env_err e
    | Unix.Unix_error (e, _, _) -> env_err (uerror e)

  let pp_assignments = B0__fmt.(vbox @@ list string)

  (* Process environment *)

  type binding = var_name (* not normalized *) * string
  type t = binding B0__string.Map.t (* map keys are normalized *)

  let empty = B0__string.Map.empty
  let fold f env acc =
    B0__string.Map.fold (fun _ (n, v) acc -> f n v acc) env acc

  let find var env =
    Option.map snd (B0__string.Map.find_opt (normalize var) env)

  let add var v env = B0__string.Map.add (normalize var) (var, v) env
  let remove var env = B0__string.Map.remove (normalize var) env
  let mem var env = B0__string.Map.mem (normalize var) env
  let override env ~by =
    if B0__string.Map.is_empty by then env else
    let lean_right _ l r = match r with
    | Some _ as v -> v
    | None -> match l with Some _ as v -> v | None -> assert false
    in
    B0__string.Map.merge lean_right env by

  let parse_assignments ?(init = B0__string.Map.empty) fold v =
    try
      let add acc assign = match B0__string.split_first ~sep:"=" assign with
      | Some (var, value) -> add var value acc
      | None ->
          B0__fmt.failwith_notrace
            "%S: cannot parse VAR=val assignement" assign
      in
      Ok (fold add init v)
    with
    | Failure e -> Error e

  let current () =
    match parse_assignments Array.fold_left (Unix.environment ()) with
    | Ok _ as v -> v
    | Error e -> env_err e
    | exception Sys_error e -> env_err e
    | exception Unix.Unix_error (e, _, _) -> env_err (uerror e)

  let of_assignments ?init l = parse_assignments ?init List.fold_left l
  let to_assignments env =
    let add var v acc = String.concat "=" [var; v] :: acc in
    fold add env []

  let pp ppf env = pp_assignments ppf (to_assignments env)
end

module Fs_base = struct
  let rec is_dir p = try (Unix.stat p).Unix.st_kind = Unix.S_DIR with
  | Unix.Unix_error (Unix.EINTR, _, _) -> is_dir p

  let rec is_symlink p = try (Unix.lstat p).Unix.st_kind = Unix.S_LNK with
  | Unix.Unix_error (Unix.EINTR, _, _) -> is_symlink p

  let rec unlink p = try Unix.unlink p with
  | Unix.Unix_error (Unix.EINTR,_, _) -> unlink p

  (* Directory operations. *)

  let dir_create ?(mode = 0o755) ~make_path dir =
    let create_op = "create directory" in
    let mkdir dir mode = Unix.mkdir (B0__fpath.to_string dir) mode in
    try
      let pmode = 0o755 in
      try Ok (mkdir dir mode; true) with
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          if is_dir (B0__fpath.to_string dir) then Ok false else
          ferr dir (err_doing create_op "Exists but not a directory")
      | Unix.Unix_error (Unix.ENOENT, _, _) when make_path ->
          let rec down = function
          | [] -> assert false
          | [dir] ->
              (try Ok (mkdir dir mode; true) with
              | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok false)
          | dir :: dirs ->
              match mkdir dir pmode with
              | () -> down dirs
              | exception Unix.Unix_error (Unix.EEXIST, _, _) -> down dirs
          in
          let rec up todo p = match mkdir p pmode with
          | () -> down todo
          | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
              up (p :: todo) (B0__fpath.parent p)
          in
          up [dir] (B0__fpath.parent dir)
    with
    | Unix.Unix_error (e, _, p) ->
        match String.equal (B0__fpath.to_string dir) p with
        | true -> ferr dir (err_doing create_op (uerror e))
        | false ->
            let perr = B0__fmt.str "%s: %s" p (uerror e) in
            ferr dir (err_doing create_op perr)

  let dir_delete ~recurse dir =
    let delete_op = "Delete directory" in
    let err e = B0__fmt.failwith_notrace "%a: %s" B0__fpath.pp dir e in
    let rec delete_symlink p =
      if is_symlink p then (unlink p; true) else false
    in
    let try_unlink file = match Unix.unlink (B0__fpath.to_string file) with
    | () -> true
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.ENOENT -> true
        | Unix.EISDIR (* Linux *) | Unix.EPERM (* POSIX *) -> false
        | Unix.EACCES when Sys.win32 ->
            (* This is what Unix.unlink returns on directories on Windows. *)
            false
        | e ->
            let ferr = B0__fmt.str "%a: %s" B0__fpath.pp file (uerror e) in
            err (err_doing delete_op ferr)
    in
    let rec delete_contents d dh todo = match Unix.readdir dh with
    | exception End_of_file -> d :: todo
    | ".." | "." -> delete_contents d dh todo
    | file ->
        let file = B0__fpath.(d / file) in
        if try_unlink file then delete_contents d dh todo else
        file :: d :: todo (* file is a dir we'll come back later for [d] *)
    in
    let rec try_delete d todo =
      match Unix.opendir (B0__fpath.to_string d) with
      | dh ->
          let dirs = match delete_contents d dh todo with
          | dirs -> Unix.closedir dh; dirs
          | exception e -> Unix.closedir dh; raise e
          in
          doit dirs
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOENT | Unix.ENOTDIR -> doit todo
          | e ->
              let derr = B0__fmt.str "%a: %s" B0__fpath.pp d (uerror e) in
              err (err_doing delete_op derr)
    and doit = function
    | [] -> ()
    | d :: ds ->
        match Unix.rmdir (B0__fpath.to_string d) with
        | () -> doit ds
        | exception Unix.Unix_error (e, _, _) ->
            match e with
            | Unix.ENOTEMPTY -> try_delete d ds
            | Unix.ENOENT | Unix.ENOTDIR -> doit ds
            | e ->
                let derr = B0__fmt.str "%a: %s" B0__fpath.pp d (uerror e) in
                err (err_doing delete_op derr)
    in
    try match Unix.rmdir (B0__fpath.to_string dir) with
    | () -> Ok true
    | exception Unix.Unix_error (e, _, _) ->
        match e with
        | Unix.ENOTEMPTY when recurse -> Ok (try_delete dir []; true)
        | Unix.ENOENT -> Ok false
        | Unix.ENOTDIR ->
            begin try
              if delete_symlink (B0__fpath.to_string dir) then Ok true else
              err (err_doing delete_op (uerror Unix.ENOTDIR))
            with
            | Unix.Unix_error (e,_,_) -> err (err_doing delete_op (uerror e))
            end
        | e -> err (err_doing delete_op (uerror e))
    with
    | Failure e -> Result.error e

  (* Handling forced file operations *)

  let err_force p = ferr p "Path exists"

  let rec handle_force ~force file =
    if force then Ok () else
    try ignore (Unix.lstat (B0__fpath.to_string file)); err_force file with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr file err_seg_not_dir
    | Unix.Unix_error (Unix.EINTR, _, _) -> handle_force ~force file
    | Unix.Unix_error (e, _, _) ->
        ferr file (err_doing doing_exist_test (uerror e))

  let rec handle_force_open_fdout
    ?(flags = Unix.[O_WRONLY; O_CREAT; O_SHARE_DELETE; O_CLOEXEC; O_TRUNC])
    ~force ~make_path ~mode file
    =
    let fls = if force then flags else Unix.O_EXCL :: flags in
    match Unix.openfile (B0__fpath.to_string file) fls mode with
    | fd -> Ok fd
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> err_force file
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        handle_force_open_fdout ~flags ~force ~make_path ~mode file
    | exception Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
        begin match dir_create ~make_path (B0__fpath.parent file) with
        | Error e -> ferr file e
        | Ok false (* existed *) -> ferr file (uerror e)
        | Ok true (* created *) ->
            handle_force_open_fdout ~flags ~force ~make_path ~mode file
        end
    | exception Unix.Unix_error (e, _, _) -> ferr file (uerror e)

  (* Path operations *)

  let rec path_exists p =
    try (ignore (Unix.stat (B0__fpath.to_string p)); Ok true) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_exists p
    | Unix.Unix_error (e, _, _) ->
        ferr p (err_doing doing_exist_test (uerror e))

  let rec path_get_mode p =
    try Ok ((Unix.stat @@ B0__fpath.to_string p).Unix.st_perm) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_get_mode p
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
    | Unix.Unix_error (e, _, _) ->
        ferr p (err_doing "Getting file mode" (uerror e))

  let rec path_set_mode p m =
    try Ok (Unix.chmod (B0__fpath.to_string p) m) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_set_mode p m
    | Unix.Unix_error (e, _, _) ->
        ferr p (err_doing "Setting file mode" (uerror e))

  let rec path_delete ~recurse p =
    try Ok (Unix.unlink (B0__fpath.to_string p); true) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_delete ~recurse p
    | Unix.Unix_error ((Unix.EPERM | Unix.EISDIR), _, _) ->
        dir_delete ~recurse p
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "Deleting" (uerror e))

  let rec path_rename ~force ~make_path src ~dst:p =
    let err e =
      B0__fmt.error "rename %a to %a: %s"
        B0__fpath.pp src B0__fpath.pp p e
    in
    match handle_force ~force p with
    | Error e -> err e
    | Ok () ->
        try Ok (Unix.rename
                  (B0__fpath.to_string src) (B0__fpath.to_string p))
        with
        | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
            begin match dir_create ~make_path (B0__fpath.parent p) with
            | Error e -> err e
            | Ok false (* existed *) -> err (uerror e)
            | Ok true (* created *) ->
                path_rename ~force ~make_path src ~dst:p
            end
        | Unix.Unix_error (Unix.EINTR, _, _) ->
            path_rename ~force ~make_path src ~dst:p
        | Unix.Unix_error (e, _, _) -> err (uerror e)

  let rec path_stat p = try Ok (Unix.stat (B0__fpath.to_string p)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> path_stat p
  | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
  | Unix.Unix_error (e, _, _) -> ferr p (err_doing "stat" (uerror e))

  (* Links *)

  let rec symlink ~force ~make_path ~src p =
    let err e =
      B0__fmt.error "symlink %a to %a: %s"
        B0__fpath.pp src B0__fpath.pp p e
    in
    try
      Ok (Unix.symlink (B0__fpath.to_string src) (B0__fpath.to_string p))
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
        begin match unlink (B0__fpath.to_string p) with
        | exception Unix.Unix_error (ENOENT, _, _) ->
            symlink ~force ~make_path ~src p
        | () -> symlink ~force ~make_path ~src p
        | exception Unix.Unix_error (e, _, _) -> err (uerror e)
        end
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR as e), _, _)
      when make_path ->
        begin match dir_create ~make_path (B0__fpath.parent p) with
        | Error e -> ferr p (err_doing "Creating path" e)
        | Ok false (* existed *) -> err (uerror e)
        | Ok true (* created *) -> symlink ~force ~make_path ~src p
        end
    | Unix.Unix_error (Unix.EINTR, _, _) -> symlink ~force ~make_path ~src p
    | Unix.Unix_error (e, _, _) -> err (uerror e)

  let rec symlink_link p =
    try
      let l = Unix.readlink (B0__fpath.to_string p) in
      match B0__fpath.of_string l with
      | Ok _ as v -> v
      | Error e -> ferr p (err_doing "Reading symlink" e)
    with
    | Unix.Unix_error (Unix.EINVAL, _, _) -> ferr p "Not a symbolic link"
    | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_link p
    | Unix.Unix_error (e, _, _) -> ferr p (uerror e)

  let rec symlink_stat p = try Ok (Unix.lstat (B0__fpath.to_string p)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_stat p
  | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
  | Unix.Unix_error (e, _, _) -> ferr p (err_doing "lstat" (uerror e))

  let copy_symlink ~force ~make_path src ~dst =
    Result.bind (symlink_link src) @@ fun src ->
    symlink ~force ~make_path ~src dst
end

module Tmp = struct
  let delete_file file = try Fs_base.unlink (B0__fpath.to_string file) with
  | Unix.Unix_error (e, _, _) -> ()

  let files = ref B0__fpath.Set.empty
  let add_file file = files := B0__fpath.Set.add file !files
  let rem_file file =
    delete_file file; files := B0__fpath.Set.remove file !files

  let delete_dir dir = ignore (Fs_base.dir_delete ~recurse:true dir)
  let dirs = ref B0__fpath.Set.empty
  let add_dir dir = dirs := B0__fpath.Set.add dir !dirs
  let rem_dir dir = delete_dir dir; dirs := B0__fpath.Set.remove dir !dirs

  let cleanup () =
    B0__fpath.Set.iter delete_file !files;
    B0__fpath.Set.iter delete_dir !dirs

  let () = at_exit cleanup

  let default_dir =
    let tmp_from_env var ~default =
      Option.value ~default (Env.var ~empty_is_none:true var)
    in
    let dir = match Sys.win32 with
    | true -> tmp_from_env "TEMP" ~default:"./"
    | false -> tmp_from_env "TMPDIR" ~default:"/tmp/"
    in
    ref (B0__fpath.ensure_trailing_dir_sep (B0__fpath.v dir))

  type name = (string -> string, unit, string) format
  let default_name = format_of_string "tmp-%s"

  let rand_num () = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFF
  let rand_str () = Printf.sprintf "%06x" (rand_num ())
  let tmp_path dir name rand =
    let dir = B0__fpath.to_string dir in
    match dir.[String.length dir - 1] = B0__fpath.natural_dir_sep_char with
    | true -> Printf.sprintf ("%s" ^^ name) dir rand
    | false ->
        Printf.sprintf ("%s%c" ^^ name)
          dir B0__fpath.natural_dir_sep_char rand

  let err dir name rand e =
    B0__fmt.error "tmp file %s: %s" (tmp_path dir name rand) e

  let err_too_many dir name =
    err dir name "XXXXXX" "Too many attempts to create"

  let attempts = 10000
  let open'
      ?(flags = Unix.[O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC])
      ?(mode = 0o600) ?(make_path = true) ?dir ?(name = default_name) ()
    =
    let dir = match dir with None -> !default_dir | Some d -> d in
    let dir = B0__fpath.ensure_trailing_dir_sep dir in
    let rec loop n = match n with
    | 0 -> err_too_many dir name
    | n ->
        let rand = rand_str () in
        try
          let file = tmp_path dir name rand in
          let fd = Unix.openfile file flags mode in
          let file = B0__fpath.v file in
          (add_file file; Ok (file, fd))
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
        | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
            begin match Fs_base.dir_create ~make_path dir with
            | Error e -> err dir name rand e
            | Ok true (* created *) -> loop n
            | Ok false (* existed *) -> err dir name rand (uerror e)
            end
        | Unix.Unix_error (e, _, _) -> err dir name rand (uerror e)
    in
    loop attempts

  let mkdir
      ?(mode = 0o700) ?(make_path = true) ?dir ?(name = default_name) ()
    =
    let dir = match dir with None -> !default_dir | Some d -> d in
    let rec loop n = match n with
    | 0 -> err_too_many dir name
    | n ->
        let rand = rand_str () in
        try
          let tdir = tmp_path dir name rand in
          let () = Unix.mkdir tdir mode in
          let tdir = B0__fpath.v tdir in
          (add_dir tdir; Ok tdir)
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
        | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
            begin match Fs_base.dir_create ~make_path dir with
            | Error e -> err dir name rand e
            | Ok true (* created *) -> loop n
            | Ok false (* existed *) -> err dir name rand (uerror e)
            end
        | Unix.Unix_error (e, _, _) -> err dir name rand (uerror e)
    in
    loop attempts

  let path ?(make_path = true) ?dir ?(name = format_of_string "tmp-%s") () =
    let dir = match dir with None -> !default_dir | Some d -> d in
    let rec loop n = match n with
    | 0 -> err_too_many dir name
    | n ->
        let rand = rand_str () in
        let file = tmp_path dir name rand in
        match Unix.access file [Unix.F_OK] with
        | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
            Ok (B0__fpath.v file)
        | exception Unix.Unix_error (e, _, _) -> err dir name rand (uerror e)
        | _ -> loop (n - 1)
    in
    if not make_path then loop attempts else
    match Fs_base.dir_create ~make_path dir with
    | Error _ as e -> e
    | Ok _ -> loop attempts
end

module File = struct
  let err_not_file = "Not a regular file"
  let err_no_such_file = "No such file"

  let channel_apply ~close c f =
    let close c = try close c with Sys_error _ -> () in
    match f c with v -> close c; v | exception e -> close c; raise e

  (* Famous file paths *)

  let is_dash = B0__fpath.equal B0__fpath.dash

  (* Existence *)

  let rec exists file =
    match (Unix.stat (B0__fpath.to_string file)).Unix.st_kind with
    | Unix.S_REG -> Ok true
    | _ -> Ok false
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
        ferr file err_seg_not_dir
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> exists file
    | exception Unix.Unix_error (e, _, _) ->
        ferr file (err_doing doing_exist_test (uerror e))

  let rec must_exist file =
    match (Unix.stat (B0__fpath.to_string file)).Unix.st_kind with
    | Unix.S_REG -> Ok ()
    | _ -> ferr file err_not_file
    | exception Unix.Unix_error (ENOENT, _,_) -> ferr file err_no_such_file
    | exception Unix.Unix_error (ENOTDIR, _, _) -> ferr file err_seg_not_dir
    | exception Unix.Unix_error (EINTR, _, _) -> must_exist file
    | exception Unix.Unix_error (e, _, _) ->
        ferr file (err_doing doing_exist_test (uerror e))

  let is_executable' file = match Unix.access file [Unix.X_OK] with
  | () -> true
  | exception Unix.Unix_error _ -> false

  let is_executable file = is_executable' (B0__fpath.to_string file)

  (* Deleting and truncating *)

  let rec delete path =
    let op = "Delete file" in
    let unlink p = match Unix.unlink p with
    | () -> Ok true
    | exception Unix.Unix_error (EINTR, _, _) -> delete path
    | exception Unix.Unix_error (ENOENT, _, _) -> (* could race *) Ok false
    | exception Unix.Unix_error (e, _, _) -> Error (err_op_path op p (uerror e))
    in
    let p = B0__fpath.to_string path in
    match (Unix.lstat p).Unix.st_kind with
    | Unix.S_REG -> (* file *) unlink p
    | Unix.S_LNK ->
        begin match (Unix.stat p).Unix.st_kind with
        | Unix.S_REG -> (* link to a file *) unlink p
        | _ -> Error (err_op_path op p err_not_file)
        | exception Unix.Unix_error (EINTR, _, _) -> delete path
        | exception Unix.Unix_error (ENOENT, _, _) -> (* dangling *) unlink p
        | exception Unix.Unix_error (e, _, _) ->
            Error (err_op_path op p (uerror e))
        end
    | _ -> Error (err_op_path op p err_not_file)
    | exception Unix.Unix_error (EINTR, _, _) -> delete path
    | exception Unix.Unix_error (ENOENT, _, _) -> Ok false
    | exception Unix.Unix_error (e, _, _) -> Error (err_op_path op p (uerror e))

  let rec truncate file size =
    try Ok (Unix.truncate (B0__fpath.to_string file) size) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> truncate file size
    | Unix.Unix_error (e, _, _) ->
        ferr file (err_doing "Truncating" (uerror e))

  (* Hard links *)

  let rec link ~force ~make_path ~src file =
    let err e =
      B0__fmt.error "link %a to %a: %s"
        B0__fpath.pp src B0__fpath.pp file e
    in
    try
      Ok (Unix.link (B0__fpath.to_string src) (B0__fpath.to_string file))
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
        begin match Fs_base.unlink (B0__fpath.to_string file) with
        | exception Unix.Unix_error (ENOENT, _, _) ->
            link ~force ~make_path ~src file
        | () -> link ~force ~make_path ~src file
        | exception Unix.Unix_error (e, _, _) -> err (uerror e)
        end
    | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR as e), _, _)
      when make_path ->
        begin match Fs_base.dir_create ~make_path (B0__fpath.parent file) with
        | Error e -> ferr file (err_doing "Creating path" e)
        | Ok false (* existed *) -> err (uerror e)
        | Ok true (* created *) -> link ~force ~make_path ~src file
        end
    | Unix.Unix_error (Unix.EINTR, _, _) -> link ~force ~make_path ~src file
    | Unix.Unix_error (e, _, _) -> err (uerror e)

  (* Reads *)

  let read_with_ic file f =
    try
      let ic, close = match is_dash file with
      | true -> stdin, fun _ -> ()
      | false -> open_in_bin (B0__fpath.to_string file), close_in
      in
      Ok (channel_apply ~close ic f)
    with
    | Sys_error e -> Result.error e

  let read_with_fd file f =
    try
      let fdin, close = match is_dash file with
      | true -> Unix.stdin, (fun _ -> ())
      | false ->
          Fd.openfile (B0__fpath.to_string file) Unix.[O_RDONLY] 0, Unix.close
      in
      Ok (Fd.apply ~close fdin f)
    with
    | Unix.Unix_error (e, _, _) -> ferr file (uerror e)

  let in_channel_to_string fd =
    let b = Bytes.create Fd.unix_buffer_size in
    let acc = Buffer.create Fd.unix_buffer_size in
    let rec loop () = match input fd b 0 (Bytes.length b) with
    | 0 -> Buffer.contents acc
    | n -> Buffer.add_subbytes acc b 0 n; loop ()
    in
    loop ()

  let read_stdin () = in_channel_to_string stdin
  let read_file file ic = match in_channel_length ic with
  | exception Sys_error e -> in_channel_to_string ic
  | len when len > Sys.max_string_length ->
      B0__fmt.failwith_notrace
        "File to read too large: %d bytes, max supported: %d"
        len Sys.max_string_length
  | len ->
      let s = Bytes.create len in
      really_input ic s 0 len;
      Bytes.unsafe_to_string s

  let read file =
    let input c = if c == stdin then read_stdin () else read_file file c in
    try read_with_ic file input with
    | Failure e | Sys_error e -> ferr file e

  (* Writes *)

  let with_tmp_fd ?flags ?mode ?make_path ?dir ?name f =
    Result.bind (Tmp.open' ?flags ?mode ?make_path ?dir ?name ()) @@
    fun (file, fd) ->
    let delete_close fd = Tmp.rem_file file; Unix.close fd in
    Ok (Fd.apply ~close:delete_close fd (f file))

  let open_tmp_fd = Tmp.open'

  let with_tmp_oc ?flags ?mode ?make_path ?dir ?name f =
    Result.bind (Tmp.open' ?flags ?mode ?make_path ?dir ?name ()) @@
    fun (file, fd) ->
    let oc = Unix.out_channel_of_descr fd in
    let delete_close oc = Tmp.rem_file file; close_out oc in
    Ok (channel_apply ~close:delete_close oc (f file))

  let rec rename_tmp src dst =
    try
      Ok (Unix.rename (B0__fpath.to_string src) (B0__fpath.to_string dst))
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> rename_tmp src dst
    | Unix.Unix_error (e, _, _) ->
        let r =
          B0__fmt.str "renaming %a to %a"
            B0__fpath.pp src B0__fpath.pp dst
        in
        Result.error (err_doing r (uerror e))

  let write_op = "Writing"

  let write_with_fd_atomic ~mode ~force ~make_path ~file f =
    Result.bind (Fs_base.handle_force ~force file) @@ fun () ->
    let do_write tmp tmp_oc = match f tmp_oc with
    | Error _ as v -> Ok v
    | Ok _ as v -> Result.map (fun () -> v) (rename_tmp tmp file)
    in
    match
      with_tmp_fd ~mode ~make_path ~dir:(B0__fpath.parent file) do_write
    with
    | Ok v -> v
    | Error e -> ferr file (err_doing write_op e)

  let write_with_fd
      ?(atomic = true) ?(mode = 0o644) ~force ~make_path file f
    =
    match is_dash file with
    | true -> Ok (Fd.apply ~close:(fun _ -> ()) Unix.stdout f)
    | false when atomic ->
        write_with_fd_atomic ~mode ~force ~make_path ~file f
    | false ->
        Result.bind
          (Fs_base.handle_force_open_fdout ~force ~make_path ~mode file) @@
        fun fd -> Ok (Fd.apply ~close:Unix.close fd f)

  let write_with_oc_atomic ~mode ~force ~make_path ~file f =
    Result.bind (Fs_base.handle_force ~force file) @@ fun () ->
    let do_write tmp tmp_oc = match f tmp_oc with
    | Error _ as v -> Ok v
    | Ok _ as v -> Result.map (fun () -> v) (rename_tmp tmp file)
    in
    match
      with_tmp_oc ~mode ~make_path ~dir:(B0__fpath.parent file) do_write
    with
    | Ok v -> v
    | Error e -> ferr file (err_doing write_op e)

  let write_with_oc
      ?(atomic = true) ?(mode = 0o644) ~force ~make_path file f
    =
    match is_dash file with
    | true -> Ok (channel_apply ~close:(fun _ -> ()) stdout f)
    | false when atomic ->
        write_with_oc_atomic ~mode ~force ~make_path ~file f
    | false ->
        Result.bind
          (Fs_base.handle_force_open_fdout ~force ~make_path ~mode file) @@
        fun fd ->
        let oc = Unix.out_channel_of_descr fd in
        Ok (channel_apply ~close:close_out oc f)

  let write ?atomic ?mode ~force ~make_path file data =
    let out data oc = Ok (output_string oc data) in
    try
      Result.join @@
      write_with_oc ?atomic ?mode ~force ~make_path file (out data)
    with
    | Sys_error e -> ferr file e

  let copy ?atomic ?mode ~force ~make_path src ~dst:file =
    let err e =
      B0__fmt.str "copy %a to %a: %s"
        B0__fpath.pp src B0__fpath.pp file e
    in
    Result.map_error err @@ Result.join @@
    read_with_fd src @@ fun fdi ->
    try match is_dash file with
    | true -> Ok (Fd.copy fdi ~dst:Unix.stdout)
    | false ->
        let mode = match mode with
        | None -> Fs_base.path_get_mode src
        | Some m -> Ok m
        in
        Result.join @@ Result.bind mode @@ fun mode ->
        write_with_fd ?atomic ~mode ~force ~make_path file @@ fun fdo ->
        Ok (Fd.copy fdi ~dst:fdo)
    with
    | Unix.Unix_error (e, _, arg) -> B0__fmt.error "%s: %s" arg (uerror e)

  let copy_to_dir ?atomic ?mode ~force ~make_path ?src_root src ~dir =
      let dst = match src_root with
      | None -> B0__fpath.(dir / B0__fpath.basename src)
      | Some src_root -> B0__fpath.reroot ~src_root ~dst_root:dir src
      in
      copy ?atomic ?mode ~force ~make_path src ~dst
end

module Dir = struct

  let err_not_dir = "Not a directory"
  let err_no_such_dir = "No such directory"

  let rec readdir ~dotfiles dir =
    let is_dot_file s = String.length s <> 0 && s.[0] = '.' in
    let rec loop ~dotfiles dir dh acc = match Unix.readdir dh with
    | exception End_of_file -> acc
    | ".." | "." -> loop ~dotfiles dir dh acc
    | n when is_dot_file n && not dotfiles -> loop ~dotfiles dir dh acc
    | n when B0__fpath.is_segment n -> loop ~dotfiles dir dh (n :: acc)
    | n -> ffail dir (B0__fmt.str "%S: Invalid file name" n)
    in
    let dh = Unix.opendir (B0__fpath.to_string dir) in
    match loop ~dotfiles dir dh [] with
    | fs -> Unix.closedir dh; fs
    | exception e ->
        (try Unix.closedir dh with Unix.Unix_error (_, _, _) -> ());
        raise e

  (* Existence *)

  let rec exists dir =
    match (Unix.stat @@ B0__fpath.to_string dir).Unix.st_kind with
    | Unix.S_DIR -> Ok true
    | _ -> Ok false
    | exception Unix.Unix_error (ENOENT, _, _) -> Ok false
    | exception Unix.Unix_error (ENOTDIR, _, _) ->
        ferr dir err_seg_not_dir
    | exception Unix.Unix_error (EINTR, _, _) -> exists dir
    | exception Unix.Unix_error (e, _, _) ->
        ferr dir (err_doing doing_exist_test (uerror e))

  let rec must_exist dir =
    match (Unix.stat @@ B0__fpath.to_string dir).Unix.st_kind with
    | Unix.S_DIR -> Ok ()
    | _ -> ferr dir err_not_dir
    | exception Unix.Unix_error (ENOENT, _, _) -> ferr dir err_no_such_dir
    | exception Unix.Unix_error (ENOTDIR, _, _) -> ferr dir err_seg_not_dir
    | exception Unix.Unix_error (EINTR, _, _) -> must_exist dir
    | exception Unix.Unix_error (e, _, _) ->
        ferr dir (err_doing doing_exist_test (uerror e))

  (* Creating *)

  let create = Fs_base.dir_create

  (* Deleting *)

  let rec delete ~recurse dir =
    let op = "Delete directory" in
    let unlink p = match Unix.unlink p with
    | () -> Ok true
    | exception Unix.Unix_error (EINTR, _, _) -> delete ~recurse dir
    | exception Unix.Unix_error (ENOENT, _, _) -> (* could race *) Ok false
    | exception Unix.Unix_error (e, _, _) -> Error (err_op_path op p (uerror e))
    in
    let p = B0__fpath.to_string dir in
    match (Unix.lstat p).Unix.st_kind with
    | Unix.S_DIR -> (* dir *) Fs_base.dir_delete ~recurse dir
    | Unix.S_LNK ->
        begin match (Unix.stat p).Unix.st_kind with
        | Unix.S_DIR -> (* link to a dir *) unlink p
        | _ -> Error (err_op_path op p err_not_dir)
        | exception Unix.Unix_error (EINTR, _, _) -> delete ~recurse dir
        | exception Unix.Unix_error (ENOENT, _, _) -> (* dangling *) unlink p
        | exception Unix.Unix_error (e, _, _) ->
            Error (err_op_path op p (uerror e))
        end
    | _ -> Error (err_op_path op p err_not_dir)
    | exception Unix.Unix_error (EINTR, _, _) -> delete ~recurse dir
    | exception Unix.Unix_error (ENOENT, _, _) -> Ok false
    | exception Unix.Unix_error (e, _, _) -> Error (err_op_path op p (uerror e))

  let rec delete_contents dir =
    let op = "Delete contents of" in
    let p = B0__fpath.to_string dir in
    match (Unix.stat p).Unix.st_kind with
    | Unix.S_DIR ->
        begin match readdir ~dotfiles:true dir with
        | exception Failure e -> Error (err_op_path op p e)
        | [] -> Ok false
        | els ->
            let delete el =
              Result.map ignore @@
              Fs_base.path_delete ~recurse:true B0__fpath.(dir / el)
            in
            match B0__list.iter_stop_on_error delete els with
            | Error e -> Error (err_op_path op p e)
            | Ok () -> Ok true
        end
    | _ -> Error (err_op_path op p err_not_dir)
    | exception Unix.Unix_error (EINTR, _, _) ->
        delete_contents dir
    | exception Unix.Unix_error (ENOENT, _, _) ->
        Error (err_op_path op p err_no_such_dir)
    | exception Unix.Unix_error (e, _, _) ->
        Error (err_op_path op p (uerror e))

  (* Contents *)

  let rec stat p = try (Unix.stat @@ B0__fpath.to_string p) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> stat p

  let rec lstat p = try (Unix.lstat @@ B0__fpath.to_string p) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> lstat p

  let fold_no_rec ~filter ~rel ~dotfiles ~follow_symlinks dir f acc =
    let rec loop stat f acc adir = function
    | [] -> Ok acc
    | n :: ns ->
        let full = B0__fpath.(adir / n) in
        match stat full with
        | st ->
            begin match st.Unix.st_kind with
            | Unix.S_DIR ->
                if filter = `Non_dir then loop stat f acc adir ns else
                let p = if rel then B0__fpath.v n else full in
                loop stat f (f st n p acc) adir ns
            | _ when filter <> `Dir ->
                let p = if rel then B0__fpath.v n else full in
                loop stat f (f st n p acc) adir ns
            | _ ->
                loop stat f acc adir ns
            end
        | exception Unix.Unix_error ((ENOENT|ENOTDIR|EBADF|EPERM), _, _) ->
            loop stat f acc adir ns
    in
    let stat = if follow_symlinks then stat else lstat in
    loop stat f acc dir (readdir ~dotfiles dir)

  let fold_rec ~prune_dir ~filter ~rel ~dotfiles ~follow_symlinks dir f acc =
    let rec loop stat todo adir rdir f acc = function
    | [] ->
        begin match todo with
        | (dir, rdir, ns) :: todo -> loop stat todo dir rdir f acc ns
        | [] -> Ok acc
        end
    | n :: ns ->
        let full = B0__fpath.(adir / n) in
        begin match stat full with
        | st ->
            begin match st.Unix.st_kind with
            | Unix.S_DIR ->
                let rp = match rdir with
                | None -> B0__fpath.v n | Some rdir -> B0__fpath.(rdir / n)
                in
                let p = if not rel then full else rp in
                if prune_dir st n p acc
                then loop stat todo adir rdir f acc ns else
                let acc = if filter = `Non_dir then acc else f st n p acc in
                let todo = (adir, rdir, ns) :: todo in
                loop stat todo full (Some rp) f acc (readdir ~dotfiles full)
            | _ when filter <> `Dir ->
                let p = if not rel then full else match rdir with
                | None -> B0__fpath.v n | Some rdir -> B0__fpath.(rdir / n)
                in
                loop stat todo adir rdir f (f st n p acc) ns
            | _ ->
                loop stat todo adir rdir f acc ns
            end
        | exception Unix.Unix_error ((ENOENT|ENOTDIR|EBADF|EPERM), _, _) ->
            loop stat todo adir rdir f acc ns
        end
    in
    let stat = if follow_symlinks then stat else lstat in
    loop stat [] dir None f acc (readdir ~dotfiles dir)

  let _fold
      ~(filter : [`Any | `Non_dir | `Dir]) ?(rel = false) ?(dotfiles = false)
      ?(follow_symlinks = true) ?(prune_dir = fun _ _ _ _ -> false) ~recurse
      f dir acc
    =
    let listing_op = "Listing" in
    try
      let fold = if recurse then fold_rec ~prune_dir else fold_no_rec in
      fold ~filter ~rel ~dotfiles ~follow_symlinks dir f acc
    with
    | Failure e -> ferr dir (err_doing listing_op e)
    | Unix.Unix_error (e, _, ep) ->
        if String.equal (B0__fpath.to_string dir) ep
        then ferr dir (err_doing listing_op @@ uerror e) else
        ferr dir (err_doing listing_op @@ B0__fmt.str "%s: %s" ep (uerror e))

  let fold ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse f dir acc =
    _fold ~filter:`Any ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
      f dir acc

  let fold_files
      ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse f dir acc
    =
    _fold ~filter:`Non_dir ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
      f dir acc

  let fold_dirs
      ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse f dir acc =
    _fold ~filter:`Dir ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
      f dir acc

  let path_list stat _ f acc = match stat.Unix.st_kind with
  | Unix.S_DIR -> B0__fpath.ensure_trailing_dir_sep f :: acc
  | _ -> f :: acc

  let prune_denied _ _ p _ =
    try (Unix.access (B0__fpath.to_string p) Unix.[R_OK; X_OK]; false) with
    | Unix.Unix_error ((EACCES | EPERM (* may happen *)), _, _) -> true

  let contents
      ?(kind = `All) ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse dir
    =
    let filter = match kind with
    | `All -> `Any | `Files -> `Non_dir | `Dirs -> `Dir
    in
    _fold ~filter ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
      path_list dir []

  (* copy *)

  let copy
      ?(rel = true) ?(atomic = true) ?(follow_symlinks = true)
      ?(prune = fun _ _ _ -> false) ~make_path ~recurse src ~dst
    =
    let err e =
      B0__fmt.str "copy %a to %a: %s"
        (B0__fmt.code' B0__fpath.pp) src
        (B0__fmt.code' B0__fpath.pp) dst e
    in
    let prune = match rel with (* we invoke [_fold] with [rel:true] *)
    | true -> fun st name p _ -> prune st name p
    | false -> fun st name p _ -> prune st name (B0__fpath.(src // p))
    in
    let copy dst st name p (chmods as acc) = match st.Unix.st_kind with
    | Unix.S_DIR (* prune was already called on it *) ->
        let dst = B0__fpath.(dst // p) in
        let mode = st.Unix.st_perm in
        let writeable = (mode land 0o200 <> 0) in
        let mode, acc = match writeable with
        | true -> mode, acc
        | false ->
            (* We need to be able to write to the directory, we remember
               the mode and the dir and set it at the end *)
            0o700, ((dst, mode) :: chmods)
        in
        ignore (Fs_base.dir_create ~mode ~make_path:false dst |>
                B0__result.error_to_failure);
        acc
    | Unix.S_REG ->
        let cp ~mode src dst =
          Result.join @@
          File.read_with_fd src @@ fun fdi ->
          Result.join @@
          File.write_with_fd
            ~atomic:true ~mode ~force:false ~make_path:false dst @@
          fun fdo -> Ok (Fd.copy fdi ~dst:fdo)
        in
        if prune st name p () then acc else
        let mode = st.Unix.st_perm in
        let src = B0__fpath.(src // p) in
        let dst = B0__fpath.(dst // p) in
        (cp ~mode src dst |> B0__result.error_to_failure); acc
    | Unix.S_LNK ->
        if prune st name p () then acc else
        let dst = B0__fpath.(dst // p) in
        let src = B0__fpath.(src // p) in
        let force = false and make_path = false in
        Fs_base.copy_symlink ~force ~make_path src ~dst
        |> B0__result.error_to_failure;
        acc
    | _ when prune st name p () (* why not *) -> acc
    | _ ->
        B0__fmt.failwith "%a: Not a regular file, directory or symlink"
          B0__fpath.pp B0__fpath.(src // p)
    in
    let rec chmod_dirs = function
    | [] -> ()
    | (d, m) :: ds ->
        (Fs_base.path_set_mode d m) |> B0__result.error_to_failure;
        chmod_dirs ds
    in
    Result.map_error err @@
    Result.bind (Fs_base.path_exists dst) @@ function
    | true -> Error "Destination path already exists"
    | false ->
        let tdst = match atomic with
        | true -> Tmp.mkdir ~make_path ~dir:(B0__fpath.parent dst) ()
        | false ->
            Result.bind (Fs_base.dir_create ~make_path dst) @@
            fun _ -> Ok dst
        in
        Result.bind tdst @@ fun tdst ->
        try
          let src_mode =
            Fs_base.path_get_mode src |> B0__result.error_to_failure
          in
          let chmods =
            _fold ~filter:`Any ~rel:true ~dotfiles:true ~follow_symlinks
              ~prune_dir:prune ~recurse (copy tdst) src ([tdst, src_mode])
            |> B0__result.error_to_failure
          in
          chmod_dirs chmods;
          match atomic with
          | false -> Ok ()
          | true ->
              Fs_base.path_rename ~force:false ~make_path:true tdst ~dst
        with Failure e ->
          if atomic then ignore (Fs_base.path_delete ~recurse:true tdst);
          Error e

  (* Default temporary directory *)

  let set_default_tmp p =
    Tmp.default_dir := B0__fpath.ensure_trailing_dir_sep p

  let default_tmp () = !Tmp.default_dir

  (* Temporary directories *)

  let with_tmp ?mode ?make_path ?dir ?name f =
    Result.bind (Tmp.mkdir ?mode ?make_path ?dir ?name ()) @@
    fun dir ->
    try let v = f dir in Tmp.rem_dir dir; Ok v with
    | e -> Tmp.rem_dir dir; raise e

  let tmp = Tmp.mkdir

  (* Current working directory *)

  let rec cwd () =
    let err e = B0__fmt.error "get cwd: %s" e in
    match B0__fpath.of_string (Unix.getcwd ()) with
    | Ok dir when B0__fpath.is_absolute dir -> Ok dir
    | Ok dir -> err (B0__fmt.str "%a is relative" B0__fpath.pp dir)
    | Error e -> err e
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> cwd ()
    | exception Unix.Unix_error (e, _, _) -> err (uerror e)

  let rec set_cwd dir =
    let err e = B0__fmt.error "set cwd to %a: %s" B0__fpath.pp dir e in
    try Ok (Unix.chdir (B0__fpath.to_string dir)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> set_cwd dir
    | Unix.Unix_error (e, _, _) -> err (uerror e)

  let with_cwd dir f =
    Result.bind (cwd ()) @@ fun old ->
    Result.bind (set_cwd dir) @@ fun () ->
    match f () with
    | v -> Result.map (fun () -> v) (set_cwd old)
    | exception e -> ignore (set_cwd old); raise e

  (* Base directories *)

  let err_dir dir fmt = B0__fmt.error ("%s directory: " ^^ fmt) dir
  let fpath_of_env_var dir var = match Env.var ~empty_is_none:true var with
  | None -> None
  | Some p ->
      match B0__fpath.of_string p with
      | Error e -> Some (err_dir dir "%s environment variable: %s" var e)
      | Ok _ as v -> Some v

  let base_dir dir var var_alt fallback = match fpath_of_env_var dir var with
  | Some r -> r
  | None ->
      match Option.bind var_alt (fpath_of_env_var dir) with
      | Some r -> r
      | None -> fallback ()

  let home_dir = "user"
  let home_var = "HOME"
  let user () =
    let home_env home_var = match fpath_of_env_var home_dir home_var with
    | Some r -> r
    | None -> err_dir home_dir "%s environment variable is undefined" home_var
    in
    (* if Sys.win32 then home_env home_win32_var else *)
    match
      (* When a tool is invoked via sudo one gets root even in getuid.
         The original user id can be found in the SUDO_UID en var *)
      let uid = match Env.var ~empty_is_none:true "SUDO_UID" with
      | None -> Unix.getuid ()
      | Some id ->
          match int_of_string_opt id with
          | None -> Unix.getuid ()
          | Some id -> id
      in
      B0__fpath.of_string (Unix.getpwuid uid).Unix.pw_dir
    with
    | Ok _ as v -> v
    | Error _ -> home_env home_var
    | exception Not_found -> home_env home_var
    | exception Unix.Unix_error (e, _, _) -> home_env home_var

  let home_fallback dir sub = match user () with
  | Error e -> err_dir dir "%s" e
  | Ok home -> Ok B0__fpath.(home // sub)

  let config_dir = "configuration"
  let config_var = "XDG_CONFIG_HOME"
  let config_var_alt = if Sys.win32 then Some "%APPDATA%" else None
  let config_fallback () = home_fallback config_dir (B0__fpath.v ".config")
  let config () =
    base_dir config_dir config_var config_var_alt config_fallback

  let data_dir = "data"
  let data_var = "XDG_DATA_HOME"
  let data_var_alt = if Sys.win32 then Some "%APPDATA%" else None
  let data_fallback () = home_fallback data_dir (B0__fpath.v ".local/share")
  let data () =
    base_dir data_dir data_var data_var_alt data_fallback

  let cache_dir = "cache"
  let cache_var = "XDG_CACHE_HOME"
  let cache_var_alt = if Sys.win32 then Some "%TEMP%" else None
  let cache_fallback () = home_fallback cache_dir (B0__fpath.v ".cache")
  let cache () =
    base_dir cache_dir cache_var cache_var_alt cache_fallback

  let runtime_dir = "runtime"
  let runtime_var = "XDG_RUNTIME_DIR"
  let runtime_var_alt = None
  let runtime_fallback () = Ok (default_tmp ())
  let runtime () =
    base_dir runtime_dir runtime_var runtime_var_alt runtime_fallback

  let state_dir = "state"
  let state_var = "XDG_STATE_DIR"
  let state_var_alt = None
  let state_fallback () = home_fallback state_dir (B0__fpath.v ".local/state")
  let state () =
    base_dir state_dir state_var state_var_alt state_fallback
end

module Path = struct

  (* Existence *)

  let exists = Fs_base.path_exists

  let rec must_exist p =
    try (Ok (ignore (Unix.stat (B0__fpath.to_string p)))) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ferr p "No such path"
    | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist p
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
    | Unix.Unix_error (e, _, _) ->
        ferr p (err_doing doing_exist_test (uerror e))

  (* Deleting and renaming *)

  let delete = Fs_base.path_delete
  let rename = Fs_base.path_rename

  (* Resolving *)

  let rec realpath p =
    try B0__fpath.of_string (Unix.realpath (B0__fpath.to_string p)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> realpath p
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
    | Unix.Unix_error (e, _, _) -> ferr p (uerror e)

  (* Copying *)

  let copy
      ?(rel = true) ?(atomic = true) ?(follow_symlinks = true)
      ?(prune = fun _ _ _ -> false) ~make_path ~recurse src ~dst
    =
    let err e =
      B0__fmt.str "copy %a to %a: %s" B0__fpath.pp src B0__fpath.pp dst e
    in
    let stat = match follow_symlinks with
    | true -> Fs_base.path_stat
    | false -> Fs_base.symlink_stat
    in
    match stat src with
    | Error e -> Error (err e)
    | Ok stat ->
        match stat.Unix.st_kind with
        | Unix.S_DIR ->
            Dir.copy ~rel ~atomic ~follow_symlinks ~prune ~make_path
              ~recurse src ~dst
        | Unix.S_LNK ->
            Result.map_error err @@
            Fs_base.copy_symlink ~force:false ~make_path src ~dst
        | _  ->
            File.copy ~atomic ~force:false ~make_path src ~dst

  (* File modes and stat *)

  let get_mode = Fs_base.path_get_mode
  let set_mode = Fs_base.path_set_mode
  let stat = Fs_base.path_stat
  let is_mount_point p =
    let err e = B0__fmt.str "is_mount_point: %s" e in
    match Fs_base.path_stat p with
    | Error e -> Error (err e)
    | Ok stat ->
        match Fs_base.path_stat B0__fpath.(p / "..") with
        | Error e -> Error (err ("parent: " ^ e))
        | Ok pstat -> Ok (stat.Unix.st_dev <> pstat.Unix.st_dev)

  (* Symlinks *)

  let symlink = Fs_base.symlink
  let symlink_link = Fs_base.symlink_link
  let symlink_stat = Fs_base.symlink_stat

  (* Temporary paths *)

  type tmp_name = Tmp.name
  let tmp = Tmp.path
end

(* Processes *)

module Cmd = struct

  (* Tool search in PATH *)

  let tool_is_path t = String.exists B0__fpath.is_dir_sep_char t
  let tool_file ~dir tool = match dir.[String.length dir - 1] with
  | c when B0__fpath.is_dir_sep_char c -> dir ^ tool
  | _ -> String.concat B0__fpath.natural_dir_sep [dir; tool]

  let search_in_path_env_var tool = match Unix.getenv "PATH" with
  | exception Not_found ->
      Error (`Msg "The PATH environment variable is undefined")
  | p ->
      let rec loop tool = function
      | "" ->
          Error (`Dirs (String.split_on_char B0__fpath.search_path_sep.[0] p))
      | p ->
          let dir, p =
            let sep = B0__fpath.search_path_sep in
            match B0__string.split_first ~sep p with
            | None -> p, ""
            | Some (dir, p) -> dir, p
          in
          if dir = "" then loop tool p else
          let tool_file = tool_file ~dir tool in
          match File.is_executable' tool_file with
          | false -> loop tool p
          | true -> Ok (B0__fpath.v tool_file)
      in
      loop tool p

  let search_in_dirs ~dirs tool =
    let rec loop tool = function
    | [] -> Error (`Dirs (List.map B0__fpath.to_string dirs))
    | d :: dirs ->
        let tool_file = tool_file ~dir:(B0__fpath.to_string d) tool in
        match File.is_executable' tool_file with
        | false -> loop tool dirs
        | true -> Ok (B0__fpath.v tool_file)
    in
    loop tool dirs

  let path_search ?(win_exe = Sys.win32) ?path () = fun cmd ->
    match B0__cmd.find_tool cmd with
    | None -> B0__fmt.error "No tool to lookup: the command is empty"
    | Some tool ->
        let tool =
          let tool = B0__fpath.to_string tool and suffix = ".exe" in
          if not win_exe || String.ends_with ~suffix tool then tool else
          (tool ^ suffix)
        in
        match tool_is_path tool with
        | true ->
            if File.is_executable' tool
            then Ok (B0__cmd.set_tool (B0__fpath.v tool) cmd)
            else B0__fmt.error "%s: No such executable file" tool
        | false ->
            let file = match path with
            | None -> search_in_path_env_var tool
            | Some dirs -> search_in_dirs ~dirs tool
            in
            match file with
            | Ok file -> Ok (B0__cmd.set_tool file cmd)
            | Error (`Msg e) -> B0__fmt.error "%s: %s" tool e
            | Error (`Dirs dirs) ->
                B0__fmt.error "@[<v1>%s: No such tool found in:@,%a@]"
                  tool (B0__fmt.(list string)) dirs

  (* Tool search *)

  let default_search = path_search ()
  let find ?(search = default_search) cmd = Result.to_option (search cmd)
  let find_first ?search cmds = List.find_map (find ?search) cmds
  let get ?(search = default_search) cmd = search cmd
  let rec get_first ?(search = default_search) cmds =
    if cmds = [] then Error "No tool to lookup: tool list is empty" else
    match search (List.hd cmds) with
    | Ok _ as v -> v
    | Error e ->
        let rec loop = function
        | [] ->
            B0__fmt.error "@[<v1>%a@,neither as:@,%a@]"
              B0__fmt.lines e (B0__fmt.list B0__cmd.pp) (List.tl cmds)
        | cmd :: cmds ->
            match search cmd with
            | Ok _ as v -> v
            | Error _ -> loop cmds
        in
        loop (List.tl cmds)

  (* Process completion statuses *)

  type status = [ `Exited of int | `Signaled of int ]

  let status_of_unix_status = function
  | Unix.WEXITED e -> `Exited e
  | Unix.WSIGNALED s -> `Signaled s
  | Unix.WSTOPPED _ -> assert false

  let pp_status ppf = function
  | `Exited n -> B0__fmt.pf ppf "@[exited [%d]@]" n
  | `Signaled s -> B0__fmt.pf ppf "@[signaled [%a]@]" B0__fmt.sys_signal s

  let pp_cmd_status ppf (cmd, st) =
    B0__fmt.pf ppf "cmd [%s]: %a" (B0__cmd.to_string cmd) pp_status st

  (* Process standard inputs *)

  type stdi =
  | In_string of string
  | In_file of B0__fpath.t
  | In_fd of { fd : Unix.file_descr; close : bool }

  let in_string s = In_string s
  let in_file f = In_file f
  let in_fd ~close fd = In_fd { fd; close }
  let in_stdin = In_fd { fd = Unix.stdin; close = false }
  let in_null = In_file B0__fpath.null
  let stdi_to_fd fds = function
  | In_fd { fd; close } -> if close then Fd.Set.add fd fds; fd
  | In_string s ->
      begin try
        (* We write the input string to a temporary file. *)
        let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE] in
        let f, fd = B0__result.error_to_failure (Tmp.open' ~flags ()) in
        Fd.Set.add fd fds;
        Tmp.rem_file f; (* We don't need the actual file. *)
        ignore (Unix.write_substring fd s 0 (String.length s));
        ignore (Unix.lseek fd 0 Unix.SEEK_SET);
        fd
      with
      | Unix.Unix_error (e, _, _) ->
          B0__fmt.failwith_notrace "tmp file for stdin: %s" (uerror e)
      end
  | In_file f ->
      try
        let f = B0__fpath.to_string f in
        let fd = Fd.openfile f Unix.[O_RDONLY] 0o644 in
        Fd.Set.add fd fds; fd
      with Unix.Unix_error (e, _, _) ->
        B0__fmt.failwith_notrace "open file %a for stdin: %s"
          B0__fpath.pp f (uerror e)

  (* Process standard outputs *)

  type stdo =
  | Out_fd of { fd : Unix.file_descr; close : bool }
  | Out_file of
      { mode : int; force : bool; make_path : bool; file : B0__fpath.t }

  let out_file ?(mode = 0o644) ~force ~make_path file =
    Out_file { mode; force; make_path; file }

  let out_fd ~close fd = Out_fd { fd; close }
  let out_stdout = Out_fd { fd = Unix.stdout; close = false }
  let out_stderr = Out_fd { fd = Unix.stderr; close = false }
  let out_null = out_file ~force:true ~make_path:false B0__fpath.null

  let stdo_to_fd fds = function
  | Out_fd { fd; close } -> if close then Fd.Set.add fd fds; fd
  | Out_file { mode; force; make_path; file } ->
      let flags = Unix.[O_WRONLY; O_CREAT; O_TRUNC] in
      match
        Fs_base.handle_force_open_fdout ~flags ~force ~make_path ~mode file
      with
      | Error e -> B0__fmt.failwith_notrace "open for output: %s" e
      | Ok fd -> Fd.Set.add fd fds; fd

  (* Low-level command spawn *)

  type pid = { pid : int; cmd : B0__cmd.t }
  type spawn_tracer =
    pid option -> Env.assignments option -> cwd:B0__fpath.t option ->
    B0__cmd.t -> unit

  let spawn_tracer_nop _ _ ~cwd:_ _ = ()
  let spawn_tracer_log level =
    if level = B0__log.Quiet then spawn_tracer_nop else
    let header = function
    | None -> "EXECV" | Some pid -> "EXEC:" ^ string_of_int pid.pid
    in
    let pp_env ppf = function
    | None -> ()
    | Some env ->
        B0__fmt.pf ppf "%a@," (B0__fmt.list B0__fmt.OCaml.string) env
    in
    fun pid env ~cwd cmd ->
      B0__log.msg level (fun m ->
          m ~header:(header pid) "@[<v>%a%a@]" pp_env env B0__cmd.pp_dump cmd)

  let spawn_tracer' = Atomic.make (spawn_tracer_log Debug)
  let spawn_tracer () = Atomic.get spawn_tracer'
  let set_spawn_tracer tracer = Atomic.set spawn_tracer' tracer

  let rec getcwd () = try Unix.getcwd () with
  | Unix.Unix_error (Unix.EINTR, _, _) -> getcwd ()
  | Unix.Unix_error (e, _, _) ->
      B0__fmt.failwith_notrace "getcwd: %s" (uerror e)

  let rec chdir cwd = try Unix.chdir cwd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> chdir cwd
  | Unix.Unix_error (e, _, _) ->
      B0__fmt.failwith_notrace "chdir %s: %s" cwd (uerror e)

  let spawn_err cmd e = match B0__cmd.is_empty cmd with
  | true -> Result.error e
  | false -> B0__fmt.error "cmd %s: %s" (B0__cmd.to_string cmd) e

  let spawn_cwd = function None -> getcwd () | Some d -> B0__fpath.to_string d
  let spawn_env = function
  | None -> Unix.environment ()
  | Some e -> Array.of_list e

  let _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd =
    match B0__cmd.to_list cmd with
    | [] -> failwith "no command, empty command line"
    | line ->
        try
          let env' = spawn_env env in
          let cwd' = spawn_cwd cwd in
          let line = Array.of_list line in
          let exe = line.(0) in
          let stdin = stdi_to_fd fds stdin in
          let stdout = stdo_to_fd fds stdout in
          let stderr = stdo_to_fd fds stderr in
          let old_cwd = getcwd () in
          let change_cwd = not @@ String.equal old_cwd cwd' in
          if change_cwd then chdir cwd';
          let pid =
            { pid = Unix.create_process_env exe line env' stdin stdout stderr;
              cmd }
          in
          if change_cwd then chdir old_cwd; (* XXX pid zombie on fail. *)
          Fd.Set.close_all fds;
          (Atomic.get spawn_tracer') (Some pid) env ~cwd cmd;
          pid
        with
        | e ->
            (* In case one of the std{i,o}_to_fd raises *)
            let add_out_fd fds = function
            | Out_fd { fd ; close = true } -> Fd.Set.add fd fds
            | _ -> ()
            in
            add_out_fd fds stdout; add_out_fd fds stderr;
            raise e

  (* Blocking command execution *)

  let rec run_collect pid = match Unix.waitpid [] pid.pid with
  | _, status -> status_of_unix_status status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> run_collect pid
  | exception Unix.Unix_error (e, _, _) ->
      B0__fmt.failwith_notrace "waitpid [%d]: %s" pid.pid (uerror e)

  let run_status
      ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
      ?(stderr = out_stderr) cmd
    =
    let fds = Fd.Set.empty () in
    try
      let pid = _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd in
      Ok (run_collect pid)
    with
    | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
    | Unix.Unix_error (e, _, _) ->
        Fd.Set.close_all fds; spawn_err cmd (uerror e)

  let run_status_out
      ?env ?cwd ?(stdin = in_stdin) ?(stderr = `Stdo out_stderr) ~trim cmd
    =
    let fds = Fd.Set.empty () in
    try
      let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC] in
      let tmpf, fd = B0__result.error_to_failure (Tmp.open' ~flags ()) in
      let stdout = out_fd ~close:false fd in
      let stderr = match stderr with `Out -> stdout | `Stdo o -> o in
      let pid = _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd in
      let status = run_collect pid in
      let out = Fd.read_file (B0__fpath.to_string tmpf) fd in
      Tmp.rem_file tmpf; Fd.close_noerr fd;
      Ok (status, if trim then String.trim out else out)
    with
    | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
    | Unix.Unix_error (e, _, _) ->
        Fd.Set.close_all fds; spawn_err cmd (uerror e)

  let run ?env ?cwd ?stdin ?stdout ?stderr cmd =
    match run_status ?env ?cwd ?stdin ?stdout ?stderr cmd with
    | Ok (`Exited 0) -> Ok ()
    | Ok st -> B0__fmt.error "%a" pp_cmd_status (cmd, st)
    | Error _ as e -> e

  let run_out ?env ?cwd ?stdin ?stderr ~trim cmd =
    match run_status_out ?env ?cwd ?stdin ?stderr ~trim cmd with
    | Error _ as e -> e
    | Ok (`Exited 0, v) -> Ok v
    | Ok (st, "") -> B0__fmt.error "%a" pp_cmd_status (cmd, st)
    | Ok (st, o) ->
        B0__fmt.error "@[<v>%a after outputing:@, @[%a@]@]"
          pp_cmd_status (cmd, st) B0__fmt.lines o

  (* Non-blocking command *)

  let pid_cmd pid = pid.cmd
  let pid_to_int pid = pid.pid

  let spawn
      ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
      ?(stderr = out_stderr) cmd
    =
    let fds = Fd.Set.empty () in
    try Ok (_spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd) with
    | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
    | Unix.Unix_error (e, _, _) ->
        Fd.Set.close_all fds; spawn_err cmd (uerror e)

  let rec spawn_poll_status pid = match Unix.waitpid Unix.[WNOHANG] pid.pid with
  | 0, _ -> Ok None
  | _, status -> Ok (Some (status_of_unix_status status))
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> spawn_poll_status pid
  | exception Unix.Unix_error (e, _, _) ->
      B0__fmt.error "poll_status: waitpid [%d]: %s" pid.pid (uerror e)

  let rec spawn_wait_status pid = match Unix.waitpid [] pid.pid with
  | _, status -> Ok (status_of_unix_status status)
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> spawn_wait_status pid
  | exception Unix.Unix_error (e, _, _) ->
      B0__fmt.error "waitpid [%d]: %s" pid.pid (uerror e)

  let spawn_wait pid = match spawn_wait_status pid with
  | Ok (`Exited 0) -> Ok ()
  | Ok st ->
      B0__fmt.error "@[waitpid [%d]: %a@]" pid.pid pp_cmd_status (pid.cmd, st)
  | Error _ as e -> e

  let kill pid sg = match Unix.kill pid.pid sg with
  | () -> Ok ()
  | exception Unix.Unix_error (e, _, _) ->
      B0__fmt.error "kill %d with %a: %s" pid.pid B0__fmt.sys_signal sg
        (uerror e)

  (* execv

     On Windows when Unix.execv[e] is invoked, control is returned to
     the controlling terminal when the child process starts (vs. child
     process terminates on POSIX). This entails all sort of weird
     behaviour. To workaround this, our execv[e] on Windows simply
     runs the program as a sub-process on which we waitpid(2) and then
     exit with the resulting status. *)

  let _execv_win32 ~env file cmd =
    let exit pid = match Unix.waitpid [] pid with
    | _, (Unix.WEXITED c) -> exit c
    | _, (Unix.WSIGNALED sg) ->
        Unix.(kill (getpid ()) sg);
        (* In case we don't get killed, exit with bash convention. *)
        exit (128 + sg)
    | _ -> assert false
    in
    let env = spawn_env env in
    exit Unix.(create_process_env file cmd env stdin stderr stderr)

  let _execv_posix ~env file cmd =
    Ok (Unix.execve file cmd (spawn_env env))

  let _execv = if Sys.win32 then _execv_win32 else _execv_posix

  let execv ?env ?cwd ?argv0 cmd =
    let err_execv f e = B0__fmt.error "execv %s: %s" f e in
    match B0__cmd.to_list cmd with
    | [] -> Error "execv: empty command line"
    | file :: args as all ->
        let args = match argv0 with None -> all | Some n -> n :: args in
        try
          let file = Unix.realpath file in
          let reset_cwd = match cwd with
          | None -> Fun.id
          | Some cwd ->
            let old_cwd = getcwd () in
            chdir (B0__fpath.to_string cwd);
            fun () -> try chdir old_cwd with Failure _ -> ()
          in
          Fun.protect ~finally:reset_cwd @@ fun () ->
          (Atomic.get spawn_tracer') None env ~cwd cmd;
          _execv ~env file (Array.of_list args)
        with
        | Failure e -> err_execv file e
        | Unix.Unix_error (e, _, _) -> err_execv file (uerror e)

  type t = B0__cmd.t
end

module Exit = struct
  let log_error e = B0__log.err (fun m -> m "@[%a@]" B0__fmt.lines e)

  type code = int
  type execv =
    { env : Env.assignments option;
      cwd : B0__fpath.t option;
      argv0 : string option;
      cmd : B0__cmd.t }

  type t = Code : code -> t | Execv : execv -> t
  let code c = Code c
  let execv ?env ?cwd ?argv0 cmd = Execv { env; cwd; argv0; cmd }

  let get_code = function Code c -> c | _ -> invalid_arg "not an Exit.Code"

  let ok = Code 0
  let no_such_name = Code 122
  let some_error = Code 123
  let cli_error = Code 124
  let internal_error = Code 125

  let exit_some_error e = log_error e; some_error
  let of_result = function Ok () -> ok | Error e -> exit_some_error e
  let of_result' = function Ok e -> e | Error e -> exit_some_error e

  let execv_env e = e.env
  let execv_cwd e = e.cwd
  let execv_argv0 e = e.argv0
  let execv_cmd e = e.cmd

  let rec exit ?(on_error = some_error) = function
  | Code c -> Stdlib.exit c
  | Execv { env; cwd; argv0; cmd } ->
      match Cmd.execv ?env ?cwd ?argv0 cmd with
      | Ok _ -> assert false
      | Error e -> log_error e; exit on_error

  let on_sigint ~hook f =
    let hook _ = hook (); Stdlib.exit 130 (* as if SIGINT signaled *) in
    let previous = Sys.signal Sys.sigint (Sys.Signal_handle hook) in
    let restore () = Sys.set_signal Sys.sigint previous in
    Fun.protect ~finally:restore f
end

(* Sleeping and timing *)

let sleep = Mtime.sleep
let relax () = try Unix.sleepf 0.0001 with Unix.Unix_error _ -> ()

module Cpu = struct
  external logical_count : unit -> int = "ocaml_b0_cpu_logical_count"

  (* Measuring CPU time *)

  module Time = struct

    (* CPU time spans *)

    module Span = struct
      type t =
        { utime : Int64.t; stime : Int64.t;
          children_utime : Int64.t; children_stime : Int64.t; }

      let make ~utime ~stime ~children_utime ~children_stime =
        { utime = B0__mtime.Span.to_uint64_ns utime;
          stime = B0__mtime.Span.to_uint64_ns stime;
          children_utime = B0__mtime.Span.to_uint64_ns children_utime;
          children_stime = B0__mtime.Span.to_uint64_ns children_stime;  }

      let zero =
        { utime = 0L; stime = 0L; children_utime = 0L; children_stime = 0L }

      let utime c = B0__mtime.Span.of_uint64_ns c.utime
      let stime c = B0__mtime.Span.of_uint64_ns c.stime
      let children_utime c = B0__mtime.Span.of_uint64_ns c.children_utime
      let children_stime c = B0__mtime.Span.of_uint64_ns c.children_stime
    end

    (* CPU counters *)

    let sec_to_span sec = Int64.of_float (sec *. 1e9)

    type counter = Span.t
    let counter () : Span.t =
      let now = Unix.times () in
      { utime = sec_to_span now.Unix.tms_utime;
        stime = sec_to_span now.Unix.tms_stime;
        children_utime = sec_to_span now.Unix.tms_cutime;
        children_stime = sec_to_span now.Unix.tms_cstime; }

    let count (c : Span.t) : Span.t =
      let now = Unix.times () in
      { utime = Int64.sub (sec_to_span now.Unix.tms_utime) c.utime;
        stime = Int64.sub (sec_to_span now.Unix.tms_stime) c.stime;
        children_utime =
          Int64.sub (sec_to_span now.Unix.tms_cutime) c.children_utime;
        children_stime =
          Int64.sub (sec_to_span now.Unix.tms_cstime) c.children_stime; }
  end
end

(* uname(2) bindings *)

external uname_machine : unit -> string = "ocaml_b0_uname_machine"
external uname_sysname : unit -> string = "ocaml_b0_uname_sysname"

(* OS name, version and architecture *)

module Name = struct
  type id = string
  let id_unknown = "unknown"
  type t =
  | Bsd of id
  | Darwin of id
  | Linux of id
  | Windows of id
  | Other of id

  let id = function
  | Bsd id -> id | Darwin id -> id | Linux id -> id | Windows id -> id
  | Other id ->id

  let of_string ?family s =
    let id = B0__string.Ascii.lowercase s in
    let id = if id = "" then id_unknown else id in
    match family with
    | None ->
        begin match id with
        | "bsd" | "freebsd" |  "netbsd" | "openbsd" -> Bsd id
        | "darwin" | "macos" | "ios" -> Darwin id
        | "linux" | "alpine" | "arch" | "centos" | "debian" | "fedora"
        | "gentoo" | "opensuse" | "rhel" | "suse" | "ubuntu" -> Linux id
        | "windows" | "cygwin" -> Windows id
        | id -> Other id
        end
    | Some Bsd _ -> Bsd id | Some Darwin _ -> Darwin id
    | Some Linux _ -> Linux id | Some Windows _ -> Windows id
    | Some Other _ -> Other id

  let bsd = Bsd "bsd"
  let darwin = Darwin "darwin"
  let linux = Linux "linux"
  let windows = Windows "windows"
  let unknown = Other id_unknown

  let equal a0 a1 = match a0, a1 with
  | Bsd _, Bsd _ -> true | Darwin _, Darwin _ -> true
  | Linux _, Linux _ -> true | Windows _, Windows _ -> true
  | Other id0, Other id1 when String.equal id0 id1 -> true
  | _ -> false

  let compare n0 n1 = match n0, n1 with
  | Bsd _, Bsd _ -> 0 | Darwin _, Darwin _ -> 0
  | Linux _, Linux _ -> 0 | Windows _, Windows _ -> 0
  | Other n0, Other n1 when String.equal n0 n1 -> 0
  | n0, n1 -> Stdlib.compare n0 n1

  let pp ppf a = B0__fmt.string ppf @@ match a with
  | Bsd _ -> "bsd" | Darwin _ -> "darwin"
  | Linux _ -> "linux" | Windows _ -> "windows"
  | Other id -> id

  let pp_id = B0__fmt.using id B0__fmt.string

  (* Determining the values, so much code… *)

  let init = Other "" (* stub value, never returned *)
  let name = Atomic.make init
  let like = Atomic.make init
  let version = Atomic.make "" (* stub value, never returned *)

  let darwin_info uname_sysname =
    let get_plist_key key xml = (* Easy to thwart… *)
      let find ~start ~sub s = match B0__string.find_first ~start ~sub s with
      | None -> raise Exit | Some i -> i
      in
      let open_tag = "<string>" and close_tag = "</string>" in
      let k = find ~start:0 ~sub:key xml in
      let i = find ~start:k ~sub:open_tag xml in
      let j = find ~start:i ~sub:close_tag xml in
      let first = i + String.length open_tag and last = j - 1 in
      B0__string.subrange ~first ~last xml
    in
    let plist = "/System/Library/CoreServices/SystemVersion.plist" in
    let default = uname_sysname, id_unknown in
    match File.read (B0__fpath.v plist) with
    | Error e -> (* iOS will end up here, let's not link more stuff for now *)
        B0__log.debug (fun m -> m "%s" e); default
    | Ok plist_xml ->
        try
          let name = get_plist_key "ProductName" plist_xml in
          let version = get_plist_key "ProductVersion" plist_xml in
          B0__string.Ascii.lowercase name, version
        with
        | Exit ->
            B0__log.debug (fun m -> m "Failed to parse %s" plist); default

  let etc_os_release_info uname_sysname =
    let unquote s =
      let s = String.trim s in
      if String.length s >= 2 && s.[0] = '"'
      then String.sub s 1 (String.length s - 2)
      else s
    in
    let info = "/etc/os-release" in
    match File.read (B0__fpath.v info) with
    | Error e ->
        B0__log.debug (fun m -> m "%s" e);
        uname_sysname, uname_sysname, id_unknown
    | Ok data ->
        let rec loop id id_like version = function
        | [] ->
            let id = if id = "" then uname_sysname else id in
            let id_like = Option.value ~default:id id_like in
            let id_like = if id_like = "" then id else id_like in
            let version = if version = "" then id_unknown else version in
            id, id_like, version
        | l :: ls ->
            match B0__string.split_first ~sep:"=" (String.trim l) with
            | None -> loop id id_like version ls
            | Some ("ID", id) -> loop (unquote id) id_like version ls
            | Some ("ID_LIKE", ids) ->
                let ids = String.split_on_char ' ' (unquote ids) in
                let id_like = Some (List.hd ids) in
                loop id id_like version ls
            | Some ("VERSION_ID", vers) -> loop id id_like (unquote vers) ls
            | _ -> loop id id_like version ls
        in
        loop "" None "" (String.split_on_char '\n' data)

  external caml_win32_version : unit -> int * int * int * int =
    "ocaml_b0_caml_win32_version"

  let windows_info uname_sysname =
    let maj, min, build, rev = caml_win32_version () in
    uname_sysname, Printf.sprintf "%d.%d.%d.%d" maj min build rev

  let init_info () =
    let sysname = B0__string.Ascii.lowercase (uname_sysname ()) in
    let n, name_like, vers = match sysname with
    | "darwin" ->
        let id, version = darwin_info sysname in
        (Darwin id), (Darwin id), version
    | "linux" ->
        let id, id_like, version = etc_os_release_info sysname in
        (Linux id), (Linux id_like), version
    | "freebsd" ->
        let id, id_like, version = etc_os_release_info sysname in
        (Bsd id), (Bsd id_like), version
    | "openbsd" ->
        (Bsd sysname), (Bsd sysname), id_unknown
    | "netbsd" ->
        (Bsd sysname), (Bsd sysname), id_unknown
    | "windows" ->
        let id, version = windows_info sysname in
        (Windows id), (Windows id), version
    | sysname when String.starts_with ~prefix:"cygwin_nt" sysname ->
        let id, version = windows_info sysname in
        (Windows id), (Windows id), version
    | id ->
        (Other id), (Other id), id_unknown (* version "unknown" *)
    in
    Atomic.set name n; Atomic.set like name_like; Atomic.set version vers
end

let name ?(id_like = false) () =
  let name = if id_like then Name.like else Name.name in
  match Atomic.get name with
  | Name.Other "" -> Name.init_info (); Atomic.get name
  | v -> v

let version () = match Atomic.get Name.version with
| "" -> Name.init_info (); Atomic.get Name.version | v -> v

module Arch = struct
  type id = string
  let id_unknown = "unknown"
  type t =
  | Arm32 of id | Arm64 of id | Ppc32 of id | Ppc64 of id
  | Riscv32 of id | Riscv64 of id | X86_32 of id | X86_64 of id
  | Other of id

  let of_string ?family s =
    let id = B0__string.Ascii.lowercase s in
    let id = String.map (function '-' -> '_' | c -> c) id in
    let id = if id = "" then id_unknown else id in
    match family with
    | None ->
        begin match id with
        | "arm32" | "armv8b" | "armv8l" as id -> Arm32 id
        | "arm64" | "aarch64_be" | "aarch64" as id -> Arm64 id
        | "ppc32" | "powerpc" | "ppc" | "ppcle" as id -> Ppc32 id
        | "ppc64" | "ppc64le" as id -> Ppc64 id
        | "riscv32" as id -> Riscv32 id
        | "riscv64" as id -> Riscv64 id
        | "x86_32" | "x86" | "i386" | "i586" | "i686" as id -> X86_32 id
        | "x86_64" | "amd64" as id -> X86_64 id
        | id when
            String.(starts_with ~prefix:"armv5" id ||
                    starts_with ~prefix:"armv6" id ||
                    starts_with ~prefix:"earmv6" id ||
                    starts_with ~prefix:"armv7" id ||
                    starts_with ~prefix:"earmv7" id) -> Arm32 id
        | id -> Other id
        end
    | Some Arm32 _ -> Arm32 id | Some Arm64 _ -> Arm64 id
    | Some Ppc32 _ -> Ppc32 id | Some Ppc64 _ -> Ppc64 id
    | Some Riscv32 _ -> Riscv32 id | Some Riscv64 _ -> Riscv64 id
    | Some X86_32 _ -> X86_32 id | Some X86_64 _ -> X86_64 id
    | Some Other _ -> Other id


  let id = function
  | Arm32 id | Arm64 id | Ppc32 id | Ppc64 id | Riscv32 id | Riscv64 id
  | X86_32 id | X86_64 id | Other id -> id

  let bits = function
  | Arm32 _ | Ppc32 _ | Riscv32 _ | X86_32 _ -> Some 32
  | Arm64 _ | Ppc64 _ | Riscv64 _ | X86_64 _ -> Some 64
  | Other _ -> None

  let arm32 = Arm32 "arm32"
  let arm64 = Arm64 "arm64"
  let ppc32 = Ppc32 "ppc32"
  let ppc64 = Ppc64 "ppc64"
  let riscv32 = Riscv32 "riscv32"
  let riscv64 = Riscv64 "riscv64"
  let x86_32 = X86_32 "x86_32"
  let x86_64 = X86_64 "x86_64"
  let unknown = Other id_unknown

  let equal a0 a1 = match a0, a1 with
  | Arm32 _, Arm32 _ -> true | Arm64 _, Arm64 _ -> true
  | Ppc32 _, Ppc32 _ -> true | Ppc64 _, Ppc64 _ -> true
  | Riscv32 _, Riscv32 _ -> true | Riscv64 _, Riscv64 _ -> true
  | X86_32 _, X86_32 _ -> true | X86_64 _, X86_64 _ -> true
  | Other id0, Other id1 when String.equal id0 id1 -> true
  | _ -> false

  let compare a0 a1 = match a0, a1 with
  | Arm32 _, Arm32 _ -> 0 | Arm64 _, Arm64 _ -> 0
  | Ppc32 _, Ppc32 _ -> 0 | Ppc64 _, Ppc64 _ -> 0
  | Riscv32 _, Riscv32 _ -> 0 | Riscv64 _, Riscv64 _ -> 0
  | X86_32 _, X86_32 _ -> 0 | X86_64 _, X86_64 _ -> 0
  | a0, a1 -> Stdlib.compare a0 a1

  let pp ppf a = B0__fmt.string ppf @@ match a with
  | Arm32 _ -> "arm32" | Arm64 _ -> "arm64"
  | Ppc32 _ -> "ppc32" | Ppc64 _ -> "ppc64"
  | Riscv32 _ -> "riscv32" | Riscv64 _ -> "riscv64"
  | X86_32 _ -> "x86_32" | X86_64 _ -> "x86_64"
  | Other id -> id

  let pp_id = B0__fmt.using id B0__fmt.string
  let pp_bits =
    B0__fmt.using bits B0__fmt.(option int ~none:(any "<unknown>"))
end

let arch () = Arch.of_string (uname_machine ())

(* Bazaar *)

let exn_don't_catch = function
| Stack_overflow | Out_of_memory | Sys.Break -> true
| _ -> false
