(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

module Trash = struct
  type t = { dir : Fpath.t }
  let create dir = { dir }
  let dir t = t.dir
  let trash t p =
    Result.map_error (Fmt.str "trashing %a: %s" Fpath.pp_quoted p) @@
    Result.bind (Os.Path.exists p) @@ function
    | false -> Ok ()
    | true ->
        let (* deal with races *) force = true and make_path = true in
        Result.bind (Os.Path.tmp ~make_path ~dir:t.dir ~name:"%s" ()) @@
        fun garbage -> Os.Path.rename ~force ~make_path ~src:p garbage

  let err_delete t err =
    Fmt.error "delete trash %a: %s" Fpath.pp_quoted t.dir err

  let delete_blocking t =
    Result.bind (Os.Path.delete ~recurse:true t.dir) @@ fun _ -> Ok ()

  let delete_win32 ~block t = match block with
  | true -> delete_blocking t
  | false ->
      let rm = Cmd.(arg "cmd.exe" % "/c" % "rd" % "/s" % "/q" %% path t.dir) in
      match Os.Cmd.spawn rm (* XXX redirect stdio to Fpath.null ? *) with
      | Ok _pid -> Ok ()
      | Error e -> err_delete t e

  let rec delete_posix ~block t = match block with
  | true -> delete_blocking t
  | false ->
      try match Unix.fork () with
      | 0 -> ignore (delete_blocking t); exit 0
      | _pid -> Ok ()
      with
      | Unix.Unix_error (err, _, _) -> err_delete t (Unix.error_message err)

  let delete = if Sys.win32 then delete_win32 else delete_posix
end

module File_cache = struct
  let uerr = Unix.error_message
  let err op err = Fmt.str "cache %s: %s" op err
  let err_key op key err = Fmt.str "cache %s %s: %s" op key err

  (* Caches

     Given a cache directory [dir] and a key [KEY]:

     1. Its ordered file contents are stored in the files [dir/KEY.k/X]
        with [X] the zero-based index of the file in the list as a fixed
        number of hexadecimal numbers (allows binary sort on files names
        to be used for [find]). The last file in the sequence has its
        number prefixed by [z] (dir/KEY.k/zX), this is used by [revive] to
        match the number of files to bind without having to [readdir] the
        key directory.
     2. Its metadata is stored in the file [dir/KEY.k/zm] so that it's
        the last file in the binary sort.
     3. XXX should we put keys in prefix subdirs like git does ? Some
        fs get slow on many entries (NTFS ?) *)

  type feedback = [ `File_cache_need_copy of Fpath.t ]
  type key = string
  type t =
    { feedback : feedback -> unit;
      dir : string;
      mutable need_copy : Fpath.t option; (* path of first Unix.EXDEV error *) }

  let create ?(feedback = fun _ -> ()) dir =
    Result.bind (Os.Dir.create ~make_path:true dir) @@ fun _ ->
    let dir = Fpath.to_dir_path dir (* assumed e.g. by key_dir *) in
    Ok { feedback; dir = Fpath.to_string dir; need_copy = None }

  let dir c = Fpath.v c.dir
  let need_copy c = c.need_copy

  (* Contructing file paths into the cache *)

  let key_ext = ".k"
  let key_meta_filename = "zm"

  let filename_is_key_dir dir = String.is_suffix ~affix:key_ext dir
  let key_dir c k = String.concat "" [c.dir; k; key_ext; Fpath.dir_sep]
  let key_of_filename fname = String.drop_right (String.length key_ext) fname
  let key_dir_of_filename c fname =
    String.concat "" [c.dir; fname; Fpath.dir_sep]

  let to_hex_digit n = Char.unsafe_chr @@ n + if n < 10 then 0x30 else 0x61 - 10
  let ilog16 x =
    let rec f p x = match x with 0 -> p | x -> f (p + 1) (x lsr 4) in f (-1) x

  let filenum_width list_len = 1 + ilog16 list_len
  let filenum_str ~filenum_width i =
    let hex = Bytes.create filenum_width in
    let rec loop hex i k = match k < 0 with
    | true -> Bytes.unsafe_to_string hex
    | false ->
        Bytes.unsafe_set hex k @@ to_hex_digit (i land 0xF);
        loop hex (i lsr 4) (k - 1)
    in
    loop hex i (filenum_width - 1)

  let key_meta_file c key =
    String.concat "" [c.dir; key; key_ext; Fpath.dir_sep; key_meta_filename ]

  let key_file c key ~filenum_width ~is_last i =
    (* XXX we could blit directly rather than constructing these lists *)
    String.concat "" @@ match is_last with
    | false ->
        [c.dir; key; key_ext; Fpath.dir_sep; filenum_str ~filenum_width i ]
    | true ->
        [c.dir; key; key_ext; Fpath.dir_sep; "z"; filenum_str ~filenum_width i ]

  (* File system interaction. The following function handle Unix
     errors in the way the cache deems convenient. *)

  let read_file file =
    let fd = Unix.openfile file Unix.[O_RDONLY] 0 in
    Os.Fd.apply ~close:Unix.close fd (Os.Fd.read_file file)

  let write_flags =
    Unix.[O_WRONLY; O_CREAT; O_SHARE_DELETE; O_CLOEXEC; O_TRUNC; O_EXCL]

  let rec unlink_noerr p = try Unix.unlink p with
  | Unix.Unix_error (Unix.EINTR, _, _) -> unlink_noerr p
  | Unix.Unix_error (_, _, _) -> ()

  let write_file dst s =
    let fd = Unix.openfile dst write_flags 0o644 in
    try
      let write fd = ignore @@ Unix.write_substring fd s 0 (String.length s) in
      Os.Fd.apply ~close:Unix.close fd write
    with
    | e -> unlink_noerr dst; raise e

  let rec copy src dst =
    (* Like link(2) if [src] doesn't exist raises ENOENT. If [dst]
       exists raises [EEXIST]. If the copy fails unlinks [dst]. *)
    match (Unix.stat src).Unix.st_perm with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> copy src dst
    | mode ->
        let fdi = Unix.openfile src Unix.[O_RDONLY] 0 in
        let fdo = Unix.openfile dst write_flags mode in
        try
          Os.Fd.apply ~close:Unix.close fdi @@ fun fdi ->
          Os.Fd.apply ~close:Unix.close fdo @@ fun fdo ->
          Os.Fd.copy ~src:fdi fdo
        with
        | e -> unlink_noerr dst; raise e

  let rec make_path p =
    let mkdir dir = Unix.mkdir (Fpath.to_string dir) 0o755 in
    let dir = Fpath.parent p in
    try mkdir dir with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        let rec down = function
        | [] -> ()
        | d :: ds as arg ->
            match mkdir d with
            | () -> down ds
            | exception Unix.Unix_error (Unix.EEXIST, _, _) -> down ds
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> down arg
        in
        let rec up todo d = match mkdir d with
        | () -> down todo
        | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
            up (d :: todo) (Fpath.parent d)
        | exception Unix.Unix_error (Unix.EEXIST, _, _) -> down todo
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> up todo d
        in
        up [dir] (Fpath.parent dir)
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> make_path p

  let fold_dir_filenames dir f acc =
    let rec closedir dh = try Unix.closedir dh with
    | Unix.Unix_error (Unix.EINTR, _, _) -> closedir dh
    | Unix.Unix_error (_, _, _) -> (* not really interested *) ()
    in
    let rec filenames dh f acc = match Unix.readdir dh with
    | ".." | "." -> filenames dh f acc
    | n -> filenames dh f (f acc n)
    | exception End_of_file -> acc
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.failwith_notrace "%s: %s" dir (uerr e)
    in
    match Unix.opendir dir with
    | dh ->
        begin match filenames dh f acc with
        | fs -> closedir dh; Some fs
        | exception e -> closedir dh; raise e
        end
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.failwith_notrace "%s: %s" dir (uerr e)

  let dir_filenames dir = fold_dir_filenames dir (fun acc n -> n :: acc) []
  let dir_delete_files dir =
    let rec delete_file dir () fname =
      let p = dir ^ fname in
      try Unix.unlink p with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ()
      | Unix.Unix_error (Unix.EINTR, _, _) -> delete_file dir () fname
      | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "%s: %s" p (uerr e)
    in
    ignore @@ fold_dir_filenames dir (delete_file dir) ()

  (* Functions on key directories *)

  let key_dir_files kdir =
    let string_rev_compare f0 f1 = String.compare f1 f0 in
    let file_path f = Fpath.v (kdir ^ f) in
    match dir_filenames kdir with
    | None -> None
    | Some fs ->
        match List.sort string_rev_compare @@ fs with
        | f :: fs when String.equal f key_meta_filename ->
            Some (file_path key_meta_filename, List.rev_map file_path fs)
        | _ -> Fmt.failwith_notrace "%s: corrupted key" kdir

  let key_dir_delete kdir =
    let rec dir_delete d = try Unix.rmdir d; true with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> false
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> false
    | Unix.Unix_error (Unix.EINTR, _, _) -> dir_delete d
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "%s: %s" d (uerr e)
    in
    dir_delete_files kdir; dir_delete kdir

  let key_dir_exit_if_used kdir =
    let rec stat p = try Unix.stat p with
    | Unix.Unix_error (Unix.EINTR, _, _) -> stat p
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "%s: %s" p (uerr e)
    in
    let file_used f = (stat f).Unix.st_nlink <> 1 in
    let used () f = if file_used (kdir ^ f) then raise_notrace Exit else () in
    ignore @@ fold_dir_filenames kdir used ()

  let key_dir_stats kdir =
    let rec stat p = try Unix.stat p with
    | Unix.Unix_error (Unix.EINTR, _, _) -> stat p
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "%s: %s" p (uerr e)
    in
    let rec loop fc bc unused atime = function
    | [] -> fc, bc, unused, atime
    | f :: fs ->
        let s = stat (kdir ^ f) in
        let unused = unused && s.Unix.st_nlink = 1 in
        let atime = (max : float -> float -> float) atime s.Unix.st_atime in
        loop (fc + 1) (bc + s.Unix.st_size) unused atime fs
    in
    let fs = match dir_filenames kdir with None -> [] | Some fs -> fs in
    loop 0 0 true 0. fs

  (* Cache operations *)

  let rec mem c k = try Unix.access (key_dir c k) [Unix.F_OK]; true with
  | Unix.Unix_error (Unix.EINTR, _, _) -> mem c k
  | Unix.Unix_error (e, _, _) -> false

  let add c k meta fs =
    let rec mkdir d = try Unix.mkdir d 0o755; true with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> false
    | Unix.Unix_error (Unix.EINTR, _, _) -> mkdir d
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "%s: %s" d (uerr e)
    in
    let rec cache_file c ~src cfile = try match Option.is_none c.need_copy with
    | true -> Unix.link src cfile; true
    | false -> copy src cfile; true
    with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> false
    | Unix.Unix_error (Unix.EINTR, _, _) -> cache_file c ~src cfile
    | Unix.Unix_error (Unix.EXDEV, _, _)
    | Unix.Unix_error (Unix.EOPNOTSUPP, _, _)
    | Unix.Unix_error (Unix.ENOSYS, _, _)
    | Unix.Unix_error (Unix.EMLINK, _, _) ->
        let src_file = Fpath.v src in
        c.need_copy <- Some src_file;
        c.feedback (`File_cache_need_copy src_file);
        cache_file c ~src cfile
    | Unix.Unix_error (e, _, arg) ->
        Fmt.failwith_notrace "%s: %s: %s" src arg (uerr e)
    in
    let write_file file data = try write_file file data with
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "%s: %s" file (uerr e)
    in
    try
      let kdir = key_dir c k in
      if not (mkdir kdir) then dir_delete_files kdir;
      let success = match filenum_width (List.length fs) with
      | 0 -> true
      | filenum_width ->
          let rec loop i = function
          | [] -> true
          | f :: fs ->
              let cfile = key_file c k ~filenum_width ~is_last:(fs = []) i in
              match cache_file c ~src:(Fpath.to_string f) cfile with
              | true -> loop (i + 1) fs
              | false -> ignore (key_dir_delete kdir); false
          in
          loop 0 fs
      in
      if success
      then (write_file (kdir ^ key_meta_filename) meta; Ok true)
      else Ok false
    with
    | Failure e -> Error (err_key "add" k e)

  let rem c k = try Ok (key_dir_delete (key_dir c k)) with
  | Failure e -> Error (err_key "delete" k e)

  let find c k = try Ok (key_dir_files (key_dir c k)) with
  | Failure e -> Error (err_key "find" k e)

  let revive c k fs =
    let rec exists f = try Unix.access f [Unix.F_OK]; true with
    | Unix.Unix_error (Unix.EINTR, _, _) -> exists f
    | Unix.Unix_error (_, _, _) -> false
    in
    let rec revive_file ~did_path cfile ~dst =
      try match c.need_copy <> None with
      | true -> copy cfile (Fpath.to_string dst); true
      | false -> Unix.link cfile (Fpath.to_string dst); true
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> false
      | Unix.Unix_error (Unix.ENOENT, _, _) when not did_path ->
          make_path dst; revive_file ~did_path:true cfile ~dst
      | Unix.Unix_error (Unix.EINTR, _, _) ->
          revive_file ~did_path cfile ~dst
      | Unix.Unix_error (Unix.EXDEV, _, _)
      | Unix.Unix_error (Unix.EOPNOTSUPP, _, _)
      | Unix.Unix_error (Unix.ENOSYS, _, _)
      | Unix.Unix_error (Unix.EMLINK, _, _) ->
          c.need_copy <- Some dst;
          c.feedback (`File_cache_need_copy dst);
          revive_file ~did_path cfile ~dst
      | Unix.Unix_error (e, _, arg) ->
          Fmt.failwith_notrace "%a: %s: %s" Fpath.pp_quoted dst arg (uerr e)
    in
    try
      let fs_len = List.length fs in
      let filenum_width = filenum_width fs_len in
      let last = key_file c k ~filenum_width ~is_last:true (fs_len - 1) in
      if not (exists last) (* Tests existence and arity *) then Ok None else
      let rec loop i existed = function
      | [] -> Ok (Some (read_file (key_meta_file c k), existed))
      | f :: fs ->
          let cfile = key_file c k ~filenum_width ~is_last:(fs = []) i in
          match revive_file ~did_path:false cfile ~dst:f with
          | true -> loop (i + 1) existed fs
          | false -> loop (i + 1) (f :: existed) fs
      in
      loop 0 [] fs
    with
    | Failure e -> Error (err_key "revive" k e)

  (* Functions on cache directories *)

  let cache_fold_key_dir_names c f acc =
    let if_key f acc fn = if filename_is_key_dir fn then f acc fn else acc in
    match fold_dir_filenames c.dir (if_key f) acc with
    | None -> acc | Some acc -> acc

  let fold c f acc =
    let fold_key f acc fname =
      let key = key_of_filename fname in
      let key_dir = key_dir_of_filename c fname in
      match key_dir_files key_dir with
      | None -> acc (* That would be a race *)
      | Some (meta, fs) -> f key meta fs acc
    in
    try Ok (cache_fold_key_dir_names c (fold_key f) acc)
    with Failure e -> Error (err "fold" e)

  let keys c =
    let add_name acc fname = key_of_filename fname :: acc in
    try Ok (cache_fold_key_dir_names c add_name [])
    with Failure e -> Error (err "keys" e)

  let is_unused c k = try key_dir_exit_if_used (key_dir c k); Ok true with
  | Exit -> Ok false
  | Failure e -> Error (err_key "unused check" k e)

  let delete_unused c =
    let delete_unused_key () fname =
      let kdir = key_dir_of_filename c fname in
      try key_dir_exit_if_used kdir; ignore (key_dir_delete kdir) with
      | Exit -> ()
    in
    try Ok (cache_fold_key_dir_names c delete_unused_key ())
    with Failure e -> Error (err "delete unused" e)

  let trim_size c ~max_byte_size ~pct =
    try
      let order ((u0, a0, s0), _) ((u1, a1, s1), _) = match u0, u1 with
      | true, false -> -1 (* unused first *)
      | false, true -> 1
      | _ ->
          match compare (a0 : float) (a1 : float) with (* smaller atime *)
          | 0 -> compare (s1 : int) (s0 : int) (* greater size *)
          | cmp -> cmp
      in
      let add_key (total_size, ks) fname =
        let kdir = key_dir_of_filename c fname in
        let _, ksize, unused, atime = key_dir_stats kdir in
        (total_size + ksize), ((unused, atime, ksize), kdir) :: ks
      in
      let total_size, ks = cache_fold_key_dir_names c add_key (0, []) in
      let pct_size = truncate @@ (float total_size /. 100.) *. float pct in
      let budget = (min : int -> int -> int) max_byte_size pct_size in
      let rec delete_keys current budget = function
      | [] -> ()
      | _ when current <= budget -> ()
      | ((_, _, size), kdir) :: kdirs ->
          ignore (key_dir_delete kdir);
          delete_keys (current - size) budget kdirs
      in
      Ok (delete_keys total_size budget @@ List.sort order ks)
    with Failure e -> Error (err "trim size" e)

  module Stats = struct
    type keys = {keys_count : int; keys_file_count : int; keys_byte_size : int}
    let keys_count s = s.keys_count
    let keys_byte_size s = s.keys_byte_size
    let keys_file_count s = s.keys_file_count
    let keys_zero = { keys_count = 0; keys_file_count = 0; keys_byte_size = 0 }
    let keys_sub s0 s1 =
      { keys_count = s0.keys_count - s1.keys_count;
        keys_file_count = s0.keys_file_count - s1.keys_file_count;
        keys_byte_size = s0.keys_byte_size - s1.keys_byte_size }

    let pp_keys ppf s =
      Fmt.pf ppf "keys: %4d files: %4d size: %6a"
        s.keys_count s.keys_file_count Fmt.byte_size s.keys_byte_size

    let of_keys c ns =
      let rec loop k f b = function
      | [] -> { keys_count = k; keys_file_count = f; keys_byte_size = b }
      | n :: ns ->
          let kf, kb, _, _ = key_dir_stats (key_dir c n) in
          loop (k + 1) (f + kf) (b + kb) ns
      in
      try Ok (loop 0 0 0 ns) with
      | Failure e -> Error (err "keys stats" e)

    type cache = { all_keys : keys; unused_keys : keys }
    let zero = { all_keys = keys_zero; unused_keys = keys_zero }
    let all_keys s = s.all_keys
    let unused_keys s = s.unused_keys
    let pp =
      Fmt.record @@
      [ Fmt.field "unused" unused_keys pp_keys;
        Fmt.field " total" all_keys pp_keys ]

    let of_cache c =
      let rec loop k f b uk uf ub = function
      | [] ->
          let a = {keys_count=k; keys_file_count=f; keys_byte_size=b} in
          let u = {keys_count=uk; keys_file_count=uf; keys_byte_size=ub} in
          { all_keys = a; unused_keys = u }
      | n :: ns ->
          let kdir = key_dir_of_filename c n in
          let kf, kb, key_unused, _ = key_dir_stats kdir in
          match key_unused with
          | true -> loop (k+1) (f+kf) (b+kb) (uk+1) (uf+kf) (ub+kb) ns
          | false -> loop (k+1) (f+kf) (b+kb) uk uf ub ns
      in
      try
        let names = cache_fold_key_dir_names c (fun acc n -> n :: acc) [] in
        Ok (loop 0 0 0 0 0 0 names)
      with
      | Failure e -> Error (err "stats" e)
  end
end

module Op = struct

  (* Operation status *)

  exception Fail of string
  exception Fail_through

  type failure = Exec of string option | Missing_writes of Fpath.t list
  type status = Aborted | Executed | Failed of failure | Waiting
  let status_to_string = function
  | Aborted -> "aborted" | Executed -> "executed" | Waiting -> "waiting"
  | Failed f ->
      match f with
      | Exec None -> "failed"
      | Exec (Some msg) -> Fmt.str "failed: %s" msg
      | Missing_writes fs ->
          Fmt.str "@[<v>failed: Did not write:@,%a@]"
            (Fmt.list Fpath.pp_quoted) fs

  (* Operation kinds *)

  type copy =
    { copy_src : Fpath.t; copy_dst : Fpath.t; copy_mode : int;
      copy_linenum : int option; }

  type delete = { delete_path : Fpath.t; }
  type mkdir = { mkdir_dir : Fpath.t; mkdir_mode : int; }
  type notify_kind = [ `End | `Fail | `Info | `Start | `Warn ]
  type notify = { notify_kind : notify_kind; notify_msg : string }
  type read =
    { read_file : Fpath.t;
      (* mutable to discard it after the read as been kontinued. *)
      mutable read_data : string; }

  type spawn_stdo = [ `Ui | `File of Fpath.t | `Tee of Fpath.t ]
  type spawn_success_exits = int list
  type spawn =
    { env : Os.Env.assignments;
      relevant_env : Os.Env.assignments;
      cwd : Fpath.t;
      stdin : Fpath.t option;
      stdout : spawn_stdo;
      stderr : spawn_stdo;
      success_exits : spawn_success_exits;
      tool : Cmd.tool;
      args : Cmd.t;
      mutable spawn_stamp : string;
      mutable stdo_ui : (string, string) result option;
      mutable spawn_exit : Os.Cmd.status option; }

  type wait_files = unit
  type write =
    { write_stamp : string; write_mode : int; write_file : Fpath.t;
      (* mutable to discard it after the write is done. *)
      mutable write_data : unit -> (string, string) result; }

  (* Operations *)

  type kind =
  | Copy of copy
  | Delete of delete
  | Mkdir of mkdir
  | Notify of notify
  | Read of read
  | Spawn of spawn
  | Wait_files of wait_files
  | Write of write

  let kind_name = function
  | Copy _ -> "copy" | Delete _ -> "delete" | Notify _ -> "note"
  | Mkdir _ -> "mkdir" | Read _ -> "read" | Spawn _ -> "spawn"
  | Wait_files _ -> "wait" | Write _ -> "write"

  type id = int
  type group = string
  type t =
    { id : id;
      group : group;
      time_created : Time.span;
      mutable time_started : Time.span;
      mutable duration : Time.span;
      mutable revived : bool;
      mutable status : status;
      mutable reads : Fpath.t list;
      mutable writes : Fpath.t list;
      mutable hash : Hash.t;
      mutable post_exec : (t -> unit) option;
      mutable k : (t -> unit) option;
      kind : kind }

  type op = t
  let v
      id ~group ~time_created ~time_started ~duration ~revived ~status ~reads
      ~writes ~hash ?post_exec ?k kind
    =
    { id; group; time_created; time_started; duration; revived; status; reads;
      writes; hash; post_exec; k; kind; }

  let v_kind ~id ~group ~created ~reads ~writes ?post_exec ?k kind =
    let time_started = Time.Span.max and duration = Time.Span.zero in
    let revived = false and status = Waiting and hash = Hash.nil in
    { id; group; time_created = created; time_started; duration; revived;
      status; reads; writes; hash; post_exec; k; kind }

  let id o = o.id
  let group o = o.group
  let kind o = o.kind
  let time_created o = o.time_created
  let time_started o = o.time_started
  let time_ended o = Time.Span.add o.time_created o.duration
  let waited o = Time.Span.abs_diff o.time_started o.time_created
  let duration o = o.duration
  let revived o = o.revived
  let status o = o.status
  let reads o = o.reads
  let writes o = o.writes
  let did_not_write o =
    let rec loop acc = function
    | [] -> List.sort Fpath.compare acc
    | f :: fs ->
        match Unix.access (Fpath.to_string f) [Unix.F_OK] with
        | exception Unix.Unix_error _ -> loop (f :: acc) fs
        | () -> loop acc fs
    in
    loop [] o.writes

  let hash o = o.hash
  let discard_k o = o.k <- None
  let invoke_k o = match o.k with None -> () | Some k -> discard_k o; k o
  let discard_post_exec o = o.post_exec <- None
  let invoke_post_exec o = match o.post_exec with
  | None -> ()
  | Some hook ->
      discard_post_exec o;
      try hook o with
      | Stack_overflow as e -> raise e
      | Out_of_memory as e -> raise e
      | Sys.Break as e -> raise e
      | e ->
          let bt = Printexc.get_raw_backtrace () in
          let err =
            Fmt.str "@[<v>Post execution hook raised unexpectedly:@,%a@]"
              Fmt.exn_backtrace (e, bt)
          in
          o.status <- Failed (Exec (Some err))

  let equal o0 o1 = o0.id = o1.id
  let compare o0 o1 = (compare : int -> int -> int) o0.id o1.id
  let set_time_started o t = o.time_started <- t
  let set_time_ended o t = o.duration <- Time.Span.abs_diff t o.time_started
  let set_revived o b = o.revived <- b
  let set_status o s = o.status <- s
  let set_reads o fs = o.hash <- Hash.nil; o.reads <- fs
  let set_writes o fs = o.writes <- fs
  let set_hash o h = o.hash <- h
  let set_status_from_result o r =
    let st = match r with Ok _ -> Executed | Error e -> Failed (Exec (Some e))in
    set_status o st

  module Copy = struct
    type t = copy
    let v ~src ~dst ~mode ~linenum:l =
      { copy_src = src; copy_dst = dst; copy_mode = mode; copy_linenum = l }

    let get o = match o.kind with Copy c -> c | _ -> assert false
    let src c = c.copy_src
    let dst c = c.copy_dst
    let mode c = c.copy_mode
    let linenum c = c.copy_linenum
    let v_op ~id ~group ~created ?post_exec ?k ~mode ~linenum ~src dst
      =
      let c = { copy_src = src; copy_dst = dst; copy_mode = mode;
                copy_linenum = linenum }
      in
      let reads = [src] and writes = [dst] in
      v_kind ~id ~group ~created ~reads ~writes ?post_exec ?k (Copy c)
  end

  module Delete = struct
    type t = delete
    let v ~path = { delete_path = path }
    let get o = match o.kind with Delete d -> d | _ -> assert false
    let path d = d.delete_path
    let v_op ~id ~group ~created ?post_exec ?k delete_path =
      let d = { delete_path } in
      v_kind ~id ~group ~created ~reads:[] ~writes:[] ?post_exec ?k (Delete d)
  end

  module Mkdir = struct
    type t = mkdir
    let v ~dir ~mode = { mkdir_dir = dir; mkdir_mode = mode }
    let get o = match o.kind with Mkdir mk -> mk | _ -> assert false
    let dir mk = mk.mkdir_dir
    let mode mk = mk.mkdir_mode
    let v_op ~id ~group ~mode ~created ?post_exec ?k dir =
      let mkdir = { mkdir_dir = dir; mkdir_mode = mode } in
      v_kind
        ~id ~group ~created ~reads:[] ~writes:[dir] ?post_exec ?k (Mkdir mkdir)
  end

  module Notify = struct
    type kind = notify_kind
    let kind_to_string = function
    | `End -> "end" | `Fail -> "fail" | `Info -> "info" | `Start -> "start"
    | `Warn -> "warn"

    type t = notify
    let v ~kind ~msg = { notify_kind = kind; notify_msg = msg }
    let get o = match o.kind with Notify n -> n | _ -> assert false
    let kind n = n.notify_kind
    let msg n = n.notify_msg
    let v_op ~id ~group ~created ?post_exec ?k notify_kind notify_msg =
      let n = { notify_kind; notify_msg } in
      v_kind ~id ~group ~created ~reads:[] ~writes:[] ?post_exec ?k (Notify n)
  end

  module Read = struct
    type t = read
    let v ~file ~data = { read_file = file; read_data = data }
    let get o = match o.kind with Read r -> r | _ -> assert false
    let file r = r.read_file
    let data r = r.read_data
    let set_data r d = r.read_data <- d
    let discard_data r = r.read_data <- ""
    let v_op ~id ~group ~created ?post_exec ?k file =
      let read = { read_file = file; read_data = "" } in
      v_kind
        ~id ~group ~created ~reads:[file] ~writes:[] ?post_exec ?k (Read read)
  end

  module Spawn = struct
    type stdo = spawn_stdo
    type success_exits = spawn_success_exits
    type t = spawn
    let v
        ~env ~relevant_env ~cwd ~stdin ~stdout ~stderr ~success_exits tool
        args ~stamp ~stdo_ui ~exit
      =
      { env; relevant_env; cwd; stdin; stdout; stderr; success_exits; tool;
        args; spawn_stamp = stamp; stdo_ui; spawn_exit = exit }

    let get o = match o.kind with Spawn s -> s | _ -> assert false
    let env s = s.env
    let relevant_env s = s.relevant_env
    let cwd s = s.cwd
    let stdin s = s.stdin
    let stdout s = s.stdout
    let stderr s = s.stderr
    let success_exits s = s.success_exits
    let tool s = s.tool
    let args s = s.args
    let stamp s = s.spawn_stamp
    let set_stamp s stamp = s.spawn_stamp <- stamp
    let stdo_ui s = s.stdo_ui
    let set_stdo_ui s ui = s.stdo_ui <- ui
    let exit s = s.spawn_exit
    let set_exit s e = s.spawn_exit <- e
    let exit_to_status s = match s.spawn_exit with
    | None -> Failed (Exec None)
    | Some (`Signaled c) -> Failed (Exec None)
    | Some (`Exited c) ->
        match success_exits s with
        | [] -> Executed
        | cs when List.mem c cs -> Executed
        | cs -> Failed (Exec None)

    let v_op
        ~id ~group ~created ~reads ~writes ?post_exec ?k ~stamp ~env
        ~relevant_env ~cwd ~stdin ~stdout ~stderr ~success_exits tool args
      =
      let spawn =
        { env; relevant_env; cwd; stdin; stdout; stderr; success_exits;
          tool; args; spawn_stamp = stamp; stdo_ui = None; spawn_exit = None }
      in
      v_kind ~id ~group ~created ~reads ~writes ?post_exec ?k (Spawn spawn)
  end

  module Wait_files = struct
    type t = wait_files
    let v () = ()
    let v_op ~id ~group ~created ?post_exec ?k reads =
      v_kind ~id ~group ~created ~reads ~writes:[] ?post_exec ?k (Wait_files ())
  end

  module Write = struct
    type t = write
    let v ~stamp ~mode ~file ~data =
      { write_stamp = stamp; write_mode = mode; write_file = file;
        write_data = data }

    let get o = match o.kind with Write r -> r | _ -> assert false
    let stamp w = w.write_stamp
    let mode w = w.write_mode
    let file w = w.write_file
    let discard_data w =
      w.write_data <- fun _ -> Error "write function discarded"

    let data w =
      let data = w.write_data in
      discard_data w;
      try data () with
      | Stack_overflow as e -> raise e
      | Out_of_memory as e -> raise e
      | Sys.Break as e -> raise e
      | e ->
          let bt = Printexc.get_raw_backtrace () in
          Fmt.error "[@<v>Write function raised:@,%a@]"
            Fmt.exn_backtrace (e, bt)

    let v_op
        ~id ~group ~created ?post_exec ?k ~stamp:write_stamp ~reads ~mode
        ~write:f write_data
      =
      let w = { write_stamp; write_mode = mode; write_file = f; write_data } in
      v_kind ~id ~group ~created ~reads ~writes:[f] ?post_exec ?k (Write w)
  end

  let abort o =
    set_status o Aborted;
    discard_k o; discard_post_exec o;
    match o.kind with
    | Write w -> Write.discard_data w
    | _ -> ()

  (* Operation sets and maps *)

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  (* Operation analyses *)

  let unwritten_reads os =
    let add_path acc p = Fpath.Set.add p acc in
    let rec loop ws rs = function
    | [] -> Fpath.Set.diff rs ws
    | o :: os ->
        let ws = List.fold_left add_path ws (writes o) in
        let rs = List.fold_left add_path rs (reads o) in
        loop ws rs os
    in
    loop Fpath.Set.empty Fpath.Set.empty os

  let _write_map ~check_single os =
    let add_write o acc w = match Fpath.Map.find w acc with
    | exception Not_found -> Fpath.Map.add w (Set.singleton o) acc
    | os when check_single -> raise Exit
    | os -> Fpath.Map.add w (Set.add o os) acc
    in
    let add_writes acc o = List.fold_left (add_write o) acc (writes o) in
    List.fold_left add_writes Fpath.Map.empty os

  let write_map = _write_map ~check_single:false
  let single_writes os =
    try ignore (_write_map ~check_single:true os); true with Exit -> false

  let op_deps ~write_map o =
      let add_read_deps acc r = match Fpath.Map.find r write_map with
      | exception Not_found -> acc
      | os -> Set.union os acc
      in
      List.fold_left add_read_deps Set.empty (reads o)

  let find_read_write_cycle os =
    let path_to_start ~start path =
      let rec loop start acc = function
      | o :: os when (id o = id start) -> o :: acc
      | o :: os -> loop start (o :: acc) os
      | [] -> assert false
      in
      loop start [] path
    in
    let rec loop write_map unvisited in_path path = function
    | [] ->
        begin match Set.choose unvisited with
        | exception Not_found -> None
        | o ->
            let unvisited = Set.remove o unvisited in
            let in_path = Set.singleton o and path = [o] in
            loop write_map unvisited in_path path [o, op_deps ~write_map o]
        end
    | (o, ds) :: todo ->
        match Set.choose ds with
        | exception Not_found ->
            loop write_map unvisited (Set.remove o in_path) (List.tl path) todo
        | d ->
            if Set.mem d in_path then Some (path_to_start ~start:d path) else
            let todo = (o, Set.remove d ds) :: todo in
            match Set.mem d unvisited with
            | false -> loop write_map unvisited in_path path todo
            | true ->
                let unvisited = Set.remove d unvisited in
                let in_path = Set.add d in_path and path = d :: path in
                let todo = (d, op_deps ~write_map d) :: todo in
                loop write_map unvisited in_path path todo
    in
    loop (write_map os) (Set.of_list os) Set.empty [] []
end

module Reviver = struct

  (* Operation reviver *)

  type t =
    { clock : Time.counter;
      hash_fun : (module Hash.T);
      cache : File_cache.t;
      buffer : Buffer.t; (* buffer to encode metadata *)
      mutable file_hashes : Hash.t Fpath.Map.t; (* file hash cache *)
      mutable file_hash_dur : Time.span; (* total file hash duration *) }

  let create clock hash_fun cache =
    let buffer = Buffer.create 1024 in
    let file_hashes = Fpath.Map.empty in
    let file_hash_dur = Time.Span.zero in
    { clock; hash_fun; buffer; cache; file_hashes; file_hash_dur }

  let clock r = r.clock
  let hash_fun r = r.hash_fun
  let file_cache r = r.cache
  let timestamp r = Time.count r.clock

  (* Hashing *)

  let hash_string r s =
    let module H = (val r.hash_fun : Hash.T) in
    H.string s

  let _hash_file r f = match Fpath.Map.find f r.file_hashes with
  | h -> h
  | exception Not_found ->
      let module H = (val r.hash_fun : Hash.T) in
      let t = timestamp r in
      let h = H.file f in
      let dur = Time.Span.abs_diff (timestamp r) t in
      r.file_hash_dur <- Time.Span.add r.file_hash_dur dur;
      match h with
      | Ok h -> r.file_hashes <- Fpath.Map.add f h r.file_hashes; h
      | Error e -> failwith e

  let hash_file r f = try Ok (_hash_file r f) with Failure e -> Error e

  let hash_op_reads r o acc =
    let add_file_hash acc f = Hash.to_bytes (_hash_file r f) :: acc in
    List.fold_left add_file_hash acc (Op.reads o)

  let hash_spawn r o s =
    let stdin_stamp = function None -> "0" | Some _ -> "1" in
    let stdo_stamp = function `File _ -> "0" | `Tee _ -> "1" | `Ui -> "2" in
    let module H = (val r.hash_fun : Hash.T) in
    let acc = [Op.Spawn.stamp s] in
    let acc = (Hash.to_bytes (_hash_file r (Op.Spawn.tool s))) :: acc in
    let acc = hash_op_reads r o acc in
    let acc = stdin_stamp (Op.Spawn.stdin s) :: acc in
    let acc = stdo_stamp (Op.Spawn.stdout s) :: acc in
    let acc = stdo_stamp (Op.Spawn.stderr s) :: acc in
    let env = Op.Spawn.relevant_env s in
    let acc = List.fold_left (fun acc v -> v :: acc) acc env in
    let acc = List.rev_append (Cmd.to_stamp @@ Op.Spawn.args s) acc in
    H.string (String.concat "" acc)

  let hash_write r o w =
    let module H = (val r.hash_fun : Hash.T) in
    let acc = string_of_int (Op.Write.mode w) :: [Op.Write.stamp w] in
    let acc = hash_op_reads r o acc in
    H.string (String.concat "" acc)

  let hash_copy r o c =
    let module H = (val r.hash_fun : Hash.T) in
    let linenum_stamp = function None -> "n" | Some i -> string_of_int i in
    let acc = hash_op_reads r o [] in
    let acc = string_of_int (Op.Copy.mode c) :: acc in
    let acc = linenum_stamp (Op.Copy.linenum c) :: acc in
    H.string (String.concat "" acc)

  let hash_op r o =
    try
      Ok begin match Op.kind o with
      | Op.Spawn s -> hash_spawn r o s
      | Op.Write w -> hash_write r o w
      | Op.Copy c -> hash_copy r o c
      | Op.Delete _ | Op.Read _ | Op.Notify _ | Op.Mkdir _ | Op.Wait_files _ ->
          Hash.nil
      end
    with Failure e -> Error e

  let file_hashes r = r.file_hashes
  let file_hash_dur r = r.file_hash_dur

  (* Recording and reviving operations *)

  let spawn_meta_conv :
    ((string, string) result option * Os.Cmd.status option) Conv.t
    =
    Conv.(pair ~kind:"spawn-meta" ~docvar:"SPAWN"
            (option (result string_bytes string_bytes))
            (option os_cmd_status))

  let file_cache_key o = Hash.to_hex (Op.hash o)

  let revive_spawn r o s =
    let key = file_cache_key o in
    let writes = Op.writes o in
    Result.bind (File_cache.revive r.cache key writes) @@ function
    | None -> Ok None
    | Some (m, existed) ->
        Result.bind (Conv.of_bin spawn_meta_conv m) @@ fun (stdo_ui, exit) ->
        Op.set_revived o true;
        Op.Spawn.set_stdo_ui s stdo_ui;
        Op.Spawn.set_exit s exit;
        Op.set_status o (Op.Spawn.exit_to_status s);
        Op.invoke_post_exec o;
        Op.set_time_ended o (timestamp r);
        Ok (Some existed)

  let revive_op r o kind op_kind =
    let key = file_cache_key o in
    let writes = Op.writes o in
    Result.bind (File_cache.revive r.cache key writes) @@ function
    | None -> Ok None
    | Some (_, existed) ->
        op_kind kind;
        Op.set_revived o true;
        Op.set_status o Op.Executed;
        Op.invoke_post_exec o;
        Op.set_time_ended o (timestamp r);
        Ok (Some existed)

  let op_write w = Op.Write.discard_data w (* get rid of data closure. *)
  let op_nop _ = ()

  let revive r o =
    Op.set_time_started o (timestamp r);
    match Op.kind o with
    | Op.Spawn s -> revive_spawn r o s
    | Op.Write w -> revive_op r o w op_write
    | Op.Copy c -> revive_op r o c op_nop
    | Op.Delete _ | Op.Read _ | Op.Notify _ | Op.Mkdir _ | Op.Wait_files _ ->
        Ok None

  let record_spawn r o s =
    let spawn_meta = Op.Spawn.stdo_ui s, Op.Spawn.exit s in
    match Conv.to_bin ~buf:r.buffer spawn_meta_conv spawn_meta with
    | Error _ as e -> e
    | Ok m -> File_cache.add r.cache (file_cache_key o) m (Op.writes o)

  let record_write r o w =
    File_cache.add r.cache (file_cache_key o) "" (Op.writes o)

  let record_copy r o c =
    File_cache.add r.cache (file_cache_key o) "" (Op.writes o)

  let record r o = match Op.kind o with
  | Op.Spawn s -> record_spawn r o s
  | Op.Write w -> record_write r o w
  | Op.Copy c -> record_copy r o c
  | Op.Delete _ | Op.Read _ | Op.Notify _ | Op.Mkdir _ | Op.Wait_files _ ->
      Ok true
end

module Guard = struct

  type feedback =
    [ `File_status_repeat of Fpath.t | `File_status_unstable of Fpath.t ]

  (* The type [gop] holds a guarded operation. It keeps in [awaits]
     the files that need to become ready before the operation [op] can
     be added to the [allowed] queue of the type [t].

     The type [t] is the guard. It has operations that are allowed to
     proceed in [allowed] and maps files to their status in [files].

     If a file becomes ready it maps to Ready in [files] and the
     [Blocks] operation that were waiting on it get their [await]s
     changed so that it no longer mentions the file.

     If a file never becomes ready it maps to [Never] in [files] and
     the [Blocks] operations that where waiting on it are immediately
     aborted and added to [allowed]. These operations may still exist
     in other [Blocks] but functions take care to ignore these when
     they hit them rather than try to hunt them in a data structure
     that is not made for this. This makes sure they only get into
     [allowed] once.

     Note that when an operation is aborted by this module, it doesn't
     mark its writes as `Never in the guard (this may not be used by
     B00.Memo but it allows for example to let other operations take
     over the writes of these files). *)

  type gop = { op : Op.t; mutable awaits : Fpath.Set.t; }
  type file_status = Ready | Never | Blocks of gop list
  type t =
    { feedback : feedback -> unit;
      allowed : Op.t Queue.t;
      mutable files : file_status Fpath.Map.t }

  let create ?(feedback = fun _ -> ()) () =
    { feedback; allowed = Queue.create (); files = Fpath.Map.empty; }

  let set_file_ready g f = match Fpath.Map.find f g.files with
  | exception Not_found -> g.files <- Fpath.Map.add f Ready g.files
  | Ready -> g.feedback (`File_status_repeat f)
  | Never -> g.feedback (`File_status_unstable f)
  | Blocks gops ->
      let rem_await g f gop = match Op.status gop.op with
      | Op.Aborted -> ()
      | _ ->
          let awaits = Fpath.Set.remove f gop.awaits in
          if Fpath.Set.is_empty awaits
          then Queue.add gop.op g.allowed
          else (gop.awaits <- awaits)
      in
      g.files <- Fpath.Map.add f Ready g.files;
      List.iter (rem_await g f) gops

  let set_file_never g f = match Fpath.Map.find f g.files with
  | exception Not_found -> g.files <- Fpath.Map.add f Never g.files
  | Ready -> g.feedback (`File_status_unstable f)
  | Never -> g.feedback (`File_status_repeat f)
  | Blocks gops ->
      let rem_await g f gop = match Op.status gop.op with
      | Op.Aborted -> ()
      | _ -> Op.abort gop.op; Queue.add gop.op g.allowed
      in
      g.files <- Fpath.Map.add f Never g.files;
      List.iter (rem_await g f) gops

  let add g o =
    let rec loop g gop awaits files = function
    | [] ->
        if Fpath.Set.is_empty awaits
        then Queue.add gop.op g.allowed
        else (gop.awaits <- awaits; g.files <- files)
    | f :: fs ->
        match Fpath.Map.find f files with
        | exception Not_found ->
            let awaits = Fpath.Set.add f awaits in
            let files = Fpath.Map.add f (Blocks [gop]) files in
            loop g gop awaits files fs
        | Never -> Op.abort gop.op; Queue.add gop.op g.allowed
        | Ready -> loop g gop awaits files fs
        | Blocks gops ->
            let awaits = Fpath.Set.add f awaits in
            let files = Fpath.Map.add f (Blocks (gop :: gops)) files in
            loop g gop awaits files fs
    in
    let gop = { op = o; awaits = Fpath.Set.empty } in
    loop g gop Fpath.Set.empty g.files (Op.reads o)

  let allowed g = match Queue.take g.allowed with
  | exception Queue.Empty -> None
  | o -> Some o

  (* Stuck build anaylsis *)

  let rec add_guarded_ops acc = function
  | [] -> acc
  | gop :: gops when Op.status gop.op = Op.Aborted -> add_guarded_ops acc gops
  | gop :: gops -> add_guarded_ops (Op.Set.add gop.op acc) gops

  let guarded_ops g =
    let add f st acc = match st with
    | Blocks gops -> add_guarded_ops acc gops
    | _ -> acc
    in
    Op.Set.elements (Fpath.Map.fold add g.files Op.Set.empty)

  let ready_files g =
    let add f st acc = match st with Ready -> Fpath.Set.add f acc | _ -> acc in
    Fpath.Map.fold add g.files Fpath.Set.empty

  let never_files g =
    let add f st acc = match st with Never -> Fpath.Set.add f acc | _ -> acc in
    Fpath.Map.fold add g.files Fpath.Set.empty

  let undecided_files g =
    let add f st acc = match st with Blocks _ -> Fpath.Set.add f acc | _ -> acc
    in
    Fpath.Map.fold add g.files Fpath.Set.empty

  let root_undecided_files g =
    (* We gather ops in a set rather than their writes directly
       it might be more efficient if ops write many files *)
    let add_undecided f st (us, gops as acc) = match st with
    | Blocks ops -> (Fpath.Set.add f us, add_guarded_ops gops ops)
    | _ -> acc
    in
    let add_write acc p = Fpath.Set.add p acc in
    let add_writes o acc = List.fold_left add_write acc (Op.writes o) in
    let ops_writes gops = Op.Set.fold add_writes gops Fpath.Set.empty in
    let init = (Fpath.Set.empty, Op.Set.empty) in
    let undecided, gops = Fpath.Map.fold add_undecided g.files init in
    let ops_writes = ops_writes gops in
    Fpath.Set.diff undecided ops_writes
end

module Exec = struct
  type feedback = [ `Exec_start of Os.Cmd.pid option * Op.t ]
  type t =
    { clock : Time.counter;
      tmp_dir : Fpath.t;
      feedback : feedback -> unit;
      trash : Trash.t;
      todo : Op.t Rqueue.t; (* Waiting for OS submission. *)
      collectable : Op.t Queue.t;
      to_spawn : Op.t Queue.t; (* [dequeued] from [todo] waiting to spawn. *)
      jobs : int; (* Max number of spawned processes *)
      mutable spawn_count : int; (* Number of spawned processes *)
      mutable spawns : (Os.Cmd.pid * Fpath.t option * Op.t) list; }

  let create ?clock ?rand ?tmp_dir:tmp ?feedback ~trash ~jobs () =
    let feedback = match feedback with None -> fun _ -> () | Some f -> f in
    let clock = match clock with None -> Time.counter () | Some c -> c in
    let tmp_dir = match tmp with None -> Os.Dir.default_tmp () | Some t -> t in
    let todo = Rqueue.empty ?rand () in
    let collectable = Queue.create () in
    let to_spawn = Queue.create () in
    { clock; tmp_dir; feedback; trash; todo; collectable; to_spawn; jobs;
      spawn_count = 0; spawns = [] }

  let clock e = e.clock
  let tmp_dir e = e.tmp_dir
  let trash e = e.trash
  let jobs c = c.jobs
  let incr_spawn_count e = e.spawn_count <- e.spawn_count + 1
  let decr_spawn_count e = e.spawn_count <- e.spawn_count - 1
  let timestamp e = Time.count e.clock

  (* Operation execution *)

  let finish_exec_spawn e o ui result =
    let read_stdo_ui s ui =
      let append f0 f1 =
        Result.bind (Os.File.read f0) @@ fun f0 ->
        Result.bind (Os.File.read f1) @@ fun f1 ->
        Ok (String.trim @@ Fmt.str "%s\n%s" f0 f1)
      in
      let ret ui = match ui with Ok "" -> None | ui -> Some ui in
      match Op.Spawn.stdout s, Op.Spawn.stderr s with
      | `Ui, `Ui -> ret @@ Os.File.read ui
      | `Ui, `File _ | `File _, `Ui -> ret @@ Os.File.read ui
      | `File _, `File _ -> None
      | `Ui, `Tee f | `Tee f, `Ui -> ret @@ append f ui
      | `Tee f0, `Tee f1 -> ret @@ append f0 f1
      | `Tee f, `File _ | `File _, `Tee f -> ret @@ Os.File.read f
    in
    let s = Op.Spawn.get o in
    let stdo_ui = match ui with None -> None | Some ui -> (read_stdo_ui s ui) in
    Op.Spawn.set_stdo_ui s stdo_ui;
    begin match result with
    | Error _ as e -> Op.set_status_from_result o e
    | Ok exit ->
        Op.Spawn.set_exit s (Some exit);
        Op.set_status o (Op.Spawn.exit_to_status s)
    end;
    Op.invoke_post_exec o;
    Op.set_time_ended o (timestamp e);
    decr_spawn_count e;
    Queue.add o e.collectable

  let start_exec_spawn e o =
    let spawn s =
      let get_stdo_ui_file e = match Os.File.open_tmp_fd ~dir:e.tmp_dir () with
      | Ok (file, fd) -> Os.Cmd.out_fd ~close:true fd, Some file
      | Error e -> failwith e
      in
      try
        let env = Op.Spawn.env s and cwd = Op.Spawn.cwd s in
        let stdin = match Op.Spawn.stdin s with
        | None -> Os.Cmd.in_fd ~close:false Unix.stdin (* XXX /dev/null no ? *)
        | Some f -> Os.Cmd.in_file f
        in
        let force = true and make_path = true in
        let stdout, ui = match Op.Spawn.stdout s with
        | `File f | `Tee f -> Os.Cmd.out_file ~force ~make_path f, None
        | `Ui -> get_stdo_ui_file e
        in
        let stderr, ui = match Op.Spawn.stderr s with
        | `File f | `Tee f -> Os.Cmd.out_file ~force ~make_path f, ui
        | `Ui ->
            match ui with
            | Some _ -> stdout, ui
            | None -> get_stdo_ui_file e
        in
        let cmd = Cmd.(path (Op.Spawn.tool s) %% Op.Spawn.args s) in
        match Os.Cmd.spawn ~env ~cwd ~stdin ~stdout ~stderr cmd with
        | Error e -> failwith e
        | Ok pid ->
            e.spawns <- (pid, ui, o) :: e.spawns;
            e.feedback (`Exec_start (Some pid, o))
      with Failure err -> finish_exec_spawn e o None (Error err)
    in
    incr_spawn_count e;
    Op.set_time_started o (timestamp e);
    spawn (Op.Spawn.get o)

  let exec_op e o kind op_kind =
    Op.set_time_started o (timestamp e);
    e.feedback (`Exec_start (None, o));
    Op.set_status_from_result o (op_kind e kind);
    Op.invoke_post_exec o;
    Op.set_time_ended o (timestamp e);
    Queue.add o e.collectable

  let op_copy _e c =
    let atomic = true and force = true and make_path = true in
    let mode = Op.Copy.mode c and src = Op.Copy.src c and dst = Op.Copy.dst c in
    match Op.Copy.linenum c with
    | None -> Os.File.copy ~atomic ~force ~make_path ~mode ~src dst
    | Some line ->
        Result.bind (Os.File.read src) @@ fun c ->
        let data = Fmt.str "#line %d \"%a\"\n%s" line Fpath.pp_unquoted src c in
        Os.File.write ~atomic ~force ~make_path ~mode dst data

  let op_delete e d = Trash.trash e.trash (Op.Delete.path d)
  let op_mkdir _e mk =
    let dir = Op.Mkdir.dir mk and mode = Op.Mkdir.mode mk in
    Os.Dir.create ~mode ~make_path:true dir

  let op_notify e n = match Op.Notify.kind n with
  | `Fail -> Error (Op.Notify.msg n)
  | _ -> Ok ()

  let op_read _e read = match Os.File.read (Op.Read.file read) with
  | Ok data as r -> Op.Read.set_data read data; r
  | Error _ as r -> r

  let op_write _e w = match Op.Write.data w with
  | Error _ as r -> r
  | Ok data ->
      let mode = Op.Write.mode w in
      Os.File.write ~force:true ~make_path:true ~mode (Op.Write.file w) data

  (* Scheduling

     As it stands this implementation does all the file operations
     synchronously on submit but nothing prevents for asynchronous or
     thread pool implementations. *)

  let submit e o = match Op.kind o with
  | Op.Spawn _ -> Queue.add o e.to_spawn (* see submit_spawns *)
  | Op.Read r -> exec_op e o r op_read
  | Op.Write w -> exec_op e o w op_write
  | Op.Copy c -> exec_op e o c op_copy
  | Op.Notify n -> exec_op e o n op_notify
  | Op.Mkdir mk -> exec_op e o mk op_mkdir
  | Op.Delete d -> exec_op e o d op_delete
  | Op.Wait_files w -> exec_op e o w (fun _ _ -> Ok ())

  let submit_spawns e =
    let free = e.jobs - e.spawn_count in
    let may_spawn = Queue.length e.to_spawn in
    let spawn_limit = if free < may_spawn then free else may_spawn in
    let rec loop e = function
    | 0 -> ()
    | n ->
        match Queue.take e.to_spawn with
        | exception Queue.Empty -> ()
        | o -> start_exec_spawn e o; loop e (n - 1)
    in
    loop e spawn_limit

  let rec collect_spawns ~block e =
    let relax () = ignore (Unix.select [] [] [] 0.0001) in
    match e.spawn_count = 0 with
    | true -> ()
    | false ->
        (* We don't (and can't through B0_std.Os.Cmd API constraints)
           collect with -1 or 0 because library-wise we might collect
           things we did not spawn. On Windows there wouldn't be the
           choice anyways. This means that on a blocking collection
           there's a bit of busy waiting involved, which we mitigate
           with [relax]. Sys.sigchild and a self-pipe trick could be
           used, but again library wise it's better if we can avoid
           fiddling with signal handlers. *)
        let old_spawn_count = e.spawn_count in
        let collect spawns (pid, ui, o as p) =
          match Os.Cmd.spawn_poll_status pid with
          | Error _ as err -> finish_exec_spawn e o ui err; spawns
          | Ok None -> p :: spawns
          | Ok (Some st) -> finish_exec_spawn e o ui (Ok st); spawns
        in
        e.spawns <- List.fold_left collect [] e.spawns;
        match block && old_spawn_count = e.spawn_count with
        | true -> (* busy waiting *) relax (); collect_spawns ~block e
        | false -> ()

  let all_done e = (* [true] iff nothing left todo *)
    Rqueue.length e.todo = 0 && Queue.length e.to_spawn = 0 && e.spawn_count = 0

  let rec stir ~block e = match Rqueue.take e.todo with
  | Some o ->
      collect_spawns ~block:false e;
      submit e o;
      submit_spawns e;
      stir ~block e
  | None when all_done e -> ()
  | None -> submit_spawns e; collect_spawns ~block e

  let schedule e o = Rqueue.add e.todo o; stir ~block:false e

  let collect e ~block =
    stir ~block:false e; (* First stir a bit, it might submit/collect ops *)
    match Queue.take e.collectable with
    | op -> Some op
    | exception Queue.Empty ->
        if not block || all_done e then None else
        (stir ~block:true e; Some (Queue.take e.collectable))
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
