(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

(** Random queue *)
module Rqueue : sig
  type 'a t
  (** The type for random queues with elements of type ['a]. *)

  val empty : ?rand:Random.State.t -> unit -> 'a t
  (** [emtpy ~rand ()] is an empty random queue using [rand] as random
      state (defaults to {!Random.State.make_self_init}). *)

  val add : 'a t -> 'a -> unit
  (** [add q v] adds [v] to the queue. *)

  val take : 'a t -> 'a option
  (** [take q] removes and returns a random element in [q] (if any). *)

  val length : 'a t -> int
  (** [length q] is the number of elements in [q]. *)
end = struct
  type 'a t =
    { rand : Random.State.t;
      mutable length : int;
      mutable slots : 'a option array }

  let grow q =
    let slots' = Array.make (2 * q.length) None in
    Array.blit q.slots 0 slots' 0 q.length;
    q.slots <- slots'

  let empty ?(rand = Random.State.make_self_init ()) () =
    { rand; length = 0; slots = Array.make 256 None }

  let add q v =
    if q.length = Array.length q.slots then grow q;
    q.slots.(q.length) <- Some v;
    q.length <- q.length + 1;
    ()

  let take q = match q.length with
  | 0 -> None
  | _ ->
      let i = Random.State.int q.rand q.length in
      let v = match q.slots.(i) with None -> assert false | Some v -> v in
      q.length <- q.length - 1;
      q.slots.(i) <- q.slots.(q.length);
      q.slots.(q.length) <- None;
      Some v

  let length q = q.length
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
  let pp_feedback ppf = function
  | `File_cache_need_copy p ->
      Fmt.pf ppf "@[Warning: need copy: %a@]" Fpath.pp p

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
          Fmt.failwith_notrace "%a: %s: %s" Fpath.pp dst arg (uerr e)
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
    | Failure e -> Result.error (err_key "revive" k e)

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
    with Failure e -> Result.error (err "fold" e)

  let keys c =
    let add_name acc fname = key_of_filename fname :: acc in
    try Ok (cache_fold_key_dir_names c add_name [])
    with Failure e -> Result.error (err "keys" e)

  let is_unused c k = try key_dir_exit_if_used (key_dir c k); Ok true with
  | Exit -> Ok false
  | Failure e -> Result.error (err_key "unused check" k e)

  let delete_unused c =
    let delete_unused_key () fname =
      let kdir = key_dir_of_filename c fname in
      try key_dir_exit_if_used kdir; ignore (key_dir_delete kdir) with
      | Exit -> ()
    in
    try Ok (cache_fold_key_dir_names c delete_unused_key ())
    with Failure e -> Result.error (err "delete unused" e)

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
    with Failure e -> Result.error (err "trim size" e)

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
      Fmt.pf ppf "keys: %d files: %d size: %a"
        s.keys_count s.keys_file_count Fmt.byte_size s.keys_byte_size

    let of_keys c ns =
      let rec loop k f b = function
      | [] -> { keys_count = k; keys_file_count = f; keys_byte_size = b }
      | n :: ns ->
          let kf, kb, _, _ = key_dir_stats (key_dir c n) in
          loop (k + 1) (f + kf) (b + kb) ns
      in
      try Ok (loop 0 0 0 ns) with
      | Failure e -> Result.error (err "keys stats" e)

    type cache = { all_keys : keys; unused_keys : keys }
    let all_keys s = s.all_keys
    let unused_keys s = s.unused_keys
    let pp ppf s =
      Fmt.pf ppf "@[<v>";
      Fmt.field "total" pp_keys ppf s.all_keys; Fmt.cut ppf ();
      Fmt.field "unused" pp_keys ppf s.unused_keys;
      Fmt.pf ppf "@]"

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
      | Failure e -> Result.error (err "stats" e)
  end
end

module Op = struct
  let pp_file_contents = Fmt.elided_string ~max:150
  let pp_error_msg ppf e = Fmt.pf ppf "@[error: %s@]" e

  module Spawn = struct
    type stdo = [ `Ui | `File of Fpath.t | `Tee of Fpath.t ]
    type success_exits = int list
    type t =
      { env : Os.Env.assignments;
        relevant_env : Os.Env.assignments;
        cwd : Fpath.t;
        stdin : Fpath.t option;
        stdout : stdo;
        stderr : stdo;
        success_exits : success_exits;
        tool : Cmd.tool;
        args : Cmd.t;
        mutable stdo_ui : (string, string) result option;
        mutable result  : (Os.Cmd.status, string) result; }

    let env s = s.env
    let relevant_env s = s.relevant_env
    let cwd s = s.cwd
    let stdin s = s.stdin
    let stdout s = s.stdout
    let stderr s = s.stderr
    let success_exits s = s.success_exits
    let tool s = s.tool
    let args s = s.args
    let stdo_ui s = s.stdo_ui
    let set_stdo_ui s ui = s.stdo_ui <- ui
    let result s = s.result
    let set_result s e = s.result <- e

    (* Formatting *)

    let pp_success_exits ppf = function
    | [] -> Fmt.string ppf "any"
    | cs -> Fmt.(list ~sep:comma int) ppf cs

    let pp_cmd ppf s =
      (* XXX this could maybe moved to B0_std.Cmd. *)
      let tool = Fpath.to_string s.tool in
      let args = Cmd.to_list s.args in
      let quote = Filename.quote in
      let pp_brack = Fmt.tty_string [`Fg `Yellow] in
      let pp_tool ppf e = Fmt.tty_string [`Fg `Blue; `Bold] ppf (quote e) in
      let pp_arg ppf a = Fmt.pf ppf "%s" (quote a) in
      let pp_o_arg ppf a = Fmt.tty_string [`Faint; `Fg `Green] ppf (quote a) in
      let rec pp_args last_was_o ppf = function
      | [] -> ()
      | a :: args ->
          Fmt.char ppf ' ';
          if last_was_o then pp_o_arg ppf a else pp_arg ppf a;
          pp_args (String.equal a "-o") ppf args
      in
      Fmt.pf ppf "@[<h>"; pp_brack ppf "[";
      pp_tool ppf tool; pp_args false ppf args;
      pp_brack ppf "]"; Fmt.pf ppf "@]"; ()

    let pp_stdo ppf = function
    | `Ui -> Fmt.pf ppf "<ui>"
    | `File f -> Fpath.pp_quoted ppf f
    | `Tee f -> Fmt.pf ppf "@[<hov><ui> and@ %a@]" Fpath.pp_quoted f

    let pp_stdo_ui ~elide ppf s = match s.stdo_ui with
    | None -> Fmt.none ppf ()
    | Some (Ok d) -> if elide then pp_file_contents ppf d else String.pp ppf d
    | Some (Error e) -> pp_error_msg ppf e

    let pp_result ppf = function
    | Ok st -> Os.Cmd.pp_status ppf st
    | Error e -> pp_error_msg ppf e

    let pp ppf s =
      let pp_env = Fmt.(vbox @@ list string) in
      let pp_opt_path = Fmt.(option ~none:none) Fpath.pp_quoted in
      Fmt.field "cmd" pp_cmd ppf s; Fmt.cut ppf ();
      Fmt.field "env" pp_env ppf s.env; Fmt.cut ppf ();
      Fmt.field "relevant-env" pp_env ppf s.relevant_env; Fmt.cut ppf ();
      Fmt.field "cwd" Fpath.pp_quoted ppf s.cwd; Fmt.cut ppf ();
      Fmt.field "success-exits" pp_success_exits ppf s.success_exits;
      Fmt.cut ppf ();
      Fmt.field "stdin" pp_opt_path ppf s.stdin; Fmt.cut ppf ();
      Fmt.field "stdout" pp_stdo ppf s.stdout; Fmt.cut ppf ();
      Fmt.field "stderr" pp_stdo ppf s.stderr; Fmt.cut ppf ();
      Fmt.field "stdo-ui" (pp_stdo_ui ~elide:false) ppf s; Fmt.cut ppf ();
      Fmt.field "result" pp_result ppf s.result;
      ()
  end

  module Read = struct
    type t =
      { file : Fpath.t;
        mutable result : (string, string) result; }

    let file r = r.file
    let result r = r.result
    let set_result r res = r.result <- res

    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok d -> pp_file_contents ppf d

    let pp ppf r =
      Fmt.field "file" Fpath.pp_quoted ppf r.file; Fmt.cut ppf ();
      Fmt.field "result" pp_result ppf r.result; Fmt.cut ppf ();
      ()
  end

  module Write = struct
    type t =
      { salt : string;
        mode : int;
        file : Fpath.t;
        data : unit -> (string, string) result;
        mutable result : (unit, string) result; }

    let salt w = w.salt
    let mode w = w.mode
    let file w = w.file
    let data w = w.data
    let result w = w.result
    let set_result w res = w.result <- res

    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok () -> Fmt.string ppf "written"

    let pp ppf w =
      Fmt.field "file" Fpath.pp_quoted ppf w.file; Fmt.cut ppf ();
      Fmt.field "mode" Fmt.int ppf w.mode; Fmt.cut ppf ();
      Fmt.field "result" pp_result ppf w.result;
      ()
  end

  module Mkdir = struct
    type t =
      { dir : Fpath.t;
        mutable result : (unit, string) result; }

    let dir mk = mk.dir
    let result mk = mk.result
    let set_result mk res = mk.result <- res

    let pp_result ppf = function
    | Error e -> pp_error_msg ppf e
    | Ok () -> Fmt.string ppf "created"

    let pp ppf m =
      Fmt.field "dir" Fpath.pp_quoted ppf m.dir; Fmt.cut ppf ();
      Fmt.field "result" pp_result ppf m.result;
      ()
  end

  (* Operations *)

  type kind =
  | Spawn of Spawn.t
  | Read of Read.t
  | Write of Write.t
  | Mkdir of Mkdir.t
  | Wait_files

  let kind_name = function
  | Spawn _ -> "spawn"
  | Read  _ -> "read"
  | Write _ -> "write"
  | Mkdir _ -> "mkdir"
  | Wait_files -> "wait-files"

  let pp_kind ppf = function
  | Spawn s -> Spawn.pp ppf s
  | Read r -> Read.pp ppf r
  | Write w -> Write.pp ppf w
  | Mkdir m -> Mkdir.pp ppf m
  | Wait_files -> ()

  let pp_kind_short ppf = function
  | Spawn s -> Spawn.pp_cmd ppf s
  | Read r -> Fpath.pp_quoted ppf (Read.file r)
  | Write w -> Fpath.pp_quoted ppf (Write.file w)
  | Mkdir m -> Fpath.pp_quoted ppf (Mkdir.dir m)
  | Wait_files -> ()

  type status = Waiting | Executed | Failed | Aborted

  let pp_status ppf v = Fmt.string ppf @@ match v with
  | Waiting -> "waiting" | Executed -> "executed"
  | Failed -> "failed" | Aborted -> "aborted"

  type id = int
  type t =
    { id : id;
      creation_time : Time.span;
      mutable exec_start_time : Time.span;
      mutable exec_end_time : Time.span;
      mutable exec_revived : bool;
      mutable status : status;
      mutable reads : Fpath.t list;
      mutable writes : Fpath.t list;
      mutable hash : Hash.t;
      kind : kind }

  let v ~id creation_time ~reads ~writes kind =
    let exec_start_time = Time.Span.zero in
    let exec_end_time = Time.Span.zero in
    let exec_revived = false in
    let status = Waiting in
    let hash = Hash.nil in
    { id; creation_time; exec_start_time; exec_end_time; exec_revived; status;
      reads; writes; hash; kind; }

  let id o = o.id
  let creation_time o = o.creation_time
  let exec_start_time o = o.exec_start_time
  let exec_end_time o = o.exec_end_time
  let exec_revived o = o.exec_revived
  let exec_duration o = Time.Span.abs_diff o.exec_end_time o.exec_start_time
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
  let equal o0 o1 = o0.id = o1.id
  let compare o0 o1 = (Pervasives.compare : int -> int -> int) o0.id o1.id
  let set_exec_start_time o t = o.exec_start_time <- t
  let set_exec_end_time o t = o.exec_end_time <- t
  let set_exec_revived o b = o.exec_revived <- b
  let set_status o s = o.status <- s
  let set_reads o fs = o.hash <- Hash.nil; o.reads <- fs
  let set_writes o fs = o.writes <- fs
  let set_hash o h = o.hash <- h
  let kind o = o.kind
  let get_spawn o = match o.kind with Spawn s -> s | _ -> assert false
  let get_read o = match o.kind with Read r -> r | _ -> assert false
  let get_write o = match o.kind with Write r -> r | _ -> assert false
  let get_mkdir o = match o.kind with Mkdir mk -> mk | _ -> assert false
  let spawn
      ~id creation_time ~reads ~writes ~env ~relevant_env ~cwd ~stdin ~stdout
      ~stderr ~success_exits tool args
    =
    let spawn =
      { Spawn.env; relevant_env; cwd; stdin; stdout; stderr; success_exits;
        tool; args; stdo_ui = None; result = Error "not spawned" }
    in
    v ~id creation_time ~reads ~writes (Spawn spawn)

  let read ~id creation_time file =
    let read = { Read.file = file; result = Error "not read" } in
    v ~id creation_time ~reads:[file] ~writes:[] (Read read)

  let write ~id creation_time ~salt ~reads ~mode ~write:file data =
    let write = { Write.salt; mode; file; data; result = Error "not written"} in
    v ~id creation_time ~reads ~writes:[file] (Write write)

  let mkdir ~id creation_time dir =
    let mkdir = { Mkdir.dir = dir; result = Error "not created" } in
    v ~id creation_time ~reads:[] ~writes:[dir] (Mkdir mkdir)

  let wait_files ~id creation_time reads =
    v ~id creation_time ~reads ~writes:[] Wait_files

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  (* Formatting *)

  let pp_synopsis ppf o =
    let pp_kind ppf k = Fmt.tty_string [`Fg `Green] ppf k in
    let pp_status ppf = function
    | Executed ->
        if not o.exec_revived then () else
        Fmt.pf ppf "[%a]" (Fmt.tty_string [`Fg `Green]) "CACHED"
    | Failed -> Fmt.pf ppf "[%a]" (Fmt.tty_string [`Fg `Red]) "FAILED"
    | Aborted -> Fmt.pf ppf "[%a]" (Fmt.tty_string [`Fg `Red]) "ABORTED"
    | Waiting -> Fmt.pf ppf "[waiting]"
    in
    Fmt.pf ppf "@[%a[%a:%d]@]"
      pp_status o.status pp_kind (kind_name o.kind) o.id

  let pp_short ppf o =
    Fmt.pf ppf "@[<h>%a %a@]" pp_synopsis o pp_kind_short o.kind

  let pp_writes =
    let pp_file_write = Fmt.tty [`Faint; `Fg `Green] Fpath.pp_quoted in
    Fmt.braces @@ Fmt.list pp_file_write

  let pp_op ppf o =
    let pp_span ppf s =
      Fmt.pf ppf "%a (%ans)" Time.Span.pp s Time.Span.pp_ns s
    in
    let pp_reads = Fmt.braces @@ Fmt.list Fpath.pp_quoted in
    let dur = Time.Span.abs_diff o.exec_end_time o.exec_start_time in
    Fmt.field "writes" pp_writes ppf o.writes; Fmt.cut ppf ();
    Fmt.field "reads" pp_reads ppf o.reads; Fmt.cut ppf ();
    Fmt.field "created" pp_span ppf o.creation_time; Fmt.cut ppf ();
    Fmt.field "start" pp_span ppf o.exec_start_time; Fmt.cut ppf ();
    Fmt.field "duration" pp_span ppf dur; Fmt.cut ppf ();
    Fmt.field "hash" Hash.pp ppf o.hash; Fmt.cut ppf ();
    Fmt.field "kind" Fmt.string ppf (kind_name o.kind)

  let pp ppf o =
    Fmt.pf ppf "@[<v>%a@, @[<v>%a@]@, @[<v>%a@]@]"
      pp_synopsis o pp_kind o.kind pp_op o

  let err_style = [`Fg `Red]
  let pp_did_not_write ppf (o, fs) =
    Fmt.pf ppf "@[<v>%a@, %a@, @[<v>%a@]@, @[<v>%a@]@]"
      pp_synopsis o (Fmt.field ~style:err_style "Did not write" pp_writes) fs
      pp_kind o.kind pp_op o

  let pp_spawn_status_fail ppf o =
    let s = get_spawn o in
    Fmt.pf ppf "@[<v>%a@, Illegal exit status: %a expected: %a@, @[<v>%a@]\
                @, @[<v>%a@]@]"
      pp_synopsis o
      (Fmt.tty err_style Spawn.pp_result)
      (Spawn.result s)
      Spawn.pp_success_exits
      (Spawn.success_exits s)
      pp_kind o.kind pp_op o
end

module Reviver = struct

  (* Operation metadata converters *)

  let cmd_status_conv : Os.Cmd.status Conv.t =
    let kind = "cmd-status" in
    let bin_enc b = function
    | `Exited e -> Conv.Bin.enc_byte b 0x0; Conv.Bin.enc_byte b e
    | `Signaled s -> Conv.Bin.enc_byte b 0x1; Conv.Bin.enc_byte b (- s)
    in
    let bin_dec s ~start = match Conv.Bin.dec_byte ~kind s ~start with
    | start, 0x0 ->
        let i, v = Conv.Bin.dec_byte s ~kind ~start in i, `Exited v
    | start, 0x1 ->
        let i, v = Conv.Bin.dec_byte s ~kind ~start in i, `Signaled (- v)
    | _, byte ->
        Conv.Bin.dec_err_exceed ~kind start byte ~max:0x1
    in
    let txt_enc ppf = raise (Conv.Error (0, 0, "Not implemented")) in
    let txt_dec s ~start = raise (Conv.Error (0, 0, "Not implemented")) in
    Conv.v ~kind ~docvar:"STATUS" bin_enc bin_dec txt_enc txt_dec

  let spawn_meta_conv :
    ((string, string) result option * (Os.Cmd.status, string) result) Conv.t
    =
    Conv.(pair ~kind:"spawn-result" ~docvar:"SPAWN"
            (option (result string_bytes string_bytes))
            (result cmd_status_conv string_bytes))

  (* Operation cache *)

  type t =
    { clock : Time.counter;
      hash_fun : (module Hash.T);
      cache : File_cache.t;
      buffer : Buffer.t; (* buffer to encode metadata *)
      mutable file_hashes : Hash.t Fpath.Map.t; (* file hash cache *)
      mutable file_hash_dur : Time.span; (* file hash duration *) }

  let create ?clock ?(hash_fun = (module Hash.Xxh_64 : Hash.T)) cache =
    let clock = match clock with None -> Time.counter () | Some c -> c in
    let buffer = Buffer.create 1024 in
    let file_hashes = Fpath.Map.empty in
    let file_hash_dur = Time.Span.zero in
    { clock; hash_fun; buffer; cache; file_hashes; file_hash_dur }

  let clock r = r.clock
  let hash_fun r = r.hash_fun
  let file_cache r = r.cache


  let timestamp r = Time.count r.clock

  let hash_file r f = match Fpath.Map.find f r.file_hashes with
  | h -> h
  | exception Not_found ->
      let module H = (val r.hash_fun : Hash.T) in
      let t = timestamp r in
      let h = Result.to_failure (H.file f) in
      let dur = Time.Span.abs_diff (timestamp r) t in
      r.file_hash_dur <- Time.Span.add r.file_hash_dur dur;
      r.file_hashes <- Fpath.Map.add f h r.file_hashes;
      h

  let hash_op_reads r o acc =
    let add_file_hash acc f = Hash.to_bytes (hash_file r f) :: acc in
    List.fold_left add_file_hash acc (Op.reads o)

  let hash_spawn r o s =
    let stdin_sig = function None -> "0" | Some _ -> "1" in
    let stdo_sig = function `File _ -> "0" | `Tee _ -> "1" | `Ui -> "2" in
    let module H = (val r.hash_fun : Hash.T) in
    let acc = [Hash.to_bytes (hash_file r (Op.Spawn.tool s))] in
    let acc = hash_op_reads r o acc in
    let acc = stdin_sig (Op.Spawn.stdin s) :: acc in
    let acc = stdo_sig (Op.Spawn.stdout s) :: acc in
    let acc = stdo_sig (Op.Spawn.stderr s) :: acc in
    let env = Op.Spawn.relevant_env s in
    let acc = List.fold_left (fun acc v -> v :: acc) acc env in
    let acc = List.rev_append (Cmd.to_sig @@ Op.Spawn.args s) acc in
    let sg = String.concat "" acc in
    H.string sg

  let hash_write r o w =
    let module H = (val r.hash_fun : Hash.T) in
    let acc = string_of_int (Op.Write.mode w) :: [Op.Write.salt w] in
    let acc = hash_op_reads r o acc in
    let acc = Fpath.to_string (Op.Write.file w) :: acc in (* FIXME *)
    let sg = String.concat "" acc in
    H.string sg

  let hash_op r o = match Op.kind o with
  | Op.Spawn s -> hash_spawn r o s
  | Op.Write w -> hash_write r o w
  | Op.Read _ | Op.Mkdir _ | Op.Wait_files -> Hash.nil

  let set_op_hash r o = try Ok (Op.set_hash o (hash_op r o)) with
  | Failure e -> Result.error e

  let op_cache_key o = Hash.to_hex (Op.hash o)

  let revive_spawn r o s =
    let writes = Op.writes o in
    let key = op_cache_key o in
    Result.bind (File_cache.revive r.cache key writes) @@ function
    | None -> Ok None
    | Some (m, existed) ->
        Result.bind (Conv.of_bin spawn_meta_conv m) @@
        fun (stdo_ui, result) ->
        Op.set_exec_revived o true;
        Op.set_status o Op.Executed;
        Op.Spawn.set_stdo_ui s stdo_ui;
        Op.Spawn.set_result s result;
        Ok (Some existed)

  let revive_write r o w =
    let writes = Op.writes o in
    let key = op_cache_key o in
    Result.bind (File_cache.revive r.cache key writes) @@ function
    | None -> Ok None
    | Some (_, existed) ->
        Op.set_exec_revived o true;
        Op.set_status o Op.Executed;
        Ok (Some existed)

  let revive r o =
    Op.set_exec_start_time o (timestamp r);
    let ret = match Op.kind o with
    | Op.Spawn s -> revive_spawn r o s
    | Op.Write w -> revive_write r o w
    | Op.Read _ | Op.Mkdir _ | Op.Wait_files -> Ok None
    in
    Op.set_exec_end_time o (timestamp r);
    ret

  let record_spawn r o s =
    let spawn_meta = Op.Spawn.stdo_ui s, Op.Spawn.result s in
    match Conv.to_bin ~buf:r.buffer spawn_meta_conv spawn_meta with
    | Error _ as e -> e
    | Ok m -> File_cache.add r.cache (op_cache_key o) m (Op.writes o)

  let record_write r o w =
    File_cache.add r.cache (op_cache_key o) "" (Op.writes o)

  let record r o = match Op.kind o with
  | Op.Spawn s -> record_spawn r o s
  | Op.Write w -> record_write r o w
  | Op.Read _ | Op.Mkdir _ | Op.Wait_files -> Ok true

  let file_hashes r = r.file_hashes
  let file_hash_dur r = r.file_hash_dur
end

module Guard = struct

  (* XXX. The original [b0] had a more general and maybe efficient
     implementation by assigning integer's to events to be waited
     upon. It might be worth to reconsider that at a certain point,
     especially if other sync objects are introduced. *)

  type feedback =
  [ `File_status_repeat of Fpath.t
  | `File_status_unstable of Fpath.t ]

  let pp_feedback ppf = function
  | `File_status_repeat f -> Fmt.pf ppf "%s: file status repeated" f
  | `File_status_unstable f -> Fmt.pf ppf "%s: file status unstable" f

  (* The type [gop] keeps in [awaits] the files that need to become
     ready before the operation [op] can be added to the [allowed]
     queue of the type [t].

     The type [t] has operations that are allowed to proceed in
     [allowed] and maps files to their status in [files].

     If a file becomes ready it maps to Ready in [files] and the [Blocks]
     operation that were waiting on it get their [await]s changed so that
     it no longer mentions the file.

     If a file never becomes ready it maps to [Never] in [files] and
     the [Blocks] operations that where waiting on it are immediately
     aborted and added to [allowed]. These operations may still exist
     in other [Blocks] but functions take care to ignore these when
     they hit them. *)

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
      | _ ->
          Op.set_status gop.op Op.Aborted;
          Queue.add gop.op g.allowed
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
        | Never ->
            Op.set_status gop.op Op.Aborted;
            Queue.add gop.op g.allowed
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
  type feedback = [ `Exec_submit of Os.Cmd.pid option * Op.t ]
  let pp_feedback ppf = function
  | `Exec_submit (pid, op) ->
      let pp_pid ppf = function
      | None -> () | Some pid -> Fmt.pf ppf "[pid:%d]" (Os.Cmd.pid_to_int pid)
      in
      Fmt.pf ppf "@[[SUBMIT][%s:%d]%a %a@]"
        (Op.kind_name (Op.kind op)) (Op.id op) pp_pid pid
        Op.pp_kind_short (Op.kind op)

  type t =
    { clock : Time.counter;
      tmp_dir : Fpath.t;
      feedback : feedback -> unit;
      todo : Op.t Rqueue.t; (* Waiting for OS submission. *)
      collectable : Op.t Queue.t;
      to_spawn : Op.t Queue.t; (* [dequeued] from [todo] waiting to spawn. *)
      max_spawn : int; (* Max number of spawned processes *)
      mutable spawn_count : int; (* Number of spawned processes *)
      mutable spawns : (Os.Cmd.pid * Fpath.t option * Op.t) list; }

  let create
      ?clock ?rand ?tmp_dir:tmp ?(feedback = fun _ -> ()) ~max_spawn ()
    =
    let clock = match clock with None -> Time.counter () | Some c -> c in
    let tmp_dir = match tmp with None -> Os.Dir.default_tmp () | Some t -> t in
    let todo = Rqueue.empty ?rand () in
    let collectable = Queue.create () in
    let to_spawn = Queue.create () in
    { clock; tmp_dir; feedback; todo; collectable; to_spawn; max_spawn;
      spawn_count = 0; spawns = [] }

  let incr_spawn_count e = e.spawn_count <- e.spawn_count + 1
  let decr_spawn_count e = e.spawn_count <- e.spawn_count - 1
  let timestamp e = Time.count e.clock

  (* Operation execution *)

  let complete_spawn e o ui result =
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
    let s = Op.get_spawn o in
    let status = match result with
    | Error _ | Ok (`Signaled _) -> Op.Failed
    | Ok (`Exited c) ->
        match Op.Spawn.success_exits s with
        | [] -> Op.Executed
        | cs -> if (List.mem c cs) then Op.Executed else Op.Failed
    in
    let stdo_ui = match ui with None -> None | Some ui -> (read_stdo_ui s ui) in
    Op.Spawn.set_stdo_ui s stdo_ui;
    Op.Spawn.set_result s result;
    Op.set_status o status;
    Op.set_exec_end_time o (timestamp e);
    decr_spawn_count e;
    Queue.add o e.collectable

  let submit_spawn e o =
    let spawn s =
      let get_stdo_ui_file e = match Os.File.open_tmp_fd ~dir:e.tmp_dir () with
      | Ok (file, fd) -> Os.Cmd.out_fd ~close:true fd, Some file
      | Error e -> failwith e
      in
      try
        let env = Op.Spawn.env s in
        let cwd = Op.Spawn.cwd s in
        let stdin = match Op.Spawn.stdin s with
        | None -> Os.Cmd.in_fd ~close:false Unix.stdin (* XXX /dev/null no ? *)
        | Some f -> Os.Cmd.in_file f
        in
        let stdout, ui = match Op.Spawn.stdout s with
        | `File f | `Tee f -> Os.Cmd.out_file f, None
        | `Ui -> get_stdo_ui_file e
        in
        let stderr, ui = match Op.Spawn.stderr s with
        | `File f | `Tee f -> Os.Cmd.out_file f, ui
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
            e.feedback (`Exec_submit (Some pid, o))
      with Failure err -> complete_spawn e o None (Error err)
    in
    incr_spawn_count e;
    Op.set_exec_start_time o (timestamp e);
    spawn (Op.get_spawn o)

  let exec_read e o r =
    Op.set_exec_start_time o (timestamp e);
    e.feedback (`Exec_submit (None, o));
    let res = Os.File.read (Op.Read.file r) in
    let status = match res with Ok _ -> Op.Executed | Error _ -> Op.Failed in
    Op.Read.set_result r res;
    Op.set_status o status;
    Op.set_exec_end_time o (timestamp e);
    Queue.add o e.collectable

  let exec_write e o w =
    Op.set_exec_start_time o (timestamp e);
    e.feedback (`Exec_submit (None, o));
    let res = match Op.Write.data w () with
    | Error _ as e -> e
    | Ok data ->
        let mode = Op.Write.mode w in
        Os.File.write ~force:true ~make_path:true ~mode (Op.Write.file w) data
    in
    let status = match res with Ok _ -> Op.Executed | Error _ -> Op.Failed in
    Op.Write.set_result w res;
    Op.set_status o status;
    Op.set_exec_end_time o (timestamp e);
    Queue.add o e.collectable

  let exec_mkdir e o mk =
    Op.set_exec_start_time o (timestamp e);
    e.feedback (`Exec_submit (None, o));
    let res = Os.Dir.create ~make_path:true (Op.Mkdir.dir mk) in
    let status, res = match res with
    | Ok _ -> Op.Executed, Ok () | Error _ as e -> Op.Failed, e
    in
    Op.Mkdir.set_result mk res;
    Op.set_status o status;
    Op.set_exec_end_time o (timestamp e);
    Queue.add o e.collectable

  let exec_wait_files e o =
    Op.set_exec_start_time o (timestamp e);
    e.feedback (`Exec_submit (None, o));
    Op.set_status o Op.Executed;
    Op.set_exec_end_time o (timestamp e);
    Queue.add o e.collectable

  (* Scheduling

     As it stands this implementation does all the file operations
     synchronously on submit but nothing prevents for asynchronous or
     thread pool implementations. *)

  let submit e o = match Op.kind o with
  | Op.Spawn _ -> Queue.add o e.to_spawn (* see submit_spawns *)
  | Op.Read r -> exec_read e o r
  | Op.Write w -> exec_write e o w
  | Op.Mkdir mk -> exec_mkdir e o mk
  | Op.Wait_files -> exec_wait_files e o

  let submit_spawns e =
    let free = e.max_spawn - e.spawn_count in
    let may_spawn = Queue.length e.to_spawn in
    let spawn_limit = if free < may_spawn then free else may_spawn in
    let rec loop e = function
    | 0 -> ()
    | n ->
        match Queue.take e.to_spawn with
        | exception Queue.Empty -> ()
        | o -> submit_spawn e o; loop e (n - 1)
    in
    loop e spawn_limit

  let rec collect_spawns ~block e =
    let relax () = ignore (Unix.select [] [] [] 0.0001) in
    match e.spawn_count = 0 with
    | true -> ()
    | false ->
        (* We don't (and can't through B0_std.OS.Cmd API constraints)
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
          | Error _ as err -> complete_spawn e o ui err; spawns
          | Ok None -> p :: spawns
          | Ok (Some st) -> complete_spawn e o ui (Ok st); spawns
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

  let schedule e o = Rqueue.add e.todo o
  let collect e ~block =
    stir ~block:false e; (* First stir a bit, it might submit/collect ops *)
    match Queue.take e.collectable with
    | op -> Some op
    | exception Queue.Empty ->
        if not block || all_done e then None else
        (stir ~block:true e; Some (Queue.take e.collectable))
end

(* B01 *)

module Env = struct
  type tool_lookup = Cmd.tool -> (Fpath.t, string) result

  let env_tool_lookup ?sep ?(var = "PATH") env =
    let search_path = match String.Map.find var env with
    | exception Not_found -> "" | s -> s
    in
    match Fpath.list_of_search_path ?sep search_path with
    | Error _ as e -> fun _ -> e
    | Ok search -> fun tool -> Os.Cmd.must_find_tool ~search tool

  type t =
    { env : Os.Env.t;
      forced_env : Os.Env.t;
      lookup : tool_lookup }

  let memo lookup =
    let memo = Hashtbl.create 91 in
    fun tool -> match Hashtbl.find memo tool with
    | exception Not_found -> let p = lookup tool in Hashtbl.add memo tool p; p
    | p -> p

  let v ?lookup ?(forced_env = String.Map.empty) env =
    let lookup = match lookup with None -> env_tool_lookup env | Some l -> l in
    let lookup = memo lookup in
    { env; forced_env; lookup }

  let env e = e.env
  let forced_env e = e.forced_env
  let tool e l = e.lookup l
end

module Tool = struct
  type env_vars = string list
  let tmp_vars = ["TMPDIR"; "TEMP"; "TMP"]

  type response_file =
    { to_file : Cmd.t -> string;
      cli : Fpath.t -> Cmd.t; }

  let response_file_of to_file cli = { to_file; cli }
  let args0 =
    let to_file cmd = String.concat "\x00" (Cmd.to_list cmd) ^ "\x00" in
    let cli f = Cmd.(arg "-args0" %% path f) in
    { to_file; cli }

  type t =
    { name : Fpath.t;
      vars : env_vars;
      shielded_vars : env_vars;
      response_file : response_file option; }

  let v ?response_file ?(shielded_vars = tmp_vars) ?(vars = []) name =
    { name; vars; shielded_vars; response_file }

  let by_name ?response_file ?shielded_vars ?vars name =
    match Fpath.is_seg name with
    | false -> Fmt.invalid_arg "%S: tool is not a path segment" name
    | true -> v ?shielded_vars ?vars (Fpath.v name)

  let name t = t.name
  let vars t = t.vars
  let shielded_vars t = t.shielded_vars
  let response_file t = t.response_file
  let read_env t env =
    let add_var acc var = match String.Map.find var env with
    | v -> String.Map.add var v acc
    | exception Not_found -> acc
    in
    let relevant = List.fold_left add_var String.Map.empty t.vars in
    let all = List.fold_left add_var relevant t.shielded_vars in
    all, relevant
end

module Futs : sig
  type t
  type 'a fut
  type 'a fut_set
  val create : ?rand:Random.State.t -> unit -> t
  val fut : t -> 'a fut * 'a fut_set
  val fut_value : 'a fut -> 'a option
  val fut_wait : 'a fut -> ('a -> unit) -> unit
  val fut_set : 'a fut_set -> 'a -> unit
  val stir : t -> (unit -> unit) option
end = struct
  module Fut_id = struct
    type t = int
    let compare : int -> int -> int = compare
  end
  module Fmap = Map.Make (Fut_id)

  type 'a fut =
    { id : Fut_id.t;
      mutable value : 'a option;
      mutable konts : ('a -> unit) list;
      futs : t; }
  and 'a fut_set = 'a fut
  and e_fut = F : 'a fut -> e_fut
  and t =
    { mutable next : Fut_id.t;
      mutable waiting : e_fut Fmap.t;
      mutable kready : (unit -> unit) Rqueue.t; }

  let create ?rand () =
    { next = 0; waiting = Fmap.empty ; kready = Rqueue.empty ?rand () }

  let fut fs =
    let id = fs.next in
    let f = { id; value = None; konts = []; futs = fs} in
    (fs.next <- fs.next + 1; fs.waiting <- Fmap.add id (F f) fs.waiting; f, f)

  let fut_value f = f.value
  let fut_wait f k = match f.value with
  | Some v -> Rqueue.add f.futs.kready (fun () -> k v)
  | None -> f.konts <- k :: f.konts

  let fut_set f v = match f.value with
  | Some _ -> invalid_arg "fut value already set"
  | None ->
      f.value <- Some v;
      match Fmap.find f.id f.futs.waiting with
      | exception Not_found -> assert false
      | (F f) ->
          f.futs.waiting <- Fmap.remove f.id f.futs.waiting;
          let v = Option.get f.value in
          let add_kont k = Rqueue.add f.futs.kready (fun () -> k v) in
          List.iter add_kont f.konts;
          ()

  let stir fs = Rqueue.take fs.kready
end

module Memo = struct
  type feedback =
  [ `Fiber_exn of exn * Printexc.raw_backtrace
  | `Fiber_fail of string
  | `Miss_tool of Tool.t * string
  | `Op_cache_error of Op.t * string
  | `Op_complete of Op.t * [`Did_not_write of Fpath.t list]]

  let pp_feedback ppf = function
  | `Fiber_exn (exn, bt) ->
      Fmt.pf ppf "@[<v>fiber exception:@,%a@]" Fmt.exn_backtrace (exn, bt)
  | `Fiber_fail e ->
      Fmt.pf ppf "@[<v>fiber failed:@,%s@]" e
  | `Miss_tool (t, e) ->
      Fmt.pf ppf "@[<v>missing tool:@,%s@]" e
  | `Op_cache_error (op, e) ->
      Fmt.pf ppf "@[op %d: cache error: %s@]" (Op.id op) e
  | `Op_complete (op, (`Did_not_write fs)) ->
      match (Op.status op) with
      | Op.Failed ->
          begin match fs with
          | [] ->
              begin match Op.kind op with
              | Op.Spawn _ -> Fmt.pf ppf "@[%a@]" Op.pp_spawn_status_fail op;
              | _ -> Fmt.pf ppf "@[%a@]" Op.pp op;
              end
          | fs -> Fmt.pf ppf "@[%a@]" Op.pp_did_not_write (op, fs);
          end
      | _ ->
          match Op.kind op with
          | Op.Spawn s when Op.Spawn.stdo_ui s <> None ->
              Fmt.pf ppf "@[<v>@[<h>[DONE]%a:@]@, %a@]"
                Op.pp_short op (Op.Spawn.pp_stdo_ui ~elide:false) s
          | _ -> Fmt.pf ppf "@[<h>[DONE]%a@]" Op.pp_short op

  exception Fail of string
  type t =
    { clock : Time.counter;
      cpu_clock : Time.cpu_counter;
      feedback : feedback -> unit;
      cwd : Fpath.t;
      env : Env.t ;
      guard : Guard.t;
      reviver : Reviver.t;
      exec : Exec.t;
      futs : Futs.t;
      mutable op_id : int;
      (* XXX The fact that continuations are not part of operations
         is an artefact of using marshall on op values in B0. Since we
         won't do this  maybe we could have a field for it in Op.t though
         it may still be a good idea to not carray these closures around.  *)
      mutable konts : (t -> Op.t -> unit) Op.Map.t;
      mutable ops : Op.t list; }

  let create ?clock ?cpu_clock:cc ~feedback ~cwd env guard reviver exec =
    let clock = match clock with None -> Time.counter () | Some c -> c in
    let cpu_clock = match cc with None -> Time.cpu_counter () | Some c -> c in
    let futs = Futs.create () in
    let op_id = 0 in
    let konts = Op.Map.empty in
    let ops = [] in
    { clock; cpu_clock; feedback; cwd; env; guard; reviver; exec; futs;
      op_id; konts; ops; }

  let memo ?hash_fun ?env ?cwd ?cachedir ?(max_spawn = 4) ?feedback () =
    let feedback = match feedback with
    | Some f -> f
    | None ->
        let pp_feedback ppf = function
        | #feedback as f -> pp_feedback ppf f
        | #File_cache.feedback as f -> File_cache.pp_feedback ppf f
        | #Exec.feedback as f -> Exec.pp_feedback ppf f
        in
        Fmt.pr "@[%a@]@." pp_feedback
    in
    let fb_cache = (feedback :> File_cache.feedback -> unit) in
    let fb_exec = (feedback :> Exec.feedback -> unit) in
    let fb_memo = (feedback :> feedback -> unit) in
    let clock = Time.counter () in
    let env = match env with None -> Os.Env.current () | Some env -> Ok env in
    let cwd = match cwd with None -> Os.Dir.cwd () | Some cwd -> Ok cwd in
    Result.bind env @@ fun env ->
    Result.bind cwd @@ fun cwd ->
    let cachedir = match cachedir with
    | None -> Fpath.(cwd / "_b0" / "cache")
    | Some c -> c
    in
    Result.bind (File_cache.create ~feedback:fb_cache cachedir) @@ fun c ->
    let env = Env.v env in
    let guard = Guard.create () in
    let reviver = Reviver.create ~clock ?hash_fun c in
    let exec = Exec.create ~clock ~feedback:fb_exec ~max_spawn () in
    Ok (create ~clock ~feedback:fb_memo ~cwd env guard reviver exec)

  let clock m = m.clock
  let cpu_clock m = m.cpu_clock
  let env m = m.env
  let reviver m = m.reviver
  let guard m = m.guard
  let exec m = m.exec
  let hash_fun m = Reviver.hash_fun m.reviver
  let ops m = m.ops
  let timestamp m = Time.count m.clock
  let new_op_id m = let id = m.op_id in m.op_id <- id + 1; id

  let trap_kont_exn k m o = try k m o with
  | Fail e -> m.feedback (`Fiber_fail e)
  | Stack_overflow as e -> raise e
  | Out_of_memory as e -> raise e
  | Sys.Break as e -> raise e
  | e -> m.feedback (`Fiber_exn (e, Printexc.get_raw_backtrace ()))

  let add_op_kont m o k = m.konts <- Op.Map.add o (trap_kont_exn k) m.konts
  let rem_op_kont m o = m.konts <- Op.Map.remove o m.konts

  let continue_op m o =
    List.iter (fun f -> Guard.set_file_ready m.guard f) (Op.writes o);
    m.feedback (`Op_complete (o, `Did_not_write []));
    match Op.Map.find o m.konts with
    | k -> rem_op_kont m o; k m o | exception Not_found -> ()

  let discontinue_op m o did_not_write =
    List.iter (fun f -> Guard.set_file_never m.guard f) (Op.writes o);
    rem_op_kont m o; m.feedback (`Op_complete (o, `Did_not_write did_not_write))

  let finish_op m o = match Op.status o with
  | Op.Executed ->
      if Op.exec_revived o then continue_op m o else
      begin match Hash.equal (Op.hash o) Hash.nil with
      | true ->
          begin match Op.did_not_write o with
          | [] -> continue_op m o
          | fs -> Op.set_status o Op.Failed; discontinue_op m o fs
          end
      | false ->
          match Reviver.record m.reviver o with
          | Ok true -> continue_op m o
          | Ok false ->
              Op.set_status o Op.Failed;
              discontinue_op m o (Op.did_not_write o)
          | Error e ->
              m.feedback (`Op_cache_error (o, e));
              begin match Op.did_not_write o with
              | [] -> continue_op m o
              | fs -> Op.set_status o Op.Failed; discontinue_op m o fs
              end
      end
  | Op.Aborted | Op.Failed -> discontinue_op m o []
  | Op.Waiting -> assert false

  let submit_op m o = match Op.status o with
  | Op.Aborted -> finish_op m o
  | Op.Waiting ->
      begin match Reviver.set_op_hash m.reviver o with
      | Error e ->
          (* FIXME Does this report errors cleanly ? We really want to
             be able to say which reads were supposed to be there and are not *)
          m.feedback (`Op_cache_error (o, e));
          Op.set_status o Op.Aborted;
          finish_op m o
      | Ok () ->
          begin match Reviver.revive m.reviver o with
          | Ok None -> Exec.submit m.exec o
          | Ok (Some _) -> finish_op m o
          | Error e -> m.feedback (`Op_cache_error (o, e)); Exec.submit m.exec o
          end
      end
  | Op.Executed | Op.Failed -> assert false

  (* XXX we may blow stack continuations can add which stirs.
     XXX futures make it even worse. *)

  let rec stir ~block m = match Guard.allowed m.guard with
  | Some o -> submit_op m o; stir ~block m
  | None ->
      match Exec.collect m.exec ~block with
      | Some o -> finish_op m o; stir ~block m
      | None ->
          match Futs.stir m.futs with
          | Some k -> k (); stir ~block m
          | None -> ()

  let add_op m o = m.ops <- o :: m.ops; Guard.add m.guard o; stir ~block:false m

  let rec finish m =
    stir ~block:true m;
    match List.exists (fun o -> Op.status o = Op.Waiting) m.ops with
    | false ->
        assert (Futs.stir m.futs = None);
        Ok ()
    | true ->
        let undecided = Guard.root_undecided_files m.guard in
        Fpath.Set.iter (Guard.set_file_never m.guard) undecided;
        stir ~block:true m;
        (* TODO a cycle dep between ops will break this assertion. *)
        assert (not (List.exists (fun o -> Op.status o = Op.Waiting) m.ops));
        assert (Futs.stir m.futs = None);
        Error undecided

  (* Fibers *)

  type 'a fiber = ('a -> unit) -> unit

  let fail fmt = Fmt.kstr (fun s -> raise (Fail s)) fmt
  let fail_error = function Ok v -> v | Error e -> raise (Fail e)

  (* Files *)

  let file_ready m p =
    Guard.set_file_ready m.guard p

  let read m file k =
    let o = Op.read ~id:(new_op_id m) (timestamp m) file in
    let k m o = k (Result.get_ok @@ Op.Read.result (Op.get_read o)) in
    add_op_kont m o k; add_op m o

  let wait_files m files k =
    let o = Op.wait_files ~id:(new_op_id m) (timestamp m) files in
    let k m o = k () in
    add_op_kont m o k; add_op m o

  let write m ?(salt = "") ?(reads = []) ?(mode = 0o644) write write_data =
    let id = new_op_id m in
    let o = Op.write ~id (timestamp m) ~salt ~reads ~mode ~write write_data in
    add_op m o

  let mkdir m dir k =
    let o = Op.mkdir ~id:(new_op_id m) (timestamp m) dir in
    let k m o = k (Result.get_ok @@ Op.Mkdir.result (Op.get_mkdir o)) in
    add_op_kont m o k; add_op m o

  (* FIXME better strategy to deal with builded tools. If the tool is a
     path check for readyness if not add it to the operations reads.
     I also suspect the tool lookup approach is not exactly right at
     the moment. Maybe this will clear up when we get the configuration
     story in. *)

  type _tool =
  { tool : Tool.t;
    tool_file : Fpath.t;
    tool_env : Os.Env.assignments;
    tool_relevant_env : Os.Env.assignments; }

  type tool =
  | Miss of Tool.t * string
  | Tool of _tool

  type cmd = { cmd_tool : tool; cmd_args : Cmd.t }

  let tool_env m t =
    let env = Env.env m.env in
    let tool_env, relevant = Tool.read_env t env in
    let forced_env = Env.forced_env m.env in
    let tool_env = Os.Env.override tool_env ~by:forced_env in
    let relevant = Os.Env.override relevant ~by:forced_env in
    tool_env, relevant

  let spawn_env m cmd_tool = function
  | None -> cmd_tool.tool_env, cmd_tool.tool_relevant_env
  | Some spawn_env ->
      let env = Env.env m.env in
      let tool_env, relevant = Tool.read_env cmd_tool.tool env in
      let forced_env = Env.forced_env m.env in
      let tool_env = Os.Env.override tool_env ~by:spawn_env in
      let tool_env = Os.Env.override tool_env ~by:forced_env in
      let relevant = Os.Env.override relevant ~by:spawn_env in
      let relevant = Os.Env.override relevant ~by:forced_env in
      Os.Env.to_assignments tool_env, Os.Env.to_assignments relevant

  let tool m tool =
    let cmd_tool = match Env.tool m.env (Tool.name tool) with
    | Error e -> Miss (tool, e)
    | Ok tool_file ->
        let tool_env, tool_relevant_env = tool_env m tool in
        let tool_env = Os.Env.to_assignments tool_env in
        let tool_relevant_env = Os.Env.to_assignments tool_relevant_env in
        Tool { tool; tool_file; tool_env; tool_relevant_env }
    in
    fun cmd_args -> { cmd_tool; cmd_args }

  let spawn
      m ?(reads = []) ?(writes = []) ?env ?cwd ?stdin ?(stdout = `Ui)
      ?(stderr = `Ui) ?(success_exits = [0]) ?k cmd
    =
    match cmd.cmd_tool with
    | Miss (tool, e) -> m.feedback (`Miss_tool (tool, e))
    | Tool tool ->
        let id = new_op_id m in
        let stamp = timestamp m in
        let env, relevant_env = spawn_env m tool env in
        let cwd = match cwd with None -> m.cwd | Some d -> d in
        let o =
          Op.spawn ~id stamp ~reads ~writes ~env ~relevant_env ~cwd
            ~stdin ~stdout ~stderr ~success_exits tool.tool_file
            cmd.cmd_args
        in
        begin match k with
        | None -> ()
        | Some k ->
            let k m o = match Op.Spawn.result (Op.get_spawn o) with
            | Ok (`Exited code) -> k code
            | _ -> assert false
            in
            add_op_kont m o k;
        end;
        add_op m o

  module Fut = struct
    type memo = t
    type 'a t = 'a Futs.fut * memo
    type 'a set = 'a Futs.fut_set
    let create m = let f, s = Futs.fut m.futs in (f, m), s
    let value (f, _) = Futs.fut_value f
    let wait (f, m) k =
      let trap_kont_exn v = try k v with
      | Fail e -> m.feedback (`Fiber_fail e)
      | Stack_overflow as e -> raise e
      | Out_of_memory as e -> raise e
      | Sys.Break as e -> raise e
      | e -> m.feedback (`Fiber_exn (e, Printexc.get_raw_backtrace ()))
      in
      Futs.fut_wait f trap_kont_exn

    let set s = Futs.fut_set s
  end
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
