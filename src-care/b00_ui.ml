(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

module Cli = struct
  open Cmdliner

  (* Specifying output formats *)

  type out_fmt = [ `Normal | `Short | `Long ]
  let out_fmt
      ?docs ?(short_opts = ["s"; "short"]) ?(long_opts = ["l"; "long"]) ()
    =
    let short =
      let doc = "Short output. Line based output with only relevant data." in
      Cmdliner.Arg.info short_opts ~doc ?docs
    in
    let long =
      let doc = "Long output. Outputs as much information as possible." in
      Cmdliner.Arg.info long_opts ~doc ?docs
    in
    Cmdliner.Arg.(value & vflag `Normal [`Short, short; `Long, long])
end

module File_cache = struct
  open B000

  module Stats = struct
    let err op err = Fmt.str "cache %s: %s" op err

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
          let kf, kb, _ = B000.File_cache.key_stats c n |> Result.to_failure in
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
      | n :: ks ->
          let kf, kb, _ = B000.File_cache.key_stats c n |> Result.to_failure in
          let key_unused = false in
          match key_unused with
          | true -> loop (k+1) (f+kf) (b+kb) (uk+1) (uf+kf) (ub+kb) ks
          | false -> loop (k+1) (f+kf) (b+kb) uk uf ub ks
      in
      try
        let keys = B000.File_cache.keys c |> Result.to_failure in
        Ok (loop 0 0 0 0 0 0 keys)
      with
      | Failure e -> Error (err "stats" e)
  end

  (* High-level commands *)

  let delete ~dir keys = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok ()
  | true ->
      match keys with
      | `All ->
          Result.bind (Os.Path.delete ~recurse:true dir) @@ fun _ ->
          Result.bind (Os.Dir.create ~make_path:true dir) @@ (* recreate dir *)
          fun _ -> Ok ()
      | `Keys keys ->
          Result.bind (File_cache.create dir) @@ fun c ->
          let rec loop = function
          | [] -> Ok ()
          | k :: ks ->
              match File_cache.rem c k with
              | Error _ as e -> e
              | Ok true -> loop ks
              | Ok false ->
                  Log.warn (fun m -> m "%s: no such key in cache, ignored" k);
                  loop ks
          in
          loop keys

  let gc ~dir = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok ()
  | true ->
      (* TODO redo with a log *)
      Result.bind (File_cache.create dir) @@ fun c ->
      Error "This operation is no longer implemented (TODO again)."

  let size ~dir =
    let stats = Result.bind (Os.Dir.exists dir) @@ function
    | false -> Ok Stats.zero
    | true ->
        Result.bind (File_cache.create dir) @@ fun c ->
        Stats.of_cache c
    in
    Result.bind stats @@ fun stats ->
    Log.app (fun m -> m "@[<v>%a@]" Stats.pp stats); Ok ()

  let trim ~dir ~max_byte_size ~pct =
    Result.bind (Os.Dir.exists dir) @@ function
    | false -> Ok ()
    | true ->
        Result.bind (File_cache.create dir) @@ fun c ->
        File_cache.trim_size c ~max_byte_size ~pct

  (* Cli fragments *)

  open Cmdliner

  let key_arg =
    let of_string s = match Fpath.is_seg s with
    | true -> Ok s
    | false -> Error (`Msg "Not a valid key (not a path segment)")
    in
    Arg.conv (of_string, String.pp) ~docv:"KEY"

  let keys_none_is_all ?(pos_right = -1) () =
    let doc =
      "Select key $(docv) (repeatable). If unspecified selects all keys."
    in
    let keys = Arg.(value & pos_right 0 key_arg [] & info [] ~doc ~docv:"KEY")in
    Term.(const (function [] -> `All | ks -> `Keys ks) $ keys)
end

module Op = struct
  open B000

  (* Finding dependencies *)

  let find_deps ?(acc = Op.Set.empty) ~recursive index deps ops =
    let add_direct index o acc =
      let add_index_ops index acc p = match Fpath.Map.find p index with
      | exception Not_found -> acc
      | ops -> Op.Set.union ops acc
      in
      List.fold_left (add_index_ops index) acc (deps o)
    in
    if not recursive then (Op.Set.fold (add_direct index) ops acc) else
    let rec loop index acc seen todo = match Op.Set.choose todo with
    | exception Not_found -> acc
    | o ->
        let seen = Op.Set.add o seen in
        let todo = Op.Set.remove o todo in
        let deps = add_direct index o Op.Set.empty in
        let todo = Op.Set.(union todo (diff deps seen)) in
        let acc = Op.Set.union acc deps in
        loop index acc seen todo
    in
    loop index acc Op.Set.empty ops

  let find_needs ?acc ~recursive ~writes ops =
    find_deps ?acc ~recursive writes Op.reads ops

  let find_enables ?acc ~recursive ~reads ops =
    find_deps ?acc ~recursive reads Op.writes ops

  (* Queries *)

  type query = B000.Op.t list -> B000.Op.t list

  let select ~reads ~writes ~ids ~hashes ~groups =
    let all =
      reads = [] && writes = [] && ids = [] && hashes = [] && groups = []
    in
    if all then fun _ -> true else
    let reads = Fpath.Set.of_list reads in
    let mem_reads f = Fpath.Set.mem f reads in
    let writes = Fpath.Set.of_list writes in
    let mem_writes f = Fpath.Set.mem f writes in
    let hashes = String.Set.of_list (List.rev_map Hash.to_bytes hashes) in
    let mem_hash h = String.Set.mem h hashes in
    let groups = String.Set.of_list groups in
    let mem_group g = String.Set.mem g groups in
    fun o ->
      List.exists (( = ) (Op.id o)) ids ||
      mem_hash (Hash.to_bytes (Op.hash o)) ||
      List.exists mem_reads (Op.reads o) ||
      List.exists mem_writes (Op.writes o) ||
      mem_group (Op.group o)

  let select_deps ~needs ~enables ~recursive ~dom ops =
    if not needs && not enables then ops else
    let reads, writes = B000.Op.read_write_maps dom in
    let ops = Op.Set.of_list ops in
    let acc = Op.Set.empty in
    let acc = if needs then find_needs ~recursive ~writes ~acc ops else acc in
    let acc = if enables then find_enables ~recursive ~reads ~acc ops else acc
    in
    Op.Set.elements acc

  let op_kind_enum o = match Op.kind o with
  | Op.Copy _ -> `Copy | Op.Delete _ -> `Delete | Op.Notify _ -> `Notify
  | Op.Mkdir _ -> `Mkdir | Op.Read _ -> `Read | Op.Spawn _ -> `Spawn
  | Op.Wait_files _ -> `Wait_files | Op.Write _ -> `Write

  let op_status_enum o = match Op.status o with
  | Op.Aborted -> `Aborted | Op.Done -> `Done | Op.Failed _ -> `Failed
  | Op.Waiting -> `Waiting

  let filter ~revived ~statuses ~kinds =
    let revived_filter = match revived with
    | None -> fun _ -> true
    | Some revived -> fun o ->
      Op.revived o = revived && not (Hash.equal Hash.nil (Op.hash o))
    in
    let status_filter = match statuses with
    | [] -> fun _ -> true
    | statuses -> fun o -> List.mem (op_status_enum o) statuses
    in
    let kind_filter = match kinds with
    | [] -> fun _ -> true
    | kinds -> fun o -> List.mem (op_kind_enum o) kinds
    in
    fun o -> revived_filter o && status_filter o && kind_filter o

  let order ~by ops =
    let order_by_field cmp f o0 o1 = cmp (f o0) (f o1) in
    let order = match by with
    | `Create -> order_by_field Time.Span.compare Op.time_created
    | `Start -> order_by_field Time.Span.compare Op.time_started
    | `Wait -> order_by_field Time.Span.compare Op.waited
    | `Dur ->
        let rev_compare t0 t1 = Time.Span.compare t1 t0 in
        order_by_field rev_compare Op.duration
    in
    List.sort order ops

  let query ~select ~select_deps ~filter ~order ops =
    let sel = List.filter select ops in
    let sel = select_deps ~dom:ops sel in
    let sel = List.filter filter sel in
    order sel

  (* Command line *)

  open Cmdliner

  let hash =
    let of_string s =
      let err = Fmt.str "Could not parse hash from %S" in
      Result.map_error (fun _ -> `Msg (err s)) @@ Hash.of_hex s
    in
    Arg.conv ~docv:"HASH" (of_string, Hash.pp)

  let groups
      ?(opts = ["g"; "group"])
      ?docs
      ?(doc = "Select operations with group $(docv). Repeatable.")
      ()
    =
    let docv = "GROUP" in
    Arg.(value & opt_all string [] & info ["g"; "group"] ~doc ?docs ~docv)

  let select_cli ?docs ?(groups = groups () ?docs) () =
    let reads =
      let doc = "Select operations that read file $(docv). Repeatable." in
      Arg.(value & opt_all B0_std_ui.fpath [] &
           info ["r"; "read"] ~doc ?docs ~docv:"FILE")
    in
    let writes =
      let doc = "Select operations that wrote file $(docv). Repeatable." in
      Arg.(value & opt_all B0_std_ui.fpath [] &
           info ["w"; "write"] ~doc ?docs ~docv:"FILE")
    in
    let ids =
      let doc = "Select operation with identifier $(docv). Repeatable." in
      Arg.(value & opt_all int [] & info ["id"] ~doc ?docs ~docv:"ID")
    in
    let hashes =
      let doc = "Select operation with hash $(docv). Repeatable." in
      Arg.(value & opt_all hash [] & info ["hash"] ~doc ?docs ~docv:"HASH")
    in
    let select reads writes ids hashes groups =
      select ~reads ~writes ~ids ~hashes ~groups
    in
    Term.(const select $ reads $ writes $ ids $ hashes $ groups)

  let select_deps_cli ?docs () =
    let needs =
      let doc =
        "Once operations have been selected, replace them with all direct \
         operations needed by these before filtering. Use with option \
         $(b,--rec) to get the recursive operations."
      in
      Arg.(value & flag & info ["needs"] ~doc ?docs)
    in
    let enables =
      let doc =
        "Once operations have been selected, replace them with all direct \
         operations enabled by these before filtering. Use with option \
         $(b,--rec) to get the recursive operations."
      in
      Arg.(value & flag & info ["enables"] ~doc ?docs)
    in
    let recursive =
      let doc = "Make $(b,--needs) or $(b,--enables) recursive." in
      Arg.(value & flag & info ["rec"] ~doc ?docs)
    in
    let select_deps needs enables recursive =
      select_deps ~needs ~enables ~recursive
    in
    Term.(const select_deps $ needs $ enables $ recursive)

  let filter_cli ?docs () =
    let revived =
      let revived =
        let doc = "Keep only revivable operations that were revived." in
        Some true, Arg.info ["revived"] ~doc ?docs
      in
      let executed =
        let doc = "Keep only revivable operations that were not revived." in
        Some false, Arg.info ["u"; "unrevived"] ~doc ?docs
      in
      Arg.(value & vflag None [revived; executed])
    in
    let statuses =
      let statuses =
        let status_enum =
          [ "aborted", `Aborted; "done", `Done; "failed", `Failed;
            "waiting", `Waiting ]
        in
        let status = Arg.enum status_enum in
        let statuses = Arg.list status and docv = "STATUS,..." in
        let doc =
          Fmt.str "Keep only operations that have their status in $(i,STATUS). \
                   $(i,STATUS) must be %s" (Arg.doc_alts_enum status_enum)
        in
        Arg.(value & opt statuses [] & info ["status"] ~doc ?docs ~docv)
      in
      let errors =
        let doc = "Keep only failed operations (errors). Equivalent
                   to add $(b,failed) to the $(b,--status) option."
        in
        Arg.(value & flag & info ["e"; "errors"] ~doc ?docs)
      in
      let sts statuses errs = if errs then `Failed :: statuses else statuses in
      Term.(pure sts $ statuses $ errors)
    in
    let kinds =
      let kind_enum =
        [ "copy", `Copy; "delete", `Delete; "notify", `Notify; "mkdir", `Mkdir;
          "read", `Read; "spawn", `Spawn; "wait", `Wait_files; "write", `Write ]
      in
      let kind = Arg.enum kind_enum in
      let kinds = Arg.list kind in
      let doc =
        Fmt.str "Keep only operations that have their kind in $(i,KIND). \
                 $(i,KIND) must be %s."
          (Arg.doc_alts_enum kind_enum)
      in
      Arg.(value & opt kinds [] & info ["kind"] ~doc ?docs ~docv:"KIND,...")
    in
    let filter revived statuses kinds = filter ~revived ~statuses ~kinds in
    Term.(const filter $ revived $ statuses $ kinds)

  let order_cli ?docs () =
    let order_by =
      let order =
        [ "create", `Create; "start", `Start; "wait", `Wait; "dur", `Dur; ]
      in
      let doc =
        Fmt.str "Order by $(docv). $(docv) must be %s time."
          (Arg.doc_alts_enum order)
      in
      let order = Arg.enum order and docv = "ORDER" in
      Arg.(value & opt order `Start & info ["order-by"] ~doc ?docs ~docv)
    in
    let by_dur =
      let doc = "Order by decreasing duration. Takes over $(b,--order-by)." in
      Arg.(value & flag & info ["d"] ~doc ?docs)
    in
    let order order_by by_dur =
      let by = if by_dur then `Dur else order_by in
      order ~by
    in
    Term.(const order $ order_by $ by_dur)

  let query_cli ?docs () =
    let open Cmdliner in
    let query select select_deps filter order =
      query ~select ~select_deps ~filter ~order
    in
    Term.(const query $ select_cli ?docs () $ select_deps_cli ?docs () $
          filter_cli ?docs () $ order_cli ?docs ())

  let query_man =
    [ `P "Options are provided to select and filter operations. \
          Any operation that satifies one of the selectors and all of the \
          filters is included in the result. If no selector is specified all \
          operations are selected. If no filter is specified all selected \
          operations are returned.";
      `P "The result is sorted by execution start time, this can
          be changed with the $(b,--order-by) option." ]
end

module Memo = struct

  (* Memo feedback *)

  let op_howto ppf o = Fmt.pf ppf "b00-log --id %d" (B000.Op.id o)
  let pp_leveled_feedback
      ?(sep = Fmt.flush_nl) ?(op_howto = op_howto) ~show_op ~show_ui ~level
      ppf f
    =
    let open B000 in
    if level = Log.Quiet then () else
    match f with
    | `Exec_start (_, _) -> () (* we have B0_std.Os spawn tracer on debug *)
    | `Op_complete o ->
        if level >= show_op || level = Log.Debug
        then (B000_conv.Op.pp_line_and_ui ppf o; sep ppf ()) else
        if level >= show_ui
        then (B000_conv.Op.pp_ui ~sep ~op_howto ppf o)
    | `Miss_tool (t, e) when level >= Log.Error ->
        Fmt.pf ppf "@[<v>Missing tool:@,%s@]%a" e sep ()
    | _ ->  ()

  open Cmdliner

  (* B0 directory *)

  let b0_dir_env = "B0_DIR"
  let b0_dir_name = "_b0"
  let b0_dir
      ?(opts = ["b0-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the b0 directory.")
      ?(doc_none = "$(b,_b0) in root directory")
      ?(env = Cmdliner.Arg.env_var b0_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none B0_std_ui.fpath) None &
         info opts ~env ~doc ~docs ~docv:"DIR")

  let get_b0_dir ~cwd ~root ~b0_dir = match b0_dir with
  | None -> Fpath.(root / b0_dir_name)
  | Some d -> Fpath.(cwd // d)

  let get_b0_dir_path ~cwd ~b0_dir default p = match p with
  | None -> Fpath.(b0_dir / default)
  | Some p -> Fpath.(cwd // p)

  (* File cache directory *)

  let cache_dir_env = "B0_CACHE_DIR"
  let cache_dir_name = ".cache"
  let cache_dir
      ?(opts = ["cache-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build cache directory.")
      ?(doc_none = "$(b,.cache) in b0 directory")
      ?(env = Cmdliner.Arg.env_var cache_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none B0_std_ui.fpath) None &
         info opts ~env ~doc ~docs ~docv:"DIR")

  let get_cache_dir ~cwd ~b0_dir ~cache_dir =
    get_b0_dir_path ~cwd ~b0_dir cache_dir_name cache_dir

  (* Trash directory *)

  let trash_dir_name = ".trash"
  let get_trash_dir ~cwd ~b0_dir ~trash_dir =
    get_b0_dir_path ~cwd ~b0_dir trash_dir_name trash_dir

  (* Log file *)

  let log_file_name = ".log"
  let log_file_env = "B0_LOG_FILE"
  let log_file
      ?(opts = ["log-file"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Output (binary) build log to $(docv).")
      ?(doc_none = "$(b,.log) in b0 directory")
      ?(env = Cmdliner.Arg.env_var log_file_env) ()
    =
    let doc = "Output (binary) build log to $(docv)." in
    Arg.(value & opt (some ~none:doc_none B0_std_ui.fpath) None &
         info opts ~env ~doc ~docs ~docv:"FILE")

  let get_log_file ~cwd ~b0_dir ~log_file =
    get_b0_dir_path ~cwd ~b0_dir log_file_name log_file

  (* Jobs *)

  let jobs_env = "B0_JOBS"
  let jobs
      ?(opts = ["j"; "jobs"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Maximal number of commands to spawn concurrently.")
      ?(doc_none = "Number of CPUs available")
      ?(env = Cmdliner.Arg.env_var log_file_env) ()
    =
    let docv = "COUNT" in
    Arg.(value & opt (some ~none:doc_none int) None &
         info opts ~env ~doc ~docs ~docv)

  let get_jobs ~jobs = match jobs with
  | Some max -> max | None -> Os.Cpu.logical_count ()

  (* Hash fun *)

  let hash_fun =
    let of_string = function s -> Ok (module Hash.Xxh_64 : Hash.T) in
    let pp ppf (module H : Hash.T) = Fmt.string ppf H.id in
    Arg.conv ~docv:"HASHFUN" (of_string, pp)

  let hash_fun_env = "B0_HASH_FUN"
  let hash_fun
      ?(opts = ["hash-fun"])
      ?(docs = Manpage.s_common_options)
      ?doc
      ?(doc_none = "xxh64")
      ?(env = Cmdliner.Arg.env_var hash_fun_env) () =
    let docv = "HASHFUN" in
    let doc = match doc with
    | Some doc -> doc
    | None ->
        let ids = List.map (fun (module H : Hash.T) -> H.id) (Hash.funs ()) in
        Fmt.str "Hash function to use for caching. %a"
          Fmt.(must_be (Fmt.bold string)) ids
    in
    Arg.(value & opt (some ~none:doc_none hash_fun) None &
         info opts ~env ~doc ~docs ~docv)

  let get_hash_fun ~hash_fun = match hash_fun with
  | Some m -> m
  | None -> (module Hash.Xxh_64 : Hash.T)

  (* Logs *)

  module Log = struct

    (* Logs *)

    type t =
      { hash_count : int;
        hash_dur : Time.span;
        total_dur : Time.span;
        cpu_utime : Time.span;
        cpu_stime : Time.span;
        cpu_children_utime : Time.span;
        cpu_children_stime : Time.span;
        ops : B000.Op.t list; }

    let ops l = l.ops
    let of_memo m =
      let open B00 in
      let c = Memo.reviver m in
      let hash_count = Fpath.Map.cardinal (B000.Reviver.file_hashes c) in
      let hash_dur = B000.Reviver.file_hash_dur c in
      let total_dur = Time.count (Memo.clock m) in
      let cpu = Time.cpu_count (Memo.cpu_clock m) in
      { hash_count; hash_dur; total_dur;
        cpu_utime = Time.cpu_utime cpu; cpu_stime = Time.cpu_stime cpu;
        cpu_children_utime = Time.cpu_children_utime cpu;
        cpu_children_stime = Time.cpu_children_stime cpu;
        ops = B00.Memo.ops m; }

    (* IO *)

    let enc_time_span b s = Bincode.enc_int64 b (Time.Span.to_uint64_ns s)
    let dec_time_span s i =
      let i, s = Bincode.dec_int64 s i in
      i, Time.Span.of_uint64_ns s

    let magic = "b\x00\x00\x00log"

    let enc b l =
      Bincode.enc_magic magic b ();
      Bincode.enc_int b l.hash_count;
      enc_time_span b l.hash_dur;
      enc_time_span b l.total_dur;
      enc_time_span b l.cpu_utime;
      enc_time_span b l.cpu_stime;
      enc_time_span b l.cpu_children_utime;
      enc_time_span b l.cpu_children_stime;
      Bincode.enc_list (Bincode.enc B000_conv.Op.bincode) b l.ops

    let dec s i =
      let i, () = Bincode.dec_magic magic s 0 in
      let i, hash_count = Bincode.dec_int s i in
      let i, hash_dur = dec_time_span s i in
      let i, total_dur = dec_time_span s i in
      let i, cpu_utime = dec_time_span s i in
      let i, cpu_stime = dec_time_span s i in
      let i, cpu_children_utime = dec_time_span s i in
      let i, cpu_children_stime = dec_time_span s i in
      let i, ops = Bincode.dec_list (Bincode.dec (B000_conv.Op.bincode)) s i in
      i, { hash_count; hash_dur; total_dur; cpu_utime; cpu_stime;
           cpu_children_utime; cpu_children_stime; ops }

    let bincode = Bincode.v enc dec

    let write file l =
      let data =
        Log.time (fun _ msg -> msg "generating log") @@ fun () ->
        let buf = Buffer.create (1024 * 1024) in
        Bincode.to_string ~buf bincode l
      in
      Log.time (fun _ msg -> msg "writing log") @@ fun () ->
      Os.File.write ~force:true ~make_path:true file data

    let read file =
      Result.bind (Os.File.read file) @@ fun data ->
      Bincode.of_string ~file bincode data

    let pp_stats sel ppf l =
      let sc, st, sd, wc, wt, wd, cc, ct, cd, rt, rd, ot, od =
        let ( ++ ) = Time.Span.add in
        let rec loop sc st sd wc wt wd cc ct cd rt rd ot od = function
        | [] -> sc, st, sd, wc, wt, wd, cc, ct, cd, rt, rd, ot, od
        | o :: os ->
            let revived = B000.Op.revived o and d = B000.Op.duration o in
            let ot = ot + 1 and od = od ++ d in
            match B000.Op.kind o with
            | B000.Op.Spawn _ ->
                let sc = if revived then sc + 1 else sc in
                loop sc (st + 1) (sd ++ d) wc wt wd cc ct cd rt rd ot od os
            | B000.Op.Write _ ->
                let wc = if revived then wc + 1 else wc in
                loop sc st sd wc (wt + 1) (wd ++ d) cc ct cd rt rd ot od os
            | B000.Op.Copy _ ->
                let cc = if revived then cc + 1 else cc in
                loop sc st sd wc wt wd cc (ct + 1) (cd ++ d) rt rd ot od os
            | B000.Op.Read _ ->
                loop sc st sd wc wt wd cc ct cd (rt + 1) (rd ++ d) ot od os
            | _ ->
                loop sc st sd wc wt wd cc ct cd rt rd ot od os
        in
        loop
          0 0 Time.Span.zero 0 0 Time.Span.zero 0 0 Time.Span.zero
          0 Time.Span.zero 0 Time.Span.zero (sel l.ops)
      in
      let hc, hd = l.hash_count, l.hash_dur in
      let pp_xtime ppf (self, children) =
        let label = Fmt.tty_string [`Italic] in
        Fmt.pf ppf "%a %a" Time.Span.pp self
          (Fmt.field ~label "children" (fun c -> c) Time.Span.pp)
          children
      in
      let pp_stime ppf l = pp_xtime ppf (l.cpu_stime, l.cpu_children_stime) in
      let pp_utime ppf l = pp_xtime ppf (l.cpu_utime, l.cpu_children_utime) in
      let pp_op ppf (oc, ot, od) =
        Fmt.pf ppf "%a %d (%d revived)" Time.Span.pp od ot oc
      in
      let pp_op_no_cache ppf (ot, od) = Fmt.pf ppf "%a %d" Time.Span.pp od ot in
      let pp_totals ppf (ot, od) = Fmt.pf ppf "%a %d" Time.Span.pp od ot in
      let pp_sec s ppf _ = Fmt.tty_string [`Bold] ppf s in
      (Fmt.record @@
       [ pp_sec "selected operations";
         Fmt.field "spawns" (fun _ -> (sc, st, sd)) pp_op;
         Fmt.field "writes" (fun _ -> (wc, wt, wd)) pp_op;
         Fmt.field "copies" (fun _ -> (cc, ct, cd)) pp_op;
         Fmt.field "reads" (fun _ -> (rt, rd)) pp_op_no_cache;
         Fmt.field "all" (fun _ -> (ot, od)) pp_totals;
         pp_sec "global timings";
         Fmt.field "hashes" (fun _ -> (hc, hd)) pp_totals;
         Fmt.field "utime" Fmt.id pp_utime;
         Fmt.field "stime" Fmt.id pp_stime;
         Fmt.field "real" (fun _ -> l.total_dur) Time.Span.pp ]) ppf l

    type out_kind = [ `Normal | `Trace_event | `Stats | `Hashes ]

    let out ppf out_fmt out_kind query l = match out_kind with
    | `Normal ->
        let pp_ops pp_op ppf l = match query l.ops with
        | [] -> ()
        | ops -> Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_op) ops
        in
        begin match out_fmt with
        | `Short -> pp_ops B000_conv.Op.pp_line ppf l
        | `Normal -> pp_ops B000_conv.Op.pp_line_and_ui ppf l
        | `Long -> pp_ops B000_conv.Op.pp ppf l
        end;
        Fmt.flush ppf ()
    | `Stats ->
        Fmt.pf ppf "@[%a]@." (pp_stats query) l
    | `Trace_event ->
        let t = B0_trace.Trace_event.of_ops (query l.ops) in
        Fmt.pf ppf "%s@." (B0_serialk_json.Jsong.to_string t)
    | `Hashes ->
        let pp_hash ppf o = Hash.pp ppf (B000.Op.hash o) in
        Fmt.pf ppf "@[<v>%a@]" (Fmt.list pp_hash) (query l.ops)

    let out_kind_cli ?docs () =
      let trace_event =
        let doc = "Output build operations in Trace Event format." in
        Cmdliner.Arg.info ["trace-event"] ~doc ?docs
      in
      let stats =
        let doc = "Output statistics about the returned operations." in
        Cmdliner.Arg.info ["stats"] ~doc ?docs
      in
      let hashes =
        let doc = "Output only operation hashes." in
        Cmdliner.Arg.info ["hashes"] ~doc ?docs
      in
      let fmts = [`Trace_event, trace_event; `Stats, stats; `Hashes, hashes] in
      Cmdliner.Arg.(value & vflag `Normal fmts)
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
