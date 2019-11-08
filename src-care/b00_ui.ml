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

  (* Selecting *)

  let is_selected ~reads ~writes ~ids ~hashes ~groups =
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

  let read_write_indexes ops =
    let rec loop reads writes = function
    | o :: os ->
        let add acc p = Fpath.Map.add_to_set (module Op.Set) p o acc in
        let reads = List.fold_left add reads (Op.reads o) in
        let writes = List.fold_left add writes (Op.writes o) in
        loop reads writes os
    | [] -> reads, writes
    in
    loop Fpath.Map.empty Fpath.Map.empty ops

  let find_deps ?(acc = Op.Set.empty) ~recursive index deps ops =
    let add_direct index o acc =
      let add_index_ops index acc p = match Fpath.Map.find p index with
      | exception Not_found -> acc
      | ops -> Op.Set.union ops acc
      in
      List.fold_left (add_index_ops index) acc (deps o)
    in
    match recursive with
    | false -> Op.Set.fold (add_direct index) ops acc
    | true ->
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

  (* Filtering *)

  let op_kind_enum o = match Op.kind o with
  | Op.Copy _ -> `Copy | Op.Delete _ -> `Delete | Op.Notify _ -> `Notify
  | Op.Mkdir _ -> `Mkdir | Op.Read _ -> `Read | Op.Spawn _ -> `Spawn
  | Op.Wait_files _ -> `Wait_files | Op.Write _ -> `Write

  let op_status_enum o = match Op.status o with
  | Op.Aborted -> `Aborted | Op.Done -> `Done | Op.Failed _ -> `Failed
  | Op.Waiting -> `Waiting

  let filter ~revived ~statuses ~kinds =
    let revived_filter = match revived with
    | None -> fun _ -> true | Some revived -> fun o -> Op.revived o = revived
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

  (* Ordering *)

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

  (* Combine selection, filtering and ordering *)

  let select
      ~reads ~writes ~ids ~hashes ~groups ~needs ~enables ~recursive
      ~revived ~statuses ~kinds ~order_by ops
    =
    let is_selected = is_selected ~reads ~writes ~ids ~hashes ~groups in
    let sel = List.filter is_selected ops in
    let sel = Op.Set.of_list sel in
    let sel = match not needs && not enables with
    | true -> sel
    | false ->
        let reads, writes = read_write_indexes ops in
        let acc = Op.Set.empty in
        let acc = if needs then find_needs ~recursive ~writes ~acc sel else acc
        in
        if enables then find_enables ~recursive ~reads ~acc sel else acc
    in
    let sel = Op.Set.elements sel in
    let filter = filter ~revived ~statuses ~kinds in
    let sel = List.filter filter sel in
    order ~by:order_by sel

  let select_cli ?docs () =
    let open Cmdliner in
    let hash =
      let of_string s =
        let err = Fmt.str "Could not parse hash from %S" in
        Result.map_error (fun _ -> `Msg (err s)) @@ Hash.of_hex s
      in
      Arg.conv ~docv:"HASH" (of_string, Hash.pp)
    in
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
    let groups =
      let doc = "Select operations with group $(docv). Repeatable." in
      let docv = "GROUP" in
      Arg.(value & opt_all string [] & info ["g"; "group"] ~doc ?docs ~docv)
    in
    let needs =
      let doc =
        "Once operations have been selected, also add all direct operations \
         needed by these before filtering. Use with option $(b,--rec) to \
         get the recursive operations."
      in
      Arg.(value & flag & info ["needs"] ~doc ?docs)
    in
    let enables =
      let doc =
        "Once operations have been selected, also add all direct operations \
         enabled by these before filtering. Use with option $(b,--rec) to \
         get the recursive operations."
      in
      Arg.(value & flag & info ["enables"] ~doc ?docs)
    in
    let recursive =
      let doc = "Make $(b,--needs) or $(b,--enables) recursive." in
      Arg.(value & flag & info ["rec"] ~doc ?docs)
    in
    let revived =
      let revived =
        let doc = "Keep only revived operations." in
        Some true, Arg.info ["revived"] ~doc ?docs
      in
      let executed =
        let doc = "Keep only truly executed (non revived) operations." in
        Some false, Arg.info ["executed"] ~doc ?docs
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
          Fmt.str "Keep only operations that have their status in $(docv). \
                   $(i,STATUS) must be %s" (Arg.doc_alts_enum status_enum)
        in
        Arg.(value & opt statuses [] & info ["status"] ~doc ?docs ~docv)
      in
      let errors =
        let doc = "Keep only failed operations (errors). Equivalent
                   to add 'failed' to the $(b,--status) option."
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
        Fmt.str "Keep only operations that have their kind in $(docv). \
                 $(i,KIND) must be %s."
          (Arg.doc_alts_enum kind_enum)
      in
      Arg.(value & opt kinds [] & info ["kind"] ~doc ?docs ~docv:"KIND,...")
    in
    let select
        reads writes ids hashes groups needs enables recursive revived statuses
        kinds order_by =
      select ~reads ~writes ~ids ~hashes ~groups ~needs ~enables ~recursive
        ~revived ~statuses ~kinds ~order_by
    in
    Term.(const select $ reads $ writes $ ids $ hashes $ groups $ needs $
          enables $ recursive $ revived $ statuses $ kinds $ order_by)

  let select_man =
    [ `P "Options are provided to select and filter operations. \
          Any operation that satifies one of the selectors and all of the
          filters is included the result. If no selector is specified all
          operations are selected. If no filter is specified all selected
          operations are returned.";
      `P "The result is sorted by execution start time, this can
          be changed with the $(b,--order-by) option." ]
end

module Memo = struct
  let pp_faint pp = Fmt.tty [`Faint] pp
  let read_howto = Fmt.any "b00-log -r "
  let write_howto = Fmt.any "b00-log -w "
  let op_howto ppf o = Fmt.pf ppf "b00-log --id %d" (B000.Op.id o)
  let pp_howto_file howto = Fmt.(pp_faint howto ++ B000_conv.Op.pp_file_write)
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
    | `Op_cache_error (op, e) when level >= Log.Error ->
        Fmt.pf ppf "@[op %d: cache error: %s@]%a" (B000.Op.id op) e sep ()
    | _ ->  ()

  let pp_failed ppf () = Fmt.(tty [`Fg `Red] string) ppf  "FAILED"
  let pp_never_ready ?(read_howto = read_howto) ppf fs =
    let err = match Fpath.Set.cardinal fs with
    | 1 -> "This file never became ready"
    | _ -> "These files never became ready"
    in
    Fmt.pf ppf "@[<v>[%a] %s:@,\
               \ @[<v>%a@,See operations reading them for details.@]@]"
      pp_failed () err (Fpath.Set.pp (pp_howto_file read_howto)) fs

  let writes_cycle os =
    let deps prev next =
      let prev_writes = Fpath.Set.of_list (B000.Op.writes prev) in
      let next_reads = Fpath.Set.of_list (B000.Op.reads next) in
      Fpath.Set.inter prev_writes next_reads
    in
    match os with
    | [] -> []
    | [o] -> [deps o o]
    | first :: _ ->
        let rec loop first acc = function
        | prev :: (next :: _ as os) -> loop first (deps prev next :: acc) os
        | prev :: [] -> List.rev (deps prev first :: acc)
        | [] -> assert false
        in
        loop first [] os

  let pp_ops_cycle ?(write_howto = write_howto) ppf os =
    let pp_self_cycle ~write_howto ppf writes =
      let these_file, them = match Fpath.Set.cardinal writes with
      | 1 -> "This file is", "it"
      | _ -> "These files are", "them"
      in
      Fmt.pf ppf
        "%s read and written by the same operation:@,\
        \ @[<v>%a@,See the operation writing %s for details.@]@]"
        these_file (Fpath.Set.pp (pp_howto_file write_howto)) writes them
    in
    let pp_cycle ~write_howto ppf ws =
      Fmt.pf ppf
        "Operations writing these files form \
         a cycle:@, @[<v>%a@,\
         The last written file is read by the operation writing the first \
         one.@,\
         See operations writing them for details.@]"
        (Fmt.list (pp_howto_file write_howto)) ws
    in
    let pp_ops ppf os =
      let writes = writes_cycle os in
      match os with
      | [] -> assert false
      | [o] -> pp_self_cycle ~write_howto ppf (List.hd writes)
      | os ->
          let writes = try List.(rev @@ rev_map Fpath.Set.choose writes) with
          | Not_found -> assert false
          in
          pp_cycle ~write_howto ppf writes
    in
    Fmt.pf ppf "@[<v>[%a] %a@]" pp_failed () pp_ops os

  let pp_error ?(sep = Fmt.flush_nl) ?read_howto ?write_howto () ppf =
    function
    | B00.Memo.Failures -> ()
    | B00.Memo.Cycle ops -> pp_ops_cycle ?write_howto ppf ops; sep ppf ()
    | B00.Memo.Never_became_ready fs ->
        pp_never_ready ?read_howto ppf fs; sep ppf ()

  (* Cli *)

  open Cmdliner

  let b0_dir_name = "_b0"
  let cache_dir_name = ".cache"
  let trash_dir_name = ".trash"
  let log_file_name = ".log"
  let b0_dir_env = "B0_DIR"
  let cache_dir_env = "B0_CACHE_DIR"
  let log_file_env = "B0_LOG_FILE"

  let b0_dir
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the b0 directory.")
      ?(doc_none = "$(b,_b0) in root directory")
      ?(env = Cmdliner.Arg.env_var b0_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none B0_std_ui.fpath) None &
         info ["b0-dir"] ~env ~doc ~docs ~docv:"DIR")

  let cache_dir
      ?(opts = ["cache-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build cache directory.")
      ?(doc_none = "$(b,.cache) in b0 directory")
      ?(env = Cmdliner.Arg.env_var cache_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none B0_std_ui.fpath) None &
         info opts ~env ~doc ~docs ~docv:"DIR")

  let log_file
      ?(opts = ["log-file"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Output (binary) build log to $(docv).")
      ?(doc_none = "$(b,.log) in b0 directory")
      ?(env = Cmdliner.Arg.env_var log_file_env) () =
    let doc = "Output (binary) build log to $(docv)." in
    Arg.(value & opt (some ~none:doc_none B0_std_ui.fpath) None &
         info opts ~env ~doc ~docs ~docv:"FILE")

  let get_b0_dir ~cwd ~root ~b0_dir = match b0_dir with
  | None -> Fpath.(root / b0_dir_name)
  | Some d -> Fpath.(cwd // d)

  let get_path ~cwd ~b0_dir default p = match p with
  | None -> Fpath.(b0_dir / default)
  | Some p -> Fpath.(cwd // p)

  let get_cache_dir ~cwd ~b0_dir ~cache_dir =
    get_path ~cwd ~b0_dir cache_dir_name cache_dir

  let get_trash_dir ~cwd ~b0_dir ~trash_dir =
    get_path ~cwd ~b0_dir trash_dir_name trash_dir

  let get_log_file ~cwd ~b0_dir ~log_file =
    get_path ~cwd ~b0_dir log_file_name log_file

  (* Build parameters *)

  let jobs ?docs ?env () =
    let doc = "Maximal number of commands to spawn concurrently." in
    let docv = "COUNT" in
    Arg.(value & opt (some int) None & info ["j"; "jobs"] ?env ~doc ?docs ~docv)

  let find_jobs ~jobs () = match jobs with
  | Some max -> max
  | None ->
      let cpu_count = B0_machine.logical_cpu_count () in
      let cpu_count = Log.if_error ~level:Log.Warning ~use:None cpu_count in
      Option.value ~default:1 cpu_count

  (* Build log *)

  module Log = struct
    type info =
      { hash_count : int;
        hash_dur : Time.span;
        total_dur : Time.span;
        cpu_utime : Time.span;
        cpu_stime : Time.span;
        cpu_children_utime : Time.span;
        cpu_children_stime : Time.span; }

    let enc_span b s = Binc.enc_int64 b (Time.Span.to_uint64_ns s)
    let dec_span s i =
      let i, s = Binc.dec_int64 s i in
      i, Time.Span.of_uint64_ns s

    let enc_info b i =
      Binc.enc_int b i.hash_count; enc_span b i.hash_dur;
      enc_span b i.total_dur;
      enc_span b i.cpu_utime; enc_span b i.cpu_stime;
      enc_span b i.cpu_children_utime; enc_span b i.cpu_children_stime

    let dec_info s i =
      let i, hash_count = Binc.dec_int s i in let i, hash_dur = dec_span s i in
      let i, total_dur = dec_span s i in
      let i, cpu_utime = dec_span s i in let i, cpu_stime = dec_span s i in
      let i, cpu_children_utime = dec_span s i in
      let i, cpu_children_stime = dec_span s i in
      i, { hash_count; hash_dur; total_dur; cpu_utime; cpu_stime;
           cpu_children_utime; cpu_children_stime }

    let info_of_memo m =
      let open B00 in
      let c = Memo.reviver m in
      let hash_count = Fpath.Map.cardinal (B000.Reviver.file_hashes c) in
      let hash_dur = B000.Reviver.file_hash_dur c in
      let total_dur = Time.count (Memo.clock m) in
      let cpu = Time.cpu_count (Memo.cpu_clock m) in
      { hash_count; hash_dur; total_dur;
        cpu_utime = Time.cpu_utime cpu; cpu_stime = Time.cpu_stime cpu;
        cpu_children_utime = Time.cpu_children_utime cpu;
        cpu_children_stime = Time.cpu_children_stime cpu; }

    let magic = "b\x00\x00\x00log"

    let log_to_string info ops =
      let b = Buffer.create (1024 * 1024) in
      Binc.enc_magic b magic;
      enc_info b info;
      Binc.enc_list B000_conv.Op.enc b ops;
      Buffer.contents b

    let log_of_string ?(file = Os.File.dash) s =
      try
        let i = Binc.dec_magic s 0 magic in
        let i, info = dec_info s i in
        let i, ops = Binc.dec_list B000_conv.Op.dec s i in
        Binc.dec_eoi s i;
        Ok (info, ops)
      with
      | Failure e -> Fmt.error "%a:%s" Fpath.pp_unquoted file e

    let write_file file m =
      let data =
        Log.time (fun _ msg -> msg "generating log") @@ fun () ->
        log_to_string (info_of_memo m) (B00.Memo.ops m)
      in
      Log.time (fun _ msg -> msg "writing log") @@ fun () ->
      Os.File.write ~force:true ~make_path:true file data

    let read_file file =
      Result.bind (Os.File.read file) @@ fun data ->
      log_of_string ~file data

    let pp_stats ppf (info, ops) =
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
          0 Time.Span.zero 0 Time.Span.zero ops
      in
      let hc, hd = info.hash_count, info.hash_dur in
      let pp_xtime ppf (self, children) =
        let label = Fmt.tty_string [`Italic] in
        Fmt.pf ppf "%a %a" Time.Span.pp self
          (Fmt.field ~label "children" (fun c -> c) Time.Span.pp)
          children
      in
      let pp_stime ppf i = pp_xtime ppf (i.cpu_stime, i.cpu_children_stime) in
      let pp_utime ppf i = pp_xtime ppf (i.cpu_utime, i.cpu_children_utime) in
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
         Fmt.field "real" (fun _ -> info.total_dur) Time.Span.pp ]) ppf info

    let out out_fmt =
      let outf_pp_ops pp_op (_, ops) = match ops with
      | [] -> ()
      | ops -> Fmt.pr "@[<v>%a@]@." (Fmt.list pp_op) ops
      in
      let outf = match out_fmt with
      | `Short -> outf_pp_ops B000_conv.Op.pp_line
      | `Normal -> outf_pp_ops B000_conv.Op.pp_line_and_ui
      | `Long -> outf_pp_ops B000_conv.Op.pp
      | `Trace_event ->
          fun (_, ops) ->
            Fmt.pr "%s@."
              (B0_serialk_json.Jsong.to_string
                 (B0_trace.Trace_event.of_ops ops))
      | `Stats -> fun log -> Fmt.pr "%a@." pp_stats log

      in
      out_fmt, outf

    type out_fmt = [ `Normal | `Short | `Long | `Trace_event | `Stats ]
    let out_fmt_cli ?docs () =
      let out_fmt
          ?docs ?(short_opts = ["s"; "short"])
          ?(normal_opts = ["normal"]) ?(long_opts = ["l"; "long"])
          ?(trace_event_opts = ["trace-event"]) ?(stats_opts = ["stats"])
          ()
        =
        let short =
          let doc = "Short output, mostly line based with only relevant data."in
          Cmdliner.Arg.info short_opts ~doc ?docs
        in
        let normal =
          let doc = "Normal output (default)." in
          Cmdliner.Arg.info normal_opts ~doc ?docs
        in
        let long =
          let doc = "Long output with as much information as possible." in
          Cmdliner.Arg.info long_opts ~doc ?docs
        in
        let trace_event =
          let doc = "Output build operations in Trace Event format." in
          Cmdliner.Arg.info trace_event_opts ~doc ?docs
        in
        let stats =
          let doc = "Output statistics about the returned operations." in
          Cmdliner.Arg.info stats_opts ~doc ?docs
        in
        let fmts =
          [ `Short, short; `Normal, normal; `Long, long;
            `Trace_event, trace_event; `Stats, stats ]
        in
        Cmdliner.Arg.(value & vflag `Normal fmts)
      in
      Cmdliner.Term.(const out $ out_fmt ?docs ())
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
