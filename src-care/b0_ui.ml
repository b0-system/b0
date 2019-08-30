(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

module Sig_exit = struct
  let on_sigint ~hook f =
    let hook _ = hook (); exit 130 (* as if SIGINT signaled *) in
    let restore = Sys.signal Sys.sigint (Sys.Signal_handle hook) in
    let restore () = Sys.set_signal Sys.sigint restore in
    try let v = f () in restore (); v with e -> restore (); raise e
end

module Cli = struct
  open Cmdliner

  module Arg = struct
    let err_msg of_string s = Result.map_error (fun e -> `Msg e) (of_string s)
    let fpath = Arg.conv ~docv:"PATH" (err_msg Fpath.of_string, Fpath.pp_quoted)
    let cmd = Arg.conv ~docv:"CMD" (err_msg Cmd.of_string, Cmd.pp_dump)
  end

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

module B0_std = struct
  open Cmdliner

  let color ?(docs = Manpage.s_common_options) ?env () =
    let enum = ["auto", None; "always", Some `Ansi; "never", Some `None] in
    let color = Arg.enum enum in
    let enum_alts = Arg.doc_alts_enum enum in
    let doc = Fmt.str "Colorize the output. $(docv) must be %s." enum_alts in
    let docv = "WHEN" in
    Arg.(value & opt color None & info ["color"] ?env ~doc ~docv ~docs)

  let verbosity ?(docs = Manpage.s_common_options) ?env () =
    let vopts =
      let doc =
        "Increase verbosity. Repeatable, but more than twice does \
         not bring more. Takes over $(b,--verbosity)."
       (* The reason for taking over verbosity is due to cmdliner
          limitation: we cannot distinguish in choose below if it was
          set via an env var. And cli args should always take over env
          var. So verbosity set through the env var would take over -v
          otherwise. *)
      in
      Arg.(value & flag_all & info ["v"; "verbose"] ~doc ~docs)
    in
    let verbosity =
      let enum =
        [ "warning", None; (* Hack for the option's absent rendering *)
          "quiet", Some Log.Quiet;
          "error", Some Log.Error;
          "warning", Some Log.Warning;
          "info", Some Log.Info;
          "debug", Some Log.Debug; ]
      in
      let log_level = Arg.enum enum in
      let enum_alts = Arg.doc_alts_enum List.(tl enum) in
      let doc =
        Fmt.str "Be more or less verbose. $(docv) must be %s." enum_alts
      in
      Arg.(value & opt log_level None &
           info ["verbosity"] ?env ~docv:"LEVEL" ~doc ~docs)
      in
      let quiet =
        let doc = "Be quiet. Takes over $(b,-v) and $(b,--verbosity)." in
        Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
      in
      let choose quiet verbosity vopts =
        if quiet then Log.Quiet else match vopts with
        | (_ :: []) -> Log.Info
        | ( _:: _ :: _) -> Log.Debug
        | [] ->
            match verbosity with
            | Some verbosity -> verbosity
            | None -> Log.Warning
      in
      Term.(const choose $ quiet $ verbosity $ vopts)

  let log_spawn level =
    let header = function
    | None -> "EXECV"
    | Some pid -> "EXEC:" ^ string_of_int (Os.Cmd.pid_to_int pid)
    in
    let pp_env ppf = function
    | None -> ()
    | Some env -> Fmt.pf ppf "%a@," (Fmt.list String.pp_dump) env
    in
    fun pid env ~cwd cmd ->
      Log.msg level (fun m ->
          m ~header:(header pid) "@[<v>%a%a@]" pp_env env Cmd.pp_dump cmd)

  let setup_log_spawns = function
  | Log.Quiet -> ()
  | level -> Os.Cmd.set_spawn_tracer (log_spawn level)

  let cli_setup ?docs ?(log_spawns = Log.Debug) ?color_env ?verbosity_env () =
    let color = color ?docs ?env:color_env () in
    let verbosity = verbosity ?docs ?env:verbosity_env () in
    let setup color verbosity =
      let cap = match color with
      | None -> Tty.cap (Tty.of_fd Unix.stdout)
      | Some cap -> cap
      in
      Fmt.set_tty_styling_cap cap;
      B0_std.Log.set_level verbosity;
      setup_log_spawns log_spawns;
    in
    Term.(const setup $ color $ verbosity)
end

module File_cache = struct
  open B000

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
      Result.bind (File_cache.create dir) @@ fun c ->
      Result.bind (File_cache.delete_unused c) @@ fun () -> Ok ()

  let size ~dir =
    let stats = Result.bind (Os.Dir.exists dir) @@ function
    | false -> Ok File_cache.Stats.zero
    | true ->
        Result.bind (File_cache.create dir) @@ fun c ->
        File_cache.Stats.of_cache c
    in
    Result.bind stats @@ fun stats ->
    Log.app (fun m -> m "@[<v>%a@]" File_cache.Stats.pp stats); Ok ()

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

  (* XXX cleanup *)

  let is_selected ~reads ~writes ~ids ~hashes ~groups =
    let all =
      reads = [] && writes = [] && ids = [] && hashes = [] && groups = []
    in
    if all then fun _ -> true else
    let reads = Fpath.Set.of_list reads in
    let mem_reads f = Fpath.Set.mem f reads in
    let writes = Fpath.Set.of_list writes in
    let mem_writes f = Fpath.Set.mem f writes in
    let hashes =
      let to_bytes s = match Hash.of_hex s with
      | Ok b -> Hash.to_bytes b
      | Error _ (* todo *) -> Hash.to_bytes Hash.nil
      in
      String.Set.of_list (List.rev_map to_bytes hashes)
    in
    let mem_hash h = String.Set.mem h hashes in
    let groups = String.Set.of_list groups in
    let mem_group g = String.Set.mem g groups in
    fun o ->
      List.exists (( = ) (Op.id o)) ids ||
      mem_hash (Hash.to_bytes (Op.hash o)) ||
      List.exists mem_reads (Op.reads o) ||
      List.exists mem_writes (Op.writes o) ||
      mem_group (Op.group o)

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

  let filter ~revived ~status =
    let revived_filter = match revived with
    | None -> fun _ -> true | Some revived -> fun o -> Op.revived o = revived
    in
    let status_filter = match status with
    | None -> fun _ -> true
    | Some `Aborted -> fun o -> Op.status o = Op.Aborted
    | Some `Done -> fun o -> Op.status o = Op.Done
    | Some `Failed ->
        fun o -> (match Op.status o with Failed _ -> true | _ -> false)
    | Some `Waiting -> fun o -> Op.status o = Op.Waiting
    in
    fun o -> revived_filter o && status_filter o

  let select
      ~reads ~writes ~ids ~hashes ~groups ~needs ~enables ~recursive
      ~revived ~status ~order_by ops
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
    let filter = filter ~revived ~status in
    let sel = List.filter filter sel in
    order ~by:order_by sel

  let select_cli =
    let open Cmdliner in
    let order_by =
      let order =
        [ "create", `Create; "start", `Start; "wait", `Wait; "dur", `Dur; ]
      in
      let doc =
        Fmt.str "Order by $(docv). $(docv) must be %s time."
          (Arg.doc_alts_enum order)
      in
      let order = Arg.enum order in
      Arg.(value & opt order `Start & info ["order-by"] ~doc ~docv:"ORDER")
    in
    let reads =
      let doc = "Select operations that read file $(docv). Repeatable." in
      Arg.(value & opt_all Cli.Arg.fpath [] &
           info ["r"; "read"] ~doc ~docv:"FILE")
    in
    let writes =
      let doc = "Select operations that wrote file $(docv). Repeatable." in
      Arg.(value & opt_all Cli.Arg.fpath [] &
           info ["w"; "write"] ~doc ~docv:"FILE")
    in
    let ids =
      let doc = "Select operation with identifier $(docv). Repeatable." in
      Arg.(value & opt_all int [] & info ["i"; "id"] ~doc ~docv:"ID")
    in
    let hashes =
      (* Could be properly parsed *)
      let doc = "Select operation with hash $(docv). Repeatable." in
      Arg.(value & opt_all string [] & info ["hash"] ~doc ~docv:"HASH")
    in
    let groups =
      let doc = "Select operations with group $(docv). Repeatable." in
      Arg.(value & opt_all string [] & info ["g"; "group"] ~doc ~docv:"GROUP")
    in
    let needs =
      let doc =
        "Once operations have been selected, also add all direct operations \
         needed by these. Use with option $(b,--rec) to get the recursive \
         operations."
      in
      Arg.(value & flag & info ["needs"] ~doc)
    in
    let enables =
      let doc =
        "Once operations have been selected, also add all direct operations \
         enabled by these. Use with option $(b,--rec) to get the recursive \
         operations."
      in
      Arg.(value & flag & info ["enables"] ~doc)
    in
    let recursive =
      let doc = "Select recursive needs or enables."  in
      Arg.(value & flag & info ["rec"] ~doc)
    in
    let revived =
      let doc = "Keep only revived or non-revived operations." in
      Arg.(value & opt ~vopt:(Some true) (some ~none:"any" bool) None &
           info ["revived"] ~doc ~docv:"BOOL")
    in
    let status =
      let aborted =
        let doc = "Keep only aborted operations." in
        Some `Aborted, Arg.info ["aborted"] ~doc
      in
      let done_ =
        let doc = "Keep only done operations." in
        Some `Done, Arg.info ["done"] ~doc
      in
      let failed =
        let doc = "Keep only failed operations." in
        Some `Failed, Arg.info ["e"; "failed"] ~doc
      in
      let waiting =
        let doc = "Keep only waiting operations." in
        Some `Waiting, Arg.info ["waiting"] ~doc
      in
      Arg.(value & vflag None [aborted; done_; failed; waiting])
    in
    let select
        reads writes ids hashes groups needs enables recursive revived status
        order_by =
      select ~reads ~writes ~ids ~hashes ~groups ~needs ~enables ~recursive
        ~revived ~status ~order_by
    in
    Term.(const select $ reads $ writes $ ids $ hashes $ groups $ needs $
          enables $ recursive $ revived $ status $ order_by)
end

module Memo = struct
  let pp_faint pp = Fmt.tty [`Faint] pp
  let read_howto = Fmt.any "b00-log -r "
  let write_howto = Fmt.any "b00-log -w "
  let op_howto ppf o = Fmt.pf ppf "b00-log -i %d" (B000.Op.id o)
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
    | `File_cache_need_copy p when level >= Log.Warning ->
        Fmt.pf ppf "@[Warning: cache need copy: %a@]%a" Fpath.pp_quoted p sep ()
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
        | prev :: [] -> deps prev first :: acc
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
    Arg.(value & opt (some ~none:doc_none Cli.Arg.fpath) None &
         info ["b0-dir"] ~env ~doc ~docs ~docv:"DIR")

  let cache_dir
      ?(opts = ["cache-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build cache directory.")
      ?(doc_none = "$(b,.cache) in b0 directory")
      ?(env = Cmdliner.Arg.env_var cache_dir_env) ()
    =
    Arg.(value & opt (some ~none:doc_none Cli.Arg.fpath) None &
         info opts ~env ~doc ~docs ~docv:"DIR")

  let log_file
      ?(opts = ["log-file"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Output (binary) build log to $(docv).")
      ?(doc_none = "$(b,.log) in b0 directory")
      ?(env = Cmdliner.Arg.env_var log_file_env) () =
    let doc = "Output (binary) build log to $(docv)." in
    Arg.(value & opt (some ~none:doc_none Cli.Arg.fpath) None &
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
              (B0_json.Jsong.to_string (B0_trace.Trace_event.of_ops ops))
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

module Pager = struct
  open Cmdliner

  let envs =
    Term.env_info "PAGER"
      ~doc:"The pager used to display content. This is a command \
            invocation given to execvp(3)." ::
    Term.env_info "TERM"
      ~doc:"See options $(b,--color) and $(b,--no-pager)." :: []

  let don't ?docs () =
    let doc =
      "Do not display the output in a pager. This automatically happens \
       if the $(b,TERM) environment variable is $(b,dumb) or unset."
    in
    Arg.(value & flag & info ["no-pager"] ?docs ~doc)

  let find ?search ~don't () = match don't with
  | true -> Ok None
  | false ->
      match Os.Env.find ~empty_to_none:true "TERM" with
      | Some "dumb" | None -> Ok None
      | Some _ ->
          let cmds = [Cmd.arg "less"; Cmd.arg "more"] in
          let cmds =
            match Os.Env.find_value Cmd.of_string ~empty_to_none:true "PAGER"
            with
            | None -> Ok cmds
            | Some (Ok cmd) -> Ok (cmd :: cmds)
            | Some (Error _ as e) -> e
          in
          Result.bind cmds (Os.Cmd.find_first ?search)

  let pager_env () = match Os.Env.find ~empty_to_none:false "LESS" with
  | Some _ -> Ok None
  | None ->
      Result.bind (Os.Env.current_assignments ()) @@ fun env ->
      Ok (Some ("LESS=FRX" :: env))

  let page_stdout = function
  | None -> Ok ()
  | Some pager ->
      let uerr = Unix.error_message in
      let err fmt = Fmt.error ("page stdout: " ^^ fmt) in
      let rec dup2 fd0 fd1 = match Unix.dup2 fd0 fd1 with
      | () -> Ok ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> dup2 fd0 fd1
      | exception Unix.Unix_error (e, _, _) -> err "dup2: %s" (uerr e)
      in
      match pager_env () with
      | Error e -> err "%s" e
      | Ok env ->
          match Unix.pipe () with
          | exception Unix.Unix_error (e, _, _)  -> err "pipe: %s" (uerr e)
          | (pager_read, parent_write) ->
              let stdin = Os.Cmd.in_fd ~close:true pager_read in
              Unix.set_close_on_exec parent_write;
              Os.Fd.apply ~close:Unix.close parent_write @@ fun parent_write ->
              Result.bind (Os.Cmd.spawn ?env ~stdin pager) @@ fun pid ->
              Result.bind (dup2 parent_write Unix.stdout) @@ fun () ->
              let on_exit () =
                (* Before closing Unix.stdout it's better to flush
                   formatter and channels. Otherwise it's done later
                   by OCaml's standard shutdown procedure and it
                   raises Sys_error as the fd is no longer valid. *)
                (try Fmt.flush Fmt.stdout () with Sys_error _ -> ());
                (try flush stdout with Sys_error _ -> ());
                (try Unix.close Unix.stdout with Unix.Unix_error _ -> ());
                (Result.map (fun st -> ()) (Os.Cmd.spawn_wait_status pid)
                 |> Log.if_error ~use:())
              in
              at_exit on_exit;
              Ok ()

  let page_files pager files = match pager with
  | Some pager when files = [] -> Ok ()
  | Some pager -> Os.Cmd.run Cmd.(pager %% paths files)
  | None ->
      let rec loop = function
      | [] -> Ok ()
      | f :: fs ->
          match Os.File.read f with
          | Error _ as e -> e
          | Ok d ->
              Printf.printf "%s" d;
              if fs <> [] then Printf.printf "\x1C" (* U+001C FS *);
              flush stdout;
              loop fs
      in
      loop files
end

module Editor = struct
  open Cmdliner

  let envs =
    Term.env_info "VISUAL"
      ~doc:"The editor used to edit files. This is a command \
            invocation given to execvp(3) and is used before EDITOR." ::
    Term.env_info "EDITOR"
      ~doc:"The editor used to edit files. This is a command \
            invocation given to execvp(3) and is used after VISUAL." ::
    []

  let find ?search () =
    let parse_env cmds env = match cmds with
    | Error _ as e -> e
    | Ok cmds as r ->
        match Os.Env.find_value Cmd.of_string ~empty_to_none:true env with
        | None -> r
        | Some (Ok cmd) -> Ok (cmd :: cmds)
        | Some (Error _ as e) -> e
    in
    let cmds = Ok [Cmd.arg "nano"] in
    let cmds = parse_env cmds "EDITOR" in
    let cmds = parse_env cmds "VISUAL" in
    Result.bind cmds (Os.Cmd.find_first ?search)

  let edit_files editor fs = match editor with
  | None -> Error "No runnable editor found in VISUAL or EDITOR"
  | Some editor -> Os.Cmd.run_status Cmd.(editor %% paths fs)
end

module Pdf_viewer = struct
  open Cmdliner

  (* XXX support background *)

  (* Cli *)

  let pdf_viewer_var = "PDFVIEWER"
  let pdf_viewer ?docs ?(opts = ["pdf-viewer"]) () =
    let env = Arg.env_var pdf_viewer_var in
    let doc =
      "The PDF viewer command $(docv) to use. If absent either one \
       of $(b,xdg-open(1)) or $(b,open(1)) is used. If not found and \
       on Windows $(b,start) is used."
    in
    let cmd = Arg.some ~none:"OS dependent fallback" Cli.Arg.cmd in
    Arg.(value & opt cmd None & info opts ~env ~doc ?docs ~docv:"CMD")

  (* Viewer *)

  type t = Cmd.t

  let find ?search ~pdf_viewer () =
    Result.map_error (fun e -> Fmt.str "find PDF viewer: %s" e) @@
    match pdf_viewer with
    | Some cmd -> Os.Cmd.find ?search cmd
    | None ->
        Result.bind (Os.Cmd.find ?search Cmd.(arg "xdg-open")) @@ function
        | Some xdg -> Ok (Some xdg)
        | None ->
            Result.bind (Os.Cmd.find ?search Cmd.(arg "open")) @@ function
            | Some oopen -> Ok (Some oopen)
            | None ->
                if Sys.win32
                then Ok (Some Cmd.(arg "start" % "")) (* really ? *)
                else Ok None

  let show pdf_viewer file =
    Result.map_error
      (fun e -> Fmt.str "show PDF %a: %s" Fpath.pp_quoted file e) @@
    match pdf_viewer with
    | None -> Error "No PDF viewer found, use the PDFVIEWER env var to set one."
    | Some cmd -> Os.Cmd.run Cmd.(cmd %% path file)
end

module Browser = struct
  open Cmdliner

  (* Cli *)

  let browser_var = "BROWSER"
  let browser ?docs ?(opts = ["browser"]) () =
    let env = Arg.env_var browser_var in
    let doc =
      "The WWW browser command $(docv) to use. The value may be interpreted \
       and massaged depending on the OS. On macOS: the names $(b,firefox), \
       $(b,chrome) and $(b,safari) are interpreted specially; use $(b,open) \
       if you would like to use open(2); if absent the application that \
       handles the $(b,http) URI scheme is used. On other platforms if
       xdg-open(1) is found in $(b,PATH) this the program used by default."
    in
    let cmd = Arg.some ~none:"OS dependent fallback" Cli.Arg.cmd in
    Arg.(value & opt cmd None & info opts ~env ~doc ?docs ~docv:"CMD")

  let prefix ?docs ?(opts = ["prefix"]) () =
    let doc =
      "Rather than the exact URI, reload if possible, the first browser tab \
       which has the URI as a prefix. Platform and browser support for this \
       feature is severly limited."
    in
    Arg.(value & flag & info opts ~doc ?docs)

  let background ?docs ?(opts = ["background"]) () =
    let doc =
      "Keep launched applications in the background. Platform support for \
       this feature is limited."
    in
    Arg.(value & flag & info opts ~doc ?docs)

  (* macOS JavaScript automation *)

  let macos_jxa ?search () = Os.Cmd.find_tool ?search (Fpath.v "osascript")

  let macos_jxa_run jxa script cli_args =
    let stdin = Os.Cmd.in_string script in
    let cmd = Cmd.(path jxa % "-l" % "JavaScript" % "-" %% cli_args) in
    Os.Cmd.(run_out ~stdin cmd)

  let macos_jxa_default_browser_appid jxa =
    Result.map_error (fun e -> Fmt.str "default lookup: %s" e) @@
    macos_jxa_run jxa {|
     ObjC.import('CoreServices');
     var h = $.LSCopyDefaultHandlerForURLScheme($('http'));
     $.CFStringGetCStringPtr (h, 0);
    |} Cmd.empty

  (* Finding a browser *)

  type t =
  | Cmd of Cmd.t
  | Macos_chrome of Cmd.tool (* this is osascript *)
  | Macos_safari of Cmd.tool (* this is osascript *)
  | Macos_open of Cmd.tool * string option

  let browser_env_fallback browser = match browser with
  | Some _ as b -> Ok b
  | None ->
      match Os.Env.find_value Cmd.of_string ~empty_to_none:true browser_var with
      | None -> Ok None
      | Some (Ok b) -> Ok (Some b)
      | Some (Error _ as e) -> e

  let find_browser_cmd ?search cmd =
    Result.bind (Os.Cmd.find ?search cmd) @@ function
    | None -> Ok None
    | Some c -> Ok (Some (Cmd c))

  let find_macos_open ?search ~appid =
    Result.bind (Os.Cmd.find_tool ?search (Fpath.v "open")) @@ function
    | None -> Ok None
    | Some tool -> Ok (Some (Macos_open (tool, appid)))

  let find_with_macos_jxa ?search ~browser jxa = match browser with
  | Some cmd when not (Cmd.is_singleton cmd) -> find_browser_cmd ?search cmd
  | Some cmd ->
      begin match String.Ascii.lowercase @@ List.hd (Cmd.to_list cmd) with
      | "chrome" -> Ok (Some (Macos_chrome jxa))
      | "firefox" -> find_macos_open ?search ~appid:(Some "org.mozilla.firefox")
      | "open" -> find_macos_open ?search ~appid:None
      | "safari" -> Ok (Some (Macos_safari jxa))
      | _ -> find_browser_cmd ?search cmd
      end
  | None ->
      Result.bind (macos_jxa_default_browser_appid jxa) @@ function
      | "" -> find_macos_open ?search ~appid:None
      | "com.apple.safari" -> Ok (Some (Macos_safari jxa))
      | "com.google.chrome" -> Ok (Some (Macos_chrome jxa))
      | appid -> find_macos_open ?search ~appid:(Some appid)

  let find ?search ~browser () =
    Result.map_error (fun e -> Fmt.str "find browser: %s" e) @@
    Result.bind (browser_env_fallback browser) @@ fun browser ->
    Result.bind (macos_jxa ?search ()) @@ function
    | Some jxa -> find_with_macos_jxa ?search ~browser jxa
    | None ->
        match browser with
        | Some cmd -> find_browser_cmd cmd
        | None ->
            Result.bind (Os.Cmd.find Cmd.(arg "xdg-open")) @@ function
            | None -> Ok None
            | Some xdg -> Ok (Some (Cmd xdg))

  (* Show *)

  let show_cmd ~background ~prefix cmd uri = Os.Cmd.run Cmd.(cmd % uri)
  let show_macos_open ~background ~prefix:_ open_tool ~appid uri =
    let appid = match appid with
    | None -> Cmd.empty
    | Some appid -> Cmd.(arg "-b" % appid)
    in
    let cmd = Cmd.(path open_tool %% if' background (arg "-g") %% appid) in
    Os.Cmd.run Cmd.(cmd % uri)

  let show_macos_jxa name  ~background ~prefix jxa uri script =
    let bool = string_of_bool in
    let args = Cmd.(arg (bool background) %% arg (bool prefix) % uri) in
    match macos_jxa_run jxa script args with
    | Ok _ -> Ok ()
    | Error e -> Fmt.error "%s jxa: %s" name e

  let show_macos_chrome ~background ~prefix jxa uri =
    (* It seems we no longer mange to bring the window to front.
       using win.index = 1 doesn't work. Maybe we should only consider
       windows[0]. *)
    show_macos_jxa "chrome" ~background ~prefix jxa uri {|
function is_equal (s0, s1) { return s0 === s1; }
function is_prefix (p, s) { return s && s.lastIndexOf (p, 0) === 0; }
function run(argv) {
  var background = (argv[0] == 'true');
  var pred = (argv[1] == 'true') ? is_prefix : is_equal;
  var uri = argv[2];
  var app = Application ('com.google.chrome');
  if (!background) app.activate ();
  for (var w = 0; w < app.windows.length; w++) {
    var win = app.windows[w];
    var tab = win.activeTab;
	  if (pred (uri, tab.url ())) { app.reload (tab); return; }
	  for (var t = 0; t < win.tabs.length; t++) {
		  tab = win.tabs[t];
		  if (pred (uri, tab.url ())) {
		   app.reload (tab); win.activeTabIndex = t + 1; return;
		  }
	  }
  }
  if (app.windows.length == 0) { app.Window().make(); }
  if (app.windows[0].activeTab.url () === 'chrome://newtab/')
    { app.windows[0].activeTab.url = uri; }
  else { app.windows[0].tabs.push(app.Tab ({ url : uri })); }
}|}

  let show_macos_safari ~background ~prefix jxa uri =
    (* win.index = 1 also (see chrome) doesn't work here and sadly opening
       a directory file URI opens the finder. *)
    show_macos_jxa "safari" ~background ~prefix jxa uri {|
function is_equal (s0, s1) { return s0 === s1; }
function is_prefix (p, s) { return s && s.lastIndexOf (p, 0) === 0; }
function run(argv) {
  var background = (argv[0] == 'true');
  var pred = (argv[1] == 'true') ? is_prefix : is_equal;
  var uri = argv[2];
  var app = Application ('com.apple.safari');
  if (!background) app.activate ();
  for (var w = 0; w < app.windows.length; w++) {
    var win = app.windows[w];
    var tab = win.currentTab;
	  if (pred (uri, tab.url ())) { tab.url = tab.url(); return; }
	  for (var t = 0; t < win.tabs.length; t++) {
      tab = win.tabs[t];
	    if (pred (uri, tab.url ()))
      { tab.url = tab.url(); win.currentTab = tab; return; }
	  }
  }
  if (app.windows.length == 0) { app.Document().make(); }
  if (app.windows[0].currentTab.url () === null)
    { app.windows[0].currentTab.url = uri; }
  else { app.windows[0].tabs.push(app.Tab ({ url : uri }));
         app.windows[0].currentTab =
         app.windows[0].tabs[app.windows[0].tabs.length-1]; }
}|}

  let show ~background ~prefix browser uri =
    Result.map_error (fun e -> Fmt.str "show uri %s: %s" uri e) @@
    match browser with
    | None -> Error "No browser found. Use the BROWSER env var to set one."
    | Some b ->
        match b with
        | Cmd cmd -> show_cmd ~background ~prefix cmd uri
        | Macos_chrome jxa -> show_macos_chrome ~background ~prefix jxa uri
        | Macos_safari jxa -> show_macos_safari ~background ~prefix jxa uri
        | Macos_open (open_tool, appid) ->
            show_macos_open ~background ~prefix open_tool ~appid uri
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
