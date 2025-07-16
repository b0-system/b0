(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

module Hash = struct

  let get_hash_fun ~hash_fun = match hash_fun with
  | None -> (module B0_hash.Xxh3_64 : B0_hash.T) | Some m -> m

  (* Command line interaction *)

  open Cmdliner
  open Cmdliner.Term.Syntax

  let hash_fun_assoc =
    List.map (fun ((module H : B0_hash.T) as h) -> H.id, h) (B0_hash.funs ())

  let hash_fun_conv = Arg.enum ~docv:"HASHFUN" hash_fun_assoc
  let hash_fun_var = Cmd.Env.info "B0_HASH_FUN"
  let hash_fun
      ?(opts = ["hash-fun"]) ?(docs = Manpage.s_common_options)
      ?doc ?(doc_none = B0_hash.Xxh3_64.id) ?(env = hash_fun_var) ()
    =
    let doc = match doc with
    | Some doc -> doc
    | None ->
        Fmt.str "$(docv) is the hash function to use. Must be %s."
          (Arg.doc_alts_enum hash_fun_assoc)
    in
    Arg.(value & opt (some ~none:doc_none hash_fun_conv) None &
         info opts ~env ~doc ~docs)
end

module File_cache = struct

  (* Cache stats *)

  type key_stats =
    { keys_count : int; keys_file_count : int; keys_byte_size : int}

  let pp_key_stats ppf s =
    Fmt.pf ppf " %5d  %5d  %a"
      s.keys_count s.keys_file_count (Fmt.code' Fmt.byte_size) s.keys_byte_size

  let pp_stats ppf (total, used) =
    let row = Fmt.st [`Fg `Yellow] in
    let col = Fmt.st [`Italic] in
    let pp_cols ppf () = Fmt.pf ppf "       %a  %a" col "keys" col "files" in
    Fmt.pf ppf "@[<v>%a%a@,%a%a@,%a@]"
      row "total" pp_key_stats total
      row "used " pp_key_stats used
      pp_cols ()

  let pp_stats ppf (total, used) =
    let row = Fmt.st [`Fg `Yellow] in
    let col = Fmt.st [`Italic] in
    let pp_size ppf s = Fmt.pf ppf "%6s" (Fmt.str "%a" Fmt.byte_size s) in
    let pp_cols ppf () = Fmt.pf ppf "       %a    %a" col "total" col "used" in
    Fmt.pf ppf "@[<v>%a@,%a %6d  %6d@,%a %6d  %6d@,%a %a  %a@]"
      pp_cols ()
      row "keys " total.keys_count used.keys_count
      row "files" total.keys_file_count used.keys_file_count
      row "size "
      (Fmt.code' pp_size) total.keys_byte_size
      (Fmt.code' pp_size) used.keys_byte_size

  let stats_of_cache c ~used =
    let rec loop tk tf tb uk uf ub = function
    | [] ->
        let t = {keys_count=tk; keys_file_count=tf; keys_byte_size=tb} in
        let u = {keys_count=uk; keys_file_count=uf; keys_byte_size=ub} in
        t, u
    | k :: ks ->
        let kf, kb, _ =
          B0_zero.File_cache.key_stats c k |> Result.error_to_failure
        in
        match String.Set.mem k used with
        | true -> loop (tk+1) (tf+kf) (tb+kb) (uk+1) (uf+kf) (ub+kb) ks
        | false -> loop (tk+1) (tf+kf) (tb+kb) uk uf ub ks
    in
    try
      let keys = B0_zero.File_cache.keys c |> Result.error_to_failure in
      Ok (loop 0 0 0 0 0 0 keys)
    with
    | Failure e -> Error (Fmt.str "cache stats: %s" e)

  (* High-level commands *)

  let keys_of_success_ops ops =
    let add_op acc o =
      let h = B0_zero.Op.hash o in
      if not (B0_hash.is_nil h) && B0_zero.Op.status o = B0_zero.Op.Success
      then String.Set.add (B0_hash.to_hex h) acc
      else acc
    in
    List.fold_left add_op String.Set.empty ops

  let delete ~dir keys =
    let* exists = Os.Dir.exists dir in
    if not exists then Ok false else
    match keys with
    | `All ->
        (* Delete and recreate dir *)
        let _existed = Os.Path.delete ~recurse:true dir in
        let _existed = Os.Dir.create ~make_path:true dir in
        Ok true
    | `Keys keys ->
        let* cache = B0_zero.File_cache.make dir in
        let delete cache key =
          Log.if_error ~use:() @@
          let* existed = B0_zero.File_cache.rem cache key in
          if existed then Ok () else
          (Log.warn begin fun m ->
              m "No key %a in cache, ignored." Fmt.code key
            end;
           Ok ())
        in
        List.iter (delete cache) keys; Ok true

  let gc ~dir ~used =
    let* exists = Os.Dir.exists dir in
    if not exists then Ok false else
    let* conf = B0_zero.File_cache.make dir in
    let* keys = B0_zero.File_cache.keys conf in
    let unused k = not (String.Set.mem k used) in
    let unused = List.filter unused keys in
    let delete conf k =
      Log.if_error ~use:() @@
      let* _existed = B0_zero.File_cache.rem conf k in
      Ok ()
    in
    List.iter (delete conf) unused;
    Ok true

  let keys ~dir =
    let* exists = Os.Dir.exists dir in
    if not exists then Ok false else
    let* cache = B0_zero.File_cache.make dir in
    let* keys = B0_zero.File_cache.keys cache in
    Fmt.pr "@[<v>%a@]@." Fmt.(list string) keys;
    Ok true

  let stats ~dir ~used =
    let* exists = Os.Dir.exists dir in
    if not exists then Ok false else
    let* cache = B0_zero.File_cache.make dir in
    let* stats = stats_of_cache cache ~used in
    Fmt.pr "@[<v>%a@]@." pp_stats stats;
    Ok true

  let trim ~dir ~used ~max_byte_size ~pct =
    let* exists = Os.Dir.exists dir in
    if not exists then Ok false else
    let is_unused k = not (String.Set.mem k used) in
    let* c = B0_zero.File_cache.make dir in
    let* () = B0_zero.File_cache.trim_size c ~is_unused ~max_byte_size ~pct in
    Ok true

  (* Cli fragments *)

  open Cmdliner
  open Cmdliner.Term.Syntax

  let key_arg =
    let parser s =
      if Fpath.is_segment s then Ok s else
      Error ("Not a valid key (not a path segment)")
    in
    let completion = Arg.Completion.make ~files:true () in
    Arg.Conv.make ~completion ~parser ~pp:String.pp ~docv:"KEY" ()

  let keys_none_is_all ?(first = 0) () =
    let+ keys =
      let doc =
        "Select $(docv) (repeatable). If unspecified selects all keys."
      in
      Arg.(value & pos_right (first - 1) key_arg [] & info [] ~doc)
    in
    match keys with
    | [] -> `All | keys -> `Keys keys

  let trim_cli ?(mb_opts = ["to-mb"]) ?(pct_opts = ["to-pct"]) ?docs () =
    let+ trim_to_mb =
      let doc = "Trim the cache to at most $(docv) megabytes." in
      let docv = "MB" in
      Arg.(value & opt (some int) None & info mb_opts ~doc ?docs ~docv)
    and+ trim_to_pct =
      let doc = "Trim the cache to at most $(docv)% of the current size." in
      let docv = "PCT" in
      Arg.(value & opt (some int) None & info ["to-pct"] ~doc ~docv)
    in
    match trim_to_mb, trim_to_pct with
    | None, None -> max_int, 50
    | None, Some pct -> max_int, pct
    | Some mb, None -> mb * 1000 * 1000, 100
    | Some mb, Some pct -> mb * 1000 * 1000, pct

  let dir_var = Cmd.Env.info "B0_CACHE_DIR"
  let dirname = ".cache"
  let dir
      ?(opts = ["b0-cache-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build cache directory.")
      ?doc_absent:(absent = "$(b,.cache) in b0 directory")
      ?(env = dir_var) ()
    =
    Arg.(value & opt (some B0_std_cli.dirpath) None &
         info opts ~env ~absent ~doc ~docs)
end

module Op = struct
  open B0_zero

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

  type query = B0_zero.Op.t list -> B0_zero.Op.t list

  let select ~reads ~writes ~ids ~hashes ~marks =
    let all =
      reads = [] && writes = [] && ids = [] && hashes = [] && marks = []
    in
    if all then fun _ -> true else
    let reads = Fpath.Set.of_list reads in
    let mem_reads f = Fpath.Set.mem f reads in
    let writes = Fpath.Set.of_list writes in
    let mem_writes f = Fpath.Set.mem f writes in
    let hashes =
      String.Set.of_list (List.rev_map B0_hash.to_binary_string hashes)
    in
    let mem_hash h = String.Set.mem h hashes in
    let marks = String.Set.of_list marks in
    let mem_mark m = String.Set.mem m marks in
    fun o ->
      List.exists (( = ) (Op.id o)) ids ||
      mem_hash (B0_hash.to_binary_string (Op.hash o)) ||
      List.exists mem_reads (Op.reads o) ||
      List.exists mem_writes (Op.writes o) ||
      mem_mark (Op.mark o)

  let select_deps ~needs ~enables ~recursive ~dom ops =
    if not needs && not enables then ops else
    let reads, writes = B0_zero.Op.read_write_maps dom in
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
  | Op.Aborted -> `Aborted | Op.Success -> `Success | Op.Failed _ -> `Failed
  | Op.Waiting -> `Waiting

  let filter ~revived ~statuses ~kinds =
    let revived_filter = match revived with
    | None -> fun _ -> true
    | Some `Revived -> Op.revived
    | Some `Unrevived -> fun o -> not (Op.revived o) && Op.supports_reviving o
    | Some `Executed -> fun o -> not (Op.revived o)
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
    | `Create -> order_by_field Mtime.Span.compare Op.time_created
    | `Start -> order_by_field Mtime.Span.compare Op.time_started
    | `Wait -> order_by_field Mtime.Span.compare Op.waited
    | `Dur ->
        let rev_compare t0 t1 = Mtime.Span.compare t1 t0 in
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
  open Cmdliner.Term.Syntax

  let hash =
    let parser s =
      let err _ = Fmt.str "Could not parse hash from %S" s in
      Result.map_error err (B0_hash.of_hex s)
    in
    Arg.Conv.make ~parser ~pp:B0_hash.pp ~docv:"HASH" ()

  let marks
      ?(opts = ["m"; "mark"]) ?docs
      ?(doc = "Select operations marked by $(docv). Repeatable.")
      ?(docv = "MARK")
      ()
    =
    let docv = "MARK" in
    Arg.(value & opt_all string [] & info ["m"; "mark"] ~doc ?docs ~docv)

  let select_cli ?docs ?(marks = marks () ?docs) () =
    let+ reads =
      let doc = "Select operations that read file $(docv). Repeatable." in
      Arg.(value & opt_all B0_std_cli.filepath [] &
           info ["r"; "read"] ~doc ?docs)
    and+ writes =
      let doc = "Select operations that wrote file $(docv). Repeatable." in
      Arg.(value & opt_all B0_std_cli.filepath [] &
           info ["w"; "write"] ~doc ?docs)
    and+ ids =
      let doc = "Select operation with identifier $(docv). Repeatable." in
      Arg.(value & opt_all int [] & info ["id"] ~doc ?docs ~docv:"ID")
    and+ hashes =
      let doc = "Select operation with hash $(docv). Repeatable." in
      Arg.(value & opt_all hash [] & info ["hash"] ~doc ?docs ~docv:"HASH")
    and+ marks in
    select ~reads ~writes ~ids ~hashes ~marks

  let select_deps_cli ?docs () =
    let+ needs =
      let doc =
        "Once operations have been selected, replace them with all direct \
         operations needed by these before filtering. Use with option \
         $(b,--rec) to get the recursive operations."
      in
      Arg.(value & flag & info ["needs"] ~doc ?docs)
    and+ enables =
      let doc =
        "Once operations have been selected, replace them with all direct \
         operations enabled by these before filtering. Use with option \
         $(b,--rec) to get the recursive operations."
      in
      Arg.(value & flag & info ["enables"] ~doc ?docs)
    and+ recursive =
      let doc = "Make $(b,--needs) or $(b,--enables) recursive." in
      Arg.(value & flag & info ["rec"] ~doc ?docs)
    in
    select_deps ~needs ~enables ~recursive

  let filter_cli ?docs () =
    let+ revived =
      let revived =
        let doc = "Keep only revivable operations that were revived." in
        Some `Revived, Arg.info ["revived"] ~doc ?docs
      in
      let unrevived =
        let doc = "Keep only revivable operations that were executed." in
        Some `Unrevived, Arg.info ["u";"unrevived"] ~doc ?docs
      in
      let executed =
        let doc =
          "Keep only operations that were executed. See also $(b,-u)."
        in
        Some `Executed, Arg.info ["executed"] ~doc ?docs
      in
      Arg.(value & vflag None [revived; unrevived; executed])
    and+ statuses =
      let statuses =
        let status_enum =
          [ "aborted", `Aborted; "success", `Success; "failed", `Failed;
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
        let doc =
          "Keep only failed operations. Equivalent to add $(b,failed) to \
           the $(b,--status) option."
        in
        Arg.(value & flag & info ["e"; "failed"] ~doc ?docs)
      in
      let sts statuses errs = if errs then `Failed :: statuses else statuses in
      Term.(const sts $ statuses $ errors)
    and+ kinds =
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
    filter ~revived ~statuses ~kinds

  let order_cli ?docs () =
    let+ order_by =
      let order =
        [ "create", `Create; "start", `Start; "wait", `Wait; "dur", `Dur; ]
      in
      let doc =
        Fmt.str "Order by $(docv). $(docv) must be %s time."
          (Arg.doc_alts_enum order)
      in
      let order = Arg.enum order and docv = "ORDER" in
      Arg.(value & opt order `Start & info ["order-by"] ~doc ?docs ~docv)
    and+ by_dur =
      let doc = "Order by decreasing duration. Takes over $(b,--order-by)." in
      Arg.(value & flag & info ["d"] ~doc ?docs)
    in
    let by = if by_dur then `Dur else order_by in
    order ~by

  let s_selection_options = "BUILD OPERATION SELECTION OPTIONS"

  let query_cli ?(docs = s_selection_options) () =
    let+ select = select_cli ~docs ()
    and+ select_deps = select_deps_cli ~docs ()
    and+ filter = filter_cli ~docs ()
    and+ order = order_cli ~docs () in
    query ~select ~select_deps ~filter ~order

  let query_man = [
    `P "Options are provided to select and filter operations. Any operation \
        that satifies one of the selectors and all of the filters is \
        included in the result. If no selector is specified all operations \
        are selected. If no filter is specified all selected operations are \
        returned. By default the result is sorted by execution start time, \
        this can be changed with the $(b,--order-by) or $(b,-d) option." ]
end

module Log = struct

  (* Log formatters *)

  let hashed_byte_size file_hashes =
    let add_file f _ acc = match Unix.stat (Fpath.to_string f) with
    | exception Unix.Unix_error (_, _, _) -> 0
    | s -> acc + s.Unix.st_size
    in
    Fpath.Map.fold add_file file_hashes 0

  let pp_stats ~hashed_size sel ppf l =
    let sc, st, sd, wc, wt, wd, cc, ct, cd, rt, rd, ot, od =
      let ( ++ ) = Mtime.Span.add in
      let rec loop sc st sd wc wt wd cc ct cd rt rd ot od = function
      | [] -> sc, st, sd, wc, wt, wd, cc, ct, cd, rt, rd, ot, od
      | o :: os ->
          let revived = B0_zero.Op.revived o and d = B0_zero.Op.duration o in
          let ot = ot + 1 and od = od ++ d in
          match B0_zero.Op.kind o with
          | B0_zero.Op.Spawn _ ->
              let sc = if revived then sc + 1 else sc in
              loop sc (st + 1) (sd ++ d) wc wt wd cc ct cd rt rd ot od os
          | B0_zero.Op.Write _ ->
              let wc = if revived then wc + 1 else wc in
              loop sc st sd wc (wt + 1) (wd ++ d) cc ct cd rt rd ot od os
          | B0_zero.Op.Copy _ ->
              let cc = if revived then cc + 1 else cc in
              loop sc st sd wc wt wd cc (ct + 1) (cd ++ d) rt rd ot od os
          | B0_zero.Op.Read _ ->
              loop sc st sd wc wt wd cc ct cd (rt + 1) (rd ++ d) ot od os
          | _ ->
              loop sc st sd wc wt wd cc ct cd rt rd ot od os
      in
      loop
        0 0 Mtime.Span.zero 0 0 Mtime.Span.zero 0 0 Mtime.Span.zero
        0 Mtime.Span.zero 0 Mtime.Span.zero (sel (B0_memo_log.ops l))
    in
    let pp_totals ppf (ot, od) = Fmt.pf ppf "%a %d" Mtime.Span.pp od ot in
    let pp_hashes ppf l =
      let file_hashes = B0_memo_log.file_hashes l in
      let hash_dur = B0_memo_log.hash_dur l in
      let hc, hd = Fpath.Map.cardinal file_hashes, hash_dur in
      let hs = if not hashed_size then 0 else hashed_byte_size file_hashes
      in
      let pp_hashed_size ppf s =
        let label = Fmt.st [`Italic] in
        match hashed_size with
        | true -> Fmt.field ~label "size" (fun c -> c) Fmt.byte_size ppf s
        | false -> ()
      in
      Fmt.pf ppf "%a %a" pp_totals (hc, hd) pp_hashed_size hs
    in
    let pp_xtime ppf (self, children) =
      let label = Fmt.st [`Italic] in
      Fmt.pf ppf "%a %a" Mtime.Span.pp self
        (Fmt.field ~label "children" (fun c -> c) Mtime.Span.pp)
        children
    in
    let pp_stime ppf l =
      let cpu_dur = B0_memo_log.cpu_dur l in
      let t = Os.Cpu.Time.Span.(stime cpu_dur, children_stime cpu_dur) in
      pp_xtime ppf t
    in
    let pp_utime ppf l =
      let cpu_dur = B0_memo_log.cpu_dur l in
      let t = Os.Cpu.Time.Span.(utime cpu_dur, children_utime cpu_dur) in
      pp_xtime ppf t
    in
    let pp_op ppf (oc, ot, od) =
      Fmt.pf ppf "%a %d (%d revived)" Mtime.Span.pp od ot oc
    in
    let pp_op_no_cache ppf (ot, od) =
      Fmt.pf ppf "%a %d" Mtime.Span.pp od ot
    in
    let pp_sec s ppf _ = Fmt.st [`Bold] ppf s in
    let total_dur = B0_memo_log.total_dur l in
    (Fmt.record @@
     [ pp_sec "selected operations";
       Fmt.field "spawns" (fun _ -> (sc, st, sd)) pp_op;
       Fmt.field "writes" (fun _ -> (wc, wt, wd)) pp_op;
       Fmt.field "copies" (fun _ -> (cc, ct, cd)) pp_op;
       Fmt.field "reads" (fun _ -> (rt, rd)) pp_op_no_cache;
       Fmt.field "all" (fun _ -> (ot, od)) pp_totals;
       pp_sec "global timings";
       Fmt.field "jobs" B0_memo_log.jobs Fmt.int;
       Fmt.field "hashes" Fun.id pp_hashes;
       Fmt.field "utime" Fun.id pp_utime;
       Fmt.field "stime" Fun.id pp_stime;
       Fmt.field "real" (fun _ -> total_dur) Mtime.Span.pp ]) ppf l

  type format =
  [ `Hashed_files | `Op_hashes | `Ops | `Path | `Stats | `Root_hashed_files
  | `Trace_event | `Diagnosis ]

  let pp_op = function
  | `Short -> B0_zero_conv.Op.pp_line
  | `Normal -> B0_zero_conv.Op.pp_line_and_ui
  | `Long -> B0_zero_conv.Op.pp

  let pp_op_hash = function
  | `Short | `Normal -> Fmt.using B0_zero.Op.hash B0_hash.pp
  | `Long ->
      fun ppf o ->
        Fmt.int ppf (B0_zero.Op.id o); Fmt.sp ppf ();
        B0_hash.pp ppf (B0_zero.Op.hash o)

  let pp_hashed_file = function
  | `Short -> Fmt.using fst Fpath.pp_unquoted
  | `Normal | `Long ->
      fun ppf (f, h) ->
        B0_hash.pp ppf h; Fmt.char ppf ' '; Fpath.pp_unquoted ppf f

  let pp ?(sep = Fmt.cut) ~format ~output_details ~query ~path () ppf l =
    match format with
    | `Path ->
        Fmt.pf ppf "@[%a@]%a" Fpath.pp_unquoted path sep ()
    | `Ops ->
        let ops = query (B0_memo_log.ops l) in
        if ops = [] then () else
        Fmt.pf ppf "@[<v>%a@]%a" (Fmt.list (pp_op output_details)) ops sep ()
    | `Stats ->
        let hashed_size =
          output_details <> `Short (* do it by default for now *)
        in
        Fmt.pf ppf "@[%a@]%a" (pp_stats ~hashed_size query) l sep ()
    | `Trace_event ->
        let ops = query (B0_memo_log.ops l) in
        if ops = [] then () else
        let t = B0_build_trace.Trace_event.of_ops ops in
        Fmt.pf ppf "@[%s@]%a" (B0_json.Jsong.to_string t) sep ()
    | `Op_hashes ->
        let has_hash o = not (B0_hash.is_nil (B0_zero.Op.hash o)) in
        let ops = List.filter has_hash (query (B0_memo_log.ops l)) in
        if ops = [] then () else
        Fmt.pf ppf "@[<v>%a@]%a"
          (Fmt.list (pp_op_hash output_details)) ops sep ()
    | `Root_hashed_files ->
        let writes =
          let add_write acc f = Fpath.Set.add f acc in
          let add_op acc o =
            List.fold_left add_write acc (B0_zero.Op.writes o)
          in
          List.fold_left add_op Fpath.Set.empty (B0_memo_log.ops l)
        in
        let add_file writes f h acc =
          if Fpath.Set.mem f writes then acc else (f, h) :: acc
        in
        let file_hashes = B0_memo_log.file_hashes l in
        let roots = Fpath.Map.fold (add_file writes) file_hashes [] in
        if roots = [] then () else
        Fmt.pf ppf "@[<v>%a@]%a"
          (Fmt.list (pp_hashed_file output_details)) roots sep ()
    | `Hashed_files ->
        let pp_hashed_files =
          Fmt.iter_bindings Fpath.Map.iter (pp_hashed_file output_details)
        in
        let file_hashes = B0_memo_log.file_hashes l in
        if Fpath.Map.is_empty file_hashes then () else
        Fmt.pf ppf "@[<v>%a@]%a" pp_hashed_files file_hashes sep ()
    | `Diagnosis ->
        let ops = query (B0_memo_log.ops l) in
        if ops = []
        then Log.warn (fun m -> m "No operation selected") else
        match B0_zero.Op.find_build_correctness_errors ops with
        | Ok () ->
            Fmt.pf ppf "@[%a found.@]%a"
              (Fmt.st [`Fg `Green]) "No problem" sep ()
        | Error errs ->
            let pp_op = pp_op output_details in
            let pp_err = B0_zero_conv.Op.pp_build_correctness_error ~pp_op in
            let count = List.length errs in
            let one ppf i = Fmt.pf ppf "%d problem" i in
            let problem = Fmt.cardinal ~one () in
            Fmt.pf ppf "@[<v>%a Found %a@,%a@]%a"
              Fmt.puterr () problem count Fmt.(list pp_err) errs sep ()

  (* Command line interaction *)

  open Cmdliner
  open Cmdliner.Term.Syntax

  let s_output_format_options = "OUTPUT FORMAT OPTIONS"
  let format_cli ?(docs = s_output_format_options) () =
    let a opt doc = Arg.info [opt] ~doc ~docs in
    let fmts =
      [ `Hashed_files, a "hashed-files"
          "Output the path of every hashed file.";
        `Op_hashes, a "op-hashes"
          "Output the hashes (cache keys) of selected operations.";
        `Ops, a "ops"
          "Output selected operations (default).";
        `Path, a "path"
          "Output the path to the log file.";
        `Root_hashed_files, a "root-hashed-files"
          "Output the path of hashed files that are not written by any \
           of the build operations in the log.";
        `Stats, a "stats"
          "Output statistics about the build and selected operations.";
        `Trace_event, a "trace-event"
          "Output selected operations in Trace Event format.";
        `Diagnosis, a "diagnosis"
          "Output a diagnosis of the build correctness of selected \
           operations.";
      ]
    in
    Arg.(value & vflag `Ops fmts)


  (* Log file *)

  let filename = "_log"
  let file_var = Cmd.Env.info "B0_LOG_FILE"
  let file
      ?(opts = ["log-file"]) ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build log file.")
      ?doc_absent:(absent = "$(b,_log) in b0 directory")
      ?(env = file_var) ()
    =
    Arg.(value & opt (some B0_std_cli.filepath) None &
         info opts ~absent ~env ~doc ~docs ~docv:"LOG_FILE")
end


(* Memo feedback *)

let pp_op_howto ppf o = Fmt.pf ppf "b0-log --id %d" (B0_zero.Op.id o)
let pp_leveled_feedback
    ?(sep = Fmt.flush_nl) ?(op_howto = pp_op_howto)
    ~output_op_level ~output_ui_level ~level ppf f
  =
  if level = B0_std.Log.Quiet then () else
  match f with
  | `Exec_start (_, _) -> () (* we have B0_std.Os spawn tracer on debug *)
  | `Op_complete o ->
      if level >= output_op_level || level = B0_std.Log.Debug
      then (B0_zero_conv.Op.pp_line_and_ui ppf o; sep ppf ()) else
      if level >= output_ui_level
      then (B0_zero_conv.Op.pp_ui ~sep ~op_howto ppf o)
  | _ ->  ()

(* Jobs *)

let trash_dirname = ".trash"


let get_jobs ~jobs = match jobs with
| Some max -> max | None -> Os.Cpu.logical_count ()

let jobs_var = Cmdliner.Cmd.Env.info "B0_JOBS"
let jobs
    ?(opts = ["j"; "jobs"]) ?(docs = Cmdliner.Manpage.s_common_options)
    ?(doc = "Maximal number of commands to spawn concurrently.")
    ?doc_none:(absent = "Number of CPUs available")
    ?(env =  jobs_var) ()
  =
  Cmdliner.Arg.(value & opt (some int) None &
                info opts ~env ~absent ~doc ~docs ~docv:"COUNT")
