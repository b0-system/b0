(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Cmdliner

(* Specifying b0 definitions. *)

let units ?docs ?(doc = "Use unit $(docv).") () =
  Arg.(value & opt_all string [] & info ["u"; "unit"] ?docs ~doc ~docv:"UNIT")

let x_units ?docs ?(doc = "Exclude unit $(docv). Takes over inclusion.") () =
  let docv = "UNIT" in
  Arg.(value & opt_all string [] & info ["x"; "x-unit"] ?docs ~doc ~docv)

let packs ?docs ?(doc = "Use pack $(docv).")  () =
  Arg.(value & opt_all string [] & info ["p"; "pack"] ?docs ~doc ~docv:"PACK")

let x_packs ?docs ?(doc = "Exclude pack $(docv). Takes over inclusion.") () =
  let docv = "PACK" in
  Arg.(value & opt_all string [] & info ["X"; "x-pack"] ?docs ~doc ~docv)

let get_excluded_units ~x_units ~x_packs =
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false x_units in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:false x_packs in
  let add_unit acc u = B0_unit.Set.add u acc in
  let add_pack_units p acc = List.fold_left add_unit acc (B0_pack.units p) in
  let packs = B0_pack.Set.of_list packs in
  Ok (B0_pack.Set.fold add_pack_units packs (B0_unit.Set.of_list units))

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
      match not (Hash.is_nil h) && B0_zero.Op.status o = B0_zero.Op.Success with
      | true -> String.Set.add (Hash.to_hex h) acc
      | false -> acc
    in
    List.fold_left add_op String.Set.empty ops

  let delete ~dir keys = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok false
  | true ->
      match keys with
      | `All ->
          (* Delete and recreate dir *)
          Result.bind (Os.Path.delete ~recurse:true dir) @@ fun _ ->
          Result.bind (Os.Dir.create ~make_path:true dir) @@ fun _ -> Ok true
      | `Keys keys ->
          Result.bind (B0_zero.File_cache.make dir) @@ fun c ->
          let delete c k =
            Log.if_error ~use:() @@
            Result.bind (B0_zero.File_cache.rem c k) @@ function
            | true -> Ok ()
            | false ->
                Log.warn begin fun m ->
                  m "No key %a in cache, ignored." Fmt.code k
                end;
                Ok ()
          in
          List.iter (delete c) keys; Ok true

  let gc ~dir ~used = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok false
  | true ->
      Result.bind (B0_zero.File_cache.make dir) @@ fun c ->
      Result.bind (B0_zero.File_cache.keys c) @@ fun keys ->
      let unused k = not (String.Set.mem k used) in
      let unused = List.filter unused keys in
      let delete c k =
        ignore (B0_zero.File_cache.rem c k |> Log.if_error ~use:false)
      in
      List.iter (delete c) unused;
      Ok true

  let keys ~dir = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok false
  | true ->
      Result.bind (B0_zero.File_cache.make dir) @@ fun c ->
      Result.bind (B0_zero.File_cache.keys c) @@ fun keys ->
      Log.stdout (fun m -> m "@[<v>%a@]" Fmt.(list string) keys);
      Ok true

  let stats ~dir ~used = Result.bind (Os.Dir.exists dir) @@ function
  | false -> Ok false
  | true ->
      Result.bind (B0_zero.File_cache.make dir) @@ fun c ->
      Result.bind (stats_of_cache c ~used) @@ fun stats ->
      Log.stdout (fun m -> m "@[<v>%a@]" pp_stats stats);
      Ok true

  let trim ~dir ~used ~max_byte_size ~pct =
    Result.bind (Os.Dir.exists dir) @@ function
    | false -> Ok false
    | true ->
        let is_unused k = not (String.Set.mem k used) in
        let* c = B0_zero.File_cache.make dir in
        let* c =
          B0_zero.File_cache.trim_size c ~is_unused ~max_byte_size ~pct
        in
        Ok true

  (* Cli fragments *)

  let key_arg =
    let of_string s = match Fpath.is_seg s with
    | true -> Ok s | false -> Error ("Not a valid key (not a path segment)")
    in
    Arg.conv' (of_string, String.pp) ~docv:"KEY"

  let keys_none_is_all ?pos_right:(kp = -1) () =
    let doc =
      "Select $(docv) (repeatable). If unspecified selects all keys."
    in
    let keys =
      Arg.(value & pos_right kp key_arg [] & info [] ~doc ~docv:"KEY")
    in
    Term.(const (function [] -> `All | ks -> `Keys ks) $ keys)

  let trim_cli ?(mb_opts = ["to-mb"]) ?(pct_opts = ["to-pct"]) ?docs () =
    let trim_to_mb =
      let doc = "Trim the cache to at most $(docv) megabytes." in
      let docv = "MB" in
      Arg.(value & opt (some int) None & info mb_opts ~doc ?docs ~docv)
    in
    let trim_to_pct =
      let doc = "Trim the cache to at most $(docv)% of the current size." in
      let docv = "PCT" in
      Arg.(value & opt (some int) None & info ["to-pct"] ~doc ~docv)
    in
    let trim trim_to_mb trim_to_pct = match trim_to_mb, trim_to_pct with
    | None, None -> max_int, 50
    | None, Some pct -> max_int, pct
    | Some mb, None -> mb * 1000 * 1000, 100
    | Some mb, Some pct -> mb * 1000 * 1000, pct
    in
    Term.(const trim $ trim_to_mb $ trim_to_pct)
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
      String.Set.of_list (List.rev_map Hash.to_binary_string hashes)
    in
    let mem_hash h = String.Set.mem h hashes in
    let marks = String.Set.of_list marks in
    let mem_mark m = String.Set.mem m marks in
    fun o ->
      List.exists (( = ) (Op.id o)) ids ||
      mem_hash (Hash.to_binary_string (Op.hash o)) ||
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

  let hash =
    let of_string s =
      let err _ = Fmt.str "Could not parse hash from %S" s in
      Result.map_error err (Hash.of_hex s)
    in
    Arg.conv' ~docv:"HASH" (of_string, Hash.pp)

  let marks
      ?(opts = ["m"; "mark"]) ?docs
      ?(doc = "Select operations marked by $(docv). Repeatable.")
      ?(docv = "MARK")
      ()
    =
    let docv = "MARK" in
    Arg.(value & opt_all string [] & info ["m"; "mark"] ~doc ?docs ~docv)

  let select_cli ?docs ?(marks = marks () ?docs) () =
    let reads =
      let doc = "Select operations that read file $(docv). Repeatable." in
      Arg.(value & opt_all B0_std_cli.fpath [] &
           info ["r"; "read"] ~doc ?docs ~docv:"FILE")
    in
    let writes =
      let doc = "Select operations that wrote file $(docv). Repeatable." in
      Arg.(value & opt_all B0_std_cli.fpath [] &
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
    let select reads writes ids hashes marks =
      select ~reads ~writes ~ids ~hashes ~marks
    in
    Term.(const select $ reads $ writes $ ids $ hashes $ marks)

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
    in
    let statuses =
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

  let s_selection_options = "BUILD OPERATION SELECTION OPTIONS"
  let query_cli ?(docs = s_selection_options) () =
    let query select select_deps filter order =
      query ~select ~select_deps ~filter ~order
    in
    Term.(const query $ select_cli ~docs () $ select_deps_cli ~docs () $
          filter_cli ~docs () $ order_cli ~docs ())

  let query_man =
    [ `P "Options are provided to select and filter operations. \
          Any operation that satifies one of the selectors and all of the \
          filters is included in the result. If no selector is specified all \
          operations are selected. If no filter is specified all selected \
          operations are returned. By default the result is sorted by \
          execution start time, this can be changed with the $(b,--order-by) \
          or $(b,-d) option." ]
end

module Memo = struct

  (* Memo feedback *)

  let op_howto ppf o = Fmt.pf ppf "b0-log --id %d" (B0_zero.Op.id o)
  let pp_leveled_feedback
      ?(sep = Fmt.flush_nl) ?(op_howto = op_howto) ~show_op ~show_ui ~level
      ppf f
    =
    let open B0_zero in
    if level = Log.Quiet then () else
    match f with
    | `Exec_start (_, _) -> () (* we have B0_std.Os spawn tracer on debug *)
    | `Op_complete o ->
        if level >= show_op || level = Log.Debug
        then (B0_zero_conv.Op.pp_line_and_ui ppf o; sep ppf ()) else
        if level >= show_ui
        then (B0_zero_conv.Op.pp_ui ~sep ~op_howto ppf o)
    | _ ->  ()

  (* b0 directory *)

  let b0_dir_env = "B0_DIR"
  let b0_dir_name = "_b0"
  let b0_dir
      ?(opts = ["b0-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the b0 directory.")
      ?doc_none:(absent = "$(b,_b0) in root directory")
      ?(env = Cmd.Env.info b0_dir_env) ()
    =
    Arg.(value & opt (some B0_std_cli.fpath) None &
         info opts ~env ~absent ~doc ~docs ~docv:"DIR")

  let get_b0_dir ~cwd ~root ~b0_dir = match b0_dir with
  | None -> Fpath.(root / b0_dir_name)
  | Some d -> Fpath.(cwd // d)

  let get_b0_dir_path ~cwd ~b0_dir default p = match p with
  | None -> Fpath.(b0_dir / default)
  | Some p -> Fpath.(cwd // p)

  let find_dir_with_b0_dir ~start =
    let rec loop p = match Fpath.is_root p with
    | true -> None
    | false ->
        match Os.Dir.exists Fpath.(p / b0_dir_name) with
        | Error _ | Ok false -> loop (Fpath.parent p)
        | Ok true -> Some p
    in
    if Fpath.is_rel start then None else (loop start)

  (* File cache directory *)

  let cache_dir_env = "B0_CACHE_DIR"
  let cache_dir_name = ".cache"
  let cache_dir
      ?(opts = ["cache-dir"])
      ?(docs = Manpage.s_common_options)
      ?(doc = "Use $(docv) for the build cache directory.")
      ?doc_none:(absent = "$(b,.cache) in b0 directory")
      ?(env = Cmd.Env.info cache_dir_env) ()
    =
    Arg.(value & opt (some B0_std_cli.fpath) None &
         info opts ~env ~absent ~doc ~docs ~docv:"DIR")

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
      ?(opts = ["log-file"]) ?docs
      ?(doc = "Use $(docv) for the build log file.")
      ?doc_none:(absent = "$(b,.log) in b0 directory")
      ?(env = Cmd.Env.info log_file_env) ()
    =
    Arg.(value & opt (some B0_std_cli.fpath) None &
         info opts ~absent ~env ~doc ?docs ~docv:"LOG_FILE")

  let get_log_file ~cwd ~b0_dir ~log_file =
    get_b0_dir_path ~cwd ~b0_dir log_file_name log_file

  (* Jobs *)

  let jobs_env = "B0_JOBS"
  let jobs
      ?(opts = ["j"; "jobs"]) ?docs
      ?(doc = "Maximal number of commands to spawn concurrently.")
      ?doc_none:(absent = "Number of CPUs available")
      ?(env = Cmd.Env.info jobs_env) ()
    =
    Arg.(value & opt (some int) None &
         info opts ~env ~absent ~doc ?docs ~docv:"COUNT")

  let get_jobs ~jobs = match jobs with
  | Some max -> max | None -> Os.Cpu.logical_count ()

  (* Hash fun *)

  let hash_fun_assoc =
    List.map (fun ((module H : Hash.T) as h) -> H.id, h) (Hash.funs ())

  let hash_fun_conv = Arg.enum ~docv:"HASHFUN" hash_fun_assoc
  let hash_fun_env = "B0_HASH_FUN"
  let hash_fun
      ?(opts = ["hash-fun"]) ?docs ?doc ?(doc_none = Hash.Xxh3_64.id)
      ?(env = Cmd.Env.info hash_fun_env) ()
    =
    let doc = match doc with
    | Some doc -> doc
    | None ->
        Fmt.str "$(docv) is the hash function to use. Must be %s."
          (Arg.doc_alts_enum hash_fun_assoc)
    in
    Arg.(value & opt (some ~none:doc_none hash_fun_conv) None &
         info opts ~env ~doc ?docs)

  let get_hash_fun ~hash_fun = match hash_fun with
  | None -> (module Hash.Xxh3_64 : Hash.T) | Some m -> m

  (* Logs *)

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

    type out_format =
    [ `Hashed_files | `Op_hashes | `Ops | `Path | `Stats | `Root_hashed_files
    | `Trace_event | `Diagnosis ]

    let pp_op = function
    | `Short -> B0_zero_conv.Op.pp_line
    | `Normal -> B0_zero_conv.Op.pp_line_and_ui
    | `Long -> B0_zero_conv.Op.pp

    let pp_op_hash = function
    | `Short | `Normal -> Fmt.using B0_zero.Op.hash Hash.pp
    | `Long ->
        fun ppf o ->
          Fmt.int ppf (B0_zero.Op.id o); Fmt.sp ppf ();
          Hash.pp ppf (B0_zero.Op.hash o)

    let pp_hashed_file = function
    | `Short -> Fmt.using fst Fpath.pp_unquoted
    | `Normal | `Long ->
        fun ppf (f, h) ->
          Hash.pp ppf h; Fmt.char ppf ' '; Fpath.pp_unquoted ppf f

    let out ppf format details query ~path l = match format with
    | `Path ->
        Fmt.pf ppf "@[%a@]@." Fpath.pp_unquoted path
    | `Ops ->
        let ops = query (B0_memo_log.ops l) in
        if ops = [] then () else
        Fmt.pf ppf "@[<v>%a@]@." (Fmt.list (pp_op details)) ops
    | `Stats ->
        let hashed_size = details <> `Short (* do it by default for now *) in
        Fmt.pf ppf "@[%a@]@." (pp_stats ~hashed_size query) l
    | `Trace_event ->
        let ops = query (B0_memo_log.ops l) in
        if ops = [] then () else
        let t = B0_build_trace.Trace_event.of_ops ops in
        Fmt.pf ppf "@[%s@]@." (B0_json.Jsong.to_string t)
    | `Op_hashes ->
        let has_hash o = not (Hash.is_nil (B0_zero.Op.hash o)) in
        let ops = List.filter has_hash (query (B0_memo_log.ops l)) in
        if ops = [] then () else
        Fmt.pf ppf "@[<v>%a@]@." (Fmt.list (pp_op_hash details)) ops
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
        Fmt.pf ppf "@[<v>%a@]@." (Fmt.list (pp_hashed_file details)) roots
    | `Hashed_files ->
        let pp_hashed_files =
          Fmt.iter_bindings Fpath.Map.iter (pp_hashed_file details)
        in
        let file_hashes = B0_memo_log.file_hashes l in
        if Fpath.Map.is_empty file_hashes then () else
        Fmt.pf ppf "@[<v>%a@]@." pp_hashed_files file_hashes
    | `Diagnosis ->
        let ops = query (B0_memo_log.ops l) in
        if ops = []
        then Log.warn (fun m -> m "No operation selected") else
        match B0_zero.Op.find_build_correctness_errors ops with
        | Ok () ->
            Fmt.pf ppf "@[%a found.@]@." (Fmt.st [`Fg `Green]) "No problem"
        | Error errs ->
            let pp_op = pp_op details in
            let pp_err = B0_zero_conv.Op.pp_build_correctness_error ~pp_op in
            let count = List.length errs in
            let s = if count < 1 then "s" else "s" in
            Fmt.pf ppf "@[<v>%a Found %d problem%s@,%a@]" Fmt.puterr ()
              count s Fmt.(list pp_err) errs

    let out_format_cli ?(docs = B0_std_cli.s_output_format_options) () =
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
  end
end
