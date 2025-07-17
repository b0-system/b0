(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_zero

(* Exit codes *)

let err_no_cache = 1

(* Commonalities *)

let with_cache_dir cache_dir k =
  let* exists = Os.Dir.exists cache_dir in
  if exists then k cache_dir else
  (Log.err begin fun m ->
      m "@[<v>%a:@,No such cache directory.@]" Fpath.pp cache_dir
    end;
   Ok err_no_cache)

let find_used_keys ~log_files k =
  let add_log_file file acc =
    let* l = B0_memo_log.read file in
    let ops = B0_memo_log.ops l in
    Ok (B0_memo_cli.File_cache.keys_of_success_ops ~init:acc ops)
  in
  let* keys = List.fold_stop_on_error add_log_file log_files String.Set.empty in
  k keys

let find_dirs ~b0_dir ~cache_dir =
  let* cwd = Os.Dir.cwd () in
  let root = B0_cli.find_dir_with_b0_dir ~start:cwd in
  let root = Option.value root ~default:cwd in
  let b0_dir = B0_cli.get_b0_dir ~cwd ~root ~b0_dir in
  let cache_dir = B0_cli.get_cache_dir ~cwd ~b0_dir ~cache_dir in
  Ok (cwd, b0_dir, cache_dir)

(* Commands *)

let delete ~b0_dir ~cache_dir ~kind ~keys ~log_files =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~log_files @@ fun used ->
  let* _ = B0_memo_cli.File_cache.delete ~dir ~used ~kind keys in
  Ok 0

let gc ~dry_run ~b0_dir ~cache_dir ~log_files =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~log_files @@ fun used ->
  let* _exists = B0_memo_cli.File_cache.gc ~dry_run ~dir ~used in
  Ok 0

let keys ~b0_dir ~cache_dir ~kind ~log_files =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~log_files @@ fun used ->
  let* _exists = B0_memo_cli.File_cache.keys ~dir ~used ~kind in
  Ok 0

let path ~b0_dir ~cache_dir =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  Log.stdout (fun m -> m "%a" Fpath.pp cache_dir);
  Ok 0

let stats ~b0_dir ~cache_dir ~log_files  =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~log_files @@ fun used ->
  let* _exists = B0_memo_cli.File_cache.stats ~dir ~used in
  Ok 0

let trim
   ~dry_run  ~b0_dir ~cache_dir ~log_files ~trim_spec:(max_byte_size, pct)
  =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~log_files @@ fun used ->
  let* _exists =
    B0_memo_cli.File_cache.trim ~dry_run ~dir ~used ~max_byte_size ~pct
  in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let man m =
  [ `Blocks m;
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,b0) system. See
        $(i,https:/erratique.ch/software/b0) for contact information."; ]

let exits =
  Cmd.Exit.info err_no_cache ~doc:"no cache directory found." ::
  Cmd.Exit.defaults

let set_log_level = B0_std_cli.set_log_level ()
let b0_dir = B0_cli.b0_dir ()
let cache_dir = B0_memo_cli.File_cache.dir ()
let dry_run = B0_memo_cli.File_cache.dry_run ()
let key_kind = B0_memo_cli.File_cache.key_kind_cli ()
let doc_log = "The hashes in $(docv) defines keys that are in use. Repeatable."
let log_files_pos0 =
  Arg.(pos_all B0_std_cli.filepath [] & info [] ~doc:doc_log ~docv:"LOG_FILE")

let required_log_files_pos0 = Arg.(non_empty & log_files_pos0)
let optional_log_files_pos0 = Arg.(value & log_files_pos0)

(* Commands *)

let delete_cmd =
  let doc = "Delete cache or given keys" in
  let man = man [] in
  Cmd.make (Cmd.info "delete" ~doc ~exits ~man) @@
  let+ set_log_level and+ b0_dir and+ cache_dir and+ kind = key_kind
  and+ keys = B0_memo_cli.File_cache.keys_none_is_all ()
  and+ log_files =
    Arg.(value & opt_all B0_std_cli.filepath [] &
         info ["l";"log-file"] ~doc:doc_log)
  in
  delete ~b0_dir ~cache_dir ~kind ~keys ~log_files

let gc_cmd =
  let doc = "Delete unused cache keys" in
  let man = man [
      `S Manpage.s_description;
      `P "$(cmd) trims the cache by keeping only the keys that \
          are mentioned in the given log files. This is \
          the same as $(cmd.parent) $(b,delete --unused) with the \
          given log files passed as $(b,-l) options.";
      `P "Use command $(tool) $(b,trim) to trim down to a budget."; ]
  in
  Cmd.make (Cmd.info "gc" ~doc ~exits ~man) @@
  let+ set_log_level and+ b0_dir and+ cache_dir and+ dry_run
  and+ log_files = required_log_files_pos0 in
  gc ~dry_run ~b0_dir ~cache_dir ~log_files

let keys_cmd =
  let doc = "List cache keys" in
  let man = man [`P "$(cmd) lists cache keys." ] in
  Cmd.make (Cmd.info "keys" ~doc ~exits ~man) @@
  let+ set_log_level and+ b0_dir and+ cache_dir
  and+ log_files = optional_log_files_pos0 and+ kind = key_kind in
  keys ~b0_dir ~cache_dir ~kind ~log_files

let path_cmd =
  let doc = "Output cache directory path (may not exist)" in
  let man = [`P "$(cmd) outputs the cache directory path."] in
  Cmd.make (Cmd.info "path" ~doc ~exits ~man) @@
  let+ set_log_level and+ b0_dir and+ cache_dir in
  path ~b0_dir ~cache_dir

let stats_cmd, stats_term =
  let doc = "Output cache statistics" in
  let man = man [
      `S Manpage.s_description;
      `P "$(cmd) outputs cache statistics. The given log files are used to \
          determine used keys." ]
  in
  let term =
    let+ set_log_level and+ b0_dir and+ cache_dir
    and+ log_files = optional_log_files_pos0 in
    stats ~b0_dir ~cache_dir ~log_files
  in
  Cmd.make (Cmd.info "stats" ~doc ~exits ~man) term, term

let trim_cmd =
  let doc = "Trim the cache to the minimal budget specified" in
  let man = man [
      `S Manpage.s_description;
      `P "$(cmd) trims the cache to the minimal given budget. Without \
          options trims to 50% of the current size. Keys used by the given \
          log files are preserved whenever possible.";
      `P "Use $(tool) $(b,gc) to keep only the keys mentioned in the log \
          files." ]
  in
  Cmd.make (Cmd.info "trim" ~doc ~exits ~man) @@
  let+ set_log_level and+ b0_dir and+ cache_dir and+ dry_run
  and+ log_files = optional_log_files_pos0
  and+ trim_spec = B0_memo_cli.File_cache.trim_cli () in
  trim ~dry_run ~b0_dir ~cache_dir ~log_files ~trim_spec

let tool =
  let doc = "Operate on b0 caches" in
  let man_xrefs = [`Tool "b0"; `Tool "b0-log"; `Tool "b0-hash"] in
  let man = man [
    `S Manpage.s_description;
    `P "$(cmd) operate on b0 caches.";
    `P "In general, if it provides one, it's better to use a b0 based \
        tool's own cache command like $(b,b0 cache). It has better \
        automated heuristics about which keys are in use. With this tool \
        you need to provide explicit build log files.";
    `Pre "$(cmd) $(b,path)     # Output path of inferred cache directory";
    `Noblank;
    `Pre "$(cmd) $(b,stats)    # Output cache statistics \
          (usage needs a log file)";
    `Noblank;
    `Pre "$(cmd) $(b,trim)     # Trim cache by 50%%";
    `P "By default the cache \
        directory is determined like $(b,b0) does. \
        Invoke $(cmd) $(b,path --help) for details.";
  ]
  in
  let version = "%%VERSION%%" in
  let info = Cmd.info "b0-cache" ~version ~doc ~exits ~man ~man_xrefs in
  Cmd.group info @@
  [delete_cmd; gc_cmd; keys_cmd; path_cmd; stats_cmd; trim_cmd ]

let main () = Cmd.eval_result' tool
let () = if !Sys.interactive then () else exit (main ())
