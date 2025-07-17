(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let find_used_keys ~conf =
  (* Looks up keys used by the current build and the driver build *)
  Result.map_error (Fmt.str "Cannot determine used keys: %s") @@
  let b0_dir = B0_driver.Conf.b0_dir conf in
  let* build_dir = B0_build.B0_dir.default_build_dir ~b0_dir in
  let build_log = B0_build.B0_dir.log_file ~build_dir in
  let driver_log = B0_driver.Compile.build_log conf ~driver:B0_tool.driver in
  let add_log log acc =
    let* ops = Result.map B0_memo_log.ops (B0_memo_log.read log) in
    Ok (B0_memo_cli.File_cache.keys_of_success_ops ~init:acc ops)
  in
  List.fold_stop_on_error add_log [build_log; driver_log] String.Set.empty

let try_find_used_keys ?kind ~conf () = match kind with
| Some `Any -> String.Set.empty
| None | Some (`Used | `Unused)  ->
    Log.if_error ~level:Log.Warning ~use:String.Set.empty @@
    find_used_keys ~conf

let delete ~keys ~kind conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let dir = B0_driver.Conf.cache_dir conf in
  let used = try_find_used_keys ~kind ~conf () in
  let* _exists = B0_memo_cli.File_cache.delete ~dir ~used ~kind keys in
  Ok Os.Exit.ok

let gc ~dry_run conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let dir = B0_driver.Conf.cache_dir conf in
  let* used = find_used_keys ~conf in
  let* _exists = B0_memo_cli.File_cache.gc ~dry_run ~dir ~used in
  Ok Os.Exit.ok

let keys ~kind conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let dir = B0_driver.Conf.cache_dir conf in
  let used = try_find_used_keys ~kind ~conf () in
  let* _exists = B0_memo_cli.File_cache.keys ~dir ~used ~kind in
  Ok Os.Exit.ok

let path conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let dir = B0_driver.Conf.cache_dir conf in
  Fmt.pr "%a@." Fpath.pp_unquoted dir;
  Ok Os.Exit.ok

let stats conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let dir = B0_driver.Conf.cache_dir conf in
  let used = try_find_used_keys ~conf () in
  let* _exists = B0_memo_cli.File_cache.stats ~dir ~used in
  Ok Os.Exit.ok

let trim ~dry_run ~trim_spec:(max_byte_size, pct) conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let dir = B0_driver.Conf.cache_dir conf in
  let used = try_find_used_keys ~conf () in
  let* _exists =
    B0_memo_cli.File_cache.trim ~dry_run ~dir ~used ~max_byte_size ~pct
  in
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

(* Commands *)

let dry_run = B0_memo_cli.File_cache.dry_run ()
let kind = B0_memo_cli.File_cache.key_kind_cli ()

let delete_cmd =
  let doc = "Delete cache or given keys" in
  let descr =
    `P "$(cmd) deletes the cache or the given keys. Use \
        $(cmd.parent) $(b,keys) to list them.";
  in
  B0_tool_cli.cmd_with_driver_conf "delete" ~doc ~descr @@
  let+ keys = B0_memo_cli.File_cache.keys_none_is_all () and+ kind in
  delete ~keys ~kind

let gc_cmd =
  let doc = "Only keep keys used by the build" in
  let descr = `Blocks [
      `P "$(cmd) deletes all keys except those used by the build. This is \
          the same as $(cmd.parent) $(b,delete --unused).";
      `P "Use $(cmd.parent) $(b,trim) to trim down to a size budget."; ]
  in
  B0_tool_cli.cmd_with_driver_conf "gc" ~doc ~descr @@
  let+ dry_run in
  gc ~dry_run

let keys_cmd =
  let doc = "List cache keys" in
  let descr = `P "$(cmd) lists cache keys." in
  B0_tool_cli.cmd_with_driver_conf "keys" ~doc ~descr @@
  let+ kind in
  keys ~kind

let path_cmd =
  let doc = "Output cache directory path (may not exist)" in
  let descr = `P "$(cmd) outputs the cache directory path." in
  B0_tool_cli.cmd_with_driver_conf "path" ~doc ~descr @@
  Term.const path

let stats_cmd =
  let doc = "Output cache statistics" in
  let descr =
    `P "$(cmd) outputs cache statistics. The numbers reported as 'used' are \
        for the keys used by build.";
  in
  B0_tool_cli.cmd_with_driver_conf "stats" ~doc ~descr @@
  Term.const stats

let trim_cmd =
  let doc = "Trim the cache to a given size budget" in
  let descr = `Blocks [
      `P "$(cmd) trims the cache to the minimal given size budget. Without \
          options trims to 50% of the current size. Keys used \
          by the build are preserved whenever possible.";
      `P "Use $(tool) $(b,cache gc) to only keep the keys used \
          by the build." ]
  in
  B0_tool_cli.cmd_with_driver_conf "trim" ~doc ~descr @@
  let+ trim_spec = B0_memo_cli.File_cache.trim_cli () and+ dry_run in
  trim ~dry_run ~trim_spec

let cmd =
  let doc = "Operate on the build cache" in
  let descr = `Blocks [
    `S Manpage.s_description;
    `P "$(cmd) operates on the build cache. A cache key is used by the \
        build if it corresponds to one of its operations. This is determined \
        by looking up build log files (including the build of the driver).";
    `Pre "$(cmd) $(b,stats)              # Output cache statistics";
    `Noblank;
    `Pre "$(cmd) $(b,gc)                 # Only keep keys used by the build";
    `Noblank;
    `Pre "$(cmd) $(b,trim)               # Trim cache by 50%";
    `Noblank;
    `Pre "$(cmd) $(b,trim --to-mb=100)   # Trim cache to 100MB";
  ]
  in
  B0_tool_cli.cmd_group "cache" ~doc ~descr @@
  [delete_cmd; gc_cmd; keys_cmd; path_cmd; stats_cmd; trim_cmd]
