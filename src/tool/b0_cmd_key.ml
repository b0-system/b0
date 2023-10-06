(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let list format c =
  let pp, sep = match format with
  | `Short ->
      let pp_key ppf k = B0_meta.Key.pp ppf k in
      pp_key, Fmt.cut
  | `Normal ->
      let pp_key ppf k = B0_meta.Key.pp ppf k in
      pp_key, Fmt.cut
  | `Long ->
      let pp_key ppf k = B0_meta.Key.pp ppf k in
      pp_key, Fmt.(cut ++ cut)
  in
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* keys = B0_meta.Key.get_list_or_hint ~all_if_empty:true [] in
  Log.if_error' ~use:B0_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  if keys <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) keys);
  Ok B0_cli.Exit.ok



(* Command line interface *)

open Cmdliner

let list =
  let doc = "List keys" in
  let descr =
    `P "$(iname) lists keys."
  in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  Term.(const list $ B0_tool_std.Cli.format)

let cmd =
  let doc = "Operate on keys" in
  let descr = `P "$(iname) operates on keys." in
  B0_tool_std.Cli.cmd_group "key" ~doc ~descr @@
  [list]
