(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let list ~output_details conf =
  let pp, sep = match output_details with
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
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* keys = B0_meta.Key.get_list_or_hint ~all_if_empty:true [] in
  Log.if_error' ~use:Os.Exit.some_error @@
  let no_pager = B0_driver.Conf.no_pager conf in
  let* pager = B0_pager.find ~no_pager () in
  let* () = B0_pager.page_stdout pager in
  if keys <> []
  then Log.stdout (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) keys);
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let list =
  let doc = "List keys" in
  let descr = `P "$(cmd) lists keys." in
  B0_tool_cli.cmd_with_b0_file "list" ~doc ~descr @@
  let+ output_details = B0_cli.output_details in
  list ~output_details

let cmd =
  let doc = "Operate on keys" in
  let descr = `P "$(cmd) operates on keys." in
  B0_tool_cli.cmd_group "key" ~doc ~descr @@
  [list]
