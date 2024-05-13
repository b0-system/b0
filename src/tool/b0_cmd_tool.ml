(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let pp_tool ppf n = Fmt.tty [`Fg `Green] ppf n
let list format c =
  let pp, sep = match format with
  | `Short ->
      let pp_tool ppf (n, _) = Fmt.code ppf n in
      pp_tool, Fmt.cut
  | `Normal ->
      let pp_tool ppf (n, u) =
        if n = B0_unit.name u
        then Fmt.pf ppf "@[%a %s@]" pp_tool n (B0_unit.doc u) else
        Fmt.pf ppf "@[%a (%a) %s@]" pp_tool n B0_unit.pp_name u (B0_unit.doc u)
      in
      pp_tool, Fmt.cut
  | `Long ->
      let pp_tool ppf (n, u) = B0_unit.pp ppf u in
      pp_tool, Fmt.(cut ++ cut)
  in
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* us = B0_unit.get_list_or_hint ~all_if_empty:true [] in
  let keep_tool u = match B0_unit.find_meta B0_unit.tool_name u with
  | Some n when B0_unit.tool_is_user_accessible u -> Some (n, u)
  | None | Some _ -> None
  in
  let tools = List.sort compare (List.filter_map keep_tool us) in
  Log.if_error' ~use:B0_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  if tools <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) tools);
  Ok B0_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let list =
  let doc = "List buildable tools" in
  let descr = `P "$(iname) lists given buildable tools"; in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  Term.(const list $ B0_tool_std.Cli.format)

let cmd =
  let doc = "Operate on buildable tools" in
  let descr = `Blocks [
    `P "$(iname) operates on buildable tools.";
    `P "These are the public tools in the build and the non-public tools \
        in the root scope."; ]
  in
  B0_tool_std.Cli.cmd_group "tool" ~doc ~descr @@
  [list]
