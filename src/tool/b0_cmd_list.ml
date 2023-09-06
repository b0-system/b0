(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let list format all c =
  (* XXX improve that when cmdlets and outcome actions are merged *)
  let list (module Def : B0_def.S) c format ds =
    let pp, sep = match format with
    | `Short -> Def.pp_name, Fmt.cut
    | `Normal -> Def.pp_synopsis, Fmt.cut
    | `Long -> Def.pp, Fmt.(cut ++ cut)
    in
    let* ds = Def.get_list_or_hint ~empty_means_all:true ds in
    let not_lib d = not (String.starts_with ~prefix:"." (Def.name d)) in
    let ds = if all then ds else List.filter not_lib ds in
    let don't = B0_driver.Conf.no_pager c in
    let* pager = B0_pager.find ~don't () in
    let* () = B0_pager.page_stdout pager in
    if ds <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) ds);
    Ok ()
  in
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* () = list (module B0_unit) c format [] in
  let* () = list (module B0_cmdlet) c format [] in
  Ok B0_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show b0 definitions" in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(iname) shows b0 definitions. By default library definitions are \
        not shown, invoke with $(b,--all) include them.";
    B0_tool_std.Cli.man_see_manual;
  ]
  in
  let all =
    let doc = "Show all definitions not only user definitions." in
    Arg.(value & flag & info ["a"; "all"] ~doc)
  in
  let exits = B0_driver.Exit.infos in
  let envs = B0_tool_std.Cli.pager_envs in
  Cmd.v (Cmd.info "list" ~doc ~exits ~envs ~man) @@
  B0_driver.with_b0_file ~driver:B0_tool_std.driver @@
  Term.(const list $ B0_tool_std.Cli.format $ all)
