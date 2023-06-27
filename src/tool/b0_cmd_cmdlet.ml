(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let edit clets c = B0_tool_std.Def.edit (module B0_cmdlet) c clets
let get format k clets c =
  B0_tool_std.Def.get_meta_key (module B0_cmdlet) c format k clets

let list format clets c = B0_tool_std.Def.list (module B0_cmdlet) c format clets
let show format cs c =
  let format = if format = `Normal then `Long else format in
  B0_tool_std.Def.list (module B0_cmdlet) c format cs

(* Command line interface *)

open Cmdliner

let clets ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"CMDLET")

let clets_all = clets ~right:(-1)

let list_term = Term.(const list $ B0_tool_std.Cli.format $ clets_all)

(* Commands *)

let edit =
  let doc = "Edit cmdlets" in
  let descr = `P "$(tname) opens in your editor the B0 files of given \
                  cmdlets are defined." in
  let envs = B0_tool_std.Cli.editor_envs in
  let term = Term.(const edit $ clets_all) in
  B0_tool_std.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs term

let get =
  let doc = "Get cmdlet metadata" in
  let descr = `P "$(tname) outputs the value of metadata $(i,KEY) of given \
                  cmdlets."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  let clets = clets ~right:0 in
  let term =
    Term.(const get $ B0_tool_std.Cli.format $ B0_tool_std.Cli.pos_key $ clets)
  in
  B0_tool_std.Cli.subcmd_with_b0_file "get" ~doc ~descr ~envs term

let list =
  let doc = "List cmdlets (default command)" in
  let descr = `P "$(tname) lists given cmdlets." in
  let envs = B0_tool_std.Cli.pager_envs in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr ~envs list_term

let show =
  let doc = "Show cmdlet metadata." in
  let descr = `P "$(tname) is $(b,list -l), it outputs metadata of given \
                  cmdlets."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  let term = Term.(const show $ B0_tool_std.Cli.format $ clets_all) in
  B0_tool_std.Cli.subcmd_with_b0_file "show" ~doc ~descr ~envs term

let subs = [edit; get; list; show]

let cmd =
  let doc = "Operate on cmdlets" in
  let descr = `P "$(tname) operates on cmdlets. The default command is \
                  $(tname) $(b,list)."
  in
  let envs = B0_tool_std.Cli.pager_envs and default = list_term in
  B0_tool_std.Cli.cmd_group_with_b0_file
    "cmdlet" ~doc ~descr ~envs ~default subs
