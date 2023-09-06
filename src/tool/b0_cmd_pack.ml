(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let edit clets c = B0_tool_std.Def.edit (module B0_pack) c clets
let get format k clets c =
  B0_tool_std.Def.get_meta_key (module B0_pack) c format k clets

let list format clets c = B0_tool_std.Def.list (module B0_pack) c format clets
let show format cs c =
  let format = if format = `Normal then `Long else format in
  B0_tool_std.Def.list (module B0_pack) c format cs

(* Command line interface *)

open Cmdliner

let packs ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"CMDLET")

let packs_all = packs ~right:(-1)

let list_term = Term.(const list $ B0_tool_std.Cli.format $ packs_all)

(* Commands *)

let edit =
  let doc = "Edit build packs" in
  let descr = `P "$(iname) opens in your editor the B0 files of given \
                  build packs are defined." in
  let envs = B0_tool_std.Cli.editor_envs in
  B0_tool_std.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs @@
  Term.(const edit $ packs_all)

let get =
  let doc = "Get build pack metadata" in
  let descr = `P "$(iname) outputs the value of metadata $(i,KEY) of given \
                  build packs."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  let packs = packs ~right:0 in
  B0_tool_std.Cli.subcmd_with_b0_file "get" ~doc ~descr ~envs @@
  Term.(const get $ B0_tool_std.Cli.format $ B0_tool_std.Cli.pos_key $ packs)

let list =
  let doc = "List build packs (default command)" in
  let descr = `P "$(iname) lists given build packs." in
  let envs = B0_tool_std.Cli.pager_envs in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr ~envs list_term

let show =
  let doc = "Show build pack metadata." in
  let descr = `P "$(iname) is $(b,list -l), it outputs metadata of given \
                  build packs."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  B0_tool_std.Cli.subcmd_with_b0_file "show" ~doc ~descr ~envs @@
  Term.(const show $ B0_tool_std.Cli.format $ packs_all)

let subs = [edit; get; list; show]

let cmd =
  let doc = "Operate on build packs" in
  let descr = `P "$(iname) operates on build packs. The default command is \
                  $(iname) $(b,list)."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  B0_tool_std.Cli.cmd_group_with_b0_file "pack" ~doc ~descr ~envs subs
