(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let edit ~packs conf =
  B0_tool.Def.edit (module B0_pack) conf packs

let get ~output_details ~key ~packs conf =
  B0_tool.Def.get_meta_key (module B0_pack) conf output_details key packs

let list ~output_details ~packs conf =
  B0_tool.Def.list (module B0_pack) conf output_details packs

let info ~output_details ~packs conf =
  let format = if output_details = `Normal then `Long else output_details in
  B0_tool.Def.list (module B0_pack) conf format packs

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let edit_cmd =
  let doc = "Edit build packs" in
  let descr =
    `P "$(cmd) opens in your editor the b0 files of given build packs \
        are defined."
  in
  let envs = B0_editor.Env.infos in
  B0_tool_cli.cmd_with_b0_file "edit" ~doc ~descr ~envs @@
  let+ packs = B0_cli.act_on_packs_pos0 in
  edit ~packs

let get_cmd =
  let doc = "Get build pack metadata" in
  let descr =
    `P "$(cmd) outputs the value of metadata $(i,KEY) of given build packs."
  in
  B0_tool_cli.cmd_with_b0_file "get" ~doc ~descr @@
  let+ output_details = B0_cli.output_details
  and+ key = B0_cli.required_metadata_key_pos0
  and+ packs = B0_cli.act_on_packs_pos1 in
  get ~output_details ~key ~packs

let list_cmd =
  let doc = "List build packs" in
  let descr = `P "$(cmd) lists given build packs." in
  B0_tool_cli.cmd_with_b0_file "list" ~doc ~descr @@
  let+ output_details = B0_cli.output_details
  and+ packs = B0_cli.act_on_packs_pos0 in
  list ~output_details ~packs

let info_cmd =
  let doc = "Output build pack metadata" in
  let descr =
    `P "$(cmd) is $(b,list -l), it outputs metadata of given build packs."
  in
  B0_tool_cli.cmd_with_b0_file "info" ~doc ~descr @@
  let+ output_details = B0_cli.output_details
  and+ packs = B0_cli.act_on_packs_pos0 in
  info ~output_details ~packs

let cmd =
  let doc = "Operate on build packs" in
  let descr = `P "$(cmd) operates on build packs." in
  B0_tool_cli.cmd_group "pack" ~doc ~descr @@
  [edit_cmd; get_cmd; list_cmd; info_cmd]
