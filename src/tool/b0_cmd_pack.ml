(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let edit ~packs conf =
  B0_tool.Def.edit (module B0_pack) conf packs

let get ~output_verbosity ~key ~packs conf =
  B0_tool.Def.get_meta_key (module B0_pack) conf output_verbosity key packs

let list ~output_verbosity ~packs conf =
  B0_tool.Def.list (module B0_pack) conf output_verbosity packs

let show ~output_verbosity ~packs conf =
  let format = if output_verbosity = `Normal then `Long else output_verbosity in
  B0_tool.Def.list (module B0_pack) conf format packs

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let packs ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"PACK")

let packs_tail = packs ~right:0
let packs_all = packs ~right:(-1)

let edit =
  let doc = "Edit build packs" in
  let descr =
    `P "$(cmd) opens in your editor the b0 files of given build packs \
        are defined."
  in
  let envs = B0_tool.Cli.editor_envs in
  B0_tool.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs @@
  let+ packs = packs_all in
  edit ~packs

let get =
  let doc = "Get build pack metadata" in
  let descr =
    `P "$(cmd) outputs the value of metadata $(i,KEY) of given build packs."
  in
  B0_tool.Cli.subcmd_with_b0_file "get" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity
  and+ key = B0_tool.Cli.pos_key
  and+ packs = packs_tail in
  get ~output_verbosity ~key ~packs

let list =
  let doc = "List build packs" in
  let descr = `P "$(cmd) lists given build packs." in
  B0_tool.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity and+ packs = packs_all in
  list ~output_verbosity ~packs

let show =
  let doc = "Show build pack metadata" in
  let descr =
    `P "$(cmd) is $(b,list -l), it outputs metadata of given build packs."
  in
  B0_tool.Cli.subcmd_with_b0_file "show" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity
  and+ packs = packs_all in
  show ~output_verbosity ~packs

let cmd =
  let doc = "Operate on build packs" in
  let descr = `P "$(cmd) operates on build packs." in
  B0_tool.Cli.cmd_group "pack" ~doc ~descr @@
  [edit; get; list; show]
