(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let edit clets c =
  B0_tool.Def.edit (module B0_pack) c clets

let get format k clets c =
  B0_tool.Def.get_meta_key (module B0_pack) c format k clets

let list format clets c =
  B0_tool.Def.list (module B0_pack) c format clets

let show format cs c =
  let format = if format = `Normal then `Long else format in
  B0_tool.Def.list (module B0_pack) c format cs

(* Command line interface *)

open Cmdliner

let packs ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"PACK")

let packs_tail = packs ~right:0
let packs_all = packs ~right:(-1)

let edit =
  let doc = "Edit build packs" in
  let descr =
    `P "$(iname) opens in your editor the B0 files of given build packs \
        are defined."
  in
  let envs = B0_tool.Cli.editor_envs in
  B0_tool.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs @@
  Term.(const edit $ packs_all)

let get =
  let doc = "Get build pack metadata" in
  let descr =
    `P "$(iname) outputs the value of metadata $(i,KEY) of given build packs."
  in
  B0_tool.Cli.subcmd_with_b0_file "get" ~doc ~descr @@
  Term.(const get $ B0_tool.Cli.format $ B0_tool.Cli.pos_key $
        packs_tail)

let list =
  let doc = "List build packs" in
  let descr = `P "$(iname) lists given build packs." in
  B0_tool.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  Term.(const list $ B0_tool.Cli.format $ packs_all)

let show =
  let doc = "Show build pack metadata" in
  let descr =
    `P "$(iname) is $(b,list -l), it outputs metadata of given build packs."
  in
  B0_tool.Cli.subcmd_with_b0_file "show" ~doc ~descr @@
  Term.(const show $ B0_tool.Cli.format $ packs_all)

let cmd =
  let doc = "Operate on build packs" in
  let descr = `P "$(iname) operates on build packs." in
  B0_tool.Cli.cmd_group "pack" ~doc ~descr @@
  [edit; get; list; show]
