(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let dir ~units conf =
  Log.if_error ~use:Os.Exit.no_such_name @@
  (* XXX Eventually we should use B0_env here. *)
  let* units = B0_unit.get_list_or_hint ~all_if_empty:true units in
  let b0_dir = B0_driver.Conf.b0_dir conf in
  let build_dir =
    B0_build.B0_dir.build_dir ~b0_dir ~variant:"user" (* FIXME *)
  in
  let unit_dir unit =
    B0_build.B0_dir.unit_build_dir ~build_dir ~name:(B0_unit.name unit)
  in
  let dirs = List.map unit_dir units in
  Log.stdout (fun m -> m "@[<v>%a@]" (Fmt.list Fpath.pp) dirs);
  Ok Os.Exit.ok

let edit ~units conf =
  B0_tool.Def.edit (module B0_unit) conf units

let get ~output_verbosity ~key ~units conf =
  B0_tool.Def.get_meta_key (module B0_unit) conf output_verbosity key units

let list ~output_verbosity ~units conf =
  B0_tool.Def.list (module B0_unit) conf output_verbosity units

let show ~output_verbosity ~units conf =
  let output_verbosity =
    if output_verbosity = `Normal then `Long else output_verbosity
  in
  B0_tool.Def.list (module B0_unit) conf output_verbosity units

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let units ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"UNIT")

let units_tail = units ~right:0
let units_all = units ~right:(-1)

let build_dir =
  let doc = "Output build directories of units" in
  let descr =
    `P "$(cmd) outputs build directories of given build units. The paths may \
        not exist."
  in
  B0_tool.Cli.subcmd_with_b0_file "dir" ~doc ~descr @@
  let+ units = units_all in
  dir ~units

let edit =
  let doc = "Edit build units" in
  let descr =
    `P "$(cmd) opens in your editor the b0 files in which given build units \
        are defined."
  in
  B0_tool.Cli.subcmd_with_b0_file "edit" ~doc ~descr @@
  let+ units = units_all in
  edit ~units

let get =
  let doc = "Get build unit metadata" in
  let descr =
    `P "$(cmd) outputs the value of metadata $(i,KEY) of given build units."
  in
  B0_tool.Cli.subcmd_with_b0_file "get" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity
  and+ key = B0_tool.Cli.pos_key
  and+ units = units_tail in
  get ~output_verbosity ~key ~units

let list =
  let doc = "List build units" in
  let descr = `P "$(cmd) lists given build units." in
  B0_tool.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity and+ units = units_all in
  list ~output_verbosity ~units

let show =
  let doc = "Show build unit metadata" in
  let descr =
    `P "$(cmd) is $(b,list -l), it outputs metadata of given build units."
  in
  B0_tool.Cli.subcmd_with_b0_file "show" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity and+ units = units_all in
  show ~output_verbosity ~units

let cmd =
  let doc = "Operate on build units" in
  let descr = `P "$(cmd) operates on build units." in
  B0_tool.Cli.cmd_group "unit" ~doc ~descr @@
  [build_dir; edit; get; list; show]
