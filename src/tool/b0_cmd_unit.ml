(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let output_dirs ~units conf =
  Log.if_error ~use:Os.Exit.no_such_name @@
  (* XXX Eventually we should use B0_env here. *)
  let* units = B0_unit.get_list_or_hint ~all_if_empty:true units in
  let b0_dir = B0_driver.Conf.b0_dir conf in
  let build_dir =
    B0_build.B0_dir.build_dir ~b0_dir ~variant:"user" (* FIXME *)
  in
  let unit_dir unit =
    Fpath.strip_trailing_dir_sep @@
    B0_build.B0_dir.unit_build_dir ~build_dir ~name:(B0_unit.name unit)
  in
  let dirs = List.map unit_dir units in
  Fmt.pr "@[<v>%a@]@." (Fmt.list Fpath.pp) dirs;
  Ok Os.Exit.ok

let edit ~units conf =
  B0_tool.Def.edit (module B0_unit) conf units

let get ~output_details ~key ~units conf =
  B0_tool.Def.get_meta_key (module B0_unit) conf output_details key units

let list ~output_details ~units conf =
  B0_tool.Def.list (module B0_unit) conf output_details units

let show ~output_details ~units conf =
  let output_details =
    if output_details = `Normal then `Long else output_details
  in
  B0_tool.Def.list (module B0_unit) conf output_details units

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let build_dir =
  let doc = "Output build directories of units" in
  let descr =
    `P "$(cmd) outputs build directories of given build units. The paths may \
        not exist. See also $(tool) $(b,dir)."
  in
  B0_tool.Cli.subcmd_with_b0_file "dir" ~doc ~descr @@
  let+ units = B0_tool.Cli.units_pos0 in
  output_dirs ~units

let edit =
  let doc = "Edit build units" in
  let descr =
    `P "$(cmd) opens in your editor the b0 files in which given build units \
        are defined."
  in
  B0_tool.Cli.subcmd_with_b0_file "edit" ~doc ~descr @@
  let+ units = B0_tool.Cli.units_pos0 in
  edit ~units

let get =
  let doc = "Get build unit metadata" in
  let descr =
    `P "$(cmd) outputs the value of metadata $(i,KEY) of given build units."
  in
  B0_tool.Cli.subcmd_with_b0_file "get" ~doc ~descr @@
  let+ output_details = B0_tool.Cli.output_details
  and+ key = B0_tool.Cli.required_key_pos0
  and+ units = B0_tool.Cli.units_pos1 in
  get ~output_details ~key ~units

let list =
  let doc = "List build units" in
  let descr = `P "$(cmd) lists given build units." in
  B0_tool.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  let+ output_details = B0_tool.Cli.output_details
  and+ units = B0_tool.Cli.units_pos0 in
  list ~output_details ~units

let show =
  let doc = "Show build unit metadata" in
  let descr =
    `P "$(cmd) is $(b,list -l), it outputs metadata of given build units."
  in
  B0_tool.Cli.subcmd_with_b0_file "show" ~doc ~descr @@
  let+ output_details = B0_tool.Cli.output_details
  and+ units = B0_tool.Cli.units_pos0 in
  show ~output_details ~units

let cmd =
  let doc = "Operate on build units" in
  let descr = `P "$(cmd) operates on build units." in
  B0_tool.Cli.cmd_group "unit" ~doc ~descr @@
  [build_dir; edit; get; list; show]
