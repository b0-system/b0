(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type t =
  { b0_dir : Fpath.t;
    build : B0_build.t;
    cwd : Fpath.t;
    root_dir : Fpath.t;
    scope_dir : Fpath.t }

let make ~b0_dir ~build ~cwd ~root_dir ~scope_dir =
  { b0_dir; build; cwd; root_dir; scope_dir; }

let b0_dir env = env.b0_dir
let cwd env = env.cwd
let root_dir env = env.root_dir
let scope_dir env = env.scope_dir
let scratch_dir env = B0_dir.scratch_dir ~b0_dir:env.b0_dir
let unit_dir env u = B0_build.build_dir env.build u

let in_root_dir env p = Fpath.(root_dir env // p)
let in_scope_dir env p = Fpath.(scope_dir env // p)
let in_scratch_dir env p = Fpath.(scratch_dir env // p)
let in_unit_dir env u p = Fpath.(unit_dir env u // p)

let build env = env.build

(* TODO lookup the builds *)

let get_tool ?(no_build = false) env cmd = Os.Cmd.get_tool cmd
let get_cmd ?(no_build = false) env cmd = Os.Cmd.get cmd

let unit_file_exe env u =
  if B0_unit.Set.mem u (B0_build.did_build env.build)
  then Result.map Fut.sync (B0_unit.get_meta B0_unit.exe_file u) else
  Fmt.error "Cannot get executable of unit %a: it did not build."
    B0_unit.pp_name u

let unit_cmd env u = Result.map Cmd.path (unit_file_exe env u)
