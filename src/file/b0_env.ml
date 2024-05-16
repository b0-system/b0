(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* The ocamldep invocation in boot/strap which only runs on [.ml] files
   misses that. *)

type t = B0_defs.b0_env

let make ~b0_dir ~build ~cwd ~root_dir ~scope_dir ~driver_env =
  let build_env = B0_memo.env (B0_build.memo build) in
  let built_tools = lazy begin
    (* FIXME it would be nice to have a check that the build
         finished here. *)
    B0_defs.tool_name_map (B0_build.did_build build)
  end
  in
  B0_defs.{ b0_dir; build; built_tools; cwd; root_dir; scope_dir; build_env;
            driver_env; }

(* Directories. *)

let b0_dir env = env.B0_defs.b0_dir
let cwd env = env.B0_defs.cwd
let root_dir env = env.B0_defs.root_dir
let scope_dir env = env.B0_defs.scope_dir
let scratch_dir env = B0_dir.scratch_dir ~b0_dir:env.B0_defs.b0_dir
let unit_dir env u = B0_build.unit_dir env.B0_defs.build u

let in_root_dir env p = Fpath.(root_dir env // p)
let in_scope_dir env p = Fpath.(scope_dir env // p)
let in_scratch_dir env p = Fpath.(scratch_dir env // p)
let in_unit_dir env u p = Fpath.(unit_dir env u // p)
let build env = env.B0_defs.build

type dir = [`Cwd | `Root_dir | `Scope_dir | `Unit_dir ]

let pp_dir ppf = function
| `Cwd -> Fmt.string ppf "current working directory"
| `Scope_dir -> Fmt.string ppf "scope directory"
| `Unit_dir -> Fmt.string ppf "unit directory"
| `Root_dir -> Fmt.string ppf "root directory"

let dir env = function
| `Cwd -> cwd env
| `Root_dir -> root_dir env
| `Scope_dir -> scope_dir env
| `Unit_dir -> invalid_arg "Cannot lookup `Unit_dir"

let in_dir env d p = Fpath.(dir env d // p)

(* Process environments. *)

type env = [ `Build_env | `Driver_env ]
let build_env env = env.B0_defs.build_env
let driver_env env = env.B0_defs.driver_env
let env env = function
| `Build_env -> env.B0_defs.build_env
| `Driver_env -> env.B0_defs.driver_env

let pp_env ppf = function
| `Build_env -> Fmt.string ppf "build environment"
| `Driver_env -> Fmt.string ppf "b0 invocation environment"

(* Tool lookup *)

let get_cmd ?(skip_build = false) env cmd =
  if skip_build then Os.Cmd.get cmd else
  let tool_map = Lazy.force env.B0_defs.built_tools in
  match Cmd.find_tool cmd with
  | None -> Fmt.error "No tool to lookup: the command is empty"
  | Some tool ->
      match String.Map.find_opt (Fpath.to_string tool) tool_map with
      | None -> Os.Cmd.get cmd
      | Some u ->
          Result.map (fun v -> Cmd.path (Fut.sync v))
            (B0_defs.Unit.get_meta B0_defs.exe_file u)

let unit_exe_file env u =
  if B0_defs.Unit.Set.mem u (B0_build.did_build env.B0_defs.build)
  then Result.map Fut.sync (B0_defs.Unit.get_meta B0_defs.exe_file u) else
  Fmt.error "Cannot get executable of unit %a: it did not build."
    B0_defs.Unit.pp_name u

let unit_exe_file_cmd env u = Result.map Cmd.path (unit_exe_file env u)
