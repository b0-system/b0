(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* B0_build.t *)

module rec Build_def : sig
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B0_memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_dir : Fpath.t;
      store : B0_store.t;
      must_build : Unit.Set.t;
      may_build : Unit.Set.t;
      mutable requested : Unit.t String.Map.t;
      mutable waiting : Unit.t B0_random_queue.t; }
end = struct
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B0_memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_dir : Fpath.t;
      store : B0_store.t;
      must_build : Unit.Set.t;
      may_build : Unit.Set.t;
      mutable requested : Unit.t String.Map.t;
      mutable waiting : Unit.t B0_random_queue.t; }
end

(* B0_unit.t *)

and Unit_def : sig
  type build_proc = Build_def.t -> unit Fut.t
  and t = { def : B0_def.t; build_proc : build_proc; }
  include B0_def.VALUE with type t := t
end = struct
  type build_proc = Build_def.t -> unit Fut.t
  and t = { def : B0_def.t; build_proc : build_proc; }
  let def_kind = "unit"
  let def u = u.def
  let pp_name_str = Fmt.code
end

and Unit : sig include B0_def.S with type t = Unit_def.t end
  = B0_def.Make (Unit_def)

type b0_unit = Unit.t
type b0_unit_set = Unit.Set.t
type b0_build = Build_def.t
type b0_env =
  { b0_dir : Fpath.t;
    build : b0_build;
    built_tools : b0_unit String.Map.t Lazy.t;
    cwd : Fpath.t;
    root_dir : Fpath.t;
    scope_dir : Fpath.t;
    build_env : Os.Env.t;
    driver_env : Os.Env.t; }

type build_proc = Unit_def.build_proc

let unit_build_proc u = u.Unit_def.build_proc
let unit_is_public u = match Unit.find_meta B0_meta.public u with
| None -> false | Some b -> b

let tool_is_user_accessible u = unit_is_public u || Unit.in_root_scope u

(* These keys are defined here so that B0_env.t can use them. *)

let () = B0_scope.open_lib ~module':__MODULE__ "unit"

let exe_file : Fpath.t Fut.t B0_meta.key =
  let doc = "Absolute file path to a built executable." in
  let pp_value = Fmt.any "<built value>" in
  B0_meta.Key.make "exe-file" ~doc ~pp_value

let tool_name =
  let doc = "Executable tool name without platform specific extension" in
  B0_meta.Key.make "tool-name" ~doc ~pp_value:Fmt.string

let () = B0_scope.close ()

(* We also maintain a tool name index for built tools.
   XXX this should be reviewed at some point. *)

let tool_name_index : Unit.t list String.Map.t ref = ref String.Map.empty

let add_tool_name u = match Unit.find_meta tool_name u with
| None -> ()
| Some n -> tool_name_index := String.Map.add_to_list n u !tool_name_index

let tool_name_map units =
  let warn_dup_tool use ign n =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a defined both by unit %a and %a.@,\
       Ignoring definition in unit %a.@]"
      Fmt.code n Unit.pp_name use
      Unit.pp_name ign Unit.pp_name ign
  in
  let warn_no_exe_file u n =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a defined by unit %a does not specify a@,\
       B0_meta.exe_file key. It will not be used in the build (if needed).@]"
      Fmt.code n Unit.pp_name u
  in
  let add_unit u acc =
    if not (tool_is_user_accessible u) then acc else
    match B0_meta.find tool_name (Unit.meta u) with
    | None -> acc
    | Some t ->
        match String.Map.find_opt t acc with
        | Some u' -> warn_dup_tool u u' t; acc
        | None ->
            if B0_meta.mem exe_file (Unit.meta u)
            then String.Map.add t u acc
            else (warn_no_exe_file u t; acc)
  in
  Unit.Set.fold add_unit units String.Map.empty

let unit_make ?doc ?meta n build_proc =
  let def = Unit.define ?doc ?meta n in
  let u = { Unit_def.def; build_proc } in
  Unit.add u; add_tool_name u; u
