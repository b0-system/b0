(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Execution environments.

    This is notably what actions get access to.

    {b Note.} At some point we need a build/host distinction here. *)

open B0_std

type t = B0_defs.b0_env
(** The type for execution environments. *)

val make :
  b0_dir:Fpath.t -> build:B0_build.t -> cwd:Fpath.t -> root_dir:Fpath.t ->
  scope_dir:Fpath.t -> driver_env:Os.Env.t -> t
(** [make ~cwd ~scope_dir ~root_dir ~action] is an execution context
    with given parameters. See corresponding accessors for semantics. *)

(** {1:dirs Directories}

    See also the {{!rel}relative file resolutions}. *)

val b0_dir : t -> Fpath.t
(** [b0_dir env] is the {{!page-manual.root_dir}b0 directory}. *)

val cwd : t -> Fpath.t
(** [cwd env] is the absolute path to the current working directory. *)

val root_dir : t -> Fpath.t
(** [root_dir env] is the {{!page-manual.root_dir}root directory}. *)

val scope_dir : t -> Fpath.t
(** [scope_dir env] is the absolute path to the directory of the B0 file
    in which the action is defined or the {{!root_dir}root
    directory} if the action is defined the global scope. *)

val scratch_dir : t -> Fpath.t
(** [scratch_dir env] is a shared scratch directory for actions in
    [b0_dir]. The directory must be created it may not exist, it's
    content may be destroyed at any time and actions are in charge of
    inventing a naming scheme to avoid collisions. *)

val unit_dir : t -> B0_defs.b0_unit -> Fpath.t
(** [unit_dir env u] is the build directory if [u] in the
    build [build env]. That is {!B0_build.unit_dir}[ (build env) u]. *)

type dir = [`Cwd | `Root_dir | `Scope_dir | `Unit_dir ]
(** The type for speciying directories. *)

val pp_dir : dir Fmt.t
(** [pp_dir] formats directory specification for ui purposes. *)

val dir : t -> dir -> Fpath.t
(** [dir env d] looks up [d] in [env], raises [Invalid_argument]
    on [`Unit_dir]. *)

(** {1:rel Relative file resolution} *)

val in_root_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(root_dir env // p)]. *)

val in_scope_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(scope_dir env // p)]. *)

val in_scratch_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(scratch_dir env // p)]. *)

val in_unit_dir : t -> B0_defs.b0_unit -> Fpath.t -> Fpath.t
(** [in_unit_dir env u p] is [Fpath.(unit_unit_dir env u // p)]. *)

val in_dir : t -> dir -> Fpath.t -> Fpath.t
(** [in_dir env d p] is [Fpath.(in_dir env d // p)]. *)

(** {1:env Process environments} *)

type env =
[ `Build_env (** Base build process environment. *)
| `Driver_env (** Environment of the [b0] invocation. *) ]
(** The type for environments. *)

val pp_env : env Fmt.t
(** [pp_env] formats environments. *)

val build_env : t -> Os.Env.t
(** [build_env env] is the base build process environment. *)

val driver_env : t -> Os.Env.t
(** [driver_env env] is the environment with which [b0] was invoked. *)

val env : t -> env -> Os.Env.t
(** [env env e] is the environment [e] of [env]. *)

(** {1:build Build} *)

val build : t -> B0_build.t
(** [build env] is the current build (if any). *)

(** {1:tool Tool lookup} *)

val get_cmd : ?skip_build:bool -> t -> Cmd.tool_search
(** [get_cmd env cmd] is [cmd] with its {!B0_std.Cmd.get_tool} resolved in
    the environment. If [cmd]'s tool is defined by a unit in the build
    and is {!B0_unit.tool_is_user_accessible}, it comes first in the
    search, unless [skip_build] is [true] (defaults to [false]. *)

val unit_exe_file : t -> B0_defs.b0_unit -> (Fpath.t, string) result
(** [unit_exe_file env u] looks up the {!B0_unit.exe_file}
    of [u] in [env]. This errors if [u] can't be found in the build
    of if [u] has no such key. *)

val unit_exe_file_cmd : t -> B0_defs.b0_unit -> (Cmd.t, string) result
(** [unit_exe_file env u] is [Result.map Cmd.path (unit_exe_file env u)]. *)
