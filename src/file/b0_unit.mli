(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Build units.

    A build unit is a named build procedure with metadata associated
    to it. Build units are the smallest unit of build in B0. *)

open B0_std

(** {1:proc Build procedures} *)

type build
(** The type for builds, see {!B0_build}. *)

type build_proc = build -> unit Fut.t
(** The type for unit build procedures. Note that when the future
    determines the build may not be finished. *)

val build_nop : build_proc
(** [build_nop] does nothing. *)

(** {1:units Units} *)

type t
(** The type for build units. *)

val make : ?doc:string -> ?meta:B0_meta.t -> string -> build_proc -> t
(** [make n proc ~doc ~meta ~action] is a build unit named [n] with build
    procedure [proc], synopsis [doc] and metada [meta]. *)

val build_proc : t -> build_proc
(** [proc u] are the unit's build procedure. *)

(** {1:meta Unit executions}

    These properties pertain to the interface that allows to execute
    units after they have been built as is done for example by the
    [b0] command.

    Execution procedure, will likely be refined when we get proper
    build environments and cross.

    {ul
    {- If the unit's {!exec} is defined this is invoked with
       the result of {!get_exec_env} and {!get_exec_cwd}.}
    {- Otherwise {!exe_file} is executed with cwd and environemts
       defined y {!get_exec_env} and {!get_exec_cwd}}} *)

type env
(** The type for execution environments, see {!B0_env}. *)

(** {2:env Environment} *)

type exec_env =
[ `Build_env (** The build's environment. *)
| `Build_env_override of Os.Env.t
   (** The build's environment overriden by give values. *)
| `Custom_env of string * (env -> t -> Os.Env.t Fut.t)
   (** Doc string and function. *)
| `Env of Os.Env.t (** This exact environment. *) ]
(** The type for execution environments. *)

val exec_env : exec_env B0_meta.key
(** [exec_env] is the default execution environment. If unspecified this
    is [`Build_env].  *)

val get_exec_env : env -> t -> Os.Env.t option Fut.t
(** [get_exec_env b u] performs the logic to get the execution environment. *)

(** {2:cwd Cwd} *)

type exec_cwd =
[ `Build_dir (** The entity's build directory. *)
| `Cwd (** The user's current working directory. *)
| `Custom_dir of string * (env -> t -> Fpath.t Fut.t)
   (** Doc string and function. *)
| `In of [ `Build_dir | `Root_dir | `Scope_dir ] * Fpath.t
| `Root_dir (** The root B0 file directory. *)
| `Scope_dir (** The directory of the scope where the entity is defined. *) ]
(** The type for specifying execution's current working directory. *)

val exec_cwd : exec_cwd B0_meta.key
(** [exec_cwd] is the default current working directory for executing
    an entity. If unspecified this should be [`Cwd] which should be
    the users's current working directory. *)

val get_exec_cwd : env -> t -> Fpath.t option Fut.t
(** [get_exec_cwd b u] performs the logic to get the cwd. *)

(** {2:execution Execution} *)

val tool_name : string B0_meta.key
(** [tool_name] is an executable name without the platform specific
    executable extension. *)

val exe_file : Fpath.t Fut.t B0_meta.key
(** [exe_file] is an absolute file path to an executable build by the unit. *)

val exec :
  (string *
   (env -> ?env:Os.Env.t -> ?cwd:Fpath.t -> t -> args:Cmd.t ->
    Os.Exit.t Fut.t)) B0_meta.key
(** [exec] is a metadata key to store a custom execution procedure.
    The string is a doc string for the procedure.
    The function is given the build, the result of {!get_exec_env} and
    {!get_exec_cwd} and the arguments to give to the procedure. *)

val find_exec : t -> (env -> t -> args:Cmd.t -> Os.Exit.t Fut.t) option
(** [find_exec b u] if either {!val-exe_file} or {!val-exec} is defined
    returns a function to call that  determines
    {!get_exec_env} and {!get_exec_cwd} and runs {!val-exec} or
    {!B0_std.Os.Exit.exec} with {!exe_file}. *)

val is_public : t -> bool
(** [is_public u] is [true] iff [u]'s meta has {!B0_meta.public}
    set to [true]. *)

val get_or_suggest_tool : keep:(t -> bool) -> string -> (t list, t list) result
(** [get_or_suggest_tool tool_name] are tools names whose
    {!tool_name} match [tool_name] and are filtered by [keep] *)

val tool_is_user_accessible : t -> bool
(** [tool_is_user_accessible u] assumes [u] has a tool name. This then
    returns [true] iff [u] {!is_public} or {!in_root_scope}. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t (** @inline *)

val tool_name_map : Set.t -> t String.Map.t
(** [tool_name_map units] are the user accessible tools defined by the
    set of units [units]. *)

(**/**)
module Build : sig
  type build_unit := t
  type t = build
  val memo : t -> B0_memo.t
  val must_build : t -> Set.t
  val may_build : t -> Set.t
  val did_build : t -> Set.t
  val require : t -> build_unit -> unit
  val current : t -> build_unit
  val current_meta : t -> B0_meta.t
  val current_scope_dir : t -> Fpath.t
  val current_build_dir : t -> Fpath.t
  val shared_build_dir : t -> Fpath.t
  val scope_dir : t -> build_unit -> Fpath.t
  val build_dir : t -> build_unit -> Fpath.t
  val in_build_dir : t -> Fpath.t -> Fpath.t
  val in_scope_dir : t -> Fpath.t -> Fpath.t
  val in_shared_build_dir : t -> Fpath.t -> Fpath.t
  val make :
    root_dir:Fpath.t -> b0_dir:Fpath.t -> variant:string ->
    store:B0_store.binding list -> B0_memo.t ->
    may_build:Set.t -> must_build:Set.t -> t

  val store : t -> B0_store.t
  val get : t -> 'a B0_store.key -> 'a Fut.t
  val self : t B0_store.key
  val run : t -> (unit, unit) result
  val did_build : t -> Set.t
end

module Env : sig
  type build_unit := t
  type t = env
  val make :
    b0_dir:Fpath.t ->
    build:Build.t ->
    cwd:Fpath.t ->
    root_dir:Fpath.t -> scope_dir:Fpath.t -> t
  val b0_dir : t -> Fpath.t
  val cwd : t -> Fpath.t
  val root_dir : t -> Fpath.t
  val scope_dir : t -> Fpath.t
  val scratch_dir : t -> Fpath.t
  val unit_dir : t -> build_unit -> Fpath.t
  val in_root_dir : t -> Fpath.t -> Fpath.t
  val in_scope_dir : t -> Fpath.t -> Fpath.t
  val in_scratch_dir : t -> Fpath.t -> Fpath.t
  val in_unit_dir : t -> build_unit -> Fpath.t -> Fpath.t
  val build : t -> Build.t
  val get_tool :
    ?no_build:bool ->
    'a -> Cmd.tool -> (Fpath.t, string) result
  val get_cmd :
    ?no_build:bool -> 'a -> Cmd.t -> (Cmd.t, string) result
  val unit_file_exe : t -> build_unit -> (Fpath.t, string) result
  val unit_cmd : t -> Set.elt -> (Cmd.t, string) result
end
(**/**)
