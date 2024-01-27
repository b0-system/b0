(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Build units.

    A build unit is a named build procedure with metadata associated
    to it. Build units are the smallest unit of build in B0 files. *)

open B0_std

(** {1:proc Build procedures} *)

type b0_build
(** The type for builds, see {!B0_build}. *)

type build_proc = b0_build -> unit Fut.t
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

(** {2:executable Metadata for built executables} *)

val tool_name : string B0_meta.key
(** [tool_name] is an executable name without the platform specific
    executable extension. *)

val exe_file : Fpath.t Fut.t B0_meta.key
(** [exe_file] is an absolute file path to an executable build by the unit. *)

val is_public : t -> bool
(** [is_public u] is [true] iff [u]'s meta has {!B0_meta.public}
    set to [true]. *)

val get_or_suggest_tool : keep:(t -> bool) -> string -> (t list, t list) result
(** [get_or_suggest_tool tool_name] are tools names whose
    {!tool_name} match [tool_name] and are filtered by [keep] *)

val tool_is_user_accessible : t -> bool
(** [tool_is_user_accessible u] assumes [u] has a tool name. This then
    returns [true] iff [u] {!is_public} or {!in_root_scope}. *)

(** {1:meta Unit executions}  *)

type b0_env
(** The type for execution environments, see {!B0_env}. *)

(** Unit executions.

    These properties pertain to the interface that allows to execute
    units after they have been built as is done for example by the
    [b0] command.

    The execution procedure, will likely be refined when we get proper
    build environments and cross but basically:

    {ul
    {- The environment and the cwd are respectively defined
       by {!Exec.get_env} and {!Exec.get_cwd}. Additional
       cli arguments may be provided by the driver.}
    {- The actual entity to execute is either defined by {!Exec.key} if present
       or by {!B0_unit.exe_file}}} *)
module Exec : sig

  type b0_unit := t

  (** {1:env Environment} *)

  type env =
  [ `Build_env (** The build environment. *)
  | `Driver_env (** The b0 invocation process environment. *)
  | `Override of [`Build_env | `Driver_env] * Os.Env.t
  (** Environment overriden by given values. *)
  | `Env of Os.Env.t (** This exact environment. *)
  | `Fun of string * (b0_env -> b0_unit -> (Os.Env.t, string) result)
    (** Doc string and function. *) ]
  (** The type for execution environments. *)

  val env : env B0_meta.key
  (** [env] specifies the environement for executing a unit. If unspecified
      this is [`Build_env].  *)

  val get_env : b0_env -> b0_unit -> (Os.Env.t, string) result
  (** [get_env env u] performs the logic to get the execution
      environment {!val-env} for unit [u] in environment [env]. *)

  (** {1:cwd Cwd} *)

  type cwd =
  [ `Cwd (** The user's current working directory. *)
  | `Root_dir (** The root B0 file directory. *)
  | `Scope_dir (** The directory of the scope where the entity is defined. *)
  | `Unit_dir (** The unit's build directory. *)
  | `In of [ `Cwd | `Unit_dir | `Root_dir | `Scope_dir ] * Fpath.t
  | `Fun of string * (b0_env -> b0_unit -> (Fpath.t, string) Result.t)
    (** Doc string and function. *) ]
  (** The type for execution working directories. *)

  val cwd : cwd B0_meta.key
  (** [cwd] specifies the current working directory for executing a unit.
      If unspecified this is [`Cwd]. *)

  val get_cwd : b0_env -> b0_unit -> (Fpath.t, string) result
  (** [get_cwd env u] performs the logic to get the cwd {!val-cwd}
      for unit [u] in environment [env]. *)

  (** {1:execution Execution} *)

  type t =
    [ `Unit_exe (** The unit's {!exe_file} *)
    | `Cmd of string *
              (b0_env -> b0_unit -> args:Cmd.t -> (Cmd.t, string) result)
    (** Doc string and a function that returns a command to
        execute with {!B0_std.Os.Cmd.execv}. *)
    | `Fun of
        string *
        (b0_env -> ?env:Os.Env.assignments -> ?cwd:Fpath.t ->
         b0_unit -> args:Cmd.t -> (Os.Exit.t, string) result)
        (** Doc string and a function. The function is given the result
            of {!get_cwd} and {!get_env}. *)
   ]
  (** The type for specifying unit executions. *)

  val key : t B0_meta.key
  (** [key] specifies the execution for a unit. If unspecified this is
      [`Unit_exe]. *)

  val find :
    b0_unit ->
    (b0_env -> b0_unit -> args:Cmd.t -> (Os.Exit.t, string) result) option
    (** [find u] is a function, if any, for executing unit [u]
        according to {!val-key}.  Given the environment, the unit and
        additional arguments potentially provided by the driver it
        performs the full execution logic. *)
end

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t (** @inline *)

val tool_name_map : Set.t -> t String.Map.t
(** [tool_name_map units] are the user accessible tools defined by the
    set of units [units]. *)

(**/**)
module Build : sig
  type build_unit := t
  type t = b0_build
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
  type t = b0_env
  val make :
    b0_dir:Fpath.t -> build:Build.t -> cwd:Fpath.t -> root_dir:Fpath.t ->
    scope_dir:Fpath.t -> driver_env:Os.Env.t -> t

  val b0_dir : t -> Fpath.t
  val build : t -> Build.t
  val cwd : t -> Fpath.t
  val root_dir : t -> Fpath.t
  val scope_dir : t -> Fpath.t
  val scratch_dir : t -> Fpath.t
  val unit_dir : t -> build_unit -> Fpath.t
  val in_root_dir : t -> Fpath.t -> Fpath.t
  val in_scope_dir : t -> Fpath.t -> Fpath.t
  val in_scratch_dir : t -> Fpath.t -> Fpath.t
  val in_unit_dir : t -> build_unit -> Fpath.t -> Fpath.t

  type dir = [`Cwd | `Root_dir | `Scope_dir | `Unit_dir ]
  val pp_dir : dir Fmt.t
  val dir : t -> dir -> Fpath.t
  val in_dir : t -> dir -> Fpath.t -> Fpath.t

  type env = [`Build_env | `Driver_env]
  val pp_env : env Fmt.t
  val build_env : t -> Os.Env.t
  val driver_env : t -> Os.Env.t
  val env : t -> env -> Os.Env.t

  val get_cmd : ?skip_build:bool -> t -> Cmd.t -> (Cmd.t, string) result
  val unit_exe_file : t -> build_unit -> (Fpath.t, string) result
  val unit_exe_file_cmd : t -> build_unit -> (Cmd.t, string) result
end
(**/**)
