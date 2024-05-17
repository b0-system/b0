(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Build units.

    A unit is a named build procedure and an optional action to perform
    once the unit has built. Units are the smallest unit of build in B0 files.

    Either of the build procedure or the action can be a nop. Actions
    can also require a bit more than the unit itself from the build in
    order to run. *)

open B0_std

(** {1:proc Build procedures} *)

type build_proc = B0_build.t -> unit Fut.t
(** The type for unit build procedures. Note that when the future
    determines the build may not be finished. *)

val build_nop : build_proc
(** [build_nop] does nothing. *)

(** {1:units Units} *)

type t = B0_defs.b0_unit
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

val outcomes : Fpath.t list Bval.t B0_meta.key
(** [outcomes] is the set of public file outcomes. *)

val is_public : t -> bool
(** [is_public u] is [true] iff [u]'s meta has {!B0_meta.public}
    set to [true]. *)

val get_or_suggest_tool : keep:(t -> bool) -> string -> (t list, t list) result
(** [get_or_suggest_tool tool_name] are tools names whose
    {!tool_name} match [tool_name] and are filtered by [keep] *)

val tool_is_user_accessible : t -> bool
(** [tool_is_user_accessible u] assumes [u] has a tool name. This then
    returns [true] iff [u] {!is_public} or {!in_root_scope}. *)

(** {1:action Actions}  *)

(** Actions.

    These properties pertain to the interface that allows to execute
    an action after a unit has build as is done for example by the
    [b0] or [b0 test] commands.

    The execution procedure, will likely be refined when we get proper
    build environments and cross but basically:

    {ul
    {- The environment and the cwd are respectively defined
       by {!Exec.get_env} and {!Exec.get_cwd}. Additional
       cli arguments may be provided by the driver.}
    {- The actual entity to execute is either defined by {!Action.key} if
       present or by {!B0_unit.exe_file}}} *)
module Action : sig

  type b0_unit := t

  (** {1:env Environment} *)

  type env =
  [ `Build_env (** The build environment. *)
  | `Driver_env (** The b0 invocation process environment. *)
  | `Override of [`Build_env | `Driver_env] * Os.Env.t
  (** Environment overriden by given values. *)
  | `Env of Os.Env.t (** This exact environment. *)
  | `Fun of string * (B0_env.t -> b0_unit -> (Os.Env.t, string) result)
    (** Doc string and function. *) ]
  (** The type for action execution environments. *)

  val env : env B0_meta.key
  (** [env] specifies the environement for executing a unit. If unspecified
      this is [`Build_env].  *)

  val get_env : B0_env.t -> b0_unit -> (Os.Env.t, string) result
  (** [get_env env u] performs the logic to get the execution
      environment {!val-env} for unit [u] in environment [env]. *)

  (** {1:cwd Cwd} *)

  type cwd =
  [ `Cwd (** The user's current working directory. *)
  | `Root_dir (** The root B0 file directory. *)
  | `Scope_dir (** The directory of the scope where the entity is defined. *)
  | `Unit_dir (** The unit's build directory. *)
  | `In of [ `Cwd | `Unit_dir | `Root_dir | `Scope_dir ] * Fpath.t
  | `Fun of string * (B0_env.t -> b0_unit -> (Fpath.t, string) Result.t)
    (** Doc string and function. *) ]
  (** The type for action execution working directories. *)

  val cwd : cwd B0_meta.key
  (** [cwd] specifies the current working directory for executing a unit.
      If unspecified this is [`Cwd]. *)

  val get_cwd : B0_env.t -> b0_unit -> (Fpath.t, string) result
  (** [get_cwd env u] performs the logic to get the cwd {!val-cwd}
      for unit [u] in environment [env]. *)

  (** {1:action Action}

      {b Warning.} Actions may be executed in parallel, either at the
      OS or OCaml level. Keep that in mind if you make funny
      things. *)

  type func = B0_env.t -> b0_unit -> args:Cmd.t -> Os.Exit.t

  type t =
  [ `Unit_exe (** The unit's {!exe_file} *)
  | `Cmd of string *
            (B0_env.t -> b0_unit -> args:Cmd.t -> (Cmd.t, string) result)
    (** Doc string and a function that returns a command to
        execute with {!B0_std.Os.Cmd.execv}. *)
  | `Fun of
      string *
      (B0_env.t -> b0_unit -> args:Cmd.t -> (Os.Exit.t, string) result)
      (** Doc string and a function. The function is given the result
            of {!get_cwd} and {!get_env}. *)
  ]
  (** The type for specifying unit executions. *)

  (** {1:meta Meta keys}

      These keys collectively define the behaviour of actions.

      {b FIXME.} Except for keys do we really need that to be keys ? *)


  val key : t B0_meta.key
  (** [key] specifies the execution for a unit. If unspecified this is
      [`Unit_exe]. *)

  val units : b0_unit list B0_meta.key
  (** [units] units that need to be built for the action. *)

  val dyn_units : (args:Cmd.t -> b0_unit list) B0_meta.key
  (** [dyn_units] is a hack. *)

  val packs : B0_pack.t list B0_meta.key
  (** [packs] are packs thatneed to be built for the action. *)

  val store : B0_store.binding list B0_meta.key
  (** [store] are constraints on the build for the action to run. *)

  val find :
    b0_unit ->
    (B0_env.t -> b0_unit -> args:Cmd.t -> (Os.Exit.t, string) result) option
    (** [find u] is a function, if any, for executing unit [u]
        according to {!val-key}.  Given the environment, the unit and
        additional arguments potentially provided by the driver it
        performs the full execution logic. *)

  (** {2:script Action functions for script execution} *)

  val exec_file :
    ?env:Os.Env.assignments -> ?cwd:Fpath.t -> Cmd.t -> func
  (** [exec_file env cmd] executes [cmd] using
      the {{!B0_env.scope_dir}scope directory} is used
      as the default [cwd] and to resolve [file] if it is relative. This
      signature is twisted so that you can simply write:
      {[
        let myscript =
          B0_action.make "myscript" @@
          B0_action.exec_file' ~/"scripts/myscript"
      ]} *)

  val exec_cmd :
    ?env:Os.Env.assignments -> ?cwd:Fpath.t -> B0_env.t -> Cmd.t -> Os.Exit.t
    (** [exec_tool tool cmd] executes [cmd]. The {{!B0_env.scope_dir}scope
        directory} is used as the default [cwd]. The tool of [cmd] is looked up
        using {!B0_env.get_cmd}, if that fails the error is logged and we exit
        we and exits with {!B0_cli.Exit.some_error}. *)

end

(** {1:making Making unit actions}

    These helper functions create units with a {!build_nop} build
    procedure. *)

val of_action :
  ?store:B0_store.binding list -> ?packs:B0_pack.t list ->
  ?units:t list -> ?dyn_units:(args:Cmd.t -> t list) ->
  ?doc:string -> ?meta:B0_meta.t -> string -> Action.func -> t
(** [make name func] is an action named [name] implemented with action
    function [func]. [units] and [packs] are units and packs that must
    be built to run the action. [store] are store bindings that must
    be enforced in the build. *)

val of_action' :
  ?store:B0_store.binding list ->
  ?units:t list -> ?dyn_units:(args:Cmd.t -> t list) ->
  ?doc:string -> ?meta:B0_meta.t -> string ->
  (B0_env.t -> t -> args:Cmd.t -> (unit, string) result) -> t
(** [make'] is like {!make} but with a function whose result
    is turned into an error code via {!exit_of_result}. *)

(** {2:cli Command line interaction}

    Use {!B0_cli} to parse actions arguments and {!B0_cli.Exit} for
    exit codes. Given a suitable {!Cmdliner} term this function can be
    used to implement the action's command.

    {b TODO} Add a quick getopt interface. *)

val of_cmdliner_cmd :
  ?store:B0_store.binding list -> ?packs:B0_pack.t list ->
  ?units:t list -> ?dyn_units:(args:Cmd.t -> t list) ->
  ?doc:string -> ?meta:B0_meta.t -> string ->
  (t -> B0_env.t -> Os.Exit.t Cmdliner.Cmd.t) -> t
(** [of_cmdliner_cmd name cmd] is like {!make_action} is an action from the
    Cmdliner command [cmd]. See also {!eval_cmdliner_term}.

    {b Note.} The command is under a thunk to avoid toplevel inits.
    This entails a bit of repetition for name and doc but you can access
    those of the action in the thunk to define the cmdliner command. *)

val eval_cmdliner_term :
  ?man_xrefs:Cmdliner.Manpage.xref list -> ?man:Cmdliner.Manpage.block list ->
  ?envs:Cmdliner.Cmd.Env.info list -> ?exits:Cmdliner.Cmd.Exit.info list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  t -> B0_env.t -> Os.Exit.t Cmdliner.Term.t -> args:Cmd.t -> Os.Exit.t
(** [eval act env cmd t] defines a action command by evaluating the cmdliner
    term [t] with arguments [cmd]. The menagerie of optional
    parameters define a {!Cmdliner.Term.info} value for the term, see
    the docs there. By default [doc] is derived from the unit's doc string
    and [exits] is {!B0_cli.Exit.infos}. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t (** @inline *)
  with type Set.t = B0_defs.Unit.Set.t

(** {1:tools Tool name map} *)

val tool_name_map : Set.t -> t String.Map.t
(** [tool_name_map units] are the user accessible tools defined by the
    set of units [units]. *)
