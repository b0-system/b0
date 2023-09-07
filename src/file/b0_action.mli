(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Actions

    Actions are user defined custom procedures. They can involve
    a build or not.

    They can be used to run build artefacts, test suites,
    script or generic software life-cycle procedures.

    See the {{!page-action_manual}action manual} for a short introduction. *)

open B0_std

type t
(** The type for actions. *)

(** Action execution environments. *)
module Env : sig

  type action = t
  (** See {!B0_action.t}. *)

  type t
  (** The type for action execution environments. *)

  val v :
    cwd:Fpath.t -> scope_dir:Fpath.t -> root_dir:Fpath.t -> b0_dir:Fpath.t ->
    action:action -> t
  (** [v ~cwd ~scope_dir ~root_dir ~action] is an execution context
      with given parameters. See corresponding accessors for semantics. *)

  val cwd : t -> Fpath.t
  (** [cwd c] is the absolute path to the current working directory. *)

  val scope_dir : t -> Fpath.t
  (** [scope_dir c] is the absolute path to the directory of the B0 file
      in which the action is defined or the {{!root_dir}root
      directory} if the action is defined the global scope. *)

  val root_dir : t -> Fpath.t
  (** [root_dir c] is the {{!page-manual.root_dir}root directory}. *)

  val b0_dir : t -> Fpath.t
  (** [b0_dir c] is the {{!page-manual.root_dir}b0 directory}. *)

  val scratch_dir : t -> Fpath.t
  (** [scratch_dir c] is a shared scratch directory for actions in [b0_dir].
      The directory must be created it may not exist, it's content may
      be destroyed at any time and actions are in charge of inventing
      a naming scheme to avoid collisions. *)

  val action : t -> action
  (** [action e] is the executing action. *)
end

type func = Env.t -> Cmd.t -> Os.Exit.t
(** The type for action implementations.

    A function that given an execution environment and command line
    arguments eventually specifies a way to exit. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> func -> t
(** [v name cmd] is an action named [name] implemented
    with [cmd]. *)

val of_cmdliner_cmd :
  ?doc:string -> ?meta:B0_meta.t -> string ->
  (Env.t -> Os.Exit.t Cmdliner.Cmd.t) -> t
(** [of_cli_cmd name cmd] is an action from the Cmdliner command [cmd].

    {b Note.} The command is under a thunk to avoid toplevel inits.
    This entails a bit of repetition for name and doc that you can
    easily factor out in your definition. *)

val func : t -> func
(** [func a] is the action function. *)

(** {1:shortcuts Shortcuts} *)

val exit_of_result : (unit, string) result -> Os.Exit.t
(** [exit_of_result v] exits with {!B0_cli.Exit.ok} if [v] is [Ok ()]
    and logs the Error and exits with {!B0_cli.Exit.some_error} if [v]
    is [Error _]. *)

val exit_of_result' : (Os.Exit.t, string) result -> Os.Exit.t
(** [exit_of_result' v] exits with [e] if [v] is [Ok e] and logs the
    Error and exits with {!B0_cli.Exit.some_error} if [v] is [Error _]. *)

val in_scope_dir : Env.t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(Env.root_dir env // p)]). *)

val in_root_dir : Env.t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(Env.root_dir env // p)]). *)

val in_scratch_dir : Env.t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(Env.scratch_dir env // p)]). *)

(** {1:script Script execution} *)

val exec_file : ?env:Os.Env.assignments -> ?cwd:Fpath.t -> Fpath.t -> func
(** [exec_file exe e args] executes the file [exe] with arguments
    [cmd]. The {{!Env.scope_dir}scope directory} is used as the
    default [cwd] and to resolve relative [exe] paths. *)

val exec_tool : ?env:Os.Env.assignments -> ?cwd:Fpath.t -> Cmd.tool -> func
(** [exec_tool tool e args] executes the tool [exe] with arguments
    [cmd] The {{!Env.scope_dir}scope directory} is used as the default
    [cwd].  [exe] is looked up using {!B0_std.Os.Cmd.get_tool}, if
    that fails the error is logged and we exit we and exits with
    {!B0_cli.Exit.some_error}. *)

(** {1:cli Command line interaction}

    Use {!B0_cli} to parse actions arguments and {!B0_cli.Exit} for
    exit codes. Given a suitable {!Cmdliner} term this function can be
    used to implement the action's command. *)

val eval :
  ?man_xrefs:Cmdliner.Manpage.xref list -> ?man:Cmdliner.Manpage.block list ->
  ?envs:Cmdliner.Cmd.Env.info list -> ?exits:Cmdliner.Cmd.Exit.info list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  Env.t -> Cmd.t -> Os.Exit.t Cmdliner.Term.t -> Os.Exit.t
(** [eval e cmd t] defines a action command by evaluating the cmdliner
    term [t] with arguments [cmd]. The menagerie of optional
    parameters define a {!Cmdliner.Term.info} value for the term, see
    the docs there. By default [doc] is derived from the action's doc string
    and [exits] is {!B0_cli.Exit.infos}. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t
