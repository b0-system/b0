(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Actions.

    Actions are user defined custom procedures. They can involve
    a build or not.

    They can be used to run build artefacts, test suites,
    script or generic software life-cycle procedures.

    See the {{!page-action}action manual} for a short introduction. *)

open B0_std

type t
(** The type for actions. *)

type func = t -> B0_env.t -> args:Cmd.t -> Os.Exit.t
(** The type for action implementations.

    A function that given the action, execution environment and command line
    arguments eventually specifies a way to exit. *)

val make :
  ?store:B0_store.binding list -> ?packs:B0_pack.t list ->
  ?units:B0_unit.t list -> ?dyn_units:(args:Cmd.t -> B0_unit.t list) ->
  ?doc:string -> ?meta:B0_meta.t -> string -> func -> t
(** [make name func] is an action named [name] implemented with action
    function [func]. [units] and [packs] are units and packs that must
    be built to run the action. [store] are store bindings that must
    be enforced in the build. *)

val make' :
  ?store:B0_store.binding list -> ?packs:B0_pack.t list ->
  ?units:B0_unit.t list -> ?dyn_units:(args:Cmd.t -> B0_unit.t list) ->
  ?doc:string -> ?meta:B0_meta.t -> string ->
  (t -> B0_env.t -> args:Cmd.t -> (unit, string) result) -> t
(** [make'] is like {!make} but with a function whose result
    is turned into an error code via {!exit_of_result}. *)

val func : t -> func
(** [func a] is the action function. *)

val units : t -> B0_unit.t list
(** [units a] are the units that must build for running the action. *)

val dyn_units : t -> args:Cmd.t -> B0_unit.t list
(** [dyn_units a args] are the dynamic units of the action.

    {b FIXME.} This is a temporary hack. *)

val packs : t -> B0_pack.t list
(** [packs a] are the packs that must build for running the action. *)

val store : t -> B0_store.binding list
(** [store a] are the store bindings to enforce on the build. *)

(** {1:shortcuts Shortcuts} *)

val exit_of_result : (unit, string) result -> Os.Exit.t
(** [exit_of_result v] exits with {!B0_cli.Exit.ok} if [v] is [Ok ()]
    and logs the Error and exits with {!B0_cli.Exit.some_error} if [v]
    is [Error _]. *)

val exit_of_result' : (Os.Exit.t, string) result -> Os.Exit.t
(** [exit_of_result' v] exits with [e] if [v] is [Ok e] and logs the
    Error and exits with {!B0_cli.Exit.some_error} if [v] is [Error _]. *)

(** {1:script Action functions for script execution} *)

val exec_file :
  ?env:Os.Env.assignments -> ?cwd:Fpath.t ->
  B0_env.t -> Fpath.t -> args:Cmd.t -> Os.Exit.t
(** [exec_file env file ~args] executes [file] with arguments [args].
    The {{!B0_env.scope_dir}scope directory} is used
    as the default [cwd] and to resolve [file] if it is relative. *)

val exec_file' :
  ?env:Os.Env.assignments -> ?cwd:Fpath.t -> Fpath.t -> func
(** [exec_file'] is {!exec_file} twisted a bit differently so
    that if you don't need to tweak arguments or lookup the environment
    you can simply write:
    {[
let myscript =
  B0_action.make "myscript" @@
  B0_action.exec_file' ~/"scripts/myscript"
    ]} *)

val exec_tool :
  ?env:Os.Env.assignments -> ?cwd:Fpath.t ->
  B0_env.t -> Cmd.tool -> args:Cmd.t -> Os.Exit.t
(** [exec_tool tool e args] executes the tool [exe] with arguments
    [cmd] The {{!B0_env.scope_dir}scope directory} is used as the default
    [cwd]. [exe] is looked up using {!B0_std.Os.Cmd.get_tool}, if
    that fails the error is logged and we exit we and exits with
    {!B0_cli.Exit.some_error}. *)

(** {1:cli Command line interaction}

    Use {!B0_cli} to parse actions arguments and {!B0_cli.Exit} for
    exit codes. Given a suitable {!Cmdliner} term this function can be
    used to implement the action's command.

    {b TODO} Add a quick getopt interface. *)

val of_cmdliner_cmd :
  ?store:B0_store.binding list -> ?packs:B0_pack.t list ->
  ?units:B0_unit.t list -> ?dyn_units:(args:Cmd.t -> B0_unit.t list) ->
  ?doc:string -> ?meta:B0_meta.t -> string ->
  (t -> B0_env.t -> Os.Exit.t Cmdliner.Cmd.t) -> t
(** [of_cli_cmd name cmd] is like {!make} is an action from the
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
    the docs there. By default [doc] is derived from the action's doc string
    and [exits] is {!B0_cli.Exit.infos}. *)


(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t
