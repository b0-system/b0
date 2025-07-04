(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner support for {!B0_std}. *)

open B0_std

(** {1:exit Exits} *)

(** Program exits.

    Support for {!B0_std.Os.Exit} exits which are more evolved
    than those provided by cmdliner. *)
module Exit : sig

  (** {1:codes Common exit codes} *)

  val infos : Cmdliner.Cmd.Exit.info list
  (** [infos] has the infos of {!Cmdliner.Term.default_exits},
      {!no_such_name}, {!some_error} and those above.  *)

  (** {1:cmdliner Evaluating and exiting} *)

  val of_eval_result :
    ?term_error:Os.Exit.t ->
    (Os.Exit.t Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result ->
    Os.Exit.t
  (** [of_eval_result ~term_error r] is:
      {ul
      {- [e] if [r] is [Ok (`Ok e)]}
      {- {!ok} if [r] is [Ok _]}
      {- [Os.Exit.Code Cmdliner.Exit.cli_error] if [r] is [Error `Parse].}
      {- [Os.Exit.Code Cmdliner.Exit.internal_error] if [r] is [Error `Exn].}
      {- [Os.Exit.code term_error] if [r] is [Error `Term]. [term_error]
         (defaults to [Os.Exit.Code Cmdliner.Exit.cli_error]).}} *)
end

(** {1:conv Argument converters} *)

val fpath : Fpath.t Cmdliner.Arg.conv
(** [fpath] is a converter for file paths. No existence checks are
        performed on the path. *)

val cmd : Cmd.t Cmdliner.Arg.conv
(** [cmd] is a converter for commands. *)

(** {1:output_format Specifying output format} *)

val s_output_verbosity_options : string
(** [s_output_format_options] is a manual section called
    ["OUTPUT VERBOSITY OPTIONS"] *)

type output_verbosity = [ `Normal | `Short | `Long ]
(** The type for specifying output verbosity. *)

val output_verbosity :
  ?docs:string -> ?short_opts:string list -> ?long_opts:string list ->
  unit -> output_verbosity Cmdliner.Term.t
(** [output_verbosity ~short_opts ~long_opts ()] are mutually
    exclusive options to specify output verbosity as short or long.
    Without options this is [`Normal]. [short_opts] defaults to
    [["s"; "short"]] and [long_opts] default to [["l";
    "long"]]. [docs] is the manual section in which options are
    documented, defaults to {!s_output_format_options}. *)

(** Fragments for setting up {!B0_std}.

    TODO maybe we should just side effect in the options, it would be simpler
    to setup. There is now {!log_setup} that does so. Also I think the color
    madness stuff should go. *)

(** {1:setup Setup}

    Configure {{!B0_std.Fmt.styled}styled output} and
    {{!B0_std.Log.set_level}log verbosity} and the
    {!B0_std.Os.Cmd.spawn_tracer}. *)

val get_styler : Fmt.styler option option -> Fmt.styler
(** [get_styler styler] determines [styler] by falling back to
    {!Fmt.styler}. *)

val get_log_level : Log.level option -> Log.level
(** [get_log_level level] determines [level] with {!B0_std.Log.Warning} if
    [level] is [None]. *)

val setup : Fmt.styler -> Log.level -> log_spawns:Log.level -> unit
(** [setup styler log_level ~log_spawns] sets:
    {ul
    {- {!B0_std.Fmt.set_styler} with [styler].}
    {- {!B0_std.Log.set_level} with [log_level].}
    {- {!B0_std.Os.Cmd.set_spawn_tracer} with
      {!B0_std.Log.spawn_tracer}[ log_spawns]
         iff [level >= log_spawn].}}
      {b Warning.} If [level < log_spawn] but {!B0_std.Log.level} is
      increased after this call, the spawns won't be traced (most cli
      programs do not change after the initial setup). Do your own
      setup if that is a problem for you. *)

(** {1:cli Cli arguments} *)

val styler_of_string : string -> (Fmt.styler option, string) result
(** [styler_of_string v] parses:
    {ul
    {- [""], ["auto"] into [None]}
    {- ["always"] into [Some `Ansi]}
    {- ["never"] into [Some `None]}} *)

val color :
  ?docs:string -> ?env:Cmdliner.Cmd.Env.info -> unit ->
  Fmt.styler option option Cmdliner.Term.t
(** [color ~docs ~env ()] is a cli interface for specifying
    formatting styling with a [--color] option. [docs] is where
    the options are documented (defaults to
    {!Cmdliner.Manpage.s_common_options}). [env], if provided, is an
    environment variable to set the value (use something like
    ["MYPROGRAM_COLOR"]). [None] is returned if the value is not set
    on the cli or via the env var. *)

val log_level :
  ?none:Log.level -> ?docs:Cmdliner.Manpage.section_name ->
  ?env:Cmdliner.Cmd.Env.info -> unit -> Log.level option Cmdliner.Term.t
(** [log_level ~none ~docs ~env ()] is a cli interface for
    specifiying a logging level with various options. [docs] is
    where the options are documented (defaults to
    {!Cmdliner.Manpage.s_common_options}). [env], if provided, is an
    environment variable to set the value (use something like
    ["MYPROGRAM_VERBOSITY"]). [none] is used to document the level
    when the log level is unspecified (defaults to
    [Log.Warning]). [None] is returned if the value is not set on
    the cli or via the env var. *)

val configure_log :
  ?docs:Cmdliner.Manpage.section_name -> ?env:Cmdliner.Cmd.Env.info ->
  ?spawns:Log.level Cmdliner.Term.t -> ?absent:Log.level -> unit ->
  unit Cmdliner.Term.t
(** [log_setup] is a cli interface for specifying a logging level with
    various options and setting up {!B0_std.Log.set_level}
    and {!B0_std.Os.Cmd.spawn_tracer} with it.

    The default value of [absent] is [Log.Warning] and the default
    value of [spawns] is {!Log.Debug}. The default value of [env] is
    {!log_level_var}.

    {b Warning.} If [level < log_spawn] but {!B0_std.Log.level} is
    increased after this call, the spawns won't be traced (most cli
    programs do not change after the initial setup). Do your own
    setup if that is a problem for you. *)

val log_level_var : Cmdliner.Cmd.Env.info
(** [log_level_var] describes the default environment variable
    [LOG_LEVEL] of {!configure_log}. *)
