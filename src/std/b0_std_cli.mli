(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner support for [more] programs.

    The {{!page-b0_std_cookbook.blueprints}cookbook blueprints} has some examples. *)

open B0_std

(** Program exits.

    {b FIXME} review that.

    Support for {!B0_std.Os.Exit} exits which are more evolved
    than those provided by cmdliner. *)
module Exit : sig

  (** {1:codes Common exit codes} *)

  val infos : Cmdliner.Cmd.Exit.info list
  (** [infos] has the infos of {!Cmdliner.Term.default_exits},
      [no_such_name], [some_error] and those above.  *)

  (** {1:cmdliner Evaluating and exiting}

      {b FIXME I think this can be removed}. *)

  val of_eval_result :
    ?term_error:Os.Exit.t ->
    (Os.Exit.t Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result ->
    Os.Exit.t
  (** [of_eval_result ~term_error r] is:
      {ul
      {- [e] if [r] is [Ok (`Ok e)]}
      {- [ok] if [r] is [Ok _]}
      {- [Os.Exit.Code Cmdliner.Exit.cli_error] if [r] is [Error `Parse].}
      {- [Os.Exit.Code Cmdliner.Exit.internal_error] if [r] is [Error `Exn].}
      {- [Os.Exit.code term_error] if [r] is [Error `Term]. [term_error]
         (defaults to [Os.Exit.Code Cmdliner.Exit.cli_error]).}} *)
end

(** {1:conv Miscellaneous argument converters} *)

val path : Fpath.t Cmdliner.Arg.conv
(** [path] is a converter for file system paths. No existence checks
    are performed on the path. Completes files and directories. *)

val filepath : Fpath.t Cmdliner.Arg.conv
(** [filepath] is a converter for file paths. No existence checks are
    performed on the path. Completes files. *)

val dirpath : Fpath.t Cmdliner.Arg.conv
(** [dirpath] is a converter for directory paths. No existence checks
    are performed on the path. Completes directories. *)

val cmd : Cmd.t Cmdliner.Arg.conv
(** [cmd] is a converter for commands which are parsed using
    {!B0_std.Cmd.of_string}. Completes files. *)

(** {1:styling ANSI text styling} *)

val no_color :
  ?docs:string -> ?env:Cmdliner.Cmd.Env.info option -> unit ->
  bool Cmdliner.Term.t
(** [no_color ~docs ~env ()] is a [--no-color] command line flag.
    {ul
    {- [docs] is the manual section in which the option is documented
       (defaults to {!Cmdliner.Manpage.s_common_options})}
    {- [env] is an environment variable to define the default value. It
       defaults to {!no_color_var} which is [NO_COLOR]. The
       environment variable lookup performed by cmdliner for flags is tweaked
       to match what is requested by {:https://no-color.org}.}} *)

val set_no_color :
  ?docs:Cmdliner.Manpage.section_name -> ?env:Cmdliner.Cmd.Env.info option ->
  unit -> unit Cmdliner.Term.t
(** [set_no_color ()] behaves like {!no_color} and sets
    {!B0_std.Fmt.styler} to [Plain] when it's [true]. See
    {{!page-b0_std_cookbook.blueprint_color_log}an example}. *)

val no_color_var : Cmdliner.Cmd.Env.info
(** [no_color_var] describes the default environment variable [NO_COLOR]
    of {!no_color} and {!set_no_color}. *)

(** {1:log_level Log level} *)

val log_level :
  ?docs:Cmdliner.Manpage.section_name -> ?absent:Log.level ->
  ?env:Cmdliner.Cmd.Env.info option -> unit -> Log.level Cmdliner.Term.t
(** [log_level ~docs ~abset ~env ()] is a command line interface for
    specifying a log level with these options:
    {ul
    {- The [--log-level=LEVEL] option to specify the level directly.}
    {- The repeatable [-v] and [--verbose] flags specify the level as
       [Info] (one occurence) or [Debug] (two occurences).}
    {- The [-q] and [--quiet] flags take over all options and specify
       the level as [Quiet].}}
    The other arguments are:
    {ul
    {- [docs] is the manual section in which the options are documented
       (defaults to {!Cmdliner.Manpage.s_common_options})}
    {- [absent] is the level when none is specified it defaults to
       {!B0_std.Log.Warning}}
    {- [env] is an environment variable to define the default
       level value. It defaults to {!log_level_var} which is [LOG_LEVEL].}} *)

val set_log_level :
  ?docs:Cmdliner.Manpage.section_name -> ?absent:Log.level ->
  ?env:Cmdliner.Cmd.Env.info option -> unit -> unit Cmdliner.Term.t
(** [set_log_level] behaves like {!val-log_level} and sets the log level.
    See {{!page-b0_std_cookbook.blueprint_color_log}an example}. *)

val log_level_var : Cmdliner.Cmd.Env.info
(** [log_level_var] describes the default environment variable
    [LOG_LEVEL] of {!val-log_level} and {!val-set_log_level}. *)

val log_level_conv : Log.level Cmdliner.Arg.Conv.t
(** [log_level_conv] is a converter for log level. *)

(** {1:output_details Specifying output level of details} *)

val s_output_details_options : string
(** [s_output_format_options] is a manual section called
    ["OUTPUT DETAILS OPTIONS"] *)

type output_details =
[ `Short (** Short lined-based output with essential details. *)
| `Normal (** Normal output. *)
| `Long (** Long output with as much details as possible. *) ]
(** The type for specifying output level of details. *)

val output_details : ?docs:string -> unit -> output_details Cmdliner.Term.t
(** [output_details ()] are mutually exclusive options to specify an output
    level of detail as [`Short], [`Normal] or [`Long].
    {ul
    {- Without options this is [`Normal].}
    {- Options [-s] or [--short] specify [`Short].}
    {- Options [-l] or [--long] specify [`Long].}}
    [docs] is the manual section in which options are documented, defaults to
    {!s_output_details_options}. *)

(** {1:net Networking} *)

val socket_endpoint_conv :
  default_port:int -> Os.Socket.Endpoint.t Cmdliner.Arg.conv
(** [socket_endpoint_conv] is a convert for socket endpoints using
    {!default_port} as the default port. It parses strings with
    {!B0_std.Os.Socket.Endpoint.of_string}. *)

val socket_endpoint_listener :
  ?opts:string list -> ?docs:string -> default_port:int ->
  ?default_endpoint:Os.Socket.Endpoint.t -> unit ->
  Os.Socket.Endpoint.t Cmdliner.Term.t
(** [socket_endpoint_listener] is an option for specifying a network endpoint
    to listen on.
    {ul
    {- [default_port] is the default port when unspecified.}
    {- [default_endpoint] is the default endpoint when unspecified.
       Defaults to [`Host ("localhost", default_port)]}
    {- [docs] is the manual section where the options are documented}
    {- [opts] are the options to use (defaults to ["l"; "listen"]).}} *)
