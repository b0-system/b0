(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Program log.

    The module is modelled after [Logs] logging, see
    {{!Logs.basics}this quick introduction}. It can be made to log on
    a [Logs] source, see {{!Log.logger}here}.

    @canonical B0_std.Log *)

(** {1:levels Reporting levels} *)

type level = Quiet | Stdout | Stderr | Error | Warning | Info | Debug (** *)
(** The type for reporting levels. They are meant to be used
    as follows:
    {ul
    {- [Quiet] doesn't report anything.}
    {- [Stdout] can be used for the standard output of an application.
       Using this instead of [stdout] directly allows the output to be
       silenced by [Quiet] which may be desirable, or not.}
    {- [Stderr] can be used for the standard error of an application.
       Using this instead of [stderr] directly
       allows the output to be silenced by [Quiet] which may be
       desirable, or not.}
    {- [Error] is an error condition that prevents the program from
        running.}
    {- [Warning] is a suspicious condition that does not prevent the
       program from running normally but may eventually lead to an
       error condition.}
    {- [Info] is a condition that allows the program {e user} to
       get a better understanding of what the program is doing.}
    {- [Debug] is a condition that allows the program {e developer}
       to get a better understanding of what the program is doing.}} *)

val level : unit -> level
(** [level ()] is the current reporting level. The initial level
    is set to {!Warning}. *)

val set_level : level -> unit
(** [set_level l] sets the current reporting level to [l]. *)

val pp_level : level B0__fmt.t
(** [pp_level ppf l] prints and unspecified representation of [l]
    on [ppf]. *)

val level_to_string : level -> string
(** [level_to_string l] converts [l] to a string representation. *)

val level_of_string : string -> (level, string) result
(** [level_of_string s] parses a level from [s] according to the
    representation of {!level_to_string}. *)

(** {1:func Log functions} *)

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
(** The type for client specified message formatting functions. See
    {!Logs.msgf}.

    [header] interpretation is up to the reported but [None] should
    automatially output headers that depend on the level and [Some ""]
    should not output any header leaving full control to the client. *)

type 'a log = ('a, unit) msgf -> unit
(** The type for log functions. See {!Logs.log}. *)

val msg : level -> 'a log
(** See {!Logs.msg}. *)

val quiet : 'a log
(** [quiet] is [msg Quiet]. *)

val stdout : 'a log
(** [stdout] is [msg Stdout]. *)

val stderr : 'a log
(** [stderr] is [msg Stderr]. *)

val err : 'a log
(** [err] is [msg Error]. *)

val warn : 'a log
(** [warn] is [msg Warning]. *)

val info : 'a log
(** [info] is [msg Info]. *)

val debug : 'a log
(** [debug] is [msg Debug]. *)

val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
(** [kmsg k level m] logs [m] with level [level] and continues with [k]. *)

(** {2:result Logging [result] value [Error] messages} *)

val if_error :
  ?level:level -> ?header:string -> use:'a -> ('a, string) result -> 'a
(** [if_error ~level ~use v r] is:
    {ul
    {- [v], if [r] is [Ok v]}
    {- [use] and [e] is logged with [level] (defaults to [Error]), if
       [r] is [Error e].}} *)

val if_error' :
  ?level:level -> ?header:string -> use:'a -> ('a, string) result ->
  ('a, 'b) result
(** [if_error'] is {!if_error} wrapped by {!Result.ok}. *)

val if_error_pp :
  ?level:level -> ?header:string -> 'b B0__fmt.t ->
  use:'a -> ('a, 'b) result -> 'a
(** [if_error_pp ~level pp ~use r] is
    {ul
    {- [v], if [r] is [Ok v].}
    {- [use] and [e] is logged with [level] (defaults to [Error]) using
       [pp], if [r] is [Error e].}} *)

val if_error_pp' :
  ?level:level -> ?header:string -> 'b B0__fmt.t -> use:'a ->
  ('a, 'b) result -> ('a, 'b) result
(** [if_error_pp'] is {!if_error_pp'} wrapped by {!Result.ok} *)

(** {2:time Logging time} *)

val time :
  ?level:level ->
  ('a -> (('b, Format.formatter, unit, 'a) format4 -> 'b) -> 'a) ->
  (unit -> 'a) -> 'a
(** [time ~level m f] logs [m] with level [level] (defaults to
    [Info]) and the time [f ()] took as the log header.

    {b Note.} The current log level is determined after [f] has been
    called this means [f] can change it to affect the log
    operation. This allows [f] to be the main function of your
    program and let it set the log level. *)

(** {2:spawns Spawn logging} *)

val spawn_tracer : level -> B0__os.Cmd.spawn_tracer
(** [spawn_tracer level] is a {{!B0_std.Os.Cmd.tracing}spawn tracer}
    that logs with level [level]. If [level] is {!Log.Quiet} this is
    {!B0_std.Os.Cmd.spawn_tracer_nop}. *)

(** {1:monitoring Log monitoring} *)

val err_count : unit -> int
(** [err_count ()] is the number of messages logged with level
    [Error]. *)

val warn_count : unit -> int
(** [warn_count ()] is the number of messages logged with level
    [Warning]. *)

(** {1:logger Logger}

    The following function allows to change the logging backend.
    Note that in this case {{!monitoring}monitoring} and
    {{!levels}level} functions are no longer relevant. *)

type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }
(** The type for the basic logging function. The function is never
    invoked with a level of [Quiet]. *)

val kmsg_nop : kmsg
(** [nop_kmsg] is a logger that does nothing. *)

val kmsg_default : kmsg
(** [kmsg_default] is the default logger that logs messages on
    {!Fmt.stderr} except for {!Log.App} level which logs on
    {!Fmt.stdout}. *)

val set_kmsg : kmsg -> unit
(** [set_kmsg kmsg] sets the logging function to [kmsg]. *)
