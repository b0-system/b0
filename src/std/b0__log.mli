(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Program log.

    Examples:
{[
  let () = Log.warn (fun m -> m "The queue is full (%d elements)" count)
  let () = Log.err (fun m -> m "The request timed out after %a" Mtime.pp dur)
  let items =
    Log.time (fun v m -> m "Purged, %d items remaining" (List.length v)) @@
    (fun () -> purge items)
]}
    See also the {{!page-cookbook.logging}cookbook} on logging.

    {b TODO.} Think about implicit locations.

    @canonical B0_std.Log *)

(** {1:levels Reporting levels} *)

type level =
| Quiet (** Do not report anything. *)
| Stdout (** Outputs to the [stdout] of the program. Using this allows the
             output to be silenced when the {!level} is set to [Quiet],
             which may be desirable, or not. *)
| Stderr (** Outputs to the [stderr] of the program. Using this allows the
             output to be silenced when the {!level} is set to [Quiet],
             which may be desirable, or not. *)
| Error (** For error conditions that prevent the program from running
            correctly. *)
| Warning (** For suspicious conditions that do not prevent the program
              from running normally but may eventually lead to an
              error condition. *)
| Info (** For conditions that allow the program {e user} to
           get a better understanding of what the program is doing. *)
| Debug (** For conditions that allow the program {e developer} to
            get a better understanding of what the program is doing. *)
(** The type for reporting levels. *)

val level : unit -> level
(** [level ()] is the reporting level. The initial level is set to
    {!Warning}. *)

val set_level : level -> unit
(** [set_level l] sets the reporting level to [l]. *)

val level_to_string : level -> string
(** [level_to_string l] converts [l] to a string representation. *)

val level_of_string : string -> (level, string) result
(** [level_of_string s] parses a level from [s] according to the
    representation of {!level_to_string}. *)

(** {1:func Log functions} *)

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
(** The type for client specified message formatting functions.

    A message formatting function is called with a message construction
    function [m]. The message formatting function must call the given message
    construction function with an optional header, a format string and its
    arguments to define the
    message contents. Here are a few examples of message formatting functions:
    {[
      (fun m -> m "%d messages to send" n)
      (fun m -> m ~header:"emails" "%d messages to send" n)
    ]}
    The interpretation of the optional [header] argument of [m] is up
    to the {{!Reporter}reporter} but [None] should automatically
    output a header that depend on the log level and [Some ""] should
    not output any header, leaving full control of the log formatting
    to the client. *)

type 'a log = ('a, unit) msgf -> unit
(** The type for log functions. *)

val msg : level -> 'a log
(** [msg level (fun m -> m fmt …)] logs with level [level] a message
    formatted with [fmt]. For the semantics of levels see {!type-level}. *)

val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
(** [kmsg k level (fun m -> m fmt …)] logs with level [level] a message
    formatted with [fmt] and continues with [k]. *)

val quiet : 'a log
(** [quiet] is {!msg}[ Quiet]. *)

val stdout : 'a log
(** [stdout] is {!msg}[ Stdout]. *)

val stderr : 'a log
(** [stderr] is {!msg}[ Stderr]. *)

val err : 'a log
(** [err] is {!msg}[ Error]. *)

val warn : 'a log
(** [warn] is {!msg}[ Warning]. *)

val info : 'a log
(** [info] is {!msg}[ Info]. *)

val debug : 'a log
(** [debug] is {!msg}[ Debug]. *)

(** {2:result Logging [result] errors} *)

val if_error :
  ?level:level -> ?header:string -> use:'a -> ('a, string) result -> 'a
(** [if_error ~level ~use r] is:
    {ul
    {- [v], if [r] is [Ok v]}
    {- [use] and [e] is logged using {!Fmt.lines} with [level]
       (defaults to [Error]), if [r] is [Error e].}} *)

val if_error' :
  ?level:level -> ?header:string -> use:'a -> ('a, string) result ->
  ('a, 'b) result
(** [if_error'] is {!if_error} wrapped by {!Result.ok}. *)

val if_error_pp :
  'b B0__fmt.t -> ?level:level -> ?header:string ->
  use:'a -> ('a, 'b) result -> 'a
(** [if_error_pp ~level pp ~use r] is
    {ul
    {- [v], if [r] is [Ok v].}
    {- [use] and [e] is logged with [level] (defaults to [Error]) using
       [pp], if [r] is [Error e].}} *)

val if_error_pp' :
  'b B0__fmt.t -> ?level:level -> ?header:string -> use:'a ->
  ('a, 'b) result -> ('a, 'b) result
(** [if_error_pp'] is {!if_error_pp'} wrapped by {!Result.ok} *)

(** {2:time Logging timings} *)

val time :
  ?level:level ->
  ('a -> (('b, Format.formatter, unit, 'a) format4 -> 'b) -> 'a) ->
  (unit -> 'a) -> 'a
(** [time ~level m f] logs [m] with level [level] (defaults to
    [Info]) and the time [f ()] took as the message header with
    {!Mtime.Span.pp}.

    {b Note.} The reporting {!level} is determined after [f] has been
    called. This means [f] can change it to affect the report.
    See for example {!page-cookbook.logging_main} *)

(** {2:log Logging values} *)

val value : ?level:level -> ?id:string -> 'a B0__fmt.t -> 'a -> 'a
(** [value pp v] reports [v] on [level] (defaults to {!Stderr}) with
    [pp] if [id] is specified this is of the form "%s: %a" and returns [v] *)

(** {2:spawns Logging spawns} *)

val spawn_tracer : level -> B0__os.Cmd.spawn_tracer
(** [spawn_tracer level] is a {{!B0_std.Os.Cmd.tracing}spawn tracer}
    that logs with level [level]. If [level] is {!Log.Quiet} this is
    {!B0_std.Os.Cmd.spawn_tracer_nop}. *)

(** {1:monitoring Monitoring} *)

val err_count : unit -> int
(** [err_count ()] is the number of messages logged with level
    [Error]. *)

val warn_count : unit -> int
(** [warn_count ()] is the number of messages logged with level
    [Warning]. *)

(** {1:reporting Reporting} *)

(** Reporting. *)
module Reporter : sig

  type t = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }
  (** The type for log message reporters. [kmsg] is never invoked with
      a level of [Quiet] or with a level smaller than the reporting
      {!level}. *)

  val nop : t
  (** [nop] is a logger that reports nothing. *)

  val default : t
  (** [default] is the default reporter. It logs {!Log.Stdout} messages
      on {!Fmt.stdout} and all other messages on {!Fmt.stderr}. *)

  val get : unit -> t
  (** [get ()] is the reporter. *)

  val set : t -> unit
  (** [set r] sets the reporter to [r]. *)
end
