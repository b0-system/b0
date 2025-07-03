(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Standard needs for b0 programs.

    Open this module to use it. It redefines a few standard
    modules and introduces a few new ones. *)

(** {1:std Std} *)

module Cmd = B0__cmd
module Fmt = B0__fmt
module Fpath = B0__fpath
module Mtime = B0__mtime
module Os = B0__os

(** {1:stdlib_extensions [Stdlib] extensions} *)

module Char = B0__char
module List = B0__list
module Result = B0__result
module String = B0__string
module Type = B0__type


(** Program log.

    Support for program logging. Not to be used by build logic.

    The module is modelled after [Logs] logging, see
    {{!Logs.basics}this quick introduction}. It can be made
    to log on a [Logs] source, see {{!Log.logger}here}. *)
module Log : sig

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
      {- [Warning] is a suspicious condition that does not prevent
         the program from running normally but may eventually lead to
         an error condition.}
      {- [Info] is a condition that allows the program {e user} to
         get a better understanding of what the program is doing.}
      {- [Debug] is a condition that allows the program {e developer}
         to get a better understanding of what the program is doing.}} *)

  val level : unit -> level
  (** [level ()] is the current reporting level. The initial level
      is set to {!Warning}. *)

  val set_level : level -> unit
  (** [set_level l] sets the current reporting level to [l]. *)

  val pp_level : level Fmt.t
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
    ?level:level -> ?header:string -> 'b Fmt.t -> use:'a -> ('a, 'b) result ->
    'a
  (** [if_error_pp ~level pp ~use r] is
      {ul
      {- [v], if [r] is [Ok v].}
      {- [use] and [e] is logged with [level] (defaults to [Error]) using
         [pp], if [r] is [Error e].}} *)

  val if_error_pp' :
    ?level:level -> ?header:string -> 'b Fmt.t -> use:'a -> ('a, 'b) result ->
    ('a, 'b) result
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

  val spawn_tracer : level -> Os.Cmd.spawn_tracer
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
end


(** Future values.

    A future is an undetermined value that becomes determined at an an
    arbitrary point in the future. The future acts as a placeholder
    for the value while it is undetermined. *)
module Fut : sig

  (** {1:fut Future values} *)

  type 'a t
  (** The type for futures with values of type ['a]. *)

  val make : unit -> 'a t * ('a -> unit)
  (** [make ()] is [(f, set)] with [f] the future value and [set]
      the function to [set] it. The latter can be called only once,
      [Invalid_argument] is raised otherwise. *)

  val await : 'a t -> ('a -> unit) -> unit
  (** [await f k] waits for [f] to be determined and continues with [k v]
      with [v] the value of the future. If the future never determines
      [k] is not invoked. [k] must not raise. *)

  val value : 'a t -> 'a option
  (** [value f] is [f]'s value, if any. *)

  val sync : 'a t -> 'a
  (** [sync f] waits for [f] to determine. {b Warning.} This is relaxed busy
      waiting. *)

  val return : 'a -> 'a t
  (** [return v] is a future that determines [v]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn f] is [return (fn v)] with [v] the value of [f]. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind f fn] is the future [fn v] with [v] the value of [f]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair f0 f1] determines with the value of [f0] and [f1]. *)

  val of_list : 'a t list -> 'a list t
  (** [of_list fs] determines with the values of all [fs], in the same order. *)

  (** Future syntax. *)
  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [let*] is {!bind}. *)

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    (** [and*] is {!pair}. *)
  end
end

(** Blocking values.

    {b Note.} In direct style the {!Fut.t} values would go away.
    For now be bundled lazy blocking values in the same structure. *)
module Bval : sig

  type 'a setter
  (** The type for setting blocking value. *)

  type 'a t
  (** The type for immutable blocking values. *)

  val make : unit -> 'a t * 'a setter
  (** [make ()] is a blocking value and a setter to set it. *)

  val of_val : 'a -> 'a t
  (** [of_val v] is a (non-)blocking value holding [v]. *)

  val of_lazy_fun : (unit -> 'a) -> 'a t
  (** [of_lazy_fun f] is a blocking value that runs [f]
      iff {!get} or {!poll} is called on the value.

      {b XXX.} Something should be said about the context in
      which f runs.  *)

  val of_setter : 'a setter -> 'a t
  (** [of_setter s] is the blocking value of [s]. *)

  val is_lazy : 'a t -> bool
  (** [is_lazy bv] is [true] iff [bv] is a lazily triggered value. *)

  (** {1:setting Setting} *)

  val set : 'a setter -> 'a -> unit
  (** [set s v] sets the blocking value [of_setter s] to value [v].
      Raises [Invalid_argument] if [set] is already set. *)

  val try_set : 'a setter -> 'a -> bool
  (** [try_set s v] is [true] if [iv] was set to [v] and [false]
      if [iv] was already set. *)

  (** {1:getting Getting} *)

  val get : 'a t -> 'a Fut.t
  (** [get bv] is the value of [bv]. In direct style,
      this should be a blocking call. *)

  val poll : 'a t -> 'a option
  (** [poll bv] is [None] if [get bv] would block
      and [Some _] if it does not block. If [bv] was created
      with {!of_lazy_fun}, this ensure the computation gets triggered. *)

  val stir : 'a t -> unit
  (** [stir bv] is [ignore (poll v)]. Useful if you know [bv] will
      be needed later and may be a {!of_lazy_fun}. *)

  (** {1:formatting Formatting} *)

  val pp : 'a Fmt.t -> 'a t Fmt.t
  (** [pp] formats blocking values. Does not block if the value is not
      set in which case "<pending>" formatted. *)
end
