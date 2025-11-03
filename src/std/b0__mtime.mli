(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Monotonic time stamps and spans.

    This module provides support for representing monotonic wall-clock
    time. This time increases monotonically and is not subject to
    operating system calendar time adjustement. Its absolute value is
    meaningless.

    To obtain and measure monotonic time use {!B0_std.Os.Mtime}.

    @canonical B0_std.Mtime *)

(** {1:spans Time spans} *)

(** Monotonic time spans *)
module Span : sig

  (** {1:span Time spans} *)

  type t
  (** The type for non-negative monotonic time spans.

      They represent the difference between two monotonic clock
      readings with nanosecond precision (1e-9s) and can measure up to
      approximatevely 584 Julian year spans before silently rolling
      over (unlikely since this is in a single program run). *)

  val zero : t
  (** [zero] is a span of 0ns. *)

  val one : t
  (** [one] is a span of 1ns. *)

  val max_span : t
  (** [max_span] is a span of [2^64-1]ns. *)

  val add : t -> t -> t
  (** [add s0 s1] is [s0] + [s1]. {b Warning.} Rolls over on overflow. *)

  val abs_diff : t -> t -> t
  (** [abs_diff s0 s1] is the absolute difference between [s0] and [s1]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal s0 s1] is [s0 = s1]. *)

  val compare : t -> t -> int
  (** [compare s0 s1] orders span by increasing duration. *)

  val is_shorter : t -> than:t -> bool
  (** [is_shorter span ~than] is [true] iff [span] lasts less than [than]. *)

  val is_longer : t -> than:t -> bool
 (** [is_longer span ~than] is [true] iff [span] lasts more than [than]. *)

  (** {1:const Durations} *)

  val ( * ) : int -> t -> t
  (** [n * dur] is [n] times duration [n]. Does not check for
      overflow or that [n] is positive. *)

  val ns : t
  (** [ns] is a nanosecond duration, 1·10{^-9}s. *)

  val us : t
  (** [us] is a microsecond duration, 1·10{^-6}s. *)

  val ms : t
  (** [ms] is a millisecond duration, 1·10{^-3}s. *)

  val s : t
  (** [s] is a second duration, 1s. *)

  val min : t
  (** [min] is a minute duration, 60s. *)

  val hour : t
  (** [hour] is an hour duration, 3600s. *)

  val day : t
  (** [day] is a day duration, 86'400s. *)

  val year : t
  (** [year] is a Julian year duration (365.25 days), 31'557'600s. *)

  (** {1:conv Conversions} *)

  val to_uint64_ns : t -> int64
  (** [to_uint64_ns s] is [s] as an {e unsigned} 64-bit integer nanosecond
      span. *)

  val of_uint64_ns : int64 -> t
  (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond span [u]
      as a span. *)

  val of_float_ns : float -> t option
  (** [of_float_ns f] is the positive floating point nanosecond span
      [f] as a span. This is [None] if [f] is negative, non finite, or
      larger or equal than 2{^53} (~104 days, the largest exact
      floating point integer). *)

  val to_float_ns : t -> float
  (** [to_float_ns s] is [span] as a nanosecond floating point span.
      Note that if [s] is larger than 2{^53} (~104 days, the largest
      exact floating point integer) the result is an approximation and
      will not round trip with {!of_float_ns}. *)

  val to_float_s : t -> float
  (** [to_float_s s] is [1e9 * ]{!to_float_ns}[ s]. *)

  (** {1:fmt Formatting} *)

  val pp : t B0__fmt.t
  (** [pp] formats with {!Fmt.uint64_ns_span}. *)

  val pp_ns : t B0__fmt.t
  (** [pp_ns ppf s] prints [s] as an unsigned 64-bit integer nanosecond
      span. *)
end

(** {1:timestamps Timestamps}

    {b Note.} Only use timestamps if you need inter-process time
    correlation, otherwise prefer {!B0_std.Os.Mtime.elapsed} and
    {{!B0_std.Os.Mtime.monotonic_counters}counters} to measure
    time. *)

type t
(** The type for monotonic timestamps relative to an indeterminate
    system-wide event (e.g. last startup). Their absolute value has no
    meaning but can be used for inter-process time correlation. *)

val to_uint64_ns : t -> int64
(** [to_uint64_ns t] is [t] as an {e unsigned} 64-bit integer
    nanosecond timestamp. The absolute value is meaningless. *)

val of_uint64_ns : int64 -> t
(** [to_uint64_ns t] is [t] is an {e unsigned} 64-bit integer
    nanosecond timestamp as a timestamp.

    {b Warning.} Timestamps returned by this function should only be
    used with other timestamp values that are know to come from the
    same operating system run. *)

val min_stamp : t
(** [min_stamp] is the earliest timestamp. *)

val max_stamp : t
(** [max_stamp] is the latest timestamp. *)

 val pp : t B0__fmt.t
(** [pp] is a formatter for timestamps. *)

(** {1:preds Predicates} *)

val equal : t -> t -> bool
(** [equal t t'] is [true] iff [t] and [t'] are equal. *)

val compare : t -> t -> int
(** [compare t t'] orders timestamps by increasing time. *)

val is_earlier : t -> than:t -> bool
(** [is_earlier t ~than] is [true] iff [t] occurred before [than]. *)

val is_later : t -> than:t -> bool
(** [is_later t ~than] is [true] iff [t] occurred after [than]. *)

(** {1:arith Arithmetic} *)

val span : t -> t -> Span.t
(** [span t t'] is the span between [t] and [t'] regardless of the
    order between [t] and [t']. *)

val add_span : t -> Span.t -> t option
(** [add_span t s] is the timestamp [s] units later than [t] or [None] if
    the result overflows. *)

val sub_span : t -> Span.t -> t option
(** [sub_span t s] is the timestamp [s] units earlier than [t] or
    [None] if overflows. *)
