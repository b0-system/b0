(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(** Parse text lines.

    And do it regardless of the platform line ending convention. *)

(** {1:lines Lines}

    A line is ends either with ["\n"] or with ["\r\n"]. *)

val of_string : string -> string list
(** [of_string s] are [s]'s lines, including empty ones. In
    particular, this is [[""]] on the empty string. *)

val fold :
  ?file:Fpath.t -> string -> (int -> string -> 'a -> 'a) -> 'a ->
  ('a, string) result
(** [fold ~file data f acc] folds [f] with [acc] over the lines of
    [data] assuming it came from [file]. [f] is given the one-based
    line number, the line and the accumulator, it may use {!fail} to
    error which turns it in an [Error _] for [file] via {!file_error}.

    If [data] is [""] this returns [acc]. *)

(** {1:errors Parse errors} *)

val fail : int -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail n fmt] formats an error message for line [n] as ["%d:" ^^
    fmt] and raises [Failure] with no trace. *)

val file_error : ?file:Fpath.t -> string -> ('a, string) result
(** [file_error ~file e] is the error message [e] for file [file] formatted
    as ["%a:%s" Fpath.pp_unquoted f s] *)
