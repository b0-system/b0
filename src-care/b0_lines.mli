(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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
      line number, the line and the accumulator, it may use {!err} to
      error which turns it in an [Error _] for [file] via
      {!err_file}.

      If [data] is [""] this returns [acc]. *)

(** {1:errors Parse errors} *)

val err : int -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [err n fmt] formats an error message for line [n] as ["%d:" ^^
    fmt] and raises [Failure] with no trace. *)

val err_file : Fpath.t -> string -> ('a, string) result
(** [err_file file e] is the error message [e] for file [file]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
