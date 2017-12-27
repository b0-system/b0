(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** [B0] program log.

    See {!B0.Log}. *)

(* Reporting levels *)

type level = Quiet | App | Error | Warning | Info | Debug

val level : unit -> level
val set_level : level -> unit

val pp_level : level B0_fmt.t
val level_to_string : level -> string
val level_of_string : string -> (level, [`Msg of string]) Pervasives.result

(* Log functions *)

type ('a, 'b) msgf =
  (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

type 'a log = ('a, unit) msgf -> unit

val msg : level -> 'a log
val app : 'a log
val err : 'a log
val warn : 'a log
val info : 'a log
val debug : 'a log
val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b

(* Logging [result] value [Error]s} *)

val on_error :
  ?level:level -> ?header:string ->
  pp:(Format.formatter -> 'b -> unit) -> use:('b -> 'a) -> ('a, 'b) result -> 'a

val on_error_msg :
  ?level:level -> ?header:string -> use:(unit -> 'a) ->
  ('a, [`Msg of string]) result -> 'a

(* Logging timings *)

val time :
  ?level:level ->
  ('a -> (('b, Format.formatter, unit, 'a) format4 -> 'b) -> 'a) ->
  ('c -> 'a) -> 'c -> 'a

(* Log monitoring *)

val warn_count : unit -> int
val err_count : unit -> int

(* Logger *)

type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }
val set_kmsg : kmsg -> unit

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
