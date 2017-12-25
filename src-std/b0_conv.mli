(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Human and type-safe binary value conversions.

    See {!B0.conv}. *)

open B0_result

(* Converters *)

type 'a codec
val codec : (string -> 'a result) * ('a -> string result) -> 'a codec

type 'a text
val text : (string -> 'a result) * (Format.formatter -> 'a -> unit) -> 'a text

type 'a t
val v : ?docv:string -> ?codec:'a codec -> 'a text -> 'a t
val with_docv : 'a t -> string -> 'a t
val parse : 'a t -> (string -> 'a result)
val print : 'a t -> (Format.formatter -> 'a -> unit)
val decode : 'a t -> (string -> 'a result)
val encode : 'a t -> ('a -> string result)
val docv : 'a t -> string

(* Predefined converters *)

val bool : bool t
val char : char t
val int : int t
val int32 : int32 t
val int64 : int64 t
val float : float t
val string : string t
val string_non_empty : string t
val fpath : B0_fpath.t t
val file : B0_fpath.t t
val dir : B0_fpath.t t
val tool : B0_cmd.t t
val cmd : B0_cmd.t t
val enum : ?docv:string -> (string * 'a) list -> 'a t
val list : ?sep:string -> 'a t -> 'a list t
val pair : ?sep:string -> 'a t -> 'b t -> ('a * 'b) t
val option : ?none:string -> 'a t -> 'a option t

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
