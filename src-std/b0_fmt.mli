(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Format helpers

    See {!B0.Fmt}. *)

type 'a t = Format.formatter -> 'a -> unit

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val nop : 'a t
val cut : unit t
val sp : unit t
val comma : unit t
val unit : (unit, Format.formatter, unit) Pervasives.format -> unit t

val bool : bool t
val int : int t
val char : char t
val string : string t

val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
val list : ?sep:unit t -> 'a t -> 'a list t
val array : ?sep:unit t -> 'a t -> 'a array t
val option : ?none:unit t -> 'a t -> 'a option t
val none_str : unit t

val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t
val iter_bindings :
  ?sep:unit t -> (('a -> 'b -> unit) -> 'c -> unit) -> ('a * 'b) t -> 'c t

val text : string t
val lines : string t

val exn : exn t
val exn_backtrace : (exn * Printexc.raw_backtrace) t

(* Boxes *)

val box : ?indent:int -> 'a t -> 'a t
val hbox : 'a t -> 'a t
val vbox : ?indent:int -> 'a t -> 'a t
val hvbox : ?indent:int -> 'a t -> 'a t

(* Brackets *)

val parens : 'a t -> 'a t
val brackets : 'a t -> 'a t
val braces : 'a t -> 'a t

(* Fields *)

val field_label : string t
val field : string -> 'a t -> 'a t

(* Synopses and info *)

val synopsis : name:'a t -> doc:'a t -> 'a t
val info : name:'a t -> ?doc:'a t -> 'a t -> 'a t

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
