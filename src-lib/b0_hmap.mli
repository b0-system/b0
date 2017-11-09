(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Type-safe, serializable, heterogeneous value maps. See {!B0.Hmap}. *)

open B0_result

module type KEY = sig
  type 'a typed
  type 'a info
  type t = V : 'a typed -> t

  val v :
    ?loc:B0_def.loc -> ?doc:string -> string -> 'a B0_conv.t -> 'a info ->
    'a typed

  val conv : 'a typed -> 'a B0_conv.t
  val info : 'a typed -> 'a info
  val of_typed : 'a typed -> t
  include B0_def.S with type t:= t
end

module type KEY_INFO = sig
  type 'a t
  val key_kind : string
  val key_namespaced : bool
  val key_name_tty_color : B0_tty.color
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type MAP = sig
  type 'a key
  type t

  val empty : t
  val is_empty : t -> bool
  val mem : 'a key -> t -> bool
  val add : 'a key -> 'a -> t -> t
  val add_tag : bool key -> t -> t
  val singleton : 'a key -> 'a -> t
  val rem : 'a key -> t -> t
  val find : 'a key -> t -> 'a option
  val get : 'a key -> t -> 'a
  val get_or_suggest : 'a key -> t -> ('a, string list) Pervasives.result
  val flag : ?absent:bool -> bool key -> t -> bool

  type binding = B : 'a key * 'a -> binding
  val iter : (binding -> unit) -> t -> unit
  val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (binding -> bool) -> t -> bool
  val exists : (binding -> bool) -> t -> bool
  val filter : (binding -> bool) -> t -> t
  val cardinal : t -> int
  val any_binding : t -> binding option
  val get_any_binding : t -> binding

  val pp : t B0_fmt.t

  type encode_error = string * [ `Msg of string ]
  val encode : t -> (string * string) list * encode_error list

  type decode_error = string * [ `Msg of string | `Unknown ]
  val decode : (string * string) list -> t * decode_error list
end

module type S = sig
  module Key : KEY
  type 'a key = 'a Key.typed
  include MAP with type 'a key := 'a key
end

module Make (Key_info : KEY_INFO) () : S with type 'a Key.info = 'a Key_info.t

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
