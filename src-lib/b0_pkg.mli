(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Packages.

    See {!B0.Pkg}. *)

type t
type id = int

val create :
  ?loc:B0_def.loc -> ?doc:string -> ?meta:B0_meta.Pkg.t -> string -> t

include B0_def.S with type t := t

val id : t -> id
val basename : t -> string
val meta : t -> B0_meta.Pkg.t
val meta_mem : 'a B0_meta.Pkg.key -> t -> bool
val meta_find :'a B0_meta.Pkg.key -> t -> 'a option
val meta_get : 'a B0_meta.Pkg.key -> t -> 'a
val has_tag : bool B0_meta.Pkg.key -> t -> bool

(* Pkg id map and sets *)

module Idset : sig
  include Set.S with type elt := id
  val pp : ?sep:unit B0_fmt.t -> id B0_fmt.t -> t B0_fmt.t
end

module Idmap : sig
  include Map.S with type key := id
  val pp : ?sep:unit B0_fmt.t -> (id * 'a) B0_fmt.t -> 'a t B0_fmt.t
end

(* Pkg map and sets *)

type set

module Set : sig
  val pp : ?sep:unit B0_fmt.t -> t B0_fmt.t -> set B0_fmt.t
  include Set.S with type elt := t
                 and type t = set
end

type +'a map

module Map : sig
  include Map.S with type key := t
                 and type 'a t := 'a map
  val dom : 'a map -> set
  val of_list : (t * 'a) list -> 'a map
  val pp : ?sep:unit B0_fmt.t -> (t * 'a) B0_fmt.t -> 'a map B0_fmt.t
  type 'a t = 'a map
end

(* Marshalable FIXME streamline this when we move away from marshal *)

type marshalable = string * (id * (string * string) list)
val to_marshalable : t -> marshalable

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
