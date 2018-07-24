(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build units. See {!B0.Unit}.

    Build units are associated with their build function only at the
    API level in B0.ml. This allows to cut an invasive recursive
    definition among build types that stems from build units being
    used both at a high (build function taking a B0_build.t) and low
    level (identification and synchronization in B0_op and
    B0_guard). *)

type t
type id = int

val create :
  ?loc:B0_def.loc -> ?src_root:B0_fpath.t ->
  ?doc:string -> ?doc_outcome:string -> ?only_aim:B0_conf.build_aim ->
  ?pkg:B0_pkg.t -> ?meta:B0_meta.Unit.t -> string -> t

val nil : t
val src_root : t -> B0_fpath.t
val id : t -> id
include B0_def.S with type t := t

val basename : t -> string
val doc_outcome : t -> string
val only_aim : t -> B0_conf.build_aim option
val meta : t -> B0_meta.Unit.t
val meta_mem : 'a B0_meta.Unit.key -> t -> bool
val meta_add : 'a B0_meta.Unit.key -> 'a -> t -> unit
val meta_find :'a B0_meta.Unit.key -> t -> 'a option
val meta_get : 'a B0_meta.Unit.key -> t -> 'a
val has_tag : bool B0_meta.Unit.key -> t -> bool
val pkg : t -> B0_pkg.t option

(* Unit build state *)

type build_state
type build_status =
| Active | Failed of unit B0_fmt.t
| Aborted (* Happens if depends on a failed unit. *)
| Finished

val build_state_create : t -> build_state
val build_state_unit : build_state -> t
val build_state_status : build_state -> build_status
val build_state_set_status : build_state -> build_status -> unit
val build_state_activity : build_state -> int
val build_state_incr_activity : build_state -> unit
val build_state_decr_activity : build_state -> unit

(* Unit id map and sets *)

module Idset : sig
  include Set.S with type elt := id
  val pp : ?sep:unit B0_fmt.t -> id B0_fmt.t -> t B0_fmt.t
end

module Idmap : sig
  include Map.S with type key := id
  val pp : ?sep:unit B0_fmt.t -> (id * 'a) B0_fmt.t -> 'a t B0_fmt.t
end

(* Unit map and sets *)

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
