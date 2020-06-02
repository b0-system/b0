(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build packs.

    A build pack gathers a set of build units and attaches metadata to it. *)

open B00_std

(** {1:packs Packs} *)

type t
(** The type for build units. *)

val v :
  ?doc:string -> ?meta:B0_meta.t -> string -> locked:bool -> B0_unit.t list -> t
(** [v n us] is a build pack named [n] made of build units [us] and
    described by [doc]. [locked] defaults to [false], see {!locked} for
    the semantics. *)

val locked : t -> bool
(** [locked] is [true] if the pack when used in a build mandates a
    locked build. *)

val units : t -> B0_unit.t list
(** [units p] are the units of [p]. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
