(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Build packs.

    A build pack gathers a set of build units and attaches metadata to it. *)

open B0_std

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
