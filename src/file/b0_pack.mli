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

val make :
  ?doc:string -> ?meta:B0_meta.t -> string -> locked:bool -> B0_unit.t list -> t
(** [make n us] is a build pack named [n] made of build units [us] and
    described by [doc]. [locked] defaults to [false], see {!locked} for
    the semantics. *)

val locked : t -> bool
(** [locked] is [true] if the pack when used in a build mandates a
    locked build. *)

val units : t -> B0_unit.t list
(** [units p] are the units of [p]. *)

(** {1:meta_derivation Metadata derivation} *)

val find_default : unit -> t option
(** [find_default ()] is a default pack.

    {b FIXME} This is likely not a good idea. There's no context to
    override the default. *)

val derive_synopsis_and_description : t -> B0_meta.t -> B0_meta.t
(** [derive_synopsis_and_description p m] if {!B0_meta.synopsis} or
    {!B0_meta.description} are undefined in [m] it tries to fill them
    in from an existing [README.md] in the scope directory of [p]. The
    first marked up section of the file is extracted using
    {!B0_std.String.commonmark_first_section} its title is parsed
    according to the pattern '$(NAME) $(SEP) $(SYNOPSIS)' to get a
    synopsis line and the body up to the next (sub)section defines the
    description.

    {b XXX.} See if we couldn't define that as a default key value
    in the future. Though we'd need the def for getting the directory. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t
