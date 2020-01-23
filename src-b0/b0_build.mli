(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 builds.

    Centralizes the information to run and orchestrate a B0 build. *)

open B00_std

(** {1:builds Build} *)

type t = B0_unit.build
(** The type for builds. *)

val create :
  root_dir:Fpath.t -> b0_dir:Fpath.t -> B00.Memo.t -> locked:bool ->
  B0_unit.t list -> t

val run : t -> (unit, string) result
(** [run b] runs the build. *)

val memo : t -> B00.Memo.t
(** [memo b] the build memoizer. *)

val locked : t -> bool
(** [locked b] is [true] iff [b] is a locked build. In a locked build
    build units that are built are fixed before the build starts. *)

(** {1:unit Units} *)

val units : t -> B0_unit.t list
(** [units b] are the units that are part of the build. Unless
    {!locked} is [true] this list may increase over time. Build units
    should not rely on it. *)

val require_unit : t -> B0_unit.t -> unit
(** [require_unit b u] asks to build unit [u] in [b]. This fails the [Memo]
    fiber if [b] is locked and not part of {!units}. *)

val current_unit : t -> B0_unit.t
(** [current_unit b] is [b]'s current unit. *)

(** {1:dir Directories} *)

val unit_build_dir : t -> B0_unit.t -> Fpath.t
(** [unit_build_dir b u] is the build directory for the build unit [u].
    This is where [u] should write is build artefacts. *)

val unit_root_dir : t -> B0_unit.t -> Fpath.t
(** [unit_root b u] is the directory of the B0 file in which [u] was
    defined. *)

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
