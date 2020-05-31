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

val memo : t -> B00.Memo.t
(** [memo b] the memoizer for the build. *)

val store : t -> B00.Store.t
(** [store b] is the store for the build. Note that [b] itself
    can be found in store via the {!current} key. *)

val shared_build_dir : t -> Fpath.t
(** [shared_build_dir] is a build directory shared by all units of the
     build. This is used by computations shared by units. Most of the
     time one should use {!Unit.build_dir}. *)

val current_root_dir : t -> Fpath.t
(** [current_root_dir b] is the directory of the B0 file in which
    the {{!Unit.current}current} unit is defined, otherwise said this is
    {!Unit.root_dir}[ b ]{!Unit.current}. *)

val get : t -> 'a B00.Store.key -> 'a Fut.t
(** [get b k] is {!B00.Store.get}[ (store b) k]. *)

(** Built units. *)
module Unit : sig

  (** {1:ops Unit operations} *)

  val current : t -> B0_unit.t
  (** [current b] is [b]'s current unit. In the {{!B0_unit.type-proc}procedure}
      of a build unit this is the unit itself. *)

  val must_build : t -> B0_unit.Set.t
  (** [must_build b] are the units in [b] that must build. *)

  val may_build : t -> B0_unit.Set.t
  (** [may_build b] are all the units in [b] that may build, i.e. that
      can be {!required}. This includes the elements in [must_build b]. *)

  val require : t -> B0_unit.t -> unit
  (** [require b u] asks to build unit [u] in [b]. This fails the
      memo if [b] is [u] is not in {!may_build}. *)

  (** {1:dir Directories} *)

  val build_dir : t -> B0_unit.t -> Fpath.t
  (** [build_dir b u] is the build directory for the build unit [u].
      This is where [u] should write is build artefacts. *)

  val root_dir : t -> B0_unit.t -> Fpath.t
  (** [root_dir b u] is the directory of the B0 file in which [u] was
      defined. This is were unit relative paths like source files
      should be resolved. *)
end

(** {1:run Creating and running} *)

val create :
  root_dir:Fpath.t -> b0_dir:Fpath.t -> B00.Memo.t -> may_build:B0_unit.Set.t ->
  must_build:B0_unit.Set.t -> t
(** [create ~root_dir ~b0_dir m ~may_build ~must_build]
    {ul
    {- [must_build] are the build units that must be build by [b].}
    {- [may_build] are the build units that may build in [b]. [must] units
        are automatically added to this set.}} *)

val run : t -> (unit, unit) result
(** [run b] runs the build. *)

val current : t B00.Store.key
(** [current] is a store key that holds the build itself. The store
    returned by {!store} has this key bound to the build. *)

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
