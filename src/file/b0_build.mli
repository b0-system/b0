(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Builds.

    Centralizes the information to run and orchestrate a b0 build. *)

open B0_std

(** {1:builds Build} *)

type b0_unit = B0_defs.b0_unit
(** The type for build units. See {!B0_unit.t}. *)

type b0_unit_set = B0_defs.Unit.Set.t
(** The type for sets of build units. See {!B0_unit.Set.t}. *)

type t = B0_defs.b0_build
(** The type for builds. *)

val memo : t -> B0_memo.t
(** [memo b] the memoizer for the build. *)

(** {1:units Units} *)

val must_build : t -> b0_unit_set
(** [must_build b] are the units in [b] that must build. *)

val may_build : t -> b0_unit_set
(** [may_build b] are all the units in [b] that may build, i.e. that
    can be {!require}d. This includes the elements in [must_build b]. *)

val require_unit : t -> b0_unit -> unit
(** [require_unit b u] asks to build unit [u] in [b]. This fails the
    memo if [b] is [u] is not in {!may_build}. *)

val require_units : t -> b0_unit list -> unit
(** [require_units b us] is [List.iter (require_unit b) us]. *)

val current : t -> b0_unit
(** [current b] is [b]'s current unit. In the
    {{!b0_unit.type-build_proc}build procedure} of a unit this is the
    unit itself. *)

val current_meta : t -> B0_meta.t
(** [current_meta b] is [B0_unit.meta (current b)]. *)

(** {1:dir Directories}

    {b FIXME} Unify the directory story with {!B0_action} and
    We likely want to get rid of a few of the functions below. *)

val unit_dir : t -> b0_unit -> Fpath.t
(** [unit_dir b u] is the unit build directory for unit [u].
    This is where [u] should write is build artefacts. *)

val unit_scope_dir : t -> b0_unit -> Fpath.t
(** [unit_scope_dir b u] is the directory of the b0 file in which [u] was
    defined. This is were unit relative paths like source files
    should be resolved. *)

val current_dir : t -> Fpath.t
(** [current_dir b] is [unit_dir b current]. *)

val scope_dir : t -> Fpath.t
(** [scope_dir b] is [unit_scope_dir b current]. *)

val shared_dir : t -> Fpath.t
(** [shared_dir b] is a build directory shared by all units of the
     build. This is used by computations shared by units, most of the
     time one should rather use {!current_dir}. *)

(** {1:rel Relative file resolution} *)

val in_unit_dir : t -> b0_unit -> Fpath.t -> Fpath.t
(** [in_unit_dir b u p] is [Fpath.(unit_dir b u // p)]. *)

val in_unit_scope_dir : t -> b0_unit -> Fpath.t -> Fpath.t
(** [in_unit_scope_dir b u p] is [Fpath.(unit_scope_dir b u // p)]) *)

val in_current_dir : t -> Fpath.t -> Fpath.t
(** [in_current_dir b p] is [Fpath.(current_dir b // p)]). *)

val in_scope_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir b p] is [Fpath.(scope_dir b // p)]). *)

val in_shared_dir : t -> Fpath.t -> Fpath.t
(** [in_shared_dir b p] is [Fpath.(shared_dir b // p)]). *)

(** {1:store Store} *)

val store : t -> B0_store.t
(** [store b] is the store for the build. Note that [b] itself
    can be found in store via the {!self} key. *)

val get : t -> 'a B0_store.key -> 'a Fut.t
(** [get b k] is {!B0_store.get}[ (store b) k]. *)

val self : t B0_store.key
(** [self] is a store key that holds the build itself. The store
    returned by {!val-store} has this key bound to the build. *)

(** {1:run Creating and running} *)

val make :
  root_dir:Fpath.t -> b0_dir:Fpath.t -> variant:string ->
  store:B0_store.binding list -> B0_memo.t ->
  may_build:b0_unit_set -> must_build:b0_unit_set -> t
(** [make ~root_dir ~b0_dir m ~may_build ~must_build]
    {ul
    {- [must_build] are the build units that must be build by [b].}
    {- [may_build] are the build units that may build in [b]. [must] units
        are automatically added to this set.}} *)

val run : t -> (unit, unit) result
(** [run b] runs the build. *)

val did_build : t -> b0_unit_set
(** [did_build b] are the units that did build. This is meaningless before
    {!val-run} has finished. *)


(** {1:b0_dir [_b0] directory access} *)

module B0_dir : sig

  (** [_b0] directory structured access.

    {b FIXME.} Try to get rid of this. *)

  (** {1:build Builds} *)

  val build_dir : b0_dir:Fpath.t -> variant:string -> Fpath.t
  (** [build_dir ~b0_dir ~variant] is the designated directory
      for the build variant [variant] in [b0_dir]. *)

  val shared_build_dir : build_dir:Fpath.t -> Fpath.t
  (** [shared_build_dir ~build_dir] is the shared directory of [build_dir]
      obtained via {!build_dir}. *)

  val store_dir : build_dir:Fpath.t -> Fpath.t
  (** [store_dir ~build_dir] is the store directory of [build_dir] obtained
      via [!build_dir]. *)

  val unit_build_dir : build_dir:Fpath.t -> name:string -> Fpath.t

  (** {1:other Other} *)

  val scratch_dir : b0_dir:Fpath.t -> Fpath.t
  (** [scratch_dir b0_dir] can be used by cmdlets. *)
end
