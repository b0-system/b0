(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 builds.

    Centralizes the information to run and orchestrate a B0 build. *)

open B0_std

(** {1:builds Build} *)

type t = B0_unit.b0_build
(** The type for builds. *)

val memo : t -> B0_memo.t
(** [memo b] the memoizer for the build. *)

(** {1:units Units} *)

val must_build : t -> B0_unit.Set.t
(** [must_build b] are the units in [b] that must build. *)

val may_build : t -> B0_unit.Set.t
(** [may_build b] are all the units in [b] that may build, i.e. that
    can be {!require}d. This includes the elements in [must_build b]. *)

val require_unit : t -> B0_unit.t -> unit
(** [require_unit b u] asks to build unit [u] in [b]. This fails the
    memo if [b] is [u] is not in {!may_build}. *)

val require_units : t -> B0_unit.t list -> unit
(** [require_units b us] is [List.iter (require_unit b) us]. *)

val current : t -> B0_unit.t
(** [current b] is [b]'s current unit. In the
    {{!B0_unit.type-build_proc}build procedure} of a unit this is the
    unit itself. *)

val current_meta : t -> B0_meta.t
(** [current_meta b] is [B0_unit.meta (current b)]. *)

(** {1:dir Directories}

    {b FIXME} Unify the directory story with {!B0_action} and
    We likely want to get rid of a few of the functions below. *)

val unit_dir : t -> B0_unit.t -> Fpath.t
(** [unit_dir b u] is the unit build directory for unit [u].
    This is where [u] should write is build artefacts. *)

val unit_scope_dir : t -> B0_unit.t -> Fpath.t
(** [unit_scope_dir b u] is the directory of the B0 file in which [u] was
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

val in_unit_dir : t -> B0_unit.t -> Fpath.t -> Fpath.t
(** [in_unit_dir b u p] is [Fpath.(unit_dir b u // p)]. *)

val in_unit_scope_dir : t -> B0_unit.t -> Fpath.t -> Fpath.t
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
  may_build:B0_unit.Set.t -> must_build:B0_unit.Set.t -> t
(** [make ~root_dir ~b0_dir m ~may_build ~must_build]
    {ul
    {- [must_build] are the build units that must be build by [b].}
    {- [may_build] are the build units that may build in [b]. [must] units
        are automatically added to this set.}} *)

val run : t -> (unit, unit) result
(** [run b] runs the build. *)

val did_build : t -> B0_unit.Set.t
(** [did_build b] are the units that did build. This is meaningless before
    {!val-run} has finished. *)
