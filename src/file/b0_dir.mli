(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [_b0] directory structured access. *)

open B0_std

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
