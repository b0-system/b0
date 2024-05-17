(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 [build] command. *)

val get_default_build : unit -> B0_unit.t list * B0_pack.t list
val unit_set_of :
  units:B0_pack.b0_unit list -> packs:B0_pack.Set.t -> B0_unit.Set.t

val get_excluded_units :
  x_units:string list -> x_packs:string list -> (B0_unit.Set.t, string) result

val get_must_units_and_locked_packs :
  is_action:(B0_unit.t -> bool) -> units:B0_unit.t list ->
  packs:B0_pack.Set.elt list -> args:string list ->
  unit -> B0_store.binding list * B0_unit.Set.t * B0_pack.Set.t

val is_locked : lock:bool option -> locked_packs:B0_pack.Set.t -> bool

val get_may_must :
  is_locked:bool -> units:B0_unit.Set.t -> x_units:B0_unit.Set.t ->
  B0_unit.Set.t * B0_unit.Set.t

val make_build :
  B0_driver.Conf.t ->
  store:B0_store.binding list ->
  may_build:B0_unit.Set.t ->
  must_build:B0_unit.Set.t -> (B0_build.t, string) result

val show_what :
  lock:bool option -> is_locked:bool -> locked_packs:B0_pack.Set.t ->
  must_build:B0_unit.Set.t -> may_build:B0_unit.Set.t -> B0_driver.Conf.t ->
  (B0_std.Os.Exit.t, 'a) result

val env_for_unit : B0_driver.Conf.t -> B0_build.t -> B0_unit.t -> B0_env.t

val units : string list Cmdliner.Term.t
val x_units : string list Cmdliner.Term.t
val packs : string list Cmdliner.Term.t
val x_packs : string list Cmdliner.Term.t
val lock : bool option Cmdliner.Term.t
val what : bool Cmdliner.Term.t

val term : (B0_driver.Conf.t -> B0_std.Os.Exit.t) Cmdliner.Term.t
(** [term] is the command term for [build]. *)

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [build] is the command line for [build]. *)
