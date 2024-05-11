(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 [build] command. *)

val select_units :
  units:string list -> x_units:string list ->
  packs:string list -> x_packs:string list ->
  lock:bool option ->
  ((B0_unit.Set.t * B0_unit.Set.t) * bool * B0_pack.t list, string) result

val make_build :
  B0_driver.Conf.t ->
  store:B0_store.binding list ->
  may_build:B0_unit.Set.t ->
  must_build:B0_unit.Set.t -> (B0_build.t, string) result

val show_what :
  lock:bool option ->
  is_locked:bool ->
  locked_packs:B0_pack.t list ->
  must_build:B0_unit.Set.t ->
  may_build:B0_unit.Set.t -> B0_driver.Conf.t -> (B0_std.Os.Exit.t, 'a) result

val executor_env :
  B0_build.t -> B0_def.t -> B0_driver.Conf.t -> B0_env.t

val memo :
  B0_driver.Conf.t ->
  may_build:B0_unit.Set.t -> must_build:B0_unit.Set.t ->
  (B0_memo.t, string) result

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
