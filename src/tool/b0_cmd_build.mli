(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 [build] command. *)

val memo :
  B0_driver.Conf.t ->
  may_build:B0_unit.Set.t -> must_build:B0_unit.Set.t ->
  (B0_memo.Memo.t, string) result

val term : B0_std.Os.Exit.t Cmdliner.Term.t
(** [term] is the command term for [build]. *)

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [build] is the command line for [build]. *)
