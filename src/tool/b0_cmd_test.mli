(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 [test] command. *)

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [build] is the command line for [test]. *)
