(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [scope] command. *)

val vcs : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [vcs] is the [b0 vcs] command. *)

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [scope]. *)
