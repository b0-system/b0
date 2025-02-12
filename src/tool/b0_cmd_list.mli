(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [list] command. *)

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [list]. *)

val cmd_show : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [cmd_show] is the command line for [show]. *)

val cmd_edit : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [cmd_edit] is the commandline for [edit]. *)
