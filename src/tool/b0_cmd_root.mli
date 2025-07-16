(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [root] command. *)

val output_dir : B0_driver.Conf.t -> B0_std.Os.Exit.t

val cmd : B0_std.Os.Exit.t Cmdliner.Cmd.t
(** [cmd] is the command line for [root]. *)
