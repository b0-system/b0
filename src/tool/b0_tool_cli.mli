(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line support for [b0] commands.

    {b Warning.} Consider {!B0_cli} before adding
    new functionality here. *)

open B0_std
open Cmdliner

(** {1:manpage Manpage fragments} *)

val s_scope_selection : Manpage.section_name

val man_see_manual : Manpage.block

(** {1:cmds Defining commands} *)

val cmd :
  ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
  ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
  (unit -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
(** [cmd] does not require driver configuration options or a b0 file
    it justs setups logging and the tty stuff. *)

val cmd_with_driver_conf :
  ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
  ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
  (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
(** [cmd_with_driver_conf] gives the options for a driver configuration
    value. *)

val cmd_with_b0_file_if_any :
  ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
  ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
  (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
(** [cmd_with_b0_if_any] will have a b0 file if it exists and can be
    compiled. It will still execute if non of this is true. The
    command can check {!B0_driver.has_b0_file} and
    {!B0_driver.has_failed_b0_file} to understand the status at
    runtime.  If the compilation fails a warning is automatically
    logged. *)

val cmd_with_b0_file :
  ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
  ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
  (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
(** [cmd_with_b0_file] requires a functioning b0 file. *)

val cmd_group :
  ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
  ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
  ?default:Os.Exit.t Term.t -> Os.Exit.t Cmd.t list -> Os.Exit.t Cmd.t
  (** [cmd_group] just groups without requiring anything particular. *)
