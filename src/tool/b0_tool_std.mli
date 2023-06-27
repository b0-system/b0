(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [b0] driver common definitions.

    FIXME cleanup and move to [B0_cli], cmdlets may be interested. *)

open B0_std

val driver : B0_driver.t
(** [driver] is the driver definition. *)

(** {!B0_def} generic support.

    Generic implementation of a few standard commands we need for
    B0 defintions. *)
module Def : sig

  val list :
    (module B0_def.S) -> B0_driver.Conf.t -> B0_cli.output_format ->
    string list -> B0_std.Os.Exit.t
  (** [list (module Def) c details ns] lists definition [Def] named
      [ns] with details [details]. If [ns] is empty all definitions
      are listed. *)

  val edit :
    (module B0_def.S) -> B0_driver.Conf.t -> string list ->
    B0_std.Os.Exit.t
  (** [edit (module Def) c ns] edits the B0 files which define [Def]s
      named [ns]. If [ns] is empty all the B0 files that have
      definitions of kind [Def] are edited. *)

  val get_meta_key :
    (module B0_def.S) -> B0_driver.Conf.t -> B0_cli.output_format ->
    string -> string list -> B0_std.Os.Exit.t
  (** [get (module Def) k ns] gets key [k] in the metadata of
      definitions named [ns] with details [details]. If [ns] is empty
      all definitions are listed. *)
end

module Cli : sig
  open Cmdliner

  val man_see_manual : Manpage.block
  val editor_envs : Cmd.Env.info list
  val pager_envs : Cmd.Env.info list
  val format : [ `Long | `Normal | `Short ] Term.t
  val pos_key : string Term.t

  val s_scope_selection : string

  val subcmd_with_b0_file :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t

  val subcmd_with_driver_conf :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t

  val cmd_group_with_b0_file :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block ->  string -> doc:string -> descr:Manpage.block ->
    ?default:(B0_driver.Conf.t -> Os.Exit.t) Term.t ->
    Os.Exit.t Cmd.t list -> Os.Exit.t Cmd.t

  val cmd_group_with_driver_conf :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    ?default:(B0_driver.Conf.t -> Os.Exit.t) Term.t ->
    Os.Exit.t Cmd.t list -> Os.Exit.t Cmd.t
end