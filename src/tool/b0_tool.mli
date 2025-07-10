(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [b0] driver common definitions.

    FIXME cleanup and move to [B0_cli], cmdlets may be interested. *)

open B0_std

val driver : B0_driver.t
(** [driver] is the driver definition. *)

val def_list : (module B0_def.S) list
(** [def_list] is the list of kind of b0 definitions. *)

val def_list_list : (module B0_def.S) list -> B0_def.value list
(** [def_list_list defs] is the list of definitions of [def] order
    first by name then by kind. *)

val def_list_get_list_or_hint :
    (module B0_def.S) list -> all_if_empty:bool -> string list ->
    (B0_def.value list, string) result

(** {!B0_def} generic support.

    Generic implementation of a few standard commands we need for
    b0 defintions. *)
module Def : sig

  val list :
    (module B0_def.S) -> B0_driver.Conf.t -> B0_std_cli.output_details ->
    string list -> B0_std.Os.Exit.t
  (** [list (module Def) c details ns] lists definition [Def] named
      [ns] with details [details]. If [ns] is empty all definitions
      are listed. *)

  val edit :
    (module B0_def.S) -> B0_driver.Conf.t -> string list ->
    B0_std.Os.Exit.t
  (** [edit (module Def) c ns] edits the b0 files which define [Def]s
      named [ns]. If [ns] is empty all the b0 files that have
      definitions of kind [Def] are edited. *)

  val get_meta_key :
    (module B0_def.S) -> B0_driver.Conf.t -> B0_std_cli.output_details ->
    string -> string list -> B0_std.Os.Exit.t
  (** [get (module Def) k ns] gets key [k] in the metadata of
      definitions named [ns] with details [details]. If [ns] is empty
      all definitions are listed. *)
end

(** {1:cli Cli} *)

module Cli : sig
  open Cmdliner



  val man_see_manual : Manpage.block
  val editor_envs : Cmd.Env.info list

  val output_details : [ `Long | `Normal | `Short ] Term.t
  val log_format : B0_cli.Memo.Log.format Term.t
  val op_query : B0_cli.Op.query Term.t

  val pos_key : string Term.t

  val no_pager : bool Term.t
  (** N.B. only useful for {!subcmd}, it's already in the driver conf. *)

  val s_scope_selection : string

  val subcmd :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    (unit -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
  (** [subcmd] does not require driver configuration options or a b0 file
      it justs setups logging and the tty stuff. *)

  val subcmd_with_driver_conf :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
  (** [subcmd_with_driver_conf] gives the options for a driver
      configuration value. *)

  val subcmd_with_b0_file_if_any :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
  (** [subcmd_with_b0_if_any] will have a b0 file if it exists and can
      be compiled. It will still execute if non of this is true.
      The command can check {!B0_driver.has_b0_file} and
      {!B0_driver.has_failed_b0_file} to understand the status at runtime.
      If the compilation fails a warning is automatically logged. *)

  val subcmd_with_b0_file :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    (B0_driver.Conf.t -> Os.Exit.t) Term.t -> Os.Exit.t Cmd.t
  (** [subcmd_with_b0_file] requires a functioning b0 file. *)

  val cmd_group :
    ?exits:Cmd.Exit.info list -> ?envs:Cmd.Env.info list ->
    ?synopsis:Manpage.block -> string -> doc:string -> descr:Manpage.block ->
    ?default:Os.Exit.t Term.t -> Os.Exit.t Cmd.t list -> Os.Exit.t Cmd.t
  (** [cmd_group] just groups without requiring anything particular. *)
end
