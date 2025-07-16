(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [b0] driver common definitions. *)

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
