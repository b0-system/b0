(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line interface fragments and logic. *)

open B0_std
open Cmdliner

(** {1:conv Converters} *)

val def_conv : (module B0_def.S) -> string Cmdliner.Arg.Conv.t
(** [def_conv d] is a converter for definitions of type [d] which knows
    how to complete them. *)

(** {1:predef Predefined} *)

val output_details : B0_std_cli.output_details Term.t
(** [output_details] is an invocation of {!B0_std_cli.val-output_details}. *)

val log_format : B0_memo_cli.Log.format Term.t
(** [log_format] is an invocation of {!B0_memo_cli.Log.format}. *)

val memo_op_query : B0_memo_cli.Op.query Term.t
(** [memo_op_query] is an invocation of {!B0_memo_cli.Op.query_cli}. *)

val no_pager : bool Cmdliner.Term.t
(** [no_pager] is an invocation of {!B0_pager.don't}. *)

(** {1:def_spec Specifying units and packs } *)

val get_excluded_units :
  x_units:string list -> x_packs:string list -> (B0_unit.Set.t, string) result
(** [get_excluded_units ~x_units ~x_packs] is the set of units excluded
    by the given unit and pack names. *)

(** The following use options and the default doc
    string is generic of the form "use unit". *)

val use_units :
  ?docs:string -> ?doc:string -> unit -> string list Term.t
(** [units] defines unit names with [-u] and [--unit]. *)

val use_x_units :
  ?docs:string -> ?doc:string -> unit -> string list Term.t
(** [x_units] defines unit names to exclude with [-x] and [--x-unit]. *)

val use_packs :
  ?docs:string -> ?doc:string -> unit -> string list Term.t
(** [packs] defines pack names with [-p] and [--pack]. *)

val use_x_packs :
  ?docs:string -> ?doc:string -> unit -> string list Term.t
  (** [packs] defines pack names to exclude via [-X] and [--x-pack]. *)

(** The following uses the above functions but with a doc
    string that indicates that units and packs are selected for build. *)

val build_units : string list Term.t
(** [units] is {!use_units}. *)

val build_x_units : string list Term.t
(** [units] is {!use_x_units}. *)

val build_packs : string list Term.t
(** [build_packs] is {!use_packs}. *)

val build_x_packs : string list Term.t
(** [build_x_packs] is {!use_x_packs}. *)

(** The following ones are positional arguments. The doc string
    is generic of the form "unit to act on, all of them if unspecified".*)

val act_on_units_posn :
  ?doc:string -> first:int -> unit -> string list Term.t
(** [act_on_units_posn ~first ()] defines units to act on at
    position [first]. *)

val act_on_units_pos0 : string list Term.t
(** [act_on_units_pos0] is [act_on_units ~first:0 ()]. *)

val act_on_units_pos1 : string list Term.t
(** [act_on_units_pos1] is [act_on_units ~first:1 ()]. *)

val act_on_packs_posn :
  ?doc:string -> first:int -> unit -> string list Term.t
(** [act_on_packs_posn ~first ()] defines packs to act on at
    position [first]. *)

val act_on_packs_pos0 : string list Term.t
(** [act_on_packs_pos0] is [act_on_packs ~first:0 ()]. *)

val act_on_packs_pos1 : string list Term.t
(** [act_on_packs_pos1] is [act_on_packs ~first:1 ()]. *)

(** {1:b0_keys Metadata keys} *)

val required_metadata_key_pos0 : string Cmdliner.Term.t

(** {1:b0_dir B0 directory}

    Structured access to [_b0] currently lives in [B0_build.B0_dir]. *)

val get_b0_dir :
  cwd:Fpath.t -> root:Fpath.t -> b0_dir:Fpath.t option -> Fpath.t
(** [get_b0_dir ~cwd ~root ~b0_dir] determines a b0 directory. If
    [b0_dir] is [Some d] then this is [Fpath.(cwd // d)]. If [None]
    then this is [Fpath.(root / b0_dir_name)]. *)

val get_cache_dir :
    cwd:Fpath.t -> b0_dir:Fpath.t -> cache_dir:Fpath.t option -> Fpath.t
(** [get_cache_dir ~cwd ~b0_dir ~cache_dir] determines a cache directory.
    If [cache_dir] is [Some d] then this is [Fpath.(cwd // d)]. If [None]
    then this is [Fpath.(b0_dir / cache_dir)]. *)

val find_dir_with_b0_dir : start:Fpath.t -> Fpath.t option
(** [find_dir_with_b0_dir ~start] finds the first directory starting
    with [start] that has a {!b0_dir_name} directory. [None] is
    returned if none could found or if [start] is relative. *)

val b0_dir :
  ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
  ?env:Cmd.Env.info -> unit -> Fpath.t option Term.t
(** [b0_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 directory.
      {ul
      {- [opts] are the cli options to specify it, defaults to [["b0-dir"]].}
      {- [docs] is where the option is documented, defaults to
         {!Cmdliner.Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!b0_dir_env}.}} *)

val b0_dir_var : Cmd.Env.info
(** [b0_dir_env] is ["B0_DIR"]. *)

val b0_dirname : string
(** [b0_dir_name] is ["_b0"] the default b0 directory name. *)
