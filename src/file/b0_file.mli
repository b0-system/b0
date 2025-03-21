(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** b0 file source and expansion. *)

open B0_std
open B0_text

(** {1:meta Syntactic metadata} *)

type smeta
(** Metadata attached to syntactic constructs. *)

val loc : smeta -> Tloc.t
(** [loc m] is the text source location of [i]. *)

val loc_errf : smeta -> ('a, Format.formatter, unit, string) format4 -> 'a
(** [loc_errf smeta fmt] formats an error for the location in [smeta]. The
    result should be printed as is on the TTY. *)

(** {1:srcs b0 files sources} *)

type b0_boot = (string * smeta) list
(** The type for [@@@B0.boot] directive data. The list of strings. *)

type b0_include = (string * smeta) * (Fpath.t * smeta)
(** The type for [@@@B0.include] directive data. The scope name and
    the included file. *)

type require = B0_ocaml.Libname.t * smeta
(** The type for #require directive data. The library name. *)

type mod_use = Fpath.t * smeta
(** The type for #mod_use directive data. The path to the module source. *)

type t
(** The type for b0 files sources. *)

val of_string : file:Fpath.t -> string -> (t, string) result
(** [of_string ~file s] parses a b0 file from [s]. [file] is the file
    used for locations, it must be absolute. *)

val file : t -> Fpath.t
(** [file f] is the b0 file's file. *)

val b0_boots : t -> b0_boot list
(** [b0_boots f] are the individual [@@@B0.boot] directives. *)

val b0_includes : t -> b0_include list
(** [b0_includes f] are the [@@@B0.includes] directives. The scope
    name and the included file. *)

val requires : t ->  require list
(** [requires s] are the library names of the [#require] directives. *)

val mod_uses : t -> mod_use list
(** [mod_uses s] are the file path of the [#mod_use] directives. *)

val ocaml_unit : t -> string * smeta
(** [ocaml_unit s] is the script's OCaml implementation unit. *)

val pp_dump : t Fmt.t
(** [pp_dump] dumps the parsed b0 file. *)

val pp_locs : t Fmt.t
(** [pp_locs] dumps the source text locations of [s]. *)

(** {1:expanded_srcs Expanded sources} *)

type expanded
(** The type for sources with expanded expanded directives and scoping
    instructions in place. *)

val expand : t -> (expanded, string) result
(** [expand f] expands [f]'s includes. *)

val expanded_file_manifest : expanded -> Fpath.t list
(** [expanded_file_manifest e] are all the files that contributed
    to the expansion of [e] (including the unexpanded source). *)

val expanded_b0_boots : expanded -> b0_boot list
(** [expanded_b0_boots e] are all the [@@@B0.boot]s found during expansion. *)

val expanded_b0_includes : expanded -> b0_include list
(** [expanded_b0_includes e] are the expanded file includes whose
    scope names have been qualified. *)

val expanded_requires : expanded -> require list
(** [expanded_requires e] are all the requires that were found. This
    has duplicates. *)

val expanded_src : expanded -> string
(** [expanded_src e] is the expanded source with {!B0_scope}ing
    and mod_uses spliced in instructions and the call to {!B0_driver.main}. *)
