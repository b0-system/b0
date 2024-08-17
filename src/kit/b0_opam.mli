(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://opam.ocaml.org/}[opam]} support.

    See the b0 {{!page-opam}b0 opam manual} for more details. *)

open B0_std


(** {1:opam [opam]} *)

type t
(** The type for opam. *)

val get : ?search:Cmd.tool_search -> ?cmd:Cmd.t -> unit -> (t, string) result
(** [get ~search cmd ()] looks for the opam command [cmd]
    (defaults to [Cmd.tool "opam"]) in [search] (defaults to
    [Os.Cmd.get ?search]). *)

(** {1:file [opam] files} *)

(** [opam] file generation. *)
module File : sig

  type opam := t

  (** {1:gen Generic representation} *)

  type value =
    [ `Raw of string (** A raw, unescaped value *)
    | `B of bool (** A boolean. *)
    | `S of string (** A string. *)
    | `L of bool * value list (** A list of values, the boolean indicates
                                  whether line-by-line rendering should be
                                  forced. *)
    ]
  (** The type for opam values. *)

  type field = string * value
  (** The type for opam fields. The field name and its value. *)

  type section = string * string option * t
  (** The type for opam sections. The section name, the optional string and
      the contents of the section. *)

  and comment = string
  (** The type for comments. *)

  and item = [ `Comment of comment | `Field of field | `Section of section ]
  (** The type for items. *)

  and t = item list
  (** The type for generic opam file contents. *)

  val v2 : item
  (** [v2] is [opam-version: "2.0"]. *)

  val to_string : opam -> normalize:bool -> t -> (string, string) result
  (** [to_string] formats file contents to a string. If [normalize]
      is [true] this calls [opam] to lint and normalize the result. *)

  (** {1:package_files Package files} *)

  val pkg_of_meta : with_name:bool -> B0_meta.t -> t
  (** [pkg_of_meta ~with_name m] is an [opam] package file from
      [m]. Here's an account of how opam fields are populated by
      metadata keys.
      {ul
      {- ["authors:"], {!B0_meta.authors}.}
      {- ["available:"], {!B0_opam.available}.}
      {- ["build:"], {!B0_opam.build}.}
      {- ["bug-report:"], {!B0_meta.issues}.}
      {- ["conflicts:"], {!B0_opam.conflicts}.}
      {- ["description:"], {!B0_meta.description}.}
      {- ["depends:"], {!B0_opam.depends}.}
      {- ["depopts:"], {!B0_opam.depopts}.}
      {- ["dev-repo:"], {!B0_meta.repo}.}
      {- ["doc:"], {!B0_meta.online_doc}.}
      {- ["homepage:"], {!B0_meta.homepage}.}
      {- ["install:"], {!B0_opam.install}.}
      {- ["license:"], {!B0_meta.licenses}.}
      {- ["maintainer:"], {!B0_meta.maintainers}.}
      {- ["name:"], {!B0_opam.name} iff [with_name] is [true].}
      {- ["synopsis:"], {!B0_meta.synopsis}.}
      {- ["tags:"], {!B0_meta.description_tags}.}}
      Finally the contents of {!B0_opam.file_addendum} is appended
      after the definition of these fields.

      See {!B0_opam.pkg_meta_of_pack} for deriving metadata from build
      packs. *)
end

(** {1:metadata Metadata}

    These metadata keys can be specified to define opam file fields.
    Some of the metadata is covered by {{!B0_meta.std}standard keys}.
    The full map is documented in {!File.pkg_of_meta}. *)

type pkg_spec = string * string
(** The type for package specifications. A package name and a
    {{:http://opam.ocaml.org/doc/Manual.html#Filtered-package-formulas}
    filtered package formula}, use [""] if you don't have any constraint. *)

val tag : bool B0_meta.key
(** [tag] indicates the entity is related to [opam]. Adding this tag
    to a pack makes it represent an opam package for b0's opam tooling. *)

val available : string B0_meta.key
(** [available] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-available}
    [available:]} field value. This is a raw string in opam sntax
    that defines the whole field. *)

val build : string B0_meta.key
(** [build] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-build}
    [build:]} field value. This is a raw string in
    opam syntax that defines the whole field. Used to override
    automatic opam file generation, see {!pkg_meta_of_pack}.  *)

val depends : pkg_spec list B0_meta.key
(** [depends] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-depends}
    [depends:]} field value. Used to override automatic opam file
    dependency generation, see {!pkg_meta_of_pack}. *)

val depopts : pkg_spec list B0_meta.key
(** [depopts] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-depopts}
    [depopts:]} field value. *)

val conflicts : pkg_spec list B0_meta.key
(** [conflicts] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-conflicts}
    [conflicts:]} field value. *)

val file_addendum : File.t B0_meta.key
(** [file_addendum] is an [opam] file fragment appended at the end
    of a generated [opam] file. See {!B0_opam.File.pkg_of_meta}. *)

val install : string B0_meta.key
(** [install] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-install}
    [install:]} field value. This is a raw
    string in opam syntax that defines the whole field. Used to
    override automatic opam file generation, see {!pkg_meta_of_pack}. *)

val name : string B0_meta.key
(** [name] is an [opam]
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-name}
    [name:]} field value. Use to override
    automatic [opam] package name generation, see {!pkg_meta_of_pack}. *)

val pin_depends : (string * string) list B0_meta.key
(** [pin_depends] is an opam
    {{:https://opam.ocaml.org/doc/Manual.html#opamfield-pindepends}
    [pin-depends:]} field value. *)

(** {1:pkg_derivation Package derivation} *)

val pkg_name_of_pack : B0_pack.t -> string
(** [pkg_name_of_pack p] derives an opam package name for [p].
    This is either in order:
    {ol
    {- The {!name} field of [p]'s meta, if defined.}
    {- The {!B0_pack.basename} of [p] if not equal to ["default"].}
    {- The basename of [p]'s scope directory.}} *)

val pkg_meta_of_pack : B0_pack.t -> B0_meta.t
(** [pkg_meta_of_pack p] is [opam] package metadata for pack [p] ready
    to be used with {!File.pkg_of_meta} to derive an opam package
    file.

    This is [p]'s metadata with the following fields added if they
    are unspecified:
    {ul
    {- {!name}, the value of {!B0_opam.pkg_name_of_pack}[ p].}
    {- {!B0_meta.synopsis} and {!B0_meta.description}. The fields
       are tentatively derived using
       {!B0_pack.derive_synopsis_and_description}.}
    {- {!build}, a locked b0 build of the pack is defined.}
    {- {!depends}, we collect the OCaml libraries required by units
       in the pack, derive a package names out of them.
       {b FIXME.} This is the poc but it's
       not workable for now we don't have enough metadata in library
       names.}} *)

(** {1:unit [.opam] unit} *)

val unit : B0_unit.t
(** [unit] is the unit of the [.opam] action.

    See [b0 -- .opam --help] and the {{!page-opam}b0 [opam] manual}
    for more information. *)
