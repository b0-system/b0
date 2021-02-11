(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 [opam] support.

    See the {{!page-opam}B0 [opam] manual} for more details. *)

open B00_std

(** {1:tool [opam] tool} *)

val get_cmd :
  ?search:Fpath.t list -> ?cmd:B00_std.Cmd.t -> unit ->
  (Cmd.t, string) result
(** [get_cmd ()] looks for [opam] wih {!B00_std.Os.Cmd.get}. *)

(** {1:file [opam] files} *)

(** [opam] file generation. *)
module File : sig

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

  (** {1:formatting Formatting} *)

  val pp : t Fmt.t
  (** [pp] formats file contents. *)

  val to_string : t -> string
  (** [to_string] formats file contents to a string. *)

  (** {1:package_files Package files} *)

  val pkg_of_meta : with_name:bool -> B0_meta.t -> t
  (** [pkg_of_meta ~with_name m] is an [opam] package file from
      [m]. Here's an account of how opam fields are populated by
      metadata keys.
      {ul
      {- ["authors:"], {!B0_meta.authors}.}
      {- ["build:"], {!B0_opam.Meta.build}.}
      {- ["bug-report:"], {!B0_meta.issues}.}
      {- ["conflicts:"], {!B0_opam.Meta.conflicts}.}
      {- ["description:"], {!B0_meta.description}.}
      {- ["depends:"], {!B0_opam.Meta.depends}.}
      {- ["depopts:"], {!B0_opam.Meta.depopts}.}
      {- ["dev-repo:"], {!B0_meta.repo}.}
      {- ["doc:"], {!B0_meta.online_doc}.}
      {- ["homepage:"], {!B0_meta.homepage}.}
      {- ["install:"], {!B0_opam.Meta.install}.}
      {- ["license:"], {!B0_meta.licenses}.}
      {- ["maintainer:"], {!B0_meta.maintainers}.}
      {- ["name:"], {!B0_opam.Meta.name} iff [with_name] is [true].}
      {- ["synopsis:"], {!B0_meta.synopsis}.}
      {- ["tags:"], {!B0_meta.description_tags}.}}
      Finally the contents of {!B0_opam.Meta.file_addendum} is appended
      after the definition of these fields.

      See {!Meta.pkg_of_pack} for
      deriving metadata from build packs. *)
end

(** {1:meta [opam] Metadata} *)

val tag : unit B0_meta.key
(** [tag] indicates the entity is related to [opam]. *)

val pkg_name_of_pack : B0_pack.t -> string
(** [pkg_name_of_pack p] derives an opam package name for [p].
    This is either in order:
    {ol
    {- The {!Meta.name} field of [p]'s meta, if defined.}
    {- The {!B0_pack.basename} of [p] if not equal to ["default"].}
    {- The basename of [p]'s scope directory.}} *)

(** [opam] metadata.

    Some of the metadata is covered by {{!B0_meta.std}standard keys}. *)
module Meta : sig

  type pkg_spec = string * string
  (** The type for package specifications. A package name and
      a
      {{:http://opam.ocaml.org/doc/Manual.html#Filtered-package-formulas}
      filtered package formula}, use [""] if you don't have any constraint. *)

  val build : string B0_meta.key
  (** [build] is an opam [build:] field value. This is a raw string in
      opam syntax that defines the whole field. Used to override
      automatic opam file generation, see {!pkg_of_pack}.  *)

  val depends : pkg_spec list B0_meta.key
  (** [depends] is an opam [depends:] field value. Used to override
      automatic opam file dependency generation, see
      {!pkg_of_pack}. *)

  val depopts : pkg_spec list B0_meta.key
  (** [depopts] is an opam [depopts:] field value. *)

  val conflicts : pkg_spec list B0_meta.key
  (** [conflicts] is an opam [conflicts:] field value. *)

  val file_addendum : File.t B0_meta.key
  (** [file_addendum] is an [opam] file fragment appended at the end
      of a generated [opam] file. See {!B0_opam.File.pkg_of_meta}. *)

  val install : string B0_meta.key
  (** [install] is an opam [install:] field value. This is a raw
      string in opam syntax that defines the whole field. Used to
      override automatic opam file generation, see {!pkg_of_pack}. *)

  val name : string B0_meta.key
  (** [name] is an [opam] [name:] field value. Use to override
      automatic [opam] package name generation, see {!pkg_of_pack}. *)

  (** {1:pkg_derivation Package derivation} *)

  val pkg_of_pack : B0_pack.t -> B0_meta.t
  (** [pkg_of_pack p] is [opam] package metadata for pack [p] ready
      to be used with {!File.pkg_of_meta} to derive an opam package
      file.

      This is [p]'s metadata with the following fields added if they
      are unspecified:
      {ul
      {- {!name}, the value of {!B0_opam.pkg_name_of_pack}[ p].}
      {- {!B0_meta.synopsis} and {!B0_meta.description}. The fields
         are tentatively derived from an existing [README.md] in the
         {{!page-manual.scope_dir} scope directory} of [p]. The first
         marked up section of the file is extracted, its title is
         parsed according to the pattern '$(NAME) $(SEP) $(SYNOPSIS)'
         to get a synopsis line and the body up to the next (sub)section
         defines the description.}
      {- {!build}, a locked b0 build of the pack is defined.}
      {- {!depends}, we collect the OCaml libraries required by units
         in the pack, derive a package names out of them.
         {b FIXME.} This is the poc but it's
         not workable for now we don't have enough metadata in library
         names.}} *)
end

(** {1:cmdlets Cmdlets} *)

(** [.opam.*] cmdlets.

   See the {!page-b0_opam} manual and:
{v
b0 cmd -- .opam.list --help
b0 cmd -- .opam.file --help
b0 cmd -- .opam.publish --help
v}
  for more information.
 *)
module Cmdlet : sig

  val file : B0_cmdlet.t
  (** [file] is the [.opam.file] cmdlet. *)

  val list : B0_cmdlet.t
  (** [list] is the [.opam.list] cmdlet. *)

  val publish : B0_cmdlet.t
  (** [publish] is the [.opam.publish] cmdlet. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
