(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Generate files from templates.

    Exposes, functions initially used by [b0 scaffold].

    Likely needs a few design rounds. Some of the project lookup
    stuff could be migrated elsewhere. Eventually adds something
    than can be easily extended via install dir lookups like [carcass]
    did. *)

open B0_std

(** {1:project Project metadata} *)

val find_project_meta : unit -> B0_meta.t option
(** [find_project_meta ()] finds default metadata for the project.
    For now this the meta of {!B0_pack.find_default} if there is
    a B0 file linked (see {!B0_driver.has_b0_file}). *)

val get_project_meta : unit -> B0_meta.t
(** [get_project_meta ()] is like {!find_project_meta} but defaults to
    {!B0_meta.empty}. *)

val default_root_markers : string list
(** [default_root_marker] filenames that could denote the root of a
    project's source tree. *)

val find_project_name :
  ?root_markers:string list -> cwd:Fpath.t -> unit ->
  (string option, string) result
(** [find_project_name ~cwd ()] tries to determine a name for the project.
    This is either the basename of the VCS work directory or it loops
    up from the current working directory [cwd] trying to find a file a
    directory which has a file whose name is in [root_markers] which defaults to
    {!default_root_markers}. *)

(** {1:copyrights Copyrights} *)

val get_copyright_years : string option -> string
(** [get_copyright_years years] defaults the option to the
    current year, as determined by {!Unix.gettimeofday} in UTC. *)

(** {1:changes_files [CHANGES] files} *)

type changes = unit -> string
(** The type for changes scaffolders. *)

val find_changes_scaffolder : Fpath.t -> (changes, string) result
(** [find_changes_scaffolder f] is a changes scaffolder for a file
    [file] (ignored for now). *)

(** {1:license_files [LICENSE] files} *)

val find_project_license : B0_meta.t -> B0_meta.spdxid option
(** [find_project_license m] looks the first {!B0_meta.licenses} in [meta]. *)

val get_license : B0_meta.t -> B0_meta.spdxid option -> B0_meta.spdxid
(** [get_project_license meta license] defaults [license] with
    {!find_project_license} or ["ISC"] if that didn't yield anything
    (author bias, open to a better scheme). *)

val download_license_template :
  ?httpc:B0_http.Http_client.t -> strip_meta:bool -> B0_meta.spdxid ->
  (string, string) result
(** [download_license_template spdxid] tries to download a licence
    template for [spdxid]. The SPDX project doesn't seem to provide
    this. We rely on the data from {{:https://choosealicense.com/}
    [choosealicense.com]}. [strip_meta] inidicates whether the YAML preamble
    should be stripped. [httpc] is the client to use, it default to
    {!B0_http.Http_client.get}.

    {b Note.} These templates should have ["[year]"] and ["[fullname]"]
    variables to substitute. Except for licenses that do not, like
    the GPL which attributes copyrights to the FSF. *)

val license :
  ?var_years:string -> ?var_holder:string -> years:string ->
  holder:string -> string -> string * string list
(** [license text ~years ~holder] generates a license from [text] in which
    it substitutes the [var_years] (defaults to ["[year]"]) by [years] and
    [var_holder] (defaults to ["[fullname]"]) by [holder]. The tupled list
    has warning messages for variables that could not be subsituted.

    The defaults of [var_years] and [var_holder] match those needed by
    {!download_license_template} do not rely on them otherwise. *)

(** {1:readme [README] files} *)

type readme =
  project_name:string -> synopsis:string option -> B0_meta.t -> string
(** The type for [README] scaffolders. *)

val find_readme_scaffolder : Fpath.t -> (readme, string) result
(** [find_readme_scaffolder file] is a readme scaffolder for a file [file]
    (ignored for now). *)

(** {1:sources Source files} *)

val get_src_license :
  B0_meta.t -> example:bool -> B0_meta.spdxid option -> B0_meta.spdxid
(** [get_src_license] is like {!get_license} except if [example] is [true]
    it defaults to ["CC0-1.0"] (TODO define a key for that). *)

type lang =
  [ `C | `Css | `Haskell | `Html | `Javascript | `Java | `Ocaml | `Racket
  | `Rust | `Sh ]
(** The type for supported source language files. *)

val pp_lang_id : lang Fmt.t
(** [pp_lang] is a formatter for source language identifiers. *)

val lang_to_id : lang -> string
(** [lang_to_id lang] is a lowercased identifier for [lang]. *)

val lang_of_id : string -> (lang, string) result
(** [lang_of_id s] is a language frmo the given identifier. *)

val lang_of_file_ext : Fpath.ext -> lang option
(** [lang_of_file_ext] tries to guess a language from the given file
    extension. *)

type src = years:string -> holder:string -> license:B0_meta.spdxid -> string
(** The type for source scaffolders. *)

val src_scaffolder : lang -> src
(** [src_scaffolder lang] is a source scaffolder for language [lang]. *)
