(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://ocaml.github.io/odoc/odoc/index.html}odoc} support.

    {b Note.} This models the odoc tool and its invocations. Dependecy
    {e resolution} is not part of the module yet we could try to
    abstract it via a few functions and provide a full driver
    story. *)

open B0_std

(** {1:odoc Odoc} *)

val tool : B0_memo.Tool.t
(** [tool] is the [odoc] tool. *)

(** Compiling [.cmi], [.cmti], [.cmt] and [.mld] files to [.odoc] files. *)
module Compile : sig

  (** {1:compile Compile} *)

  (** Compilation dependencies, for compiling [.cmi], [.cmti] or
      [.cmt] files to [.odoc] files.

      As determined by the [odoc compile-deps] command. These dependencies
      need to be resolved to concrete [.odoc] files by some external
     mean. *)
  module Dep : sig

    (** {1:dep Dependencies} *)

    type t
    (** The type for odoc compilation dependencies. *)

    val name : t -> string
    (** [name d] is the module name of [d]. *)

    val digest : t -> Digest.t
    (** [digest d] is the digest of [d]. *)

    val pp : t Fmt.t
    (** [pp] formats a dependency. *)

    val write : B0_memo.t -> Fpath.t -> o:Fpath.t -> unit
    (** [write m cobj o] writes the odoc dependencies of the compilation
        object [cobj] to [o]. *)

    val read : B0_memo.t -> Fpath.t -> t list Fut.t
    (** [read m file] reads the result of a {!write} from
        [file] and continues with the dependencies. *)
  end

  (** Compilation writes, files written by compiling a [.cmi], [.cmti],
      [.cmt] or [.mld] file to an [.odoc] file.

      As determined by the [odoc compile-targets] command. *)
  module Writes : sig

    (** {1:writes Compilation file writes} *)

    val write :
      B0_memo.t -> Fpath.t -> to_odoc:Fpath.t -> o:Fpath.t -> unit
    (** [write m cobj ~to_odoc ~o] writes the files written by a compilation
        of [cobj] to [to_odoc] to [o]. *)

    val read : B0_memo.t -> Fpath.t -> Fpath.t list Fut.t
    (** [read m file] reads the result of a {!write} from [file]
        and continues with the files that will be written. *)
  end

  val cmd :
    ?resolve_forward_deps:bool -> ?hidden:bool -> B0_memo.t ->
    odoc_deps:Fpath.t list -> writes:Fpath.t list -> pkg:string -> Fpath.t ->
    o:Fpath.t -> unit
  (** [cmd m ~resolve_forward_deps ~hidden ~odoc_deps ~writes ~pkg cobj o]
      compiles the compilation object [cobj] part of package [pkg]
      to destination odoc file [o].
      {ul
      {- [odoc_deps] are the odoc files the compilation of [cobj] reads.
         They can be obtained by resolving the result of {!Dep} on [cobj].}
      {- [writes] specifies the writes of the command they can be
          obtained via {!Writes}.}
      {- [resolve_forward_deps] and [hidden] are the corresponding [odoc]
         options. See [odoc compile --help].}} *)

  (** {1:conv Convenience} *)

  val to_odoc :
    B0_memo.t -> ?hidden:bool -> pkg:string ->
    odoc_deps:B0_std.Fpath.t list -> B0_std.Fpath.t -> o:B0_std.Fpath.t -> unit
  (** [to_odoc m ~hidden ~pkg ~odoc_deps obj ~o] compiles [obj] (which
      can be any of a [.cmi], [.cmt], [.cmti] or [.mld] file) to an odoc
      file [o] assuming it depends on [odoc_deps] and is part of package [pkg]. *)
end

(** Generate HTML from [.odoc] files. *)
module Html : sig

  (** {1:html HTML generation} *)

  (** HTML generation dependencies, for compiling a {e package}
      ({{:https://github.com/ocaml/odoc/issues/274}sic}) to HTML
      files.

      As returned by the [odoc html-deps] command. These dependencies
      need to be resolved to concrete [.odoc] files by some external
      mean. *)
  module Dep : sig

    (** {1:deps Dependencies} *)

    type t
    (** The type for HTML generation dependencies. *)

    val pkg : t -> string
    (** [pkg d] is the package of [d]. *)

    val name : t -> string
    (** [name d] is the module name of [d]. *)

    val digest : t -> Digest.t
    (** [digest d] is the digest of [d]. *)

    val to_compile_dep : t -> Compile.Dep.t
    (** [to_compile_dep d] is [d] as a compilation depencency (simply
        drops the {!pkg}). *)

    val write :
      B0_memo.t -> odoc_files:Fpath.t list -> Fpath.t -> o:Fpath.t ->
      unit
    (** [write m ~odoc_files pkg_odoc_dir o] writes the odoc
        dependencies of the package directory [pkg_odoc_dir] that
        contains the odoc files [odoc_files] to [o]. *)

    val read : B0_memo.t -> Fpath.t -> t list Fut.t
    (** [read m file] reads the result of a {!Dep.write} from [file] and
        continues with the dependencies. *)
  end

  (** HTML generation writes, files written by generating HTML files
      from an [.odoc] file.

      As determined by the [odoc html-targets] command. *)
  module Writes : sig

    (** {1:writes HTML generation file writes} *)

    val write :
      B0_memo.t -> odoc_deps:Fpath.t list -> Fpath.t -> to_dir:Fpath.t ->
      o:Fpath.t -> unit
    (** [write m ~odoc_deps odoc ~to_dir ~o] writes to [o] the files
        written by an HTML generation of the [.odoc] file [odoc] to
        [to_dir].
        {ul
        {- [odoc_deps] are the [.odoc] file dependencies for
           the [odoc] file, they can be obtained by resolving the result
           of {!Dep} on the package odoc directory of [odoc].}} *)

    val read : B0_memo.t -> Fpath.t -> Fpath.t list Fut.t
    (** [read m file] reads the result of a {!Writes.write} from [file] and
        continues with the files that will be written. *)
  end

  val cmd :
    ?hidden:bool -> ?theme_uri:string -> B0_memo.t ->
    odoc_deps:Fpath.t list -> writes:Fpath.t list -> Fpath.t ->
    to_dir:Fpath.t -> unit
  (** [cmd m ~hidden ~theme_uri ~odoc_deps ~writes odoc ~to_dir]
      generates HTML for the [.odoc] file [odoc] to output directory
      [to_dir] with the [odoc html] command.
      {ul
      {- [odoc_deps] are the [.odoc] file dependencies for
         the [odoc] file, they can be obtained by resolving the result
         of {!Dep} on the package odoc directory of [odoc].}
      {- [writes] specifies the writes of the command, they can be obtained
         via {!Writes}.}
      {- [theme_uri] and [hidden] are the corresponding [odoc]
         options. See [odoc html --help].}} *)

  (** {1:conv Convenience} *)

  val write :
    B0_memo.t -> ?theme_uri:string -> html_dir:B0_std.Fpath.t ->
    odoc_deps:B0_std.Fpath.t list -> B0_std.Fpath.t -> unit
   (** [write m ~theme_uri ~html_dir ~odoc_deps odoc] writes the [html]
       for [odoc] in [html_dir] assuming it depends on the [odoc_files]
       in [odoc_deps]. *)
end

(** Generate HTML fragments from [.mld] files. *)
module Html_fragment : sig
  val cmd :
    B0_memo.t -> odoc_deps:Fpath.t list -> Fpath.t -> o:Fpath.t -> unit
  (** [cmd m ~odoc_deps mld ~o] generates an HTML fragment for the mld
      file [mld] to file [o] with the [odoc html-fragment] command.
      {ul
      {- [odoc_deps] are the [.odoc] file dependencies for the [mld]
         file.}} *)
end

(** Generate support files for the HTML. *)
module Support_files : sig

  (** {1:support Support files} *)

  (** Support files writes.

      As determined by the [odoc support-files-targets] command. *)
  module Writes : sig

    (** {1:writes Support files writes} *)

    val write :
      ?without_theme:bool -> B0_memo.t -> to_dir:Fpath.t -> o:Fpath.t ->
      unit
    (** [write m ~without_theme ~to_dir ~o] writes to [o] the support
        files written to the [to_dir] directory. [without_theme] is the
        corresponding [odoc] option. *)

    val read : B0_memo.t -> Fpath.t -> Fpath.t list Fut.t
    (** [read m file] reads the result of a {!Writes.write} from [file]
        and continues with the files that will be written. *)
  end

  val cmd :
    ?without_theme:bool -> B0_memo.t -> writes:Fpath.t list ->
    to_dir:Fpath.t -> unit
  (** [cmd m ~without_theme ~writes ~to_dir] writes support files
      for the HTML to the directory [to_dir].
      {ul
      {- [writes] specifies the writes of the command they can be
         obtained via {!Writes}}
      {- [without_theme] is the corresponding [odoc support-file-targets]
         option.}}  *)

  (** {1:conv Convenience} *)

  val write :
    B0_memo.t -> without_theme:bool -> html_dir:Fpath.t ->
    build_dir:Fpath.t -> unit
  (** [write m ~without_theme ~html_dir ~build_dir] writes the support
      files to [html_dir] using [build_dir] for memoizing the command. *)
end

(** Odoc theme support. *)
module Theme : sig

  (** {1:names Themes names} *)

  type name = string
  (** The type for theme names. *)

  val odoc_default : name
  (** [odoc_default] is the default odoc theme (["odoc.default"]). *)

  val odig_default : name
  (** [odig_default] is the default odig theme (["odig.default"]). *)

  val default_uri : string
  (** [default_uri] is a default uri to give to {!Html.cmd}, this
      is ["_odoc-theme"]. This will use that directory in the html
      directory. *)

  (** {2:user User preference} *)

  val config_file : Fpath.t
  (** [config_file] is the file relative to the user's
      {!B0_std.Os.Dir.config} directory for specifying the odoc theme. *)

  val get_user_preference : unit -> (name option, string) result
  (** [get_user_preference ()] is the user prefered theme name (if any). *)

  val set_user_preference : name option -> (unit, string) result
  (** [set_user_preference t] sets the user prefered theme to [t]. *)

  (** {1:themes Themes} *)

  type t
  (** The type for themes. *)

  val name : t -> name
  (** [name t] is the theme name. *)

  val path : t -> Fpath.t
  (** [path t] is the path to the theme directory. *)

  val pp_name : t Fmt.t
  (** [pp_name] formats a theme's name. *)

  val pp : t Fmt.t
  (** [pp] formats a theme. *)

  (** {1:queries Queries} *)

  val of_dir : Fpath.t -> t list
  (** [of_dir sharedir] are the themes found in [sharedir]. These are
      formed by looking up in [sharedir] for directory paths of the
      form [PKG/odoc-theme/ID/] in [sharedir] which yields a theme
      named by [PKG.ID]. *)

  val find : fallback:name option -> name -> t list -> (t, string) result
  (** [find ~fallback n ts] finds theme [n] in [ts]. An error is returned
      if [n] cannot be found, in that case it indicates [fallback] will
      be used instead (if specified). *)

  (** {1:writes Writing} *)

  val write : B0_memo.t -> t -> to_dir:Fpath.t -> unit
  (** [write m t ~to_dir] writes theme to directory [to_dir]. This simply
      copies {!path} to [to_dir]. *)
end
