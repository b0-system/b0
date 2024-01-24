(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:http://ocsigen.org/js_of_ocaml/}[js_of_ocaml]} support. *)

open B0_std

(** {1:tool Tool} *)

val tool : B0_memo.Tool.t
(** [tool] is the [js_of_ocaml] executable. *)

(** {1:units Build units} *)

(** {2:metadata Metadata} *)

val assets_root : Fpath.t B0_meta.key
(** [assets_root] indicates the path with respect to which page assets
    are are {!B0_std.Fpath.reroot}ed. Assets that are not prefixed by
    [assets_root] are simply copied at the toplevel of the build
    dir. *)

type compilation_mode = [ `Separate | `Whole ]
(** The type for [js_of_ocaml] compilation either whole program
    compilation mode for runtime efficiency and minimality or
    [`Separate] for build time efficiency. *)

val compilation_mode : compilation_mode B0_meta.key
(** [compilaton_mode] is the [js_of_ocaml] compilation mode.
    Defaults to [`Whole].

    {b FIXME} this should likely be a store key or something similar
    to the way we handle built code for OCaml. *)

val compile_opts : Cmd.t B0_meta.key
(** [compile] are options added to the [js_of_ocaml compile] subcommand.
    Defaults to {!B0_std.Cmd.empty}. *)

type source_map = [`Inline | `File ] option
(** The type for specifying source maps desires.

    {b FIXME} Like {!type-compilation_mode} this should possibly be associated
    the build. *)

val source_map : source_map B0_meta.key
(** [source_map] is the source map option. Defaults to [Some `File]. *)

val tag : bool B0_meta.key
(** [tag] indicates the entity is related to [js_of_ocaml]. *)

val toplevel : bool B0_meta.key
(** [toplevel] should be [true] to embed toplevel support in
    the js executable. *)

val link_opts : Cmd.t B0_meta.key
(** [link_opts] are options added to the [js_of_ocaml link] subcommand. *)

(** {2:exec JavaScript file} *)

val exe :
  ?wrap:(B0_unit.build_proc -> B0_unit.build_proc) -> ?doc:string ->
  ?meta:B0_meta.t -> ?assets_root:Fpath.t ->
  ?requires:B0_ocaml.Libname.t list -> ?public:bool -> ?name:string ->
  string -> srcs:B0_srcs.sels -> B0_unit.t
(** [exe exename] is a JavaScript "executable" file named [exename].
    {ul
    {- [doc] is the unit doc string.}
    {- [requires] are the OCaml libraries required to compile the JavaScript.
       Added to the unit meta as {!B0_ocaml.requires}.}
    {- [meta] is added to the}
    {- [public] is the visibility status.
       Added to the unit meta as {!B0_meta.public}'s meta.}
    {- [name] is the name of the unit (defaults to [name] with [.] replaced
       by [-]).}
    {- [srcs] are the executable sources. All files with extension [.ml],
       [.mli] and [.js] are considered for compiling and linked in the
       JavaScript file.}
    {- [wrap] allows to extend the build procedure you must call the given
       build procedure.}
    }
*)

(** {2:html_page HTML page} *)

val html_page :
  ?wrap:(B0_unit.build_proc -> B0_unit.build_proc) -> ?doc:string ->
  ?meta:B0_meta.t -> ?assets_root:Fpath.t ->
  ?requires:B0_ocaml.Libname.t list -> ?name:string ->
  ?js_file:string -> string -> srcs:B0_srcs.sels ->
  B0_unit.t
(** [html_page ?js_file page] constructs in the unit directory an HTML
    file named [page.html], a JavaScript file named [js_file] (defaults
    to [page.js]) and copies over (or generates) {!B0_file_exts.www} files
    of [srcs] as follows:
    {ul
    {- [page.html], if a file exists with that name in [srcs], this
       file is used. Otherwise a minimal HTML document linking [pagename.js]
       and [.css] assets is generated.}
    {- [js_file] is compiled by considering all files with extension
       [.ml], [.mli] and [.js] and linked against required libraries
       [requires].}
    {- The files {!B0_file_exts.www} in [srcs] minus [.js] files are
       copied over to the unit directory. If these files can be rerooted
       to the build directory according to [assets_root] they are copied
       in a hierarchy otherwise they are copied at the root of the
       directory

       {b FIXME.} Make that comprehensible.

       {b FIXME.} A competing idea was to have a notion of root induced
        by {!B0_srcs} selection. See the commented stuff there. This
        is likely something that will play better with generated assets.
        It's also sligthly borderline with deployements.}}
    The other arguments are as follows:
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [requires] are the OCaml libraries required to compile the JavaScript.}
    {- [name] is the name of the unit (defaults to [pagename]).}
    {- [jsname] is the basename of the JavaScript file, defaults to
       [pagename].}
    {- [srcs] are the sources to lookup, see above.}
    {- [wrap] allows to extend the build procedure you must call the given
       build procedure. TODO maybe remove once we have good
       {!build_fragments}.}}
*)

(** {1:build_fragments Build fragments}

    See {{!page-TODO.fragments}TODO}. *)

val compile :
  B0_memo.t -> opts:Cmd.t -> source_map:source_map ->
  jss:Fpath.t list -> byte:Fpath.t -> o:Fpath.t -> unit
(** [compile m ~source_map ~jss ~byte ~o] compiles the JavaScript
    files [jss] and byte code object or executable [byte] to the
    JavaScript file [o]. *)

val link :
  B0_memo.t -> opts:Cmd.t -> source_map:source_map -> jss:Fpath.t list ->
  o:Fpath.t -> unit
  (** [link m ~opts ~jss ~o] links the JavaScript files [jss] to [o] with
      options [opts]. *)

val build_runtime :
  B0_memo.t -> opts:Cmd.t -> jss:Fpath.t list -> o:Fpath.t -> unit
(** [build_runtime m ~jss o] writes a standalone runtime with JavaScript
    files [jss] to [o]. *)

val copy_assets :
  B0_memo.t -> B0_file_exts.map -> exts:B0_file_exts.t ->
  assets_root:Fpath.t option -> dst:B0_std.Fpath.t -> Fpath.Set.t
(** [copy_assets m srcs ~exts ~assets_root ~dst] copies [srcs] with
    extensions in [exts] to [dst]. If [assets_root] is specified
    indicates the path w.r.t. which assets are {!B0_std.Fpath.reroot}ed.
    Assets that are not prefixed by [assets_root] are simply copied
    at the toplevel of [dst].

    {b FIXME.} Not a good idea to ready them inside. But all
    this needs a good review. *)

val write_page :
  ?lang:string -> ?generator:string -> ?styles:string list ->
  ?scripts:string list -> ?title:string -> B0_memo.t ->
  o:B0_std.Fpath.t -> unit
(** [write_page m ~title ~o] writes to file [o] a full HTML document
    whose body contains only a {!B0_html.El.noscript} element that
    entices the user, in english, to enable JavaScript. [title]
    defaults to the basename of [o] without its extension, for the
    other arguments and more information see
    {!B0_html.El.basic_page}. *)
