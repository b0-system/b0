(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [js_of_ocaml] B0 file support *)

open B0_std

(** [js_of_ocaml] tool support.

    This models the {{:http://ocsigen.org/js_of_ocaml/}[js_of_ocaml]}
    tool. *)
module Tool : sig

(** {1:jsoo Js_of_ocaml} *)

type source_map = [`Inline | `File ] option
(** The type for specifying source maps desires. *)

val tool : B0_memo.Tool.t
(** [tool] is the [js_of_ocaml] executable. *)

val build_runtime :
  B0_memo.Memo.t -> opts:Cmd.t -> jss:Fpath.t list -> o:Fpath.t -> unit
(** [build_runtime m ~jss o] writes a standalone runtime with JavaScript
    files [jss] to [o]. *)

val compile :
  B0_memo.Memo.t -> opts:Cmd.t -> source_map:source_map ->
  jss:Fpath.t list -> byte:Fpath.t -> o:Fpath.t -> unit
(** [compile m ~source_map ~jss ~byte ~o] compiles the JavaScript
    files [jss] and byte code object or executable [byte] to the
    JavaScript file [o]. *)

val link :
  B0_memo.Memo.t -> opts:Cmd.t -> source_map:source_map ->
  jss:Fpath.t list -> o:Fpath.t -> unit
(** [link m ~opts ~jss ~o] links the JavaScript files [jss] to [o] with
    options [opts]. *)

val write_page :
  ?lang:string -> ?generator:string -> ?styles:string list ->
  ?scripts:string list -> ?title:string -> B0_memo.Memo.t ->
  o:B0_std.Fpath.t -> unit
(** [write_page m ~title ~o] writes to file [o] a full HTML document
    whose body contains only a {!B0_html.El.noscript} element that
    entices the user, in english, to enable JavaScript. [title]
    defaults to the basename of [o] without its extension, for the
    other arguments and more information see
    {!B0_html.El.basic_page}. *)
end

(** {1:metadata Metadata} *)

type comp_mode = [ `Separate | `Whole ]
(** The type for [js_of_ocaml] compilation either whole program
    compilation mode for runtime efficiency and minimality or
    [`Separate] for build time efficiency. *)

val comp : Cmd.t B0_meta.key
(** [comp] are options added to the [js_of_ocaml] [compile] subcommand. *)

val comp_mode : comp_mode B0_meta.key
(** [mode] is the [js_of_ocaml] compilation mode.

    {b FIXME} this should likely be a store key. *)

val source_map : Tool.source_map B0_meta.key
(** [source_map] is the source map option. *)

val tag : unit B0_meta.key
(** [tag] indicates the entity is related to [js_of_ocaml]. *)

val toplevel : bool B0_meta.key
(** [toplevel] should be [true] to embed toplevel support in
    the js executable. *)

val link : Cmd.t B0_meta.key
(** [link] are options added to the [js_of_ocaml] [link] subcommand. *)

val assets_root : Fpath.t B0_meta.key
(** [assets_root] indicates the path w.r.t. to which assets are are
    {!B0_std.Fpath.reroot}ed. Assets that are not prefixed by
    [assets_root] are simply copied at the toplevel of the build
    dir. *)

val meta :
  ?meta:B0_meta.t -> ?assets_root:Fpath.t -> ?comp:Cmd.t ->
  ?comp_mode:comp_mode -> ?link:Cmd.t -> ?requires:B0_ocaml.Lib.Name.t list ->
  ?source_map:Tool.source_map -> ?toplevel:bool -> unit -> B0_meta.t
(** [meta] creates a base metadata dictionary for compiling with
    [js_of_ocaml]. See the corresponding keys above. FIXME defaults. *)


(** {1:units Build units} *)

val exe :
  ?wrap:(B0_unit.proc -> B0_unit.proc) ->
  ?doc:string -> ?meta:B0_meta.t -> ?action:B0_unit.action ->
  ?name:string -> string -> srcs:B0_srcs.sels -> B0_unit.t
(** [exe n] is a JavaScript "executable" file named [n].
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [requires] are the OCaml libraries required to compile the JavaScript.}
    {- [name] is the name of the unit (defaults to [n] with [.] replaced
       by [-]).}
    {- [srcs] are the executable sources. All files with extension [.ml],
       [.mli] and [.js] are considered for compiling and linked in the
       JavaScript file.}
    {- [wrap] allows to extend the build procedure you must call the given
       build procedure. TODO maybe remove once we have good {!frag}.}} *)

val web :
  ?wrap:(B0_unit.proc -> B0_unit.proc) ->
  ?doc:string -> ?meta:B0_meta.t -> ?action:B0_unit.action ->
  ?name:string -> string -> srcs:B0_srcs.sels -> B0_unit.t
(** [web n] is an HTML page named [n] (without the [.html] extension FIXME
    review that).
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [requires] are the OCaml libraries required to compile the JavaScript.}
    {- [name] is the name of the unit (defaults to [n]).}
    {- [srcs] are the executable sources. All files with extension [.ml],
       [.mli] and [.js] are considered for compiling and linking the
       executable. The files {!B0_fexts.www} in [srcs] minus [.js] files are
       copied over the build directory. If these files are can be rerooted
       to the build dir according to [assets_dir] they are copied as such
       otherwise they are copied
       [assets_dir]
       {b FIXME.} A competing idea was to have a notion of root induced
        by {!B0_srcs} selection. See the commented stuff there. This
        is likely something that will play better with generated assets.
        It's also sligthly borderline with deployements.}
    {- [wrap] allows to extend the build procedure you must call the given
       build procedure. TODO maybe remove once we have good {!frag}.}}

    {b TODO document.} The js file is [n.js], if there's no [.html] source
    in the srcs a minimal HTML file is generated in which [n.js]
    is linked as a script and any css file in [srcs] as a stylesheet.*)

(** {1:frag Build fragments}

    See {{!page-TODO.fragments}TODO}. *)

val copy_assets :
  B0_memo.Memo.t -> B0_file_exts.map -> exts:B0_file_exts.t ->
  assets_root:Fpath.t option -> dst:B0_std.Fpath.t -> Fpath.Set.t
(** [copy_assets m srcs ~exts ~assets_root ~dst] copies [srcs] with
    extensions in [exts] to [dst]. If [assets_root] is specified
    indicates the path w.r.t. which assets are {!B0_std.Fpath.reroot}ed.
    Assets that are not prefixed by [assets_root] are simply copied
    at the toplevel of [dst].

    {b FIXME.} Not a good idea to ready them inside. But all
    this needs a good review. *)
