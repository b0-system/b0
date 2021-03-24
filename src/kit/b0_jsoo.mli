(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [js_of_ocaml] B0 file support *)

open B00_std
open B00
open B00_ocaml

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

val source_map : B00_jsoo.source_map B0_meta.key
(** [source_map] is the source map option. *)

val tag : unit B0_meta.key
(** [tag] indicates the entity is related to [js_of_ocaml]. *)

val toplevel : bool B0_meta.key
(** [toplevel] should be [true] to embed toplevel support in
    the js executable. *)

val link : Cmd.t B0_meta.key
(** [link] are options added to the [js_of_ocaml] [link] subcommand. *)

val assets_root : Fpath.t B0_meta.key
(** [assets_root] indicates the path w.r.t. to which assets are
    are {!Fpath.reroot}ed. Assets that are not prefoxied by [assets_root]
    are simply copied at the toplevel of the build dir. *)

val meta :
  ?meta:B0_meta.t -> ?assets_root:Fpath.t -> ?comp:Cmd.t ->
  ?comp_mode:comp_mode -> ?link:Cmd.t -> ?requires:Lib.Name.t list ->
  ?source_map:B00_jsoo.source_map -> ?toplevel:bool -> unit -> B0_meta.t
(** [meta] creates a base metadata dictionary for compiling with
    [js_of_ocaml]. See the corresponding keys above. FIXME defaults. *)


(** {1:units Build units} *)

val exe :
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
       JavaScript file.}} *)

val web :
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
       executable. The files {!B00_fexts.www} in [srcs] minus [.js] files are
       copied over the build directory. If these files are can be rerooted
       to the build dir according to [assets_dir] they are copied as such
       otherwise they are copied
       [assets_dir]
       {b FIXME.} A competing idea was to have a notion of root induced
        by {!B0_srcs} selection. See the commented stuff there. This
        is likely something that will play better with generated assets.
        It's also sligthly borderline with deployements.}}

    {b TODO document.} The js file is [n.js], if there's no [.html] source
    in the srcs a minimal HTML file is generated in which [n.js]
    is linked as a script and any css file in [srcs] as a stylesheet.*)

(** {1:frag Build fragments} *)


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
