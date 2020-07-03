(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [js_of_ocaml] B0 file support *)

(** {1:units Build units} *)

open B00_std
open B00
open B00_ocaml

(** {1:unit Units} *)

val exe :
  ?doc:string -> ?meta:B0_meta.t -> ?action:B0_unit.action ->
  ?requires:Lib.Name.t list -> ?name:string -> string -> srcs:B0_srcs.t ->
  B0_unit.t
(** [exe n] is a JavaScript "executable" file named [n] (without
    the [.js] extension.
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [requires] are the OCaml libraries required to compile the JavaScript.}
    {- [name] is the name of the unit (defaults to [n]).}
    {- [srcs] are the executable sources. All files with extension [.ml],
       [.mli] and [.js] are considered for compiling and linked in the
       JavaScript file.}} *)

val web :
  ?doc:string -> ?meta:B0_meta.t -> ?action:B0_unit.action ->
  ?requires:Lib.Name.t list -> ?name:string -> string -> srcs:B0_srcs.t ->
  B0_unit.t
(** [web n] is an HTML page named [n] (without the [.html] extension).
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [requires] are the OCaml libraries required to compile the JavaScript.}
    {- [name] is the name of the unit (defaults to [n]).}
    {- [srcs] are the executable sources. All files with extension [.ml],
       [.mli] and [.js] are considered for compiling and linking the
       executable. The files {!B00_fexts.www} in [srcs] minus [.js] files are
       copied over the build directory.}}

    {b TODO document.} The js file is [n.js], if there's an [n.html] source
    it is used otherwhise a minimal HTML file is generated in which [n.js]
    is linked as a script and any css file in [srcs] as a stylesheet.*)


(** Metadata keys *)

val tag : unit B0_meta.key
(** [tag] indicates the entity is relatd to [js_of_ocaml]. *)

module Meta : sig
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
