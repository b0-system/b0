(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B00 [js_of_ocaml] support.

    This models the {{:http://ocsigen.org/js_of_ocaml/}[js_of_ocaml]}
    tool. *)

open B00_std
open B00

(** {1:jsoo Js_of_ocaml} *)

val tool : Tool.t
(** [tool] is the [js_of_ocaml] executable. *)

val build_runtime :
  Memo.t -> opts:Cmd.t -> jss:Fpath.t list -> o:Fpath.t -> unit
(** [build_runtime m ~jss o] writes a standalone runtime with JavaScript
    files [jss] to [o]. *)

val compile :
  Memo.t -> opts:Cmd.t -> source_map:[`Inline | `File] option ->
  jss:Fpath.t list -> byte:Fpath.t -> o:Fpath.t -> unit
(** [compile m ~source_map ~jss ~byte ~o] compiles the JavaScript
    files [jss] and byte code object or executable [byte] to the
    JavaScript file [o]. *)

val link :
  Memo.t -> opts:Cmd.t -> source_map:[`Inline | `File] option ->
  jss:Fpath.t list -> o:Fpath.t -> unit
(** [link m ~opts ~jss ~o] links the JavaScript files [jss] to [o] with
    options [opts]. *)

val write_page :
  ?lang:string -> ?generator:string -> ?styles:string list ->
  ?scripts:string list -> ?title:string -> B00.Memo.t ->
  o:B00_std.Fpath.t -> unit
(** [write_page m ~title ~o] writes to file [o] a full HTML document
    whose body contains only a {!B00_htmlg.El.noscript} element that
    entices the user, in english, to enable JavaScript. [title]
    defaults to the basename of [o] without its extension, for the
    other arguments and more information see
    {!B00_htmlg.El.basic_page}. *)

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
