(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

val write :
  ?lang:string -> ?generator:string -> ?styles:string list ->
  ?scripts:string list -> ?more_head:B0_html.El.frag -> ?title:string ->
  B0_memo.t -> frag:Fpath.t -> o:Fpath.t -> unit
(** [write_page m ~frag ~o] reads [frag] and inserts it
    in an {!El.body} using {!raw} and writes a full HTML document
    to [o] using {!basic_page} (see doc of the corresponding arguments).
    If [title] is [""] or unspecified {!page_title}[ o] is used. *)
