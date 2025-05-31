(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

val write :
  ?lang:string -> ?generator:string -> ?styles:string list ->
  ?scripts:string list -> ?more_head:B0_html.El.html -> ?title:string ->
  B0_memo.t -> frag:Fpath.t -> o:Fpath.t -> unit
(** [write_page m ~frag ~o] reads [frag] and inserts it in an
    {!B0_html.El.body} using {!B0_html.El.raw} and writes a full HTML
    document to [o] using {!B0_html.El.basic_page} (see doc of the
    corresponding arguments). If [title] is [""] or unspecified
    {!B0_html.El.title_of_filepath}[ o] is used. *)
