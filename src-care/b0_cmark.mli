(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0 [cmark] support.

    This models the {{:https://github.com/commonmark/cmark}[cmark]}
    tool. There's not much to model though. *)

open B0_std

(** {1:cmark Cmark} *)

val tool : B00.Tool.t
(** [tool] is the [cmark] tool. *)

val cmd :
  ?validate_utf_8:bool -> ?format:string -> ?args:Cmd.t -> B00.Memo.t ->
  mds:Fpath.t list -> o:Fpath.t -> unit
(** [cmd m ~format ~validate_utf_8 ~args ~mds ~o] writes to file [o] the
    result of processing the CommonMark files [mds].
    {ul
       {- [format] is the output format. It defaults to ["html"] which
           outputs a fragment not a full page page (this the [--to]
           option).}
       {- [validate_utf_8] is the [--validate-utf8] option it defaults
          to [true].}
       {- [args] are additional command line arguments you may want to
          pass to [cmark].}} *)

val write_page :
  ?generator:string -> ?lang:string -> ?scripts:string list ->
  ?styles:string list -> ?title:string -> B00.Memo.t ->
  frag:Fpath.t -> o:Fpath.t -> unit
(** [write_page m ~title ~frag ~o] writes to [o] a full HTML document
    whose {!B0_web.Htmlg.El.body} contains the contents of file [frag]
    (inserted using {!B0_web.Htmlg.El.raw}). [title] defaults to the
    basename of [o] without its extension, for the other arguments
    and more information see {!B0_web.Htmlg.El.basic_page}. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
