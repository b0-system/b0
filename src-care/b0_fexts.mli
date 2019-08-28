(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Sets of file extension sets. *)

open B0_std

(** {1:sets File extension sets} *)

type t = String.Set.t
(** The type for sets of file extensions. *)

type map = Fpath.t list String.Map.t
(** The type for files mapped by their extension. *)

val v : Fpath.ext list -> t
(** [v exts] is a set for the given extensions. *)

val ext : Fpath.ext -> t
(** [ext e] is the set that containes only [e]. *)

val find_files : t -> map -> Fpath.t list
(** [find_files exts m] selects from [m] the files that have an
    extension in [exts]. *)

val exists_file : t -> map -> bool
(** [exists_file exts fm s] is [true] iff [find_file fm s] is not empty. *)

val ( + ) : t -> t -> t
(** [( + )] is {!String.Set.union}. *)

val ( - ) : t -> t -> t
(** [( - )] is {!String.Set.diff}. *)

(** {1:cst Constant sets} *)

val all : t
(** [all] is the union of all the following sets. *)

val c_lang : t
(** [c_lang] is [".h"] and [".c"]. *)

val cmark : t
(** [cmark] is [".md"]. *)

val css : t
(** [css] is [".css"]. *)

val data : t
(** [data] is [".json"] and [".xml"]. *)

val font : t
(** [font] is [".otf"], [".ttf"], [".woff"] and [".woff2"]. *)

val html_lang : t
(** [html_lang] is [".html"], [".css"] and [".js"] *)

val image : t
(** [image] is [".eps"], [".gif"], [".ico"], [".jpeg"], [".jpg"],
    [".pdf"], [".png"], [".ps"] and [".tiff"]. *)

val javascript : t
(** [javascript] is [".js"]. *)

val latex_lang : t
(** [latex_lang] is [".tex"; ".sty"; ".bib"; ".bibdoi"]. *)

val ocaml_lang : t
(** [ocaml] is [".mli"], [".ml"], [".mld"], [.mll] and [.mly]. *)

val sound : t
(** [sound] is [".aiff"], [".flac"], [".mp3"] and [".wav"]. *)

val tex : t
(** [tex] is [".tex"]. *)

val video : t
(** [video] is [".flv"], [".mov"] and [".mp4"]. *)

val www : t
(** [www] is the union of {!data}, {!font}, {!html_lang}, {!image},
    {!sound}, {!video}. *)

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
