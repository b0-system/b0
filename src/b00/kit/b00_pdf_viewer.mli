(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** PDF viewer interaction. *)

open B00_std

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val pdfviewer : string
  (** [pdfviewer] is [PDFVIEWER]. *)
end

(** {1:show_pdfs Show PDFs} *)

type t
(** The type for specifying a PDF viewer. *)

val find :
  ?search:Fpath.t list -> pdf_viewer:Cmd.t option -> unit ->
  (t option, string) result
(** [find ~search ~pdf_viewer] tries to find a PDF viewer in a
    platform dependent way. *)

val show : t option -> Fpath.t -> (unit, string) result
(** [show pdf_viewer file] shows PDF file using the viewer
    [pdf_viewer] (if [None] an error message is returned mentioning no
    viewer was found). *)

(** {1:cli Cli interaction} *)

val pdf_viewer :
  ?docs:string -> ?opts:string list -> unit -> Cmd.t option Cmdliner.Term.t
(** [pdf_viewer ~docs ~opts ()] is an option and [PDFVIEWER]
    environment variable to use with [pdf_viewer] argument of
    {!find}. [opts] are the cli options and default to
    ["pdf-viewer"]. [docs] is the documentation section where the
    option gets documented. *)


(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
