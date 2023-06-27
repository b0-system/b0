(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** PDF viewer interaction. *)

open B0_std

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
