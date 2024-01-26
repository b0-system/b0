(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** PDF viewer interaction. *)

open B0_std

(** {1:show_pdfs Show PDFs} *)

type t
(** The type PDF viewers. *)

val find :
  ?search:Cmd.tool_search -> ?cmd:Cmd.t -> unit -> (t, string) result
(** [find ?search ~pdf_viewer] tries to find a PDF viewer in a
    platform dependent way using [Os.Cmd.find ?search]. *)

val show : t -> Fpath.t -> (unit, string) result
(** [show pdf_viewer file] shows PDF [file] using [pdf_viewer]. *)

(** {1:cli Cli interface} *)

(** Environment variables. *)
module Env : sig
  val pdfviewer : string
  (** [pdfviewer] is [PDFVIEWER]. *)
end

val pdf_viewer :
  ?docs:string -> ?opts:string list -> unit -> Cmd.t option Cmdliner.Term.t
(** [pdf_viewer ~docs ~opts ()] is an option and {!Env.pdfviewer}
    environment variable to use with [pdf_viewer] argument of
    {!find}. [opts] are the cli options and default to
    ["pdf-viewer"]. [docs] is the documentation section where the
    option gets documented. *)
