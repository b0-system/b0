(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Cmdliner

(* Environment variables. *)

module Env = struct
  let pdfviewer = "PDFVIEWER"
end

(* Show PDFs *)

type t = Cmd.t

let find ?search ~pdf_viewer () =
  Result.map_error (fun e -> Fmt.str "find PDF viewer: %s" e) @@
  match pdf_viewer with
  | Some cmd -> Os.Cmd.find ?search cmd
  | None ->
      Result.bind (Os.Cmd.find ?search Cmd.(atom "xdg-open")) @@ function
      | Some xdg -> Ok (Some xdg)
      | None ->
          Result.bind (Os.Cmd.find ?search Cmd.(atom "open")) @@ function
          | Some oopen -> Ok (Some oopen)
          | None ->
              if Sys.win32
              then Ok (Some Cmd.(atom "start" % "")) (* XXX really ? *)
              else Ok None

(* XXX support background *)
let show pdf_viewer file =
  Result.map_error
    (fun e -> Fmt.str "show PDF %a: %s" Fpath.pp_quoted file e) @@
  match pdf_viewer with
  | None -> Error "No PDF viewer found, use the PDFVIEWER env var to set one."
  | Some cmd -> Os.Cmd.run Cmd.(cmd %% path file)

(* Cli interaction. *)

let pdf_viewer ?docs ?(opts = ["pdf-viewer"]) () =
  let env = Arg.env_var Env.pdfviewer in
  let doc =
    "The PDF viewer command $(docv) to use. If absent either one \
     of $(b,xdg-open(1)) or $(b,open(1)) is used. If not found and \
     on Windows $(b,start) is used."
  in
  let cmd = Arg.some ~none:"OS dependent fallback" B00_cli.cmd in
  Arg.(value & opt cmd None & info opts ~env ~doc ?docs ~docv:"CMD")

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
