(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

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
      Result.bind (Os.Cmd.find ?search Cmd.(arg "xdg-open")) @@ function
      | Some xdg -> Ok (Some xdg)
      | None ->
          Result.bind (Os.Cmd.find ?search Cmd.(arg "open")) @@ function
          | Some oopen -> Ok (Some oopen)
          | None ->
              if Sys.win32
              then Ok (Some Cmd.(arg "start" % "")) (* XXX really ? *)
              else Ok None

(* XXX support background *)
let show pdf_viewer file =
  Result.map_error
    (fun e -> Fmt.str "show PDF %a: %s" Fpath.pp_quoted file e) @@
  match pdf_viewer with
  | None -> Error "No PDF viewer found, use the PDFVIEWER env var to set one."
  | Some cmd -> Os.Cmd.run Cmd.(cmd %% path file)

(* Cli interaction. *)

open Cmdliner

let pdf_viewer ?docs ?(opts = ["pdf-viewer"]) () =
  let env = Cmd.Env.info Env.pdfviewer in
  let doc =
    "The PDF viewer command $(docv) to use. If absent either one \
     of $(b,xdg-open(1)) or $(b,open(1)) is used. If not found and \
     on Windows $(b,start) is used."
  in
  let absent = "OS dependent fallback"  in
  let cmd = Arg.conv' ~docv:"CMD" (B0_std.Cmd.of_string, B0_std.Cmd.pp_dump) in
  Arg.(value & opt (Arg.some cmd) None &
       info opts ~env ~absent ~doc ?docs ~docv:"CMD")
