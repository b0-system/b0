(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* Show PDFs *)


(* XXX support background reloads *)

module Env = struct
  let pdfviewer = "PDFVIEWER"
end

type t = Cmd.t

let env_fallback cmd = match cmd with
| Some _ as v -> Ok v
| None -> Os.Env.find' ~empty_is_none:true Cmd.of_string Env.pdfviewer

let find ?search ?cmd () =
  let* cmd = env_fallback cmd in
  match cmd with
  | Some cmd -> Os.Cmd.get ?search cmd
  | None ->
    let cmds = [Cmd.tool "xdg-open"; Cmd.tool "open"] in
    match Os.Cmd.find_first ?search cmds with
    | Some v -> Ok v
    | None ->
        if Sys.win32
        then Ok Cmd.(arg "start" % "") (* XXX really ? *) else
        Fmt.error "No PDF viewer found. Set the %a environment variable."
          Fmt.code' Env.pdfviewer

let show pdf_viewer file = Os.Cmd.run Cmd.(pdf_viewer %% path file)

(* Cli interface *)

let pdf_viewer ?docs ?(opts = ["pdf-viewer"]) () =
  let open Cmdliner in
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
