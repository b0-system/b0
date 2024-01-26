(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* Environment variables *)

module Env = struct
  let visual = "VISUAL"
  let editor = "EDITOR"
  let infos =
    let open Cmdliner in
    Cmd.Env.info visual
      ~doc:"The editor used to edit files. This is a command invocation given \
            to execvp(3) and is used before EDITOR." ::
    Cmd.Env.info editor
      ~doc:"The editor used to edit files. This is a command invocation given \
            to execvp(3) and is used after VISUAL." ::
    []

end

(* Editing *)

type t = Cmd.t

let find ?search ?cmd () = match cmd with
| Some cmd -> Os.Cmd.get ?search cmd
| None ->
    let parse_env cmds env = match cmds with
    | Error _ as e -> e
    | Ok cmds as r ->
        let empty_is_none = true in
        match Os.Env.find' ~empty_is_none Cmd.of_string env with
        | Error _ as e -> e
        | Ok None -> r
      | Ok (Some cmd) -> Ok (cmd :: cmds)
    in
    let cmds = Ok [Cmd.tool "nano"] in
    let cmds = parse_env cmds Env.editor in
    let* cmds = parse_env cmds Env.visual in
    match Os.Cmd.find_first ?search cmds with
    | None -> Error "No runnable editor found in VISUAL or EDITOR"
    | Some cmd -> Ok cmd

let edit_files editor fs = Os.Cmd.run_status Cmd.(editor %% paths fs)
