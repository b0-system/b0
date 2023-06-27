(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Result.Syntax

open Cmdliner

(* Environment variables *)

module Env = struct
  let visual = "VISUAL"
  let editor = "EDITOR"
  let infos =
    Cmd.Env.info visual
      ~doc:"The editor used to edit files. This is a command invocation given \
            to execvp(3) and is used before EDITOR." ::
    Cmd.Env.info editor
      ~doc:"The editor used to edit files. This is a command invocation given \
            to execvp(3) and is used after VISUAL." ::
    []

end

(* Editing *)

let find ?win_exe ?search () =
  let parse_env cmds env = match cmds with
  | Error _ as e -> e
  | Ok cmds as r ->
      let empty_is_none = true in
      match Os.Env.find' ~empty_is_none B0_std.Cmd.of_string env with
      | Error _ as e -> e
      | Ok None -> r
      | Ok (Some cmd) -> Ok (cmd :: cmds)
  in
  let cmds = Ok [B0_std.Cmd.arg "nano"] in
  let cmds = parse_env cmds Env.editor in
  let* cmds = parse_env cmds Env.visual in
  let rec loop = function
  | [] -> Ok None
  | cmd :: cmds ->
      match Os.Cmd.find ?win_exe ?search cmd with
      | Ok None -> loop cmds
      | v -> v
  in
  loop cmds

let edit_files editor fs = match editor with
| None -> Error "No runnable editor found in VISUAL or EDITOR"
| Some editor -> Os.Cmd.run_status B0_std.Cmd.(editor %% paths fs)
