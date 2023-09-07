(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let edit actions conf =
  B0_tool_std.Def.edit (module B0_action) conf actions

let get format k clets conf =
  B0_tool_std.Def.get_meta_key (module B0_action) conf format k clets

let list format actions conf =
  B0_tool_std.Def.list (module B0_action) conf format actions

let show format actions conf =
  let format = if format = `Normal then `Long else format in
  B0_tool_std.Def.list (module B0_action) conf format actions

let exec action_name action_args conf =
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* action = B0_action.get_or_hint action_name in
  let func = B0_action.func action in
  let cwd = B0_driver.Conf.cwd conf in
  let root_dir = Fpath.parent @@ Option.get @@ B0_driver.Conf.b0_file conf in
  let scope_dir = B0_def.scope_dir (B0_action.def action) in
  let scope_dir = Option.value scope_dir ~default:root_dir in
  let b0_dir = B0_driver.Conf.b0_dir conf in
  let exec = B0_action.Env.v ~cwd ~scope_dir ~root_dir ~b0_dir ~action in
  Ok (func exec (Cmd.list action_args))

(* Command line interface *)

open Cmdliner

let actions ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"ACTION")

let actions_all = actions ~right:(-1)

(* Commands *)

let edit =
  let doc = "Edit actions" in
  let descr = `P "$(iname) opens in your editor the B0 files where given \
                  actions are defined." in
  let envs = B0_tool_std.Cli.editor_envs in
  let term = Term.(const edit $ actions_all) in
  B0_tool_std.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs term

let exec =
  let doc = "Execute an action" in
  let action =
    let doc = "The action to execute." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"ACTION")
  in
  let action_args =
    let doc =
      "Argument for the action. Specify arguments after the $(b,--) token \
       otherwise command line options will be interpreted by $(iname)."
    in
    Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")
  in
  let synopsis = `P "$(iname) [$(i,OPTION)]… $(b,--) $(i,ACTION) [$(i,ARG)]…" in
  let descr = `P "$(iname) executes actions like $(mname) -- \
                  $(i,ACTION) [$(i,ARG)]… would."
  in
  let term = Term.(const exec $ action $ action_args) in
  B0_tool_std.Cli.subcmd_with_b0_file "exec" ~doc ~descr ~synopsis term

let get =
  let doc = "Get action metadata" in
  let descr = `P "$(iname) outputs the value of metadata $(i,KEY) of given \
                  actions."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  let actions = actions ~right:0 in
  let term =
    Term.(const get $ B0_tool_std.Cli.format $ B0_tool_std.Cli.pos_key $
          actions)
  in
  B0_tool_std.Cli.subcmd_with_b0_file "get" ~doc ~descr ~envs term

let list =
  let doc = "List actions" in
  let descr = `P "$(iname) lists given actions." in
  let envs = B0_tool_std.Cli.pager_envs in
  let term = Term.(const list $ B0_tool_std.Cli.format $ actions_all) in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr ~envs term

let show =
  let doc = "Show action metadata" in
  let descr =
    `P "$(iname) is $(b,list -l), it outputs metadata of given actions."
  in
  let envs = B0_tool_std.Cli.pager_envs in
  let term = Term.(const show $ B0_tool_std.Cli.format $ actions_all) in
  B0_tool_std.Cli.subcmd_with_b0_file "show" ~doc ~descr ~envs term

let subs = [edit; exec; get; list; show]

let cmd =
  let doc = "Operate on actions" in
  let descr = `P "$(iname) operates on actions." in
  let envs = B0_tool_std.Cli.pager_envs in
  B0_tool_std.Cli.cmd_group_with_b0_file "action" ~doc ~descr ~envs subs
