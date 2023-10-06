(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let doc = "Software construction and deployment kit"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.infos
let man = [
  `S Manpage.s_synopsis;
  `P "$(mname) $(b,--) [$(i,ACTION)] [$(i,ARG)]…"; `Noblank;
  `P "$(mname) $(i,COMMAND) …";
  `S Manpage.s_description;
  `P "B0 describes software construction and deployments using modular and \
      customizable definitions written in OCaml.";
  `Pre "Use $(iname) $(b,unit --list) to see what can be built."; `Noblank;
  `Pre "Use $(iname) $(b,--what) to see what gets built."; `Noblank;
  `Pre "Use $(iname) to build."; `Noblank;
  `Pre "Use $(iname) $(b,--) [$(i,ACTION)] [$(i,ARG)]… to build and run an \
        action or unit."; `Noblank;
  `Pre "Use $(iname) $(b,-u) $(i,UNIT) to build $(i,UNIT)."; `Noblank;
  `Pre "Use $(iname) $(b,-p) $(i,PACK) to build pack $(i,PACK).";
  `Pre "Use $(iname) [$(i,COMMAND)]… $(b,--help) for help about any \
        command.";
  `P "More information is available in the manuals, see $(b,odig doc b0).";
  B0_tool_std.Cli.man_see_manual;
  `S Manpage.s_bugs;
  `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information."; ]

let cmds =
  [ B0_cmd_action.cmd;
    B0_cmd_build.cmd;
    B0_cmd_browse.cmd;
    B0_cmd_delete.cmd;
    B0_cmd_edit.cmd;
    B0_cmd_file.cmd;
    B0_cmd_key.cmd;
    B0_cmd_list.cmd;
    B0_cmd_lock.cmd;
    B0_cmd_log.cmd;
    B0_cmd_pack.cmd;
    B0_cmd_root.cmd;
    B0_cmd_scaffold.cmd;
    B0_cmd_scope.cmd;
    B0_cmd_show.cmd;
    B0_cmd_tool.cmd;
    B0_cmd_unit.cmd;
    B0_cmd_unlock.cmd;
    B0_cmd_vcs.cmd; ]

let b0 =
  let info = Cmd.info "b0" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man in
  let default =
    B0_driver.with_b0_file ~driver:B0_tool_std.driver @@
    B0_cmd_build.term
  in
  Cmd.group info ~default cmds

let main () = Cmd.eval_value b0
let () = B0_driver.set ~driver:B0_tool_std.driver ~main
