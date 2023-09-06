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
  `Pre "Use $(mname) $(b,unit) to see what can be built."; `Noblank;
  `Pre "Use $(mname) $(b,--what) to see what gets built."; `Noblank;
  `Pre "Use $(mname) to build."; `Noblank;
  `Pre "Use $(mname) $(b,-u) $(i,UNIT) to build $(i,UNIT)."; `Noblank;
  `Pre "Use $(mname) $(b,-p) $(i,PACK) to build pack $(i,PACK).";
  `Pre "Use $(mname) [$(i,COMMAND)]… $(b,--help) for help about any \
        command.";
  `P "More information is available in the manuals, see $(b,odig doc b0).";
  B0_tool_std.Cli.man_see_manual;
  `S Manpage.s_bugs;
  `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information."; ]

let cmds =
  [ B0_cmd_build.cmd;
    B0_cmd_blueprint.cmd;
    B0_cmd_cmdlet.cmd;
    B0_cmd_cmd.cmd;
    B0_cmd_delete.cmd;
    B0_cmd_export.cmd;
    B0_cmd_file.cmd;
    B0_cmd_list.cmd;
    B0_cmd_log.cmd;
    B0_cmd_pack.cmd;
    B0_cmd_root.cmd;
    B0_cmd_scope.cmd;
    B0_cmd_unit.cmd;
    B0_cmd_vcs.cmd;
  ]

let b0 =
  let info = Cmd.info "b0" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man in
  Cmd.group info ~default:B0_cmd_build.term cmds

let main () = Cmd.eval_value b0
let () = B0_driver.set ~driver:B0_tool_std.driver ~main
