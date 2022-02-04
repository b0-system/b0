(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Cmdliner

let doc = "Software construction and deployment kit"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.infos
let man = [
  `S Manpage.s_synopsis;
  `P "$(mname) $(i,COMMAND) …"; `Noblank;
  `P "$(mname) \
      [$(b,-a) $(i,UNIT)]
      [$(b,-u) $(i,UNIT)]…  [$(b,-p) $(i,PACK)]… [$(i,OPTION)]… \
      $(b,--) [$(i,ARG)]…";

  `S Manpage.s_description;
  `P "B0 describes software construction and deployments using modular and \
      customizable definitions written in OCaml.";
  `Pre "Use $(mname) $(b,unit) to see what can be built."; `Noblank;
  `Pre "Use $(mname) $(b,--what) to see what gets built."; `Noblank;
  `Pre "Use $(mname) to build."; `Noblank;
  `Pre "Use $(mname) $(b,-a) $(i,UNIT) to build $(i,UNIT) and execute its \
        outcome action.";
  `Pre "Use $(mname) [$(i,COMMAND)]… $(b,--help) for help about any \
        command.";
  `P "More information is available in the manuals, see $(b,odig doc b0).";
  B0_b0.Cli.man_see_manual;
  `S Manpage.s_bugs;
  `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information."; ]

let cmds =
  [ B0_cmd_build.cmd;
    B0_cmd_cmdlet.cmd;
    B0_cmd_cmd.cmd;
    B0_cmd_delete.cmd;
    B0_cmd_file.cmd;
    B0_cmd_log.cmd;
    B0_cmd_pack.cmd;
    B0_cmd_root.cmd;
    B0_cmd_scope.cmd;
    B0_cmd_unit.cmd ]

let b0 =
  let info = Cmd.info "b0" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man in
  Cmd.group info ~default:B0_cmd_build.term cmds


let main () = Cmd.eval_value b0
let () = B0_driver.set ~driver:B0_b0.driver ~main

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
