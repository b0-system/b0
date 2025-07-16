(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner
open Cmdliner.Term.Syntax

(* Manual fragments *)

let s_scope_selection = "SCOPE SELECTION"

let man_see_manual = `Blocks
    [ `S Manpage.s_see_also;
      `P "Consult $(b,odig doc b0) for manuals and more details."]

let man_with_descr ?synopsis descr =
  let man =
    [ `S Manpage.s_description;
      descr;
      `S Manpage.s_commands;
      `S Manpage.s_arguments;
      `S Manpage.s_options;
      `S B0_std_cli.s_output_details_options;
      `S s_scope_selection;
      `S Manpage.s_common_options;
      man_see_manual ]
  in
  match synopsis with
  | None -> man
  | Some syn -> `S Manpage.s_synopsis :: syn :: man

(* Commands *)

let cmd ?exits ?(envs = []) ?synopsis name ~doc ~descr term =
  let man = man_with_descr ?synopsis descr in
  Cmd.make (Cmd.info name ~doc ?exits ~envs ~man) @@
  let+ () = B0_driver.Cli.set_no_color_and_log_level
  and+ term in term ()

let cmd_with_driver_conf
    ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
    term
  =
  let man = man_with_descr ?synopsis descr in
  Cmd.make (Cmd.info name ~doc ~exits ~envs ~man) @@
  let+ conf = B0_driver.Cli.conf and+ term in
  term conf

let cmd_with_b0_file_if_any
    ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
    term
  =
  let man = man_with_descr ?synopsis descr in
  Cmd.make (Cmd.info name ~doc ~exits ~envs ~man) @@
  B0_driver.with_b0_file_if_any ~driver:B0_tool.driver term

let cmd_with_b0_file
    ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
    term
  =
  let man = man_with_descr ?synopsis descr in
  Cmd.make (Cmd.info name ~doc ~exits ~envs ~man) @@
  B0_driver.with_b0_file ~driver:B0_tool.driver term

let cmd_group ?exits ?(envs = []) ?synopsis name ~doc ~descr ?default subs =
  let man = man_with_descr ?synopsis descr in
  Cmd.group (Cmd.info name ~doc ?exits ~envs ~man) ?default subs
