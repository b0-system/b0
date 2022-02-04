(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let cmd cmdlet_name cmdlet_args c =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* cmdlet = B0_cmdlet.get_or_hint cmdlet_name in
  let cmd = B0_cmdlet.cmd cmdlet in
  let cwd = B0_driver.Conf.cwd c in
  let root_dir = Fpath.parent @@ Option.get @@ B0_driver.Conf.b0_file c in
  let scope_dir = B0_def.scope_dir (B0_cmdlet.def cmdlet) in
  let scope_dir = Option.value scope_dir ~default:root_dir in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let exec = B0_cmdlet.Env.v ~cwd ~scope_dir ~root_dir ~b0_dir ~cmdlet in
  Ok (cmd exec (Cmd.list cmdlet_args))

(* Command line interface *)

open Cmdliner

let cmdlet =
  let doc = "The cmdlet to execute." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"CMDLET")

let cmdlet_args =
  let doc =
    "Argument for the cmdlet. Specify arguments after the $(b,--) token \
     otherwise command line options will be interpreted by $(mname) $(tname)."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let cmd =
  let doc = "Execute cmdlets" in
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]… $(b,--) $(i,CMDLET) [$(i,ARG)]…";
    `S Manpage.s_description;
    `P "$(tname) executes cmdlets.";
    `S Manpage.s_arguments;
    B0_b0.Cli.man_see_manual; ]
  in
  let cmd_cmd = Term.(const cmd $ cmdlet $ cmdlet_args) in
  let cmd_cmd = B0_driver.with_b0_file ~driver:B0_b0.driver cmd_cmd in
  Cmd.v (Cmd.info "cmd" ~doc ~sdocs ~exits ~man ~man_xrefs) cmd_cmd

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
