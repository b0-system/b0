(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let cmd cmdlet args c =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* clet = B0_cmdlet.get_or_hint cmdlet in
  let `Cmd cmd = B0_cmdlet.cmd clet in
  Ok (cmd clet ~argv:(cmdlet :: args))

(* Command line interface *)

open Cmdliner

let cmdlet =
  let doc = "The cmdlet to execute." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"CMDLET")

let cmdlet_args =
  let doc =
    "Argument for the cmdlet. Specify arguments after the $(b,--) \
     otherwise command line options will be interpreted by $(tname) $(mname)."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let cmd =
  let doc = "Execute cmdlets" in
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]... $(i,CMDLET) $(b,--) $(i,ARG)...";
    `S Manpage.s_description;
    `P "$(tname) executes cmdlets.";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    B0_b0.Cli.man_see_manual; ]
  in
  let cmd_cmd = Term.(const cmd $ cmdlet $ cmdlet_args) in
  B0_driver.with_b0_file ~driver:B0_b0.driver cmd_cmd,
  Term.info "cmd" ~doc ~sdocs ~exits ~man ~man_xrefs

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
