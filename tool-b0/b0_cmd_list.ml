(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let list format args c = B0_b0.Def.list (module B0_unit) c format args

(* Command line interface *)

open Cmdliner

let action =
  let action =
    ["action", `Action; "build-dir", `Build_dir; "edit", `Edit; "get", `Get;
     "list", `List; "show", `Show]
  in
  let doc =
    let alts = Arg.doc_alts_enum action in
    Fmt.str "The action to perform. $(docv) must be one of %s." alts
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let unit_args =
  let doc = "Units to list or all if unspecified." in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"UNIT")

let cmd =
  let doc = "List build units" in
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let envs = List.rev_append (B00_editor.envs ()) (B00_pager.envs ()) in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) lists build units, an alias for $(tname) $(b, unit list)";
    `S Manpage.s_arguments;
    `S Manpage.s_options;
    B0_b0.Cli.man_see_manual; ]
  in
  let list_cmd =
    Term.(const list $ B00_cli.Arg.output_details () $ unit_args)
  in
  let list_cmd = B0_driver.with_b0_file ~driver:B0_b0.driver list_cmd in
  Cmd.v (Cmd.info "list" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs) list_cmd

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
