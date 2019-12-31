(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

let get c format args = match args with
| [] ->
    Log.err (fun m -> m "No metadata key specified");
    B0_driver.Exit.some_error
| k :: ps ->
    B0_b0.Def.get (module B0_pack) c format k ps

let pack action format args c = match action with
| `Edit -> B0_b0.Def.edit (module B0_pack) c args
| `Get -> get c format args
| `List -> B0_b0.Def.list (module B0_pack) c format args
| `Show ->
    let format = if format = `Normal then `Long else format in
    B0_b0.Def.list (module B0_pack) c format args

(* Command line interface *)

open Cmdliner

let action =
  let action = [ "edit", `Edit; "get", `Get; "list", `List; "show", `Show] in
  let doc =
    let alts = Arg.doc_alts_enum action in
    Fmt.str "The action to perform. $(docv) must be one of %s." alts
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let action_args =
  let doc = "Positional arguments for the action." in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let doc = "Operate on build packs"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.Info.base_cmd
let envs = B0_editor.envs ()
let man_xrefs = [ `Main ]
let man = [
  `S Manpage.s_description;
  `P "$(tname) operates on build pack.";
  `S "ACTIONS";
  `I ("$(b,edit) [$(i,PACK)]...",
      "Edit in your editor the B0 file(s) in which all or the given packs \
       are defined.");
  `I ("$(b,get) $(i,KEY) [$(i,UNIT)]...",
      "Get metadata key $(i,KEY) of given or all packs.");
  `I ("$(b,list) [$(i,PACK)]...",
      "List all or given packs. Use with $(b,-l) to get more info on \
       pack metadata.");
  `I ("$(b,show) [$(i,PACK)]...",
      "Show is an alias for $(b,list -l)");
  `S Manpage.s_arguments;
  `S Manpage.s_options;
  B0_b0.Cli.man_see_manual; ]

let cmd =
  let pack_cmd =
    Term.(const pack $ action $ B00_ui.Cli.out_details () $ action_args)
  in
  B0_driver.with_b0_file ~driver:B0_b0.driver pack_cmd,
  Term.info "pack" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

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
