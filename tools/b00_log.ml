(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let log tty_cap log_level no_pager format details query log_file =
  let tty_cap = B00_cli.B00_std.get_tty_cap tty_cap in
  let log_level = B00_cli.B00_std.get_log_level log_level in
  B00_cli.B00_std.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let don't = no_pager || format = `Trace_event in
  Result.bind (B00_pager.find ~don't ()) @@ fun pager ->
  Result.bind (B00_pager.page_stdout pager) @@ fun () ->
  Result.bind (B00_cli.Memo.Log.read log_file) @@ fun l ->
  B00_cli.Memo.Log.out Fmt.stdout format details query ~path:log_file l;
  Ok 0

(* Command line interface *)

open Cmdliner

let version = "%%VERSION%%"
let doc = "Operate on B0 log files"
let sdocs = Manpage.s_common_options
let docs_format = "OUTPUT FORMAT"
let docs_details = "OUTPUT DETAILS"
let docs_selection = "OPTIONS FOR SELECTING OPERATIONS"
let envs = B00_pager.envs ()

let man_xrefs = [`Tool "b0"; `Tool "b00-cache"; `Tool "b00-hash" ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) tool reads build information and operations stored \
      in binary b0 log files.";
  `S Manpage.s_arguments;
  `S docs_format;
  `S docs_details;
  `P "If applicable.";
  `S docs_selection;
  `Blocks B00_cli.Op.query_man;
  `S Manpage.s_bugs;
  `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]

let log_file =
  let docv = "LOG_FILE" in
  Arg.(required & pos 0 (some B00_cli.fpath) None & info [] ~doc ~docv)

let tool =
  Cmd.v (Cmd.info "b00-log" ~version ~doc ~sdocs ~envs ~man ~man_xrefs)
    Term.(const log $
          B00_cli.B00_std.tty_cap ~docs:sdocs () $
          B00_cli.B00_std.log_level ~docs:sdocs () $
          B00_pager.don't ~docs:sdocs () $
          B00_cli.Memo.Log.out_format_cli ~docs:docs_format () $
          B00_cli.Arg.output_details ~docs:docs_details () $
          B00_cli.Op.query_cli ~docs:docs_selection () $ log_file)

let main () = exit (Cmd.eval' tool)
let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
