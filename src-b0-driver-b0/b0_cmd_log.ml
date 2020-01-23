(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let log c details format op_selector =
  Log.if_error ~use:B0_driver.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c || format = `Trace_event in
  let b0_dir = B0_driver.Conf.b0_dir c in
  (* FIXME *)
  let log_file = Fpath.(b0_dir / "b" / "user" / ".log") in
  Result.bind (B00_pager.find ~don't ()) @@ fun pager ->
  Result.bind (B00_pager.page_stdout pager) @@ fun () ->
  Result.bind (B00_ui.Memo.Log.read log_file) @@ fun l ->
  B00_ui.Memo.Log.out
    Fmt.stdout format details op_selector ~path:log_file l;
  Ok B0_driver.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Show build logs"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.Info.base_cmd
let man_xrefs = [ `Main ]
let docs_format = "OUTPUT FORMATS"
let docs_details = "OUTPUT DETAILS"
let docs_select = "OPTIONS FOR SELECTING OPERATIONS"
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command shows build information and operations in \
      various formats.";
  `S Manpage.s_options;
  `S docs_format;
  `S docs_details;
  `P "If applicable.";
  `S docs_select;
  `Blocks B00_ui.Op.query_man;
  B0_b0.Cli.man_see_manual; ]

let cmd =
  Term.(const log $ B0_driver.Cli.conf $
        B00_ui.Cli.out_details ~docs:docs_details () $
        B00_ui.Memo.Log.out_format_cli ~docs:docs_format () $
        B00_ui.Op.query_cli ~docs:docs_select ()),
  Term.info "log" ~doc ~sdocs ~exits ~man ~man_xrefs

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
