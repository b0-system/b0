(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B000

(* Exit codes and errors *)

let err_no_log_file = 1
let err_unknown = 123

let handle_unknown_error = function
| Ok i -> i | Error e -> Log.err (fun m -> m "%s" e); err_unknown

let log_cmd () no_pager (out_fmt, log_output) log_file log_filter =
  Log.if_error ~use:err_unknown @@
  let don't = no_pager || out_fmt = `Trace_event in
  Result.bind (B0_ui.Pager.find ~don't ()) @@ fun pager ->
  Result.bind (B0_ui.Pager.page_stdout pager) @@ fun () ->
  Result.bind (B0_ui.Memo.Log.read_file log_file) @@
  fun (info, ops) -> log_output (info, log_filter ops); Ok 0

(* Command line interface *)

open Cmdliner

let exits =
  Term.exit_info err_no_log_file ~doc:"the log file does not exist." ::
  Term.exit_info err_unknown ~doc:"unknown error reported on stderr." ::
  Term.default_exits

let cli_conf = B0_ui.B0_std.cli_setup ()

(* Main command *)

(* FIXME lookup in the file hierarchy for a such file. *)
let default_log = B0_ui.Memo.(Fpath.(v b0_dir_name / log_file_name))
let b0_log_file =
  let doc = "The $(docv) b0 log file to read from." and docv = "FILE" in
  Arg.(value & pos 0 B0_ui.Cli.Arg.fpath default_log & info [] ~doc ~docv)

let b00_log =
  let doc = "Show b0 build operations from log files" in
  let man_xrefs = [`Tool "b0"; `Tool "b00-cache"] in
  let docs_out_fmt = "OUTPUT FORMATS" in
  let envs = B0_ui.Pager.envs in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows build operations from b0 log files. \
        If no specific selection is specified all of them are shown. \
       The various selection options are implicitely $(b,or)-ed and can \
       be eventually filtered by $(b,--executed) or $(b,--revived).";
    `P "Operations are sorted by operation execution start time, this can
        be changed via the $(b,--order-by) option.";
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ];
  in
  Term.(const log_cmd $ cli_conf $ B0_ui.Pager.don't () $
        B0_ui.Memo.Log.out_fmt_cli ~docs:docs_out_fmt () $
        b0_log_file $
        B0_ui.Op.log_filter_cli),
  Term.info "b00-log" ~version:"%%VERSION%%" ~doc ~envs ~exits ~man
    ~man_xrefs

let () = Term.(exit_status @@ eval b00_log)

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
