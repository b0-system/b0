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

let find_root_b0 ~cwd =
  let rec loop p = match Fpath.is_root p with
  | true -> cwd
  | false ->
      if Fpath.is_root p then cwd else
      match Os.Dir.exists Fpath.(p / B0_ui.Memo.b0_dir_name) with
      | Error _ | Ok false -> loop (Fpath.parent p)
      | Ok true -> p
  in
  loop cwd

let log_cmd () no_pager b0_dir log_file (out_fmt, log_output) log_filter =
  Log.if_error ~use:err_unknown @@
  Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
  let b0_dir = B0_ui.Memo.get_b0_dir ~cwd ~root:(find_root_b0 cwd) ~b0_dir in
  let log_file = B0_ui.Memo.get_log_file ~cwd ~b0_dir ~log_file in
  let don't = no_pager || out_fmt = `Trace_event in
  Result.bind (B0_ui.Pager.find ~don't ()) @@ fun pager ->
  Result.bind (B0_ui.Pager.page_stdout pager) @@ fun () ->
  match Os.File.exists log_file with
  | Error _ as e -> e
  | Ok false ->
      Log.err (fun m -> m "%a: No such file." Fpath.pp_unquoted log_file);
      Ok err_no_log_file
  | Ok true ->
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

let b00_log =
  let doc = "Show B0 log files" in
  let sdocs = Manpage.s_common_options in
  let man_xrefs = [`Tool "b0"; `Tool "b00-cache"] in
  let docs_out_fmt = "OUTPUT FORMATS" in
  let docs_selection = "OPTIONS FOR SELECTING OPERATIONS" in
  let envs = B0_ui.Pager.envs in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command shows operations stored in binary B0 log files.";
    `Blocks B0_ui.Op.select_man;
    `S docs_out_fmt;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ];
  in
  let b0_dir = B0_ui.Memo.b0_dir () in
  let b0_log_file = B0_ui.Memo.log_file () in
  Term.(const log_cmd $ cli_conf $ B0_ui.Pager.don't () $
        b0_dir $ b0_log_file $
        B0_ui.Memo.Log.out_fmt_cli ~docs:docs_out_fmt () $
        B0_ui.Op.select_cli ~docs:docs_selection ()),
  Term.info "b00-log" ~version:"%%VERSION%%" ~doc ~sdocs ~envs ~exits ~man
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
