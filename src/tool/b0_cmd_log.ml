(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log details format op_selector c =
  Log.if_error ~use:Os.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c || format = `Trace_event in
  let b0_dir = B0_driver.Conf.b0_dir c in
  (* FIXME
     This should also be fixed in b0-cache / B0_cli.Memo.log_file *)
  let log_file = Fpath.(b0_dir / "b" / "user" / "_log") in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let* l = B0_cli.Memo.Log.read log_file in
  B0_cli.Memo.Log.out Fmt.stdout format details op_selector ~path:log_file l;
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show build logs" in
  let descr = [
    `P "The $(iname) command shows build information and operations in \
        various formats.";
    `S B0_cli.s_output_format_options;
    `S B0_cli.Op.s_selection_options;
    `Blocks B0_cli.Op.query_man ]
  in
  let descr = `Blocks descr in
  B0_tool_std.Cli.subcmd_with_driver_conf "log" ~doc ~descr @@
  Term.(const log $ B0_cli.output_format () $
        B0_cli.Memo.Log.out_format_cli () $ B0_cli.Op.query_cli ())
