(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log ~output_verbosity ~log_format ~op_query c =
  Log.if_error ~use:Os.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c || log_format = `Trace_event in
  let b0_dir = B0_driver.Conf.b0_dir c in
  (* FIXME
     This should also be fixed in b0-cache / B0_cli.Memo.log_file *)
  let log_file = Fpath.(b0_dir / "b" / "user" / "_log") in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let* l = B0_memo_log.read log_file in
  B0_cli.Memo.Log.out
    Fmt.stdout log_format output_verbosity op_query ~path:log_file l;
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show build logs" in
  let descr = `Blocks [
      `P "The $(cmd) command shows build information and operations in \
          various formats.";
      `S B0_std_cli.s_output_verbosity_options;
      `S B0_cli.Op.s_selection_options;
      `Blocks B0_cli.Op.query_man ]
  in
  B0_tool.Cli.subcmd_with_driver_conf "log" ~doc ~descr @@
  let+ output_verbosity = B0_tool.Cli.output_verbosity
  and+ log_format = B0_tool.Cli.log_format
  and+ op_query = B0_tool.Cli.op_query in
  log ~output_verbosity ~log_format ~op_query
