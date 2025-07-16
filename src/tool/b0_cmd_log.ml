(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log ~format ~output_details ~query c =
  Log.if_error ~use:Os.Exit.some_error @@
  let no_pager = B0_driver.Conf.no_pager c || format = `Trace_event in
  let b0_dir = B0_driver.Conf.b0_dir c in
  (* FIXME
     This should also be fixed in b0-cache / B0_cli.Memo.log_file *)
  let log_file = Fpath.(b0_dir / "b" / "user" / "_log") in
  let* pager = B0_pager.find ~no_pager () in
  let* () = B0_pager.page_stdout pager in
  let* log = B0_memo_log.read log_file in
  let pp =
    B0_memo_cli.Log.pp ~format ~output_details ~query ~path:log_file ()
  in
  Fmt.pr "@[<v>%a@]@?" pp log;
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Output build logs" in
  let descr = `Blocks [
      `P "The $(cmd) command outputs build information and operations in \
          various formats.";
      `Pre "$(cmd) $(b,--stats)    # Output global build satistics";
      `Noblank;
      `Pre "$(cmd) $(b,--id 1 -l)  # Output details of operation 1";
      `Noblank;
      `Pre "$(cmd) $(b,-d)         # Longest build operation first";
      `Noblank;
      `Pre "$(cmd) $(b,-e)         # Failed build operations";
      `Noblank;
      `Pre "$(cmd) $(b,-u)         # Operations that really executed";
      `S B0_std_cli.s_output_details_options;
      `S B0_memo_cli.Op.s_selection_options;
      `Blocks B0_memo_cli.Op.query_man ]
  in
  B0_tool_cli.cmd_with_driver_conf "log" ~doc ~descr @@
  let+ format = B0_cli.log_format
  and+ output_details = B0_cli.output_details
  and+ query = B0_cli.memo_op_query in
  log ~format ~output_details ~query
