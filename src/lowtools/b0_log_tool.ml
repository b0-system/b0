(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let log ~no_pager ~format ~output_format ~query ~log_file =
  let don't = no_pager || format = `Trace_event in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let* log = B0_memo_log.read log_file in
  let path = log_file in
  Ok (B0_cli.Memo.Log.out Fmt.stdout format output_format query ~path log)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let tool =
  let doc = "Operate on b0 log files" in
  let envs = B0_pager.Env.infos in
  let man_xrefs = [`Tool "b0"; `Tool "b0-cache"; `Tool "b0-hash"] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tool) tool reads build information and operations stored \
        in binary b0 log files.";
    `S Manpage.s_arguments;
    `S B0_std_cli.s_output_format_options;
    `P "If applicable.";
    `S B0_cli.Op.s_selection_options;
    `Blocks B0_cli.Op.query_man;
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,b0) build system. See
        $(i,https:/erratique.ch/software/b0) for contact information."; ]
  in
  let version = "%%VERSION%%" in
  Cmd.make (Cmd.info "b0-log" ~version ~doc ~envs ~man ~man_xrefs) @@
  let+ () = B0_std_cli.log_setup ()
  and+ no_pager = B0_pager.don't ()
  and+ format = B0_cli.Memo.Log.out_format_cli ()
  and+ output_format = B0_std_cli.output_format ()
  and+ query = B0_cli.Op.query_cli ()
  and+ log_file =
    let doc = "The log file to use." and docv = "LOG_FILE" in
    Arg.(required & pos 0 (some B0_std_cli.fpath) None & info [] ~doc ~docv)
  in
  log ~no_pager ~format ~output_format ~query ~log_file

let main () = Cmd.eval_result tool
let () = if !Sys.interactive then () else exit (main ())
