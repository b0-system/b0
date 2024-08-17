(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let log tty_cap log_level no_pager format details query log_file =
  let tty_cap = B0_std_cli.get_tty_cap tty_cap in
  let log_level = B0_std_cli.get_log_level log_level in
  B0_std_cli.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let don't = no_pager || format = `Trace_event in
  Result.bind (B0_pager.find ~don't ()) @@ fun pager ->
  Result.bind (B0_pager.page_stdout pager) @@ fun () ->
  Result.bind (B0_memo_log.read log_file) @@ fun l ->
  B0_cli.Memo.Log.out Fmt.stdout format details query ~path:log_file l;
  Ok 0

(* Command line interface *)

open Cmdliner

let log_file =
  let doc = "The log file to use." and docv = "LOG_FILE" in
  Arg.(required & pos 0 (some B0_std_cli.fpath) None & info [] ~doc ~docv)

let tool =
  let doc = "Operate on b0 log files" in
  let envs = B0_pager.Env.infos in
  let man_xrefs = [`Tool "b0"; `Tool "b0-cache"; `Tool "b0-hash" ] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) tool reads build information and operations stored \
        in binary b0 log files.";
    `S Manpage.s_arguments;
    `S B0_std_cli.s_output_format_options;
    `P "If applicable.";
    `S B0_cli.Op.s_selection_options;
    `Blocks B0_cli.Op.query_man;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]
  in
  Cmd.v (Cmd.info "b0-log" ~version:"%%VERSION%%" ~doc ~envs ~man ~man_xrefs)
    Term.(const log $ B0_std_cli.tty_cap () $
          B0_std_cli.log_level () $ B0_pager.don't () $
          B0_cli.Memo.Log.out_format_cli () $
          B0_std_cli.output_format () $
          B0_cli.Op.query_cli () $ log_file)

let main () = exit (Cmd.eval' tool)
let () = if !Sys.interactive then () else main ()
