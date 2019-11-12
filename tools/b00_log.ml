(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

let err_no_log_file = 1
let err_unknown = 123
let err_miss_implicit_log_file file =
  Log.err begin fun m ->
    m "@[<v>%a:@, No such log file, specify one explicity.@]"
      Fpath.pp_unquoted file
  end;
  Ok err_no_log_file

let log tty_cap log_level no_pager b0_dir log_file format details query =
  let tty_cap = B0_std_ui.get_tty_cap tty_cap in
  let log_level = B0_std_ui.get_log_level log_level in
  B0_std_ui.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:err_unknown @@
  Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
  let root = B00_ui.Memo.find_dir_with_b0_dir ~start:cwd in
  let root = Option.value root ~default:cwd in
  let b0_dir = B00_ui.Memo.get_b0_dir ~cwd ~root ~b0_dir in
  let implicit_log_file = log_file = None in
  let log_file = B00_ui.Memo.get_log_file ~cwd ~b0_dir ~log_file in
  let don't = no_pager || format = `Trace_event in
  Result.bind (B0_pager.find ~don't ()) @@ fun pager ->
  Result.bind (B0_pager.page_stdout pager) @@ fun () ->
  Result.bind (Os.File.exists log_file) @@ function
  | false when implicit_log_file -> err_miss_implicit_log_file log_file
  |  _ ->
      Result.bind (B00_ui.Memo.Log.read log_file) @@ fun l ->
      B00_ui.Memo.Log.out Fmt.stdout format details query ~path:log_file l;
      Ok 0

(* Command line interface *)

open Cmdliner

let version = "%%VERSION%%"
let doc = "Read b0 log files"
let sdocs = Manpage.s_common_options
let docs_format = "OUTPUT FORMAT"
let docs_details = "OUTPUT DETAILS"
let docs_selection = "OPTIONS FOR SELECTING OPERATIONS"
let envs = B0_pager.envs ()
let exits =
  Term.exit_info err_no_log_file ~doc:"the log file does not exist." ::
  Term.exit_info err_unknown ~doc:"unknown error reported on stderr." ::
  Term.default_exits

let man_xrefs = [`Tool "b0"; `Tool "b00-cache"; `Tool "b00-hash"]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) tool reads build information and operations stored \
      in binary b0 log files.";
  `S Manpage.s_arguments;
  `S docs_format;
  `S docs_details;
  `P "If applicable.";
  `S docs_selection;
  `Blocks B00_ui.Op.query_man;
  `S Manpage.s_bugs;
  `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]

let log_file =
  let log_file_pos =
    let doc =
      "Log file to read, see also the $(b,--log-file) option and the \
       $(b,B0_LOG_FILE) environment variable."
    in
    let docv = "LOG_FILE" in
    Arg.(value & pos 0 (some B0_std_ui.fpath) None & info [] ~doc ~docv)
  in
  let log_file_arg = B00_ui.Memo.log_file ~docs:sdocs () in
  let log_file pos arg = match pos, arg with
  | None, (Some _ as v) | (Some _ as v), None -> `Ok v
  | None, None -> `Ok None
  | Some _, Some _ ->
      `Error (false,
              "The --log-file option and positional argument cannot \
               be used together.")
  in
  Term.(ret (pure log_file $ log_file_pos $ log_file_arg))

let tool =
  Term.(const log $ B0_std_ui.tty_cap ~docs:sdocs () $
        B0_std_ui.log_level ~docs:sdocs () $ B0_pager.don't ~docs:sdocs () $
        B00_ui.Memo.b0_dir ~docs:sdocs () $ log_file $
        B00_ui.Memo.Log.out_format_cli ~docs:docs_format () $
        B00_ui.Cli.out_details ~docs:docs_details () $
        B00_ui.Op.query_cli ~docs:docs_selection ()),
  Term.info "b00-log" ~version ~doc ~sdocs ~envs ~exits ~man ~man_xrefs

let main () = Term.(exit_status @@ eval tool)
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
