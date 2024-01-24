(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let urify u = (* Detects if u is simply a file path and urifies it *)
  let file_url p = Fmt.str "file://%s" (Fpath.to_string p) in
  Result.value ~default:u @@
  let* p = Fpath.of_string u in
  let* exists = Os.Path.exists p in
  if not exists then Ok u else
  if Fpath.is_abs p then Ok (file_url p) else
  let* cwd = Os.Dir.cwd () in
  Ok (file_url Fpath.(cwd // p))

let show_urls tty_cap log_level background prefix browser urls =
  let tty_cap = B0_cli.B0_std.get_tty_cap tty_cap in
  let log_level = B0_cli.B0_std.get_log_level log_level in
  B0_cli.B0_std.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:1 @@
  let* browser = B0_web_browser.find ~browser () in
  let open_url u = B0_web_browser.show ~background ~prefix browser (urify u) in
  let rec loop = function
  | [] -> Ok 0
  | u :: us -> let* () = open_url u in loop us
  in
  loop urls

open Cmdliner

let version = "%%VERSION%%"
let sdocs = Manpage.s_common_options
let doc = "Show URLs in web browsers"

let man = [
  `S Manpage.s_description;
  `P "The $(iname) command show URLs specified on the command line.";
  `Blocks B0_web_browser.man_best_effort_reload; ]

let exits =
  Cmd.Exit.info 1 ~doc:"if the URL failed to load in some way" ::
  Cmd.Exit.defaults

let uris =
  let doc =
    "URL to show. If URL is an existing file path a corresponding file:// \
     URL is opened."
  in
  Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"URL")

let tool =
  Cmd.v (Cmd.info "show-url" ~version ~doc ~sdocs ~exits ~man)
    Term.(const show_urls $
          B0_cli.B0_std.tty_cap ~docs:sdocs () $
          B0_cli.B0_std.log_level ~docs:sdocs () $
          B0_web_browser.background () $
          B0_web_browser.prefix ~default:false () $
          B0_web_browser.browser () $ uris)

let main () = exit (Cmd.eval' tool)
let () = if !Sys.interactive then () else main ()
