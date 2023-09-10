(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let urify u = (* Detects if u is simply a file path and urifies it *)
  let file_uri p = Fmt.str "file://%s" (Fpath.to_string p) in
  Result.value ~default:u @@
  let* p = Fpath.of_string u in
  let* exists = Os.Path.exists p in
  if not exists then Ok u else
  if Fpath.is_abs p then Ok (file_uri p) else
  let* cwd = Os.Dir.cwd () in
  Ok (file_uri Fpath.(cwd // p))

let show_uris tty_cap log_level background prefix browser uris =
  let tty_cap = B0_cli.B0_std.get_tty_cap tty_cap in
  let log_level = B0_cli.B0_std.get_log_level log_level in
  B0_cli.B0_std.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:1 @@
  let* browser = B0_web_browser.find ~browser () in
  let open_uri u = B0_web_browser.show ~background ~prefix browser (urify u) in
  let rec loop = function
  | [] -> Ok 0
  | u :: us -> let* () = open_uri u in loop us
  in
  loop uris

open Cmdliner

let version = "%%VERSION%%"
let sdocs = Manpage.s_common_options
let doc = "Show URIs in web browsers"

let man = [
  `S Manpage.s_description;
  `P "The $(mname) command show URIs specified on the command line.";
  `P "Up to sever platform and browser limitation, $(mname) tries to limit \
      the creation of new tabs, reloading existing ones which have the same \
      URI or are, see option $(b,--prefix), prefixed by the URI."; ]

let exits =
  Cmd.Exit.info 1 ~doc:"if the URI failed to load in some way" ::
  Cmd.Exit.defaults

let uris =
  let doc =
    "URI to show. If URI is an existing file path a corresponding file:// \
     URI is opened."
  in
  Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"URI")

let tool =
  Cmd.v (Cmd.info "show-uri" ~version ~doc ~sdocs ~exits ~man)
    Term.(const show_uris $
          B0_cli.B0_std.tty_cap ~docs:sdocs () $
          B0_cli.B0_std.log_level ~docs:sdocs () $
          B0_web_browser.background ~opts:["g"; "background"] () $
          B0_web_browser.prefix ~opts:["p"; "prefix"] () $
          B0_web_browser.browser ~opts:["b"; "browser"] () $ uris)

let main () = exit (Cmd.eval' tool)
let () = if !Sys.interactive then () else main ()
