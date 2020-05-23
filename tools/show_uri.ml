(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
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
  let tty_cap = B00_std_ui.get_tty_cap tty_cap in
  let log_level = B00_std_ui.get_log_level log_level in
  B00_std_ui.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:1 @@
  let* browser = B00_www_browser.find ~browser () in
  let open_uri u = B00_www_browser.show ~background ~prefix browser (urify u) in
  let rec loop = function
  | [] -> Ok 0
  | u :: us -> let* () = open_uri u in loop us
  in
  loop uris

open Cmdliner

let show_uri =
  let version = "%%VERSION%%" in
  let sdocs = Manpage.s_common_options in
  let doc = "Show URIs in web browsers" in
  let man = [
    `S Manpage.s_description;
    `P "The $(mname) command show URIs specified on the command line.";
    `P "Up to sever platform and browser limitation, $(mname) tries to limit \
        the creation of new tabs, reloading existing ones which have the same \
        URI or are, see option $(b,--prefix), prefixed by the URI."; ]
  in
  let exits =
    Term.exit_info 1 ~doc:"if the URI failed to load in some way" ::
    Term.default_exits
  in
  let uris =
    let doc =
      "URI to show. If URI is an existing file path a corresponding file:// \
       URI is opened."
    in
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"URI")
  in
  Term.(const show_uris $
        B00_std_ui.tty_cap ~docs:sdocs () $
        B00_std_ui.log_level ~docs:sdocs () $
        B00_www_browser.background ~opts:["g"; "background"] () $
        B00_www_browser.prefix ~opts:["p"; "prefix"] () $
        B00_www_browser.browser ~opts:["b"; "browser"] () $ uris),
  Term.info "show-uri" ~version ~doc ~sdocs ~exits ~man

let main () = Term.(exit_status @@ eval show_uri)
let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
