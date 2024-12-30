(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let url_error = 1
let file_url p = Fmt.str "file://%s" (Fpath.to_string p)

let stdin_file_url ~tname =
  let* tmp, force = match tname with
  | None -> let* tmp = Os.Path.tmp ~name:"show-url-%s" () in Ok (tmp, false)
  | Some n ->
      let file = Os.Dir.default_tmp () in
      let* tmp = Fpath.add_seg file n in Ok (tmp, true)
  in
  let* data = Os.File.read Fpath.dash in
  let* () = Os.File.write ~force ~make_path:false tmp data in
  Ok (file_url tmp)

let file_url p =
  let* p = Fpath.of_string p in
  let* () = Os.Path.must_exist p in
  if Fpath.is_abs p then Ok (file_url p) else
  let* cwd = Os.Dir.cwd () in
  Ok (file_url Fpath.(cwd // p))

let prepare_url ~tname = function
| "-" -> stdin_file_url ~tname
| u -> match B0_url.scheme u with Some _ -> Ok u | None -> file_url u

let show_urls ~color ~log_level ~background ~prefix ~browser ~urls ~tname =
  let styler = B0_std_cli.get_styler color in
  let log_level = B0_std_cli.get_log_level log_level in
  B0_std_cli.setup styler log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* browser = B0_web_browser.find ?cmd:browser () in
  let open_url u = B0_web_browser.show ~background ~prefix browser u in
  Log.if_error' ~use:url_error @@
  let rec loop = function
  | [] -> Ok 0
  | u :: us ->
      let* u = prepare_url ~tname u in
      let* () = open_url u in
      loop us
  in
  loop urls

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show URLs in web browsers" in
  let exits =
    Cmd.Exit.info url_error ~doc:"if the URL failed to load in some way" ::
    Cmd.Exit.defaults
  in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command shows URLs specified on the command line.";
    `Blocks B0_web_browser.man_best_effort_reload;
    `P "For example:";
    `Pre "$(iname) $(b,https://example.org"; `Noblank;
    `Pre "$(iname) $(b,index.html) $(b,doc.pdf)";
    `P "It can also directly read $(b,stdin) by writing it to a temporary \
        file. Certain browser do not recognize certain file types without \
        an extension use $(b,-t) to specify a file name; it allows \
        reloads while keeping the file in a temporary directory.";
    `Pre "$(b,echo 'Hey' |) $(iname)"; `Noblank;
    `Pre "$(b,echo 'Hey' |) $(iname) $(b,-t hey.txt) "; `Noblank;
    `Pre "$(b,echo 'Ho!' |) $(iname) $(b,-t hey.txt) "; `Noblank;
    `Pre "$(b,dot -Tsvg graph.dot |) $(iname) $(b,-t g.svg)";
  ]
  in
  Cmd.v (Cmd.info "show-url" ~version:"%%VERSION%%" ~doc ~exits ~man) @@
  let+ color = B0_std_cli.color ()
  and+ log_level = B0_std_cli.log_level ()
  and+ background = B0_web_browser.background ()
  and+ prefix = B0_web_browser.prefix ~default:false ()
  and+ browser = B0_web_browser.browser ()
  and+ tname =
    let doc =
      "Use $(docv) as a file in the temporary directory when reading \
       from $(b,stdin). This enables reloads and allows to specify a \
       proper file suffix which your browser might need."
    in
    let docv = "FILENAME" in
    Arg.(value & opt (some string) None & info ["t"; "tmp-filename"] ~doc ~docv)
  and+ urls =
    let doc =
      "Show $(docv). If $(docv) is an existing file path a corresponding \
       file:// $(docv) is opened. If $(docv) is $(b,-) data from $(b,stdin) \
       is read and written to a temporary file which is shown."
    in
    Arg.(value & pos_all string ["-"] & info [] ~doc ~docv:"URL")
  in
  show_urls ~color ~log_level ~background ~prefix ~browser ~urls ~tname

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
