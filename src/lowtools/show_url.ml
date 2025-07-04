(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let url_error = 1

let stdin_file_url ~tname =
  let* tmp, force = match tname with
  | None -> let* tmp = Os.Path.tmp ~name:"show-url-%s" () in Ok (tmp, false)
  | Some n ->
      let file = Os.Dir.default_tmp () in
      let* tmp = Fpath.add_segment file n in Ok (tmp, true)
  in
  let* data = Os.File.read Fpath.dash in
  let* () = Os.File.write ~force ~make_path:false tmp data in
  Ok (Fmt.str "file://%s" (Fpath.to_url_path tmp))

let show_urls ~background ~prefix ~browser ~urls ~tname =
  let* browser = B0_web_browser.find ?cmd:browser () in
  Log.if_error' ~use:url_error @@
  let rec loop = function
  | [] -> Ok 0
  | url :: urls ->
      let* url = match url with
      | "-" -> stdin_file_url ~tname
      | url ->
          let* cwd = Os.Dir.cwd () in
          let root_path = Some (Fpath.to_url_path cwd) in
          Ok (B0_url.to_absolute ~scheme:"file" ~root_path url)
      in
      let* () = B0_web_browser.show ~background ~prefix browser url in
      loop urls
  in
  loop urls

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show URL, files and stdin in web browsers" in
  let exits =
    Cmd.Exit.info url_error ~doc:"if the URL failed to load in some way" ::
    Cmd.Exit.defaults
  in
  let man = [
    `S Manpage.s_description;
    `P "The $(iname) command shows URLs specified on the command line.";
    `Blocks B0_web_browser.man_best_effort_reload;
    `P "For example:";
    `Pre "$(cmd) $(b,https://example.org)"; `Noblank;
    `Pre "$(cmd) $(b,index.html) $(b,doc.pdf)";
    `P "It can also directly read $(b,stdin) by writing it to a temporary \
        file. Certain browser do not recognize certain file types without \
        an extension use $(b,-t) to specify a file name; it allows \
        reloads while keeping the file in a temporary directory.";
    `Pre "$(b,echo 'Hey' |) $(cmd)"; `Noblank;
    `Pre "$(b,echo 'Hey' |) $(cmd) $(b,-t hey.txt) "; `Noblank;
    `Pre "$(b,echo 'Ho!' |) $(cmd) $(b,-t hey.txt) "; `Noblank;
    `Pre "$(b,dot -Tsvg graph.dot |) $(cmd) $(b,-t graph.svg)";
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,b0) system. See \
        $(i,https:/erratique.ch/software/b0) for contact information.";
  ]
  in
  Cmd.make (Cmd.info "show-url" ~version:"%%VERSION%%" ~doc ~exits ~man) @@
  let+ () = B0_std_cli.configure_log ()
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
       file://$(docv) is shown. If $(docv) is $(b,-), data from $(b,stdin) \
       is read and written to a temporary file which is shown."
    in
    Arg.(value & pos_all Arg.filepath ["-"] & info [] ~doc ~docv:"URL")
  in
  show_urls ~background ~prefix ~browser ~urls ~tname

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
