(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let process_url ~browser ~background ~prefix ~output_urls ~no_pager =
  if output_urls then
    let* pager = B0_pager.find ~no_pager () in
    let* () = B0_pager.page_stdout pager in
    Ok (fun url -> Ok (Fmt.pr "%s@." url))
  else
  let* browser = B0_web_browser.find ?cmd:browser () in
  Result.ok @@ fun url ->
  let* cwd = Os.Dir.cwd () in
  let root_path = Some (Fpath.to_url_path cwd) in
  let url = Net.Url.to_absolute ~scheme:"file" ~root_path url in
  B0_web_browser.show ~background ~prefix browser url

let browse
    ~key ~packs ~browser ~background ~prefix ~output_urls ~no_pager _conf
  =
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* (V key) = B0_meta.Key.get_or_hint key in
  let no_lookup_error = packs = [] in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:true packs in
  Log.if_error' ~use:Os.Exit.some_error @@
  let* process_url =
    process_url ~browser ~background ~prefix ~output_urls ~no_pager
  in
  let rec loop = function
  | [] -> Ok Os.Exit.ok
  | pack :: packs ->
      match B0_pack.get_meta key pack with
      | Error _ when no_lookup_error -> loop packs
      | Error _ as e -> Log.if_error' ~use:Os.Exit.no_such_name e
      | Ok v ->
          let url = Fmt.str "%a" (B0_meta.Key.pp_value key) v in
          let* () = process_url url in
          loop packs
  in
  loop packs

let browse_url ~urls ~browser ~background ~prefix ~output_urls ~no_pager () =
  Log.if_error ~use:Os.Exit.some_error @@
  let* process_url =
    process_url ~browser ~background ~prefix ~output_urls ~no_pager
  in
  let* () = List.iter_stop_on_error process_url urls in
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let packs_doc =
  "The $(docv) to act on. If none is specified this looks up the \
   URL of all packs that have the metadata key."

let packs_pos0 = B0_cli.act_on_packs_posn ~first:0 ~doc:packs_doc ()
let packs_pos1 = B0_cli.act_on_packs_posn ~first:1 ~doc:packs_doc ()
let browser = B0_web_browser.browser ()
let background = B0_web_browser.background ()
let prefix = B0_web_browser.prefix ~default:false ()
let no_pager = B0_cli.no_pager
let output_urls =
  let doc = "Output URLs on $(b,stdout) rather than opening them." in
  Arg.(value & flag & info ["t"; "output-urls"] ~doc)

let browse_conf term =
  let+ term and+ browser and+ background and+ prefix and+ output_urls
  and+ no_pager in
  term ~browser ~background ~prefix ~output_urls ~no_pager

let homepage_cmd =
  let doc = "Browse homepage" in
  let descr = `P "$(cmd) opens the $(b,B0_meta.homepage) URL." in
  B0_tool_cli.cmd_with_b0_file "homepage" ~doc ~descr @@ browse_conf @@
  let+ packs = packs_pos0 in
  browse ~key:".meta.homepage" ~packs

let issues_cmd =
  let doc = "Browse issues" in
  let descr = `P "$(cmd) opens the $(b,B0_meta.issues) URL." in
  B0_tool_cli.cmd_with_b0_file "issues" ~doc ~descr @@ browse_conf @@
  let+ packs = packs_pos0 in
  browse ~key:".meta.issues" ~packs

let key_cmd =
  let doc = "Browse a metadata key value" in
  let descr = `P "$(cmd) opens the URL found in $(i,KEY)." in
  B0_tool_cli.cmd_with_b0_file "key" ~doc ~descr @@ browse_conf @@
  let+ key = B0_cli.required_metadata_key_pos0 and+ packs = packs_pos1 in
  browse ~key ~packs

let online_doc_cmd =
  let doc = "Browse online documentation" in
  let descr = `P "$(cmd) opens the $(b,B0_meta.online_doc) URL." in
  B0_tool_cli.cmd_with_b0_file "online-doc" ~doc ~descr @@ browse_conf @@
  let+ packs = packs_pos0 in
  browse ~key:".meta.online-doc" ~packs

let url_cmd =
  let doc = "Browse URLs" in
  let descr = `Blocks
      (`P "$(cmd) opens the given URLs. See also the $(b,show-url) command \
           line tool" :: B0_web_browser.man_best_effort_reload)
  in
  let urls =
    let doc = "The $(docv) to open." in
    Arg.(non_empty & pos_all filepath [] & info [] ~doc ~docv:"URL")
  in
  B0_tool_cli.cmd "url" ~doc ~descr @@ browse_conf @@
  let+ urls in
  browse_url ~urls

let cmd =
  let doc = "Open pack metadata URLs in your browser" in
  let descr = `Blocks [
      `P "$(cmd) browses the URLs found in the metadata of packs. This gives \
          quick access to package homepages, issues, online docs, etc.";
      `Pre "$(cmd) $(b,issues)  # Browse default pack issue tracker";
    ]
  in
  B0_tool_cli.cmd_group "browse" ~doc ~descr @@
  [homepage_cmd; issues_cmd; key_cmd; online_doc_cmd; url_cmd]
