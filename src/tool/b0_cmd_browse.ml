(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let urlify u =
  (* XXX code dupe with show-uri *)
  (* Detects if u is simply a file path and urlifies it *)
  let file_uri p = Fmt.str "file://%s" (Fpath.to_string p) in
  Result.value ~default:u @@
  let* p = Fpath.of_string u in
  let* exists = Os.Path.exists p in
  if not exists then Ok u else
  if Fpath.is_abs p then Ok (file_uri p) else
  let* cwd = Os.Dir.cwd () in
  Ok (file_uri Fpath.(cwd // p))

let process_url browser background prefix show_url no_pager =
  match show_url with
  | false ->
      let* browser = B0_web_browser.find ?cmd:browser () in
      Ok (fun u -> B0_web_browser.show ~background ~prefix browser (urlify u))
  | true ->
      let* pager = B0_pager.find ~don't:no_pager () in
      let* () = B0_pager.page_stdout pager in
      Ok (fun u -> Ok (Log.app (fun m -> m "%s" u)))

let browse key packs browser background prefix show_url no_pager c =
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* (V key) = B0_meta.Key.get_or_hint key in
  let no_lookup_error = packs = [] in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:true packs in
  Log.if_error' ~use:Os.Exit.some_error @@
  let* process_url = process_url browser background prefix show_url no_pager in
  let rec loop = function
  | [] -> Ok Os.Exit.ok
  | p :: ps ->
      match B0_pack.get_meta key p with
      | Error _ when no_lookup_error -> loop ps
      | Error _ as e -> Log.if_error' ~use:Os.Exit.no_such_name e
      | Ok v ->
          let url = Fmt.str "%a" (B0_meta.Key.pp_value key) v in
          let* () = process_url url in
          loop ps
  in
  loop packs

let browse_url urls browser background prefix show_url no_pager () =
  Log.if_error ~use:Os.Exit.some_error @@
  let* process_url = process_url browser background prefix show_url no_pager in
  let rec loop = function
  | [] -> Ok Os.Exit.ok
  | url :: urls -> let* () = process_url url in loop urls
  in
  loop urls

(* Command line interface *)

open Cmdliner

let packs ~right:r =
  let doc =
    "The $(docv) to act on. If none is specified this looks up the \
     URL of all packs that have the metadata key."
  in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"PACK")

let packs_tail = packs ~right:0
let packs_all = packs ~right:(-1)

let browser = B0_web_browser.browser ()
let background = B0_web_browser.background ()
let prefix = B0_web_browser.prefix ~default:false ()
let show_url =
  let doc = "Output URLs rather than opening them." in
  Arg.(value & flag & info ["s"; "show-urls"] ~doc)

let no_pager = B0_tool_std.Cli.no_pager

let browse_conf term =
  Term.(term $ browser $ background $ prefix $ show_url $ no_pager)

let homepage =
  let doc = "Browse homepage" in
  let descr = `P "$(iname) opens the $(b,B0_meta.homepage) URL." in
  B0_tool_std.Cli.subcmd_with_b0_file "homepage" ~doc ~descr @@
  browse_conf Term.(const browse $ const ".meta.homepage" $ packs_all)

let issues =
  let doc = "Browse issues" in
  let descr = `P "$(iname) opens the $(b,B0_meta.issues) URL." in
  B0_tool_std.Cli.subcmd_with_b0_file "issues" ~doc ~descr @@
  browse_conf Term.(const browse $ const ".meta.issues" $ packs_all)

let key =
  let doc = "Browse a metadata key value" in
  let descr = `P "$(iname) opens the URL found in $(i,KEY)." in
  B0_tool_std.Cli.subcmd_with_b0_file "key" ~doc ~descr @@
  browse_conf Term.(const browse $ B0_tool_std.Cli.pos_key $ packs_tail)

let online_doc =
  let doc = "Browse online documentation" in
  let descr = `P "$(iname) opens the $(b,B0_meta.online_doc) URL." in
  B0_tool_std.Cli.subcmd_with_b0_file "online-doc" ~doc ~descr @@
  browse_conf Term.(const browse $ const ".meta.online-doc" $ packs_all)

let url =
  let doc = "Browse URLs" in
  let descr =
    `Blocks
      (`P "$(iname) opens the given URLs." ::
       B0_web_browser.man_best_effort_reload)
  in
  let urls =
    let doc = "The $(docv) to open." in
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"URL")
  in
  B0_tool_std.Cli.subcmd "url" ~doc ~descr @@
  browse_conf Term.(const browse_url $ urls)

let cmd =
  let doc = "Open pack metadata URLs in your browser" in
  let descr =
    `P "$(iname) browses the URLs found in the metadata of packs. \
        This gives quick access to package homepages, issues, online docs, etc."
  in
  B0_tool_std.Cli.cmd_group "browse" ~doc ~descr @@
  [homepage; issues; key; online_doc; url]
