(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let open_issue ~user ~repo_url ~title ~body =
  let* httpc = B0_http.Http_client.make () in
  let* auth = B0_github.Auth.make ~user () in
  let* repo = B0_github.Repo.of_url repo_url in
  let* num, url = B0_github.Issue.open' httpc auth repo ~title ~body () in
  let* path = match Net.Url.path url with
  | None -> Fmt.error "No path in returned url %s" url
  | Some path -> Ok path
  in
  let* json = B0_github.req_json_v3 httpc auth ~path `GET `Empty in
  let* html_url = B0_json.Jsonq.(query (mem "html_url" string)) json in
  Log.stdout (fun m -> m "@[%s@]" html_url);
  Ok ()

let user = None (* See B0_github.Auth.make to see how one is looked up. *)
let title = "TODO"
let body =
  {|TODO|}

let main () =
  Log.if_error ~use:1 @@
  let* () = open_issue ~user ~repo_url:Sys.argv.(1) ~title ~body in
  Ok 0

let () = if !Sys.interactive then () else exit (main ())
