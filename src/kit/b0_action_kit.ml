(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let download_url
    ?env ?stderr ?args ?(progress = true) env'
    ?mode ~force ~make_path url ~dst:file
  =
  if progress then (Log.stdout (fun m -> m "Fetching %a" Fmt.code url));
  let curlfile = Fpath.dash in
  let args = B0_http.Http_client.curl_fetch_args ?args ~progress url curlfile in
  let cmd = Cmd.(tool "curl" %% args) in
  let* curl = B0_env.get_cmd env' cmd in
  let stdout = Os.Cmd.out_file ?mode ~force ~make_path file in
  Os.Cmd.run ?env ?stderr ~stdout curl
