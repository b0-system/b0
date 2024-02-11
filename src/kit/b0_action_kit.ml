(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let fetch_url
    ?env ?stderr ?args ?(progress = true) env' url file
  =
  if progress then (Log.app (fun m -> m "Fetching %a" Fmt.code' url));
  let args = B0_http.Http_client.curl_fetch_args ?args ~progress url file in
  let cmd = Cmd.(tool "curl" %% args) in
  let* curl = B0_env.get_cmd env' cmd in
  Os.Cmd.run ?env ?stderr curl
