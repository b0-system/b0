(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let fetch_url
    ?env ?stderr ?(args = Cmd.empty) ?(progress = true) env' url file
  =
  if progress then (Log.app (fun m -> m "Fetching %a" Fmt.code' url));
  let progress = if progress then Cmd.arg "-#" else Cmd.arg "--silent" in
  let cmd =
    Cmd.(arg "curl" % "--fail" % "--show-error" %% progress % "-L" %
         "-o" %% path file %% args % url)
  in
  let* curl = B0_env.get_cmd env' cmd in
  Os.Cmd.run ?env ?stderr curl
