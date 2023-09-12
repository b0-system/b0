(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let tool = B0_memo.Tool.by_name "cmark"
let cmd ?(validate_utf_8 = true) ?(format = "html") m ~opts ~mds ~o =
  let cmark = B0_memo.tool m tool in
  let validate = Cmd.if' validate_utf_8 (Cmd.arg "--validate-utf8") in
  B0_memo.spawn m ~reads:mds ~writes:[o] ~stdout:(`File o) @@
  cmark Cmd.(arg "--to" % format %% validate %% opts %% unstamp (paths mds))

let to_html
    ?generator ?lang ?scripts ?styles ?title m ~opts ~mds ~o_frag:frag ~o
  =
  cmd m ~opts ~mds ~o:frag;
  B0_web_page.write ?generator ?lang ?scripts ?styles ?title m ~frag ~o
