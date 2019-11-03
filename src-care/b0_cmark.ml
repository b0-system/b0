(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00

let tool = Tool.by_name "cmark"
let cmd
    ?(validate_utf_8 = true) ?(format = "html") ?args:(more_args = Cmd.empty) m
    ~mds ~o
  =
  let cmark = Memo.tool m tool in
  let validate = Cmd.if' validate_utf_8 (Cmd.arg "--validate-utf8") in
  Memo.spawn m ~reads:mds ~writes:[o] ~stdout:(`File o) @@
  cmark Cmd.(arg "--to" % format %% validate %% more_args %%
             unstamp (paths mds))

let to_html
    ?generator ?lang ?scripts ?styles ?title m ~mds ~o_frag:frag ~o
  =
  cmd m ~mds ~o:frag;
  B0_htmlg.El.write_page ?generator ?lang ?scripts ?styles ?title m ~frag ~o

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
