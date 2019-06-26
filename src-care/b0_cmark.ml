(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
  cmark Cmd.(arg "--to" % format %% validate %% more_args %% shield (paths mds))

let write_page
    ?(generator = "") ?(lang = "") ?(scripts = []) ?(styles = []) ?(title = "")
    m ~frag ~o
  =
  (* FIXME this is a general fragment wrapper maybe we
     should move to Htmlg. *)
  let open B0_web.Htmlg in
  Memo.read m frag @@ fun contents ->
  let title = if title = "" then El.title_of_fpath o else title in
  let stamp =
    let data = generator :: lang :: title :: List.rev_append styles scripts in
    Hash.to_bytes (Memo.hash_string m (String.concat "" data))
  in
  Memo.write m ~stamp ~reads:[frag] o @@ fun () ->
  let open B0_web.Htmlg in
  let body = El.body [El.raw contents] in
  let page = El.basic_page ~generator ~lang ~scripts ~styles ~title body in
  Ok (El.to_string ~doc_type:true page)

let to_html
    ?generator ?lang ?scripts ?styles ?title m ~mds ~o_frag:frag ~o
  =
  cmd m ~mds ~o:frag;
  write_page ?generator ?lang ?scripts ?styles ?title m ~frag ~o

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
