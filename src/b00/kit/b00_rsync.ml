(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let rsync = Os.Cmd.get (Cmd.atom "rsync")
let copy
    ?(opts = Cmd.atom "-azh") ?(stats = true) ~delete
    ?(src_host = "") ~src ?(dst_host = "") dst
  =
  let* rsync = Os.Cmd.get (Cmd.atom "rsync") in
  (* XXX force slashes ?  *)
  let src = src_host ^ Fpath.to_string src in
  let dst = dst_host ^ Fpath.to_string dst in
  let cmd = Cmd.(rsync %% if' delete (atom "--delete") %%
                 if' stats (atom "--stats") %% opts % src % dst)
  in
  Os.Cmd.run cmd

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
