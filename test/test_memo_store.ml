(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax
open Test_memo_setup

let lookup_b0_os build_dir m =
  let store = B0_memo.Store.create m ~dir:build_dir [] in
  let* n = B0_memo.Store.get store B0_os.name in
  let* v = B0_memo.Store.get store B0_os.version in
  let* d = B0_memo.Store.get store B0_os.distribution in
  let* f = B0_memo.Store.get store B0_os.family in
  let* a = B0_memo.Store.get store B0_os.arch in
  let* an = B0_memo.Store.get store B0_os.arch_normalized in
  let* bits = B0_memo.Store.get store B0_os.arch_bits in
  Log.app (fun m ->
      m "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
        Fmt.(field "name" id string) n
        Fmt.(field "version" id string) v
        Fmt.(field "distribution" id string) d
        Fmt.(field "family" id string) f
        Fmt.(field "arch" id string) a
        Fmt.(field "arch-normalized" id string) an
        Fmt.(field "arch-bits" id int) bits);
  Fut.return ()

let test_memo_store () =
  with_memo lookup_b0_os;
  ()

let () = test_memo_store ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
