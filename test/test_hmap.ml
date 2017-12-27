(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Key_info = struct
  type 'a t = unit
  let key_kind = "door key"
  let key_namespaced = true
  let key_name_tty_color = `Default
  let pp _ ppf () = ()
end
module M = B0.Hmap.Make (Key_info) ()

let loc = B0.Def.Loc.lib "test_hmap"

let test_hmap () =
  let k1 = M.Key.v ~loc "hey" B0.Conv.bool () in
  let k2 = M.Key.v ~loc "hey" B0.Conv.int  () in
  let k3 = M.Key.v ~loc "ney" B0.Conv.int () in
  let m = M.empty |> M.add k1 true |> M.add k2 3 in
  assert (M.Key.(name (of_typed k2)) = "hey~1");
  assert (M.get k1 m = true);
  assert (M.get k2 m = 3);
  assert (M.find k3 m = None);
  let l, errs = M.encode m in
  assert (errs = []);
  let m, errs = M.decode (("ha", "ho") :: l) in
  assert (errs = ["ha", `Unknown]);
  assert (M.get k1 m = true);
  assert (M.get k2 m = 3);
  assert (M.find k3 m = None);
  ()

let tests () =
  test_hmap ();
  ()

let () = tests ()

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
