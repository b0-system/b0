(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

let test_parent () =
  let test p pp =
    assert (Fpath.equal (Fpath.parent (Fpath.v p)) (Fpath.v pp))
  in
  test "a/b/c" "a/b/";
  test "a/b" "a/";
  test "a" "./";
  test "." "../";
  test "./" "../";
  test ".." "../../";
  test "a/b/." "a/";
  test "a/b/./" "a/";
  test "a/b/./a" "a/b/./";
  test "a/.." "a/../../";
  (* abs of the above *)
  test "/a/b/c" "/a/b/";
  test "/a/b" "/a/";
  test "/a" "/";
  test "/" "/";
  test "/." "/";
  test "/./" "/";
  test "/.." "/../../";
  test "/a/b/." "/a/";
  test "/a/b/./" "/a/";
  test "/a/b/./a" "/a/b/./";
  test "/a/.." "/a/../../";
  ()

let test () =
  test_parent ();
  ()

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
