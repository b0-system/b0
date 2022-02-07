(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let assert_eq' eq pp fnd exp =
  if eq fnd exp then () else
  (Fmt.pr "@[<v3>Expected: @[%a@]@,Found: @[%a@]@]@."
     pp exp pp fnd; assert false)

let assert_fpath = assert_eq' Fpath.equal Fpath.pp_unquoted

let test_double_sep () =
  let test p q = assert (String.equal (Fpath.to_string (Fpath.v p)) q) in
  test "/////////" "//";
  test "//a///" "//a/";
  test "//////a///" "//a/";
  test "/a///bc" "/a/bc";
  test "a///bc//" "a/bc/";
  test "a///bc//c///////////////d" "a/bc/c/d";
  ()

let test_is_prefix_strip_prefix () =
  let test p q r =
    let p = Fpath.v p and q = Fpath.v q in
    match r with
    | None ->
        assert (not (Fpath.is_prefix p q));
        assert (Fpath.strip_prefix p q = None);
    | Some r ->
        let r = Fpath.v r in
        assert (Fpath.is_prefix p q);
        match Fpath.strip_prefix p q with
        | None -> assert false
        | Some r' ->
            assert (Fpath.equal r r');
            assert (Fpath.equal (Fpath.( p // r')) q);
  in
  test "a/b/" "a/b" None;
  test "a/b/" "a/b/" None;
  test "a/b" "a/b" None;
  test "a/b" "a/b/" None;
  test "a/b" "a/b/c" (Some "c");
  test "a/b" "a/b/c/" (Some "c/");
  test "a/b/" "a/b/c" (Some "c");
  test "a/b/" "a/b/c/" (Some "c/");
  test "a/b" "a/b" None;
  test "/a/b/" "/a/b" None;
  test "/a/b/" "/a/b/" None;
  test "/a/b" "/a/bc" None;
  test "/a/b" "/a/b" None;
  test "/a/b/" "/a/b" None;
  test "/a/b" "/a/b/" None;
  test "/a/b/" "/a/b/" None;
  test "/a/b" "/a/b/c" (Some "c");
  test "/a/b/" "/a/b/c" (Some "c");
  test "a" "a/b/c" (Some "b/c");
  if Sys.win32 then begin
    test "C:\\a" "C:\\a\\b" (Some "b");
  end;
  ()

let test_basename () =
  let test p b ~no_ext:b' =
    let p = Fpath.v p in
    assert (Fpath.basename p = b);
    assert (Fpath.basename ~no_ext:true p = b');
  in
  test "bla" "bla" ~no_ext:"bla";
  test "bla" "bla" ~no_ext:"bla";
  test "/" "" ~no_ext:"";
  test "/.." "" ~no_ext:"";
  test "/." "" ~no_ext:"";
  test "bla/.." "" ~no_ext:"";
  test "bla/." "" ~no_ext:"";
  test ".." "" ~no_ext:"";
  test "." "" ~no_ext:"";
  test "./a" "a" ~no_ext:"a";
  test "./a/" "a" ~no_ext:"a";
  test "./abla" "abla" ~no_ext:"abla";
  test "./abla/" "abla" ~no_ext:"abla";
  test "/abla" "abla" ~no_ext:"abla";
  test "/abla/" "abla" ~no_ext:"abla";
  test "/.ocamlinit" ".ocamlinit" ~no_ext:".ocamlinit";
  test "/.ocamlinit/" ".ocamlinit" ~no_ext:".ocamlinit";
  test "/..ocamlinit/" "..ocamlinit" ~no_ext:"..ocamlinit";
  test "hop/.emacs.d" ".emacs.d" ~no_ext:".emacs";
  test "hap/.emacs.d/" ".emacs.d" ~no_ext:".emacs";
  test "hop/.emacs.d" ".emacs.d" ~no_ext:".emacs";
  test "hap/.emacs.d/" ".emacs.d" ~no_ext:".emacs";
  test "hap/archive.tar.gz/" "archive.tar.gz" ~no_ext:"archive";
  test "hap/archive.tar.gz" "archive.tar.gz" ~no_ext:"archive";
  test "/archive.tar.gz" "archive.tar.gz" ~no_ext:"archive";
  test "archive.tar.gz/" "archive.tar.gz" ~no_ext:"archive";
  test "archive.tar.gz" "archive.tar.gz" ~no_ext:"archive";
  if Sys.win32 then begin
    test "C:archive.tar.gz" "archive.tar.gz" ~no_ext:"archive";
  end;
  ()

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

let test_relative () =
  let test p ~to_dir  q =
    let to_dir = Fpath.v to_dir and p = Fpath.v p and q = Fpath.v q in
    assert_fpath (Fpath.relative ~to_dir p) q
  in
  test "/a/b" ~to_dir:"/a/b/c" "../../b";
  test "/a/b" ~to_dir:"a" "/a/b";
  test "a/b" ~to_dir:"/a/b/c" "a/b";
  test "a/b" ~to_dir:"a/b" "../b";
  ()

let test () =
  test_double_sep ();
  test_is_prefix_strip_prefix ();
  test_parent ();
  test_basename ();
  test_relative ();
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
