(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

let test_double_sep =
  Test.test "Fpath double sep normalization" @@ fun () ->
  let test ?__POS__ p q =
    Test.string ?__POS__ (Fpath.to_string (Fpath.v p)) q
  in
  test "/////////" "//" ~__POS__;
  test "//a///" "//a/" ~__POS__;
  test "//////a///" "//a/" ~__POS__;
  test "/a///bc" "/a/bc" ~__POS__;
  test "a///bc//" "a/bc/" ~__POS__;
  test "a///bc//c///////////////d" "a/bc/c/d" ~__POS__;
  ()

let test_is_prefix_strip_prefix =
  Test.test "Fpath.{is,strip}_prefix" @@ fun () ->
  let test ?__POS__:pos p q r =
    Test.block ?__POS__:pos @@ fun () ->
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
  test "a/b/" "a/b" None ~__POS__;
  test "a/b/" "a/b/" None ~__POS__;
  test "a/b" "a/b" None ~__POS__;
  test "a/b" "a/b/" None ~__POS__;
  test "a/b" "a/b/c" (Some "c") ~__POS__;
  test "a/b" "a/b/c/" (Some "c/") ~__POS__;
  test "a/b/" "a/b/c" (Some "c") ~__POS__;
  test "a/b/" "a/b/c/" (Some "c/") ~__POS__;
  test "a/b" "a/b" None ~__POS__;
  test "/a/b/" "/a/b" None ~__POS__;
  test "/a/b/" "/a/b/" None ~__POS__;
  test "/a/b" "/a/bc" None ~__POS__;
  test "/a/b" "/a/b" None ~__POS__;
  test "/a/b/" "/a/b" None ~__POS__;
  test "/a/b" "/a/b/" None ~__POS__;
  test "/a/b/" "/a/b/" None ~__POS__;
  test "/a/b" "/a/b/c" (Some "c") ~__POS__;
  test "/a/b/" "/a/b/c" (Some "c") ~__POS__;
  test "a" "a/b/c" (Some "b/c") ~__POS__;
  if Sys.win32 then begin
    test "C:\\a" "C:\\a\\b" (Some "b") ~__POS__;
  end;
  ()

let test_basename =
  Test.test "Fpath.basename" @@ fun () ->
  let test ?__POS__:pos p b ~strip_exts:b' =
    Test.block ?__POS__:pos @@ fun () ->
    let p = Fpath.v p in
    Test.string (Fpath.basename p) b ~__POS__;
    Test.string (Fpath.basename ~strip_exts:true p) b' ~__POS__;
  in
  test "bla" "bla" ~strip_exts:"bla" ~__POS__;
  test "bla" "bla" ~strip_exts:"bla" ~__POS__;
  test "/" "" ~strip_exts:"" ~__POS__;
  test "/.." "" ~strip_exts:"" ~__POS__;
  test "/." "" ~strip_exts:"" ~__POS__;
  test "bla/.." "" ~strip_exts:"" ~__POS__;
  test "bla/." "" ~strip_exts:"" ~__POS__;
  test ".." "" ~strip_exts:"" ~__POS__;
  test "." "" ~strip_exts:"" ~__POS__;
  test "./a" "a" ~strip_exts:"a" ~__POS__;
  test "./a/" "a" ~strip_exts:"a" ~__POS__;
  test "./abla" "abla" ~strip_exts:"abla" ~__POS__;
  test "./abla/" "abla" ~strip_exts:"abla" ~__POS__;
  test "/abla" "abla" ~strip_exts:"abla" ~__POS__;
  test "/abla/" "abla" ~strip_exts:"abla" ~__POS__;
  test "/.ocamlinit" ".ocamlinit" ~strip_exts:".ocamlinit" ~__POS__;
  test "/.ocamlinit/" ".ocamlinit" ~strip_exts:".ocamlinit" ~__POS__;
  test "/..ocamlinit/" "..ocamlinit" ~strip_exts:"..ocamlinit" ~__POS__;
  test "hop/.emacs.d" ".emacs.d" ~strip_exts:".emacs" ~__POS__;
  test "hap/.emacs.d/" ".emacs.d" ~strip_exts:".emacs" ~__POS__;
  test "hop/.emacs.d" ".emacs.d" ~strip_exts:".emacs" ~__POS__;
  test "hap/.emacs.d/" ".emacs.d" ~strip_exts:".emacs" ~__POS__;
  test "hap/archive.tar.gz/" "archive.tar.gz" ~strip_exts:"archive" ~__POS__;
  test "hap/archive.tar.gz" "archive.tar.gz" ~strip_exts:"archive" ~__POS__;
  test "/archive.tar.gz" "archive.tar.gz" ~strip_exts:"archive" ~__POS__;
  test "archive.tar.gz/" "archive.tar.gz" ~strip_exts:"archive" ~__POS__;
  test "archive.tar.gz" "archive.tar.gz" ~strip_exts:"archive" ~__POS__;
  if Sys.win32 then begin
    test "C:archive.tar.gz" "archive.tar.gz" ~strip_exts:"archive" ~__POS__;
  end;
  ()

let test_parent =
  Test.test "Fpath.parent" @@ fun () ->
  let test ?__POS__ p pp =
    Test.eq ?__POS__ (module Fpath) (Fpath.parent (Fpath.v p)) (Fpath.v pp);
  in
  test "a/b/c" "a/b/" ~__POS__;
  test "a/b" "a/" ~__POS__;
  test "a" "." ~__POS__;
  test "." ".." ~__POS__;
  test "./" ".." ~__POS__;
  test "././" ".." ~__POS__;
  test "././a" "." ~__POS__;
  test "././a/" "." ~__POS__;
  test ".." "../.." ~__POS__;
  test "../.." "../../.." ~__POS__;
  test "a/b/." "a/" ~__POS__;
  test "a/b/./" "a/" ~__POS__;
  test "a/b/./a" "a/b/./" ~__POS__;
  test "a/.." "a/../.." ~__POS__;
  test "a/b/.." "a/b/../.." ~__POS__;
  test "a/../c" "." ~__POS__;
  (* abs of the above *)
  test "/a/b/c" "/a/b/" ~__POS__;
  test "/a/b" "/a/" ~__POS__;
  test "/a" "/" ~__POS__;
  test "/" "/" ~__POS__;
  test "/." "/" ~__POS__;
  test "/./" "/" ~__POS__;
  test "/././" "/" ~__POS__;
  test "/././a" "/" ~__POS__;
  test "/././a/" "/" ~__POS__;
  test "/.." "/../.." ~__POS__;
  test "/../.." "/../../.." ~__POS__;
  test "/a/b/." "/a/" ~__POS__;
  test "/a/b/./" "/a/" ~__POS__;
  test "/a/b/./a" "/a/b/./" ~__POS__;
  test "/a/.." "/a/../.." ~__POS__;
  test "/a/b/.." "/a/b/../.." ~__POS__;
  ()

let test_relative =
  Test.test "Fpath.relative" @@ fun () ->
  let test ?__POS__ p ~to_dir q =
    let to_dir = Fpath.v to_dir and p = Fpath.v p and q = Fpath.v q in
    Test.eq ?__POS__ (module Fpath) (Fpath.relative ~to_dir p) q
  in
  test "/a/b" ~to_dir:"/a/b/c" "../../b" ~__POS__;
  test "/a/b" ~to_dir:"a" "/a/b" ~__POS__;
  test "a/b" ~to_dir:"/a/b/c" "a/b" ~__POS__;
  test "a/b" ~to_dir:"a/b" "../b" ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
