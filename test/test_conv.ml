(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

(* Bad: error paths are not tested *)

let trip_bin ?(eq = ( = )) c v =
  let s = Conv.to_bin c v |> Result.to_failure in
  let v' = Conv.of_bin c s |> Result.to_failure in
  assert (eq v v')

let trip_txt ?(eq = ( = )) c v =
  let s = Conv.to_txt c v |> Result.to_failure in
  let v' = Conv.of_txt c s |> Result.to_failure in
  assert (eq v v')

let trip_conv ?eq c v = trip_bin ?eq c v; trip_txt ?eq c v

let txt_dec ?(eq = ( = )) c v s =
  let v' = Conv.of_txt c s |> Result.to_failure in
  assert (eq v v')

let test_bool () =
  trip_conv Conv.bool true; trip_conv Conv.bool false;
  txt_dec Conv.bool true "  true ";
  txt_dec Conv.bool true " ; false\n true ";
  txt_dec Conv.bool true " ; false\r\ntrue ";
  txt_dec Conv.bool true " ; false\r\n           true ; bllllll ";
  ()

let test_byte () =
  trip_conv Conv.byte 0; trip_conv Conv.byte 42; trip_conv Conv.byte 255;
  txt_dec Conv.byte 255 "255 ";
  ()

let test_int () =
  trip_conv Conv.int max_int; trip_conv Conv.int 0; trip_conv Conv.int 42;
  trip_conv Conv.int min_int;
  ()

let test_int31 () =
  trip_conv Conv.int31 (-1); trip_conv Conv.int31 0;
  trip_conv Conv.int31 (1 lsl 30 - 1);
  ()

let test_int32 () =
  trip_conv Conv.int32 (-1l); trip_conv Conv.int32 0l; trip_conv Conv.int32 42l;
  trip_conv Conv.int32 Int32.max_int; trip_conv Conv.int32 Int32.min_int;
  ()

let test_int64 () =
  trip_conv Conv.int64 (-1L); trip_conv Conv.int64 0L; trip_conv Conv.int64 42L;
  trip_conv Conv.int64 Int64.max_int; trip_conv Conv.int64 Int64.min_int;
  ()

let test_float () =
  let nan_eq x y = x <> x && y <> y in
  trip_conv ~eq:nan_eq Conv.float nan; trip_conv Conv.float infinity;
  trip_conv Conv.float neg_infinity; trip_conv Conv.float 1.0;
  trip_conv Conv.float 1.5; trip_conv Conv.float 0.; trip_conv Conv.float (-0.);
  ()

let test_atom () =
  trip_conv Conv.atom "";
  trip_conv Conv.atom "bla"; trip_conv Conv.atom "a&b";
  trip_conv Conv.atom "bl;a"; trip_conv Conv.atom "a\"b";
  trip_conv Conv.atom "()"; trip_conv Conv.atom "()";
  txt_dec Conv.atom ("habla") (" habla ");
  txt_dec Conv.atom ("habla") (" \"habla\" ");
  txt_dec Conv.atom ("hab^la") (" \"hab^^la\" ");
  txt_dec Conv.atom ("hab^l\na") (" \"hab^^l^na\" ");
  txt_dec Conv.atom ("hab^l\na") (" \"hab^^l^na\" ");
  txt_dec Conv.atom ("hab\x00a") (" \"hab^u{0000}a\" ");
  txt_dec Conv.atom ("hab\xF0\x9F\x90\xAB ") (" \"hab^u{1F42B} \" ");
  txt_dec Conv.atom ("habla") (" \"ha^\n   bla\" ");
  txt_dec Conv.atom ("habla") (" \"ha^\r\t   bla\" ");
  txt_dec Conv.atom ("ha bla") (" \"ha bla\" ");
  txt_dec Conv.atom ("") (" \"\" ");
  txt_dec Conv.atom ("()") (" \"()\" ");
  ()

let test_atom_non_empty () =
  trip_conv Conv.atom_non_empty "a";
  ()

let test_string_bytes () =
  trip_conv Conv.string_bytes ""; trip_conv Conv.string_bytes "ab";
  trip_conv Conv.string_bytes "\""; trip_conv Conv.string_bytes "\n\x00";
  txt_dec Conv.string_bytes ("\x00\xFF") ("(hex 00ff)");
  txt_dec Conv.string_bytes ("") ("(hex \"\")");
  ()

let test_option () =
  let c = Conv.(option int31) in
  trip_conv c None;
  trip_conv c (Some 1);
  trip_conv c (Some 42);
  txt_dec c None " none"; txt_dec c None " none "; txt_dec c None "none";
  txt_dec c (Some 1) " (some 1)"; txt_dec c (Some 1) " ( some 1 )";
  txt_dec c (Some 1) " ( some 1)"; txt_dec c (Some 1) " (some 1 )";
  ()

let test_result () =
  let c = Conv.(result bool atom) in
  trip_conv c (Ok true); trip_conv c (Ok false);
  trip_conv c (Error "ha"); trip_conv c (Error "");
  txt_dec c (Ok true) "  (ok true) ";
  txt_dec c (Error "hi") "(error hi)";
  ()

let test_list () =
  let ilist = Conv.(list int31) in
  let slist = Conv.(list string_bytes) in
  trip_conv ilist []; trip_conv ilist [1;2;3];
  txt_dec ilist [34;23;11] " (34 23 11)";
  txt_dec ilist [34;23;11] " ( 34    23 11   ) ";
  trip_conv slist []; trip_conv slist [""; "bla"; "hopla"];
  ()

let test_array () =
  let iarray = Conv.(array int31) in
  let sarray = Conv.(array string_bytes) in
  trip_conv iarray [||]; trip_conv iarray [|1;2;3|];
  txt_dec iarray [|34;23;11|] " (34 23 11)";
  txt_dec iarray [|34;23;11|] " ( 34    23 11   ) ";
  trip_conv sarray [||]; trip_conv sarray [|""; "bla"; "hopla"|];
  ()

let test_pair () =
  let p0 = Conv.(pair int atom) in
  let p1 = Conv.(pair p0 p0) in
  trip_conv p0 (3, "bla"); trip_conv p0 (3, "ho");
  trip_conv p1 ((3, "bla"), (5, "ho"));
  txt_dec p1 ((3, "bla"), (5, "ho")) " ((  3 bla)    ( 5 ho    )) ";
  ()

let test_enum () =
  let c = Conv.enum ~kind:"enum" ~docvar:"E" ["hey", `Hey; "ho", `Ho] in
  trip_conv c `Hey; trip_conv c `Ho;
  txt_dec c `Hey "   hey";
  txt_dec c `Ho "   ho  ";
  ()

let test_string_only () =
  trip_conv Conv.string_only ""; trip_conv Conv.string_only "ab";
  trip_conv Conv.string_only "\""; trip_conv Conv.string_only "\n\x00";
  txt_dec Conv.string_only "(())" "(())";
  ()

let test_fpath_conv () =
  trip_conv Conv.fpath (Fpath.v "bla");
  trip_conv Conv.fpath (Fpath.v "/bla(/bla");
  trip_conv Conv.fpath (Fpath.v "./");
  trip_conv Conv.fpath (Fpath.v "/");
  txt_dec Conv.fpath (Fpath.v "/") (" \"/\" ");
  ()

let test_fpath_conv_only () =
  trip_conv Conv.fpath_only (Fpath.v "bla");
  trip_conv Conv.fpath_only (Fpath.v "/bla(/bla");
  trip_conv Conv.fpath_only (Fpath.v "./");
  trip_conv Conv.fpath_only (Fpath.v "/");
  txt_dec Conv.fpath_only (Fpath.v "(())") "(())";
  ()

let test () =
  test_bool ();
  test_byte ();
  test_int ();
  test_int31 ();
  test_int64 ();
  test_float ();
  test_string_bytes ();
  test_atom ();
  test_atom_non_empty ();
  test_option ();
  test_result ();
  test_list ();
  test_array ();
  test_pair ();
  test_enum ();
  test_string_only ();
  test_fpath_conv ();
  test_fpath_conv_only ();
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
