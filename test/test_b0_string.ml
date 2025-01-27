(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open B0_std

let test_edit_distance () =
  Test.test "String.edit_distance" @@ fun () ->
  let test ?__POS__:pos ?limit x y d =
    Test.block ?__POS__:pos @@ fun () ->
    Test.int (String.edit_distance ?limit x y) d ~__POS__;
    Test.int (String.edit_distance ?limit y x) d ~__POS__;
    Test.int (String.edit_distance ?limit x x) 0 ~__POS__;
    Test.int (String.edit_distance ?limit y y) 0 ~__POS__;
  in
  test "" "" 0 ~__POS__;
  test "" "ab" 2 ~__POS__;
  test "function" "function" 0 ~__POS__;
  test "function" "fanction" 1 ~__POS__;  (* substitute *)
  test "function" "fnction" 1 ~__POS__;   (* delete *)
  test "function" "funiction" 1 ~__POS__; (* insert *)
  test "function" "funtcion" 1 ~__POS__;  (* transpose *)
  test "function" "fantcion" 2 ~__POS__;  (* substitute + transpose *)
  test "function" "fantcio" 3 ~__POS__;   (* substitute + transpose + delete *)
  test "function" "efantcio" 4 ~__POS__;  (* all *)
  test "fun" "function" 5 ~__POS__;
  test "fun" "function" ~limit:0 0;
  test "fun" "function" ~limit:1 1;
  test "fun" "function" ~limit:2 2;
  test "fun" "function" ~limit:3 3;
  test "fun" "function" ~limit:4 4;
  test "fun" "function" ~limit:5 5;
  test "fun" "function" ~limit:6 5;
  test "ca" "abc" 3 (* Damerau-Levenshtein would be 2 *) ~__POS__;
  test "élément" "élment" 1 ~__POS__;
  test "OCaml🐫" "O'Caml🐪" 2 ~__POS__;
  ()

let main () =
  Test.main @@ fun () ->
  test_edit_distance ();
  ()

let () = if !Sys.interactive then () else exit (main ())
