(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_nan_eq () =
  Test.test "default equality" @@ fun () ->
  Test.eq Test.Eq.any nan nan;
  ()

let test_string_get () =
  Test.test "String.get" @@ fun () ->
  Test.char (String.get "a" 0) 'a' ~__POS__;
  Test.invalid_arg ~__POS__ @@ (fun () -> String.get "" 0);
  ()

let test_string_append () =
  Test.test "String.append" @@ fun () ->
  Test.string (String.cat "a" "b") "ab" ~__POS__;
  Test.block ~__POS__ @@ fun () ->
  Test.int 4 4;
  Test.int 3 3;
  ()

let main () =
  Test.main @@ fun () ->
  test_nan_eq ();
  test_string_get ();
  test_string_append ();
  ()

let () = if !Sys.interactive then () else exit (main ())
