(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open B0_testing

let test_string_get () =
  Test.test "String.get" @@ fun () ->
  Test.char ~__POS__ (String.get "a" 0) 'a';
  Test.invalid_arg ~__POS__ @@ (fun () -> String.get "" 0);
  ()

let test_string_append () =
  Test.test "String.append" @@ fun () ->
  Test.string ~__POS__ (String.cat "a" "b") "ab";
  ()

let main () =
  Test.main @@ fun () ->
  test_string_get ();
  test_string_append ();
  ()

let () = if !Sys.interactive then () else exit (main ())
