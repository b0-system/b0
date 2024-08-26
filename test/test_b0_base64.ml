(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

let test_trips () =
  Test.test "round trips" @@ fun () ->
  let test ?__POS__ d e =
    let e' = B0_base64.encode d in
    let d' = B0_base64.decode e |> Result.get_ok in
    Test.string ?__POS__ e e'; Test.string ?__POS__ d' d;
  in
  test ~__POS__ "" "";
  test ~__POS__ "f" "Zg==";
  test ~__POS__ "fo" "Zm8=";
  test ~__POS__ "foo" "Zm9v";
  test ~__POS__ "foob"  "Zm9vYg==";
  test ~__POS__ "fooba"  "Zm9vYmE=";
  test ~__POS__ "foobar" "Zm9vYmFy";
  ()

let main () =
  Test.main @@ fun () ->
  test_trips ();
  ()

let () = if !Sys.interactive then () else exit (main ())
