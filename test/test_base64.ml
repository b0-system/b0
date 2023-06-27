(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let test_trip d e =
  let e' = B0_base64.encode d in
  let d' = B0_base64.decode e |> Result.get_ok in
  assert (String.equal e e'); assert (String.equal d' d)

let test_trips () =
  test_trip "" "";
  test_trip "f" "Zg==";
  test_trip "fo" "Zm8=";
  test_trip "foo" "Zm9v";
  test_trip "foob"  "Zm9vYg==";
  test_trip "fooba"  "Zm9vYmE=";
  test_trip "foobar" "Zm9vYmFy";
  ()

let test () =
  test_trips ();
  ()
