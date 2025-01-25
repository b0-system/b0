(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let test_edit_distance () =
  Test.test "String.edit_distance" @@ fun () ->
  ()

let main () =
  Test.main @@ fun () ->
  test_edit_distance ();
  ()

let () = if !Sys.interactive then () else exit (main ())
