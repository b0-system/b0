(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open B0_std

let test_next_token =
  Test.test "String.next_token" @@ fun () ->
  let snap = Snap.(t2 T.string T.string) in
  snap (String.next_token "bla") @@ __POS_OF__ ("bla", "");
  snap (String.next_token " bla ha") @@ __POS_OF__ ("bla", " ha");
  snap (String.next_token "\xFF") @@ __POS_OF__ ("", "\xFF");
  snap (String.next_token " \xFF") @@ __POS_OF__ ("", "\xFF");
  snap (String.next_token "") @@ __POS_OF__ ("", "");
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
