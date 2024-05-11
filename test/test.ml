(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let main () =
  B0_testing.Test.main @@ fun () ->
  Test_fpath.test ();
  Test_cmd.test ();
  Test_base64.test ()

let () = if !Sys.interactive then () else exit (main ())
