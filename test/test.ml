(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let () =
  Test_fmt.test ();
  Test_fpath.test ();
  Test_cmd.test ();
  Test_base64.test ();
  print_endline "All tests passed!";
  ()
