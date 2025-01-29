(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open B0_std

let main () =
  Test.main @@ fun () ->
  ()

let () = if !Sys.interactive then () else exit (main ())
