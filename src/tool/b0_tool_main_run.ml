(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  let module D = B0_tool_main (* make sure we link it *) in
  if !Sys.interactive then () else B0_driver.run ~has_b0_file:false
