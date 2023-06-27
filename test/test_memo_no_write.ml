(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Test_memo_setup

let echo = B0_memo.Tool.by_name "echo"

let cache_no_write build_dir m =
  let echo = B0_memo.Memo.tool m echo in
  B0_memo.Memo.spawn m @@ echo Cmd.(atom "Ha!")

let test_cache_no_write () =
  with_memo cache_no_write;
  ()

let () = test_cache_no_write ()
