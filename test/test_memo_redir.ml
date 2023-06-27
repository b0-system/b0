(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Test_memo_setup

let echo = B0_memo.Tool.by_name "echo"

let redir_create_path build_dir m =
  let echo = B0_memo.Memo.tool m echo in
  let redir = Fpath.(build_dir / "hey" / "ho" / "out") in
  B0_memo.Memo.spawn m ~stdout:(`File redir) @@ echo Cmd.(atom "Ha!")

let test_memo_redir () =
  with_memo redir_create_path;
  ()

let () = test_memo_redir ()
