(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let get_cmd ?search ?(cmd = Cmd.atom "rsync") () = Os.Cmd.get ?search cmd
let rsync = lazy (get_cmd ())

let copy
    ?(opts = Cmd.atom "-azh") ?(stats = false) ?(progress = true) ~delete
    ?(src_host = "") ~src ?(dst_host = "") dst
  =
  let* rsync = Lazy.force rsync in
  (* XXX force slashes ?  *)
  let src = src_host ^ Fpath.to_string src in
  let dst = dst_host ^ Fpath.to_string dst in
  let cmd = Cmd.(rsync %%
                 if' delete (atom "--delete") %%
                 if' stats (atom "--stats") %%
                 if' progress (atom "--progress") %%
                 opts % src % dst)
  in
  Os.Cmd.run cmd
