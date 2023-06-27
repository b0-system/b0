(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

type t = Cmd.t

let get ?search ?(cmd = Cmd.arg "rsync") () = Os.Cmd.get ?search cmd

let copy
  rsyncc ?(opts = Cmd.arg "-azh") ?(stats = false) ?(progress = true) ~delete
  ?(src_host = "") ~src ?(dst_host = "") dst =
  (* XXX force slashes ?  *)
  let src = src_host ^ Fpath.to_string src in
  let dst = dst_host ^ Fpath.to_string dst in
  let cmd = Cmd.(rsyncc %%
                 if' delete (arg "--delete") %%
                 if' stats (arg "--stats") %%
                 if' progress (arg "--progress") %%
                 opts % src % dst)
  in
  Os.Cmd.run cmd
