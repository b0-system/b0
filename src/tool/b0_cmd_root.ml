(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let path c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  let root = Fpath.parent b0_file in
  Log.stdout (fun m -> m "%a" Fpath.pp root);
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show the root directory" in
  let descr = `P "$(iname) outputs the b0 root directory." in
  B0_tool.Cli.subcmd_with_driver_conf "root" ~doc ~descr @@
  Term.(const path)
