(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let output_dir conf =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file conf in
  let root = Fpath.parent b0_file in
  Fmt.pr "@[%a@]@." Fpath.pp (Fpath.strip_trailing_dir_sep root);
  Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Output the root directory" in
  let descr = `P "$(cmd) outputs the b0 root directory." in
  B0_tool_cli.cmd_with_driver_conf "root" ~doc ~descr @@
  Term.const output_dir
