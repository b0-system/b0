(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let unlock c =
  let vars = [ B0_driver.Env.b0_file; B0_driver.Env.b0_dir ] in
  let pp_unset ppf var = Fmt.pf ppf "unset %s;" var in
  Log.stdout (fun m -> m "@[<v>%a@]" Fmt.(list pp_unset) vars);
  Os.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Unlock the root and b0 directory" in
  let descr = `Blocks
      [ `P "$(iname) outputs instructions to clear the environment bindings \
            performed by $(b,lock). The indented usage is:";
        `Pre "$(b,eval \\$(b0 unlock\\))"; ]
  in
  B0_tool.Cli.subcmd_with_driver_conf "unlock" ~doc ~descr @@
  Term.(const unlock)
