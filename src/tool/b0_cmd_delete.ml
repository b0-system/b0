(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let delete ~clean conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let del_dir =
    if clean then Some (B0_driver.Conf.b0_dir conf) else
    None (* for now *)
  in
  match del_dir with
  | None -> Log.stdout (fun m -> m "Nothing deleted for now!"); Ok Os.Exit.ok
  | Some del_dir ->
      let* _existed = Os.Path.delete ~recurse:true del_dir in
      Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Delete builds" in
  let descr = `Blocks [
      `P "The $(cmd) command deletes the current variant and deployement \
          builds and artefacts. The build cache is however kept intact.";
      `P "Use option $(b,--clean) option to get rid of the $(b,_b0) directory \
          altogeter. This deletes all variants and deployments an most likely \
        also gets rid of the build cache. See $(tool) $(b,cache) for finer \
          control over build cache deletions." ]
  in
  B0_tool_cli.cmd_with_driver_conf "delete" ~doc ~descr @@
  let+ clean =
    let doc = "Delete the $(b,_b0) directory." in
    Arg.(value & flag & info ["c"; "clean"] ~doc)
  in
  delete ~clean
