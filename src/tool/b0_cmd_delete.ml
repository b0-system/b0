(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let delete clean c =
  Log.if_error ~use:Os.Exit.some_error @@
  let del_dir = match clean with
  | true -> Some (B0_driver.Conf.b0_dir c)
  | false -> None (* For now *)
  in
  match del_dir with
  | None -> Log.app (fun m -> m "Nothing deleted for now!"); Ok Os.Exit.ok
  | Some del_dir ->
      let* _existed = Os.Path.delete ~recurse:true del_dir in
      Ok Os.Exit.ok

(* Command line interface *)

open Cmdliner

let clean =
  let doc = "Delete the $(b,_b0) directory." in
  Arg.(value & flag & info ["c"; "clean"] ~doc)

let cmd =
  let doc = "Delete builds" in
  let descr = [
    `P "The $(iname) command deletes the current variant and deployement \
        builds and artefacts. The build cache is however kept intact.";
    `P "Use option $(b,--clean) option to get rid of the $(b,_b0) directory \
        altogeter. This deletes all variants and deployments an most likely \
        also gets rid of the build cache. See $(mname) $(b,cache) for finer \
        control over build cache deletions." ]
  in
  let descr = `Blocks descr in
  B0_tool_std.Cli.subcmd_with_driver_conf "delete" ~doc ~descr @@
  Term.(const delete $ clean)
