(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let delete c clean =
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let del_dir = match clean with
  | true -> Some (B0_driver.Conf.b0_dir c)
  | false -> None (* For now *)
  in
  match del_dir with
  | None -> Log.app (fun m -> m "Nothing deleted for now!"); Ok B0_cli.Exit.ok
  | Some del_dir ->
      let* _existed = Os.Path.delete ~recurse:true del_dir in
      Ok B0_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Delete builds"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.infos
let man_xrefs = [ `Main; `Cmd "cache" ]
let man = [
  `S Manpage.s_description;
  `P "The $(iname) command deletes the current variant and deployement \
      builds and artefacts. The build cache is however kept intact.";
  `P "Use option $(b,--clean) option to get rid of the $(b,_b0) directory \
      altogeter. This deletes all variants and deployments an most likely \
      also gets rid of the build cache. See $(mname) $(b,cache) for finer \
      control over build cache deletions.";
  B0_tool_std.Cli.man_see_manual; ]

let clean =
  let doc = "Delete the $(b,_b0) directory." in
  Arg.(value & flag & info ["c"; "clean"] ~doc)

let cmd =
  Cmd.v (Cmd.info "delete" ~doc ~sdocs ~exits ~man ~man_xrefs) @@
  Term.(const delete $ B0_driver.Cli.conf $ clean)
