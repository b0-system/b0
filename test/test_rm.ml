(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let rm_cmd recurse p  = match Os.Path.delete ~recurse p with
| Ok _ -> 0
| Error e -> Fmt.epr "%s: %s" (Filename.basename Sys.argv.(0)) e; 1

let main () =
  let open Cmdliner in
  let cmd =
    let recurse =
      let doc = "If $(i,PATH) is a non empty directory, delete it \
                 recursively instead of errors."
      in
      Arg.(value & flag & info ["r"; "recurse"] ~doc)
    in
    let path =
      let doc = "$(docv) is file path to delete" in
      Arg.(required & pos 0 (some B0_std_cli.fpath) None &
           info [] ~doc ~docv:"PATH")
    in
    Cmd.make (Cmd.info "test-rm" ~sdocs:Manpage.s_common_options) @@
    Term.(const rm_cmd $ recurse $ path)
  in
  Cmd.eval' cmd

let () = if !Sys.interactive then () else exit (main ())
