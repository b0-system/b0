(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let cp_cmd follow_symlinks recurse src dst =
  let error e = Fmt.epr "%s: %s" (Filename.basename Sys.argv.(0)) e; 1 in
  let make_path = true in
  Result.fold ~ok:(fun () -> 0) ~error @@
  Os.Path.copy ~follow_symlinks ~make_path ~recurse src ~dst

let main () =
  let open Cmdliner in
  let cmd =
    let follow_symlinks =
      let doc = "Preserve symbolic links rather than following them." in
      let p = Arg.(value & flag & info ["s"; "preserve-symbolic-links"] ~doc) in
      Term.(const (fun p -> not p) $ p)
    in
    let recurse =
      let doc = "If $(i,SRC) is a directory copy it recursively. Otherwise
                 only copies the files therein to the destination."
      in
      Arg.(value & flag & info ["r"; "recurse"] ~doc)
    in
    let src =
      let doc = "$(docv) is the source file or directory" in
      Arg.(required & pos 0 (some B0_cli.fpath) None &
           info [] ~doc ~docv:"SRC")
    in
    let dst =
      let doc = "$(docv) is the destination path; which must not exist." in
      Arg.(required & pos 1 (some B0_cli.fpath) None &
           info [] ~doc ~docv:"DST")
    in
    Cmd.v (Cmd.info "test-cp" ~sdocs:Manpage.s_common_options)
      Term.(const cp_cmd $ follow_symlinks $ recurse $ src $ dst)
  in
  exit (Cmd.eval' cmd)

let () = if !Sys.interactive then () else main ()
