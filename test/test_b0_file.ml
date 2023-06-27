(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let b0_file_read file =
  Log.if_error ~header:"" ~use:1 @@
  Result.bind (Os.File.read file) @@ fun c ->
  Result.bind (B0_file.of_string ~file c) @@ fun f ->
  Fmt.pr "%a@." B0_file.pp_dump f;
  Fmt.pr "%a@." B0_file.pp_locs f;
  Ok 0

let main () =
  let open Cmdliner in
  let cmd =
    let path =
      let doc = "$(docv) is the b0 file to read" in
      Arg.(required & pos 0 (some B0_cli.fpath) None &
           info [] ~doc ~docv:"PATH")
    in
    Cmd.v
      (Cmd.info "test-b0-file" ~sdocs:Manpage.s_common_options)
      Term.(const b0_file_read $ path)
  in
  exit (Cmd.eval' cmd)

let () = if !Sys.interactive then () else main ()
