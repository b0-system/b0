(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

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
      Arg.(required & pos 0 (some B00_cli.fpath) None &
           info [] ~doc ~docv:"PATH")
    in
    Cmd.v
      (Cmd.info "test-b0-file" ~sdocs:Manpage.s_common_options)
      Term.(const b0_file_read $ path)
  in
  exit (Cmd.eval' cmd)

let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
