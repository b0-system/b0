(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

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
      Arg.(required & pos 0 (some B00_cli.fpath) None &
           info [] ~doc ~docv:"PATH")
    in
    Cmd.v (Cmd.info "test-rm" ~sdocs:Manpage.s_common_options)
      Term.(const rm_cmd $ recurse $ path)
  in
  exit (Cmd.eval' cmd)

let () = if !Sys.interactive then () else main ()


(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
