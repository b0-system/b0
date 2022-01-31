(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let delete c clean =
  Log.if_error ~use:B00_cli.Exit.some_error @@
  let del_dir = match clean with
  | true -> Some (B0_driver.Conf.b0_dir c)
  | false -> None (* For now *)
  in
  match del_dir with
  | None -> Log.app (fun m -> m "Nothing deleted for now!"); Ok B00_cli.Exit.ok
  | Some del_dir ->
      let* _existed = Os.Path.delete ~recurse:true del_dir in
      Ok B00_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let doc = "Delete builds"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.infos
let man_xrefs = [ `Main; `Cmd "cache" ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command deletes the current variant and deployement \
      builds and artefacts. The build cache is however kept intact.";
  `P "Use option $(b,--clean) option to get rid of the $(b,_b0) directory \
      altogeter. This deletes all variants and deployments an most likely \
      also gets rid of the build cache. See $(mname) $(b,cache) for finer \
      control over build cache deletions.";
  B0_b0.Cli.man_see_manual; ]

let clean =
  let doc = "Delete the $(b,_b0) directory." in
  Arg.(value & flag & info ["c"; "clean"] ~doc)

let cmd =
  Cmd.v (Cmd.info "delete" ~doc ~sdocs ~exits ~man ~man_xrefs)
    Term.(const delete $ B0_driver.Cli.conf $ clean)


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
