(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let root c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  let root = Fpath.parent b0_file in
  Log.app (fun m -> m "%a" Fpath.pp_unquoted root);
  Ok B00_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Show the root directory" in
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) shows the b0 root directory.";
    B0_b0.Cli.man_see_manual; ]
  in
  Cmd.v (Cmd.info "root" ~doc ~sdocs ~exits ~man ~man_xrefs)
    Term.(const root $ B0_driver.Cli.conf)


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
