(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let list format c =
  (* XXX improve that when cmdlets and outcome actions are merged *)
  let list (module Def : B0_def.S) c format ds =
    let pp, sep = match format with
    | `Short -> Def.pp_name, Fmt.cut
    | `Normal -> Def.pp_synopsis, Fmt.cut
    | `Long -> Def.pp, Fmt.(cut ++ cut)
    in
    let* ds = Def.get_list_or_hint ~empty_means_all:true ds in
    let don't = B0_driver.Conf.no_pager c in
    let* pager = B00_pager.find ~don't () in
    let* () = B00_pager.page_stdout pager in
    if ds <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) ds);
    Ok ()
  in
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* () = list (module B0_unit) c format [] in
  let* () = list (module B0_cmdlet) c format [] in
  Ok B00_cli.Exit.ok

(* Command line interface *)

open Cmdliner

(* Commands *)

let cmd =
  let doc = "Show b0 definitions" in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(tname) shows b0 definitions.";
    B0_b0.Cli.man_see_manual;
  ]
  in
  let exits = B0_driver.Exit.infos in
  let envs = B0_b0.Cli.pager_envs in
  let term =
    B0_driver.with_b0_file ~driver:B0_b0.driver
      Term.(const list $ B0_b0.Cli.format)
  in
  Cmd.v (Cmd.info "list" ~doc ~exits ~envs ~man) term


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
