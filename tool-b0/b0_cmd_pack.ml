(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax


let edit clets c = B0_b0.Def.edit (module B0_pack) c clets
let get format k clets c =
  B0_b0.Def.get_meta_key (module B0_pack) c format k clets

let list format clets c = B0_b0.Def.list (module B0_pack) c format clets
let show format cs c =
  let format = if format = `Normal then `Long else format in
  B0_b0.Def.list (module B0_pack) c format cs

(* Command line interface *)

open Cmdliner

let packs ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"CMDLET")

let packs_all = packs ~right:(-1)

let list_term = Term.(const list $ B0_b0.Cli.format $ packs_all)

(* Commands *)

let edit =
  let doc = "Edit build packs" in
  let descr = `P "$(tname) opens in your editor the B0 files of given \
                  build packs are defined." in
  let envs = B0_b0.Cli.editor_envs in
  let term = Term.(const edit $ packs_all) in
  B0_b0.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs term

let get =
  let doc = "Get build pack metadata" in
  let descr = `P "$(tname) outputs the value of metadata $(i,KEY) of given \
                  build packs."
  in
  let envs = B0_b0.Cli.pager_envs in
  let packs = packs ~right:0 in
  let term = Term.(const get $ B0_b0.Cli.format $ B0_b0.Cli.pos_key $ packs) in
  B0_b0.Cli.subcmd_with_b0_file "get" ~doc ~descr ~envs term

let list =
  let doc = "List build packs (default command)" in
  let descr = `P "$(tname) lists given build packs." in
  let envs = B0_b0.Cli.pager_envs in
  B0_b0.Cli.subcmd_with_b0_file "list" ~doc ~descr ~envs list_term

let show =
  let doc = "Show build pack metadata." in
  let descr = `P "$(tname) is $(b,list -l), it outputs metadata of given \
                  build packs."
  in
  let envs = B0_b0.Cli.pager_envs in
  let term = Term.(const show $ B0_b0.Cli.format $ packs_all) in
  B0_b0.Cli.subcmd_with_b0_file "show" ~doc ~descr ~envs term

let subs = [edit; get; list; show]

let cmd =
  let doc = "Operate on build packs" in
  let descr = `P "$(tname) operates on build packs. The default command is \
                  $(tname) $(b,list)."
  in
  let envs = B0_b0.Cli.pager_envs and default = list_term in
  B0_b0.Cli.cmd_group_with_b0_file "pack" ~doc ~descr ~envs ~default subs

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
