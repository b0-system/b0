(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

type cmd = [ `Cmd of t -> argv:string list -> Os.Exit.t ]
and t = { def : B0_def.t; cmd : cmd }

module T = struct
  type nonrec t = t
  let def_kind = "cmdlet"
  let def p = p.def
  let pp_name_str = Fmt.(code string)
end

include (B0_def.Make (T) : B0_def.S with type t := t)

let v ?doc ?meta n cmd =
  let def = define ?doc ?meta n in
  let p = { def; cmd } in add p; p

let cmd c = c.cmd

module Cli = struct
  open Cmdliner

  let info ?man_xrefs ?man ?envs ?exits ?sdocs ?docs ?doc:d ?version cmdlet =
    let doc = Option.value ~default:(doc cmdlet) d in
    let exits = Option.value ~default:B00_cli.Exit.infos exits in
    let name = name cmdlet in
    Term.info ?man_xrefs ?man ?envs ~exits ?sdocs ?docs ?version name ~doc

  let run ?info:i cmdlet ~argv t =
    let argv = Array.of_list argv in
    let info = match i with None -> info cmdlet | Some info -> info in
    B00_cli.Exit.of_eval_result @@ Term.eval ~argv (t, info)

  let run_cmds cmdlet ~argv main cmds =
    let argv = Array.of_list argv in
    B00_cli.Exit.of_eval_result @@ Term.eval_choice ~argv main cmds
end

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
