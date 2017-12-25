(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner
open B0

let setup_tty_and_log color verbosity =
  let cap = match color with
  | None -> Tty.cap @@ Tty.kind ~out:Unix.stdout
  | Some cap -> cap
  in
  Tty.set_styling_cap cap;
  Log.set_level verbosity

let setup_cwd cwd_arg =
  OS.Dir.current () >>= fun start_cwd ->
  match cwd_arg with
  | None -> Ok (start_cwd, start_cwd)
  | Some cwd ->
      let cwd = Fpath.(start_cwd // cwd) in
      OS.Dir.set_current cwd >>= fun () ->
      Ok (start_cwd, cwd)

type t =
  { root : Fpath.t option;
    start_cwd : Fpath.t;
    cwd : Fpath.t;
    b0_dir : Fpath.t option;
    color : Tty.cap option;
    verbosity : Log.level;
    driver_dir : Fpath.t option;
    force : bool;
    only : bool;
    trust : bool;
    ocamlc : string option;
    ocamlopt : string option;
    compile_kind : [`Byte | `Native | `Auto] option;
    compile : string list;
    link : string list; }

let root a = a.root
let start_cwd a = a.start_cwd
let cwd a = a.cwd
let b0_dir a = a.b0_dir
let color a = a.color
let verbosity a = a.verbosity
let driver_dir a = a.driver_dir
let force a = a.force
let only a = a.only
let trust a = a.trust
let ocamlc a = a.ocamlc
let ocamlopt a = a.ocamlopt
let compile_kind a = a.compile_kind
let compile a = a.compile
let link a = a.link

let setup =
  let setup
      root cwd b0_dir color verbosity driver_dir force only trust ocamlc
      ocamlopt compile_kind compile link
    =
    setup_tty_and_log color verbosity;
    setup_cwd cwd >>| fun (start_cwd, cwd) ->
    { root; start_cwd; cwd; b0_dir; driver_dir; color; verbosity; force;
      only; trust; ocamlc; ocamlopt; compile_kind; compile; link; }
  in
  Term.(const setup $ B0d_cli.root $ B0d_cli.cwd $ B0d_cli.b0_dir $
        B0d_cli.color $ B0d_cli.verbosity $ B0d_cli.driver_dir $
        B0d_cli.driver_force $ B0d_cli.driver_only $ B0d_cli.driver_trust $
        B0d_cli.driver_ocamlc $ B0d_cli.driver_ocamlopt $
        B0d_cli.driver_compile_kind $ B0d_cli.driver_compile $
        B0d_cli.driver_link)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
