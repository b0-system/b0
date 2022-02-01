(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let list format us c = B0_b0.Def.list (module B0_unit) c format us
let edit us c = B0_b0.Def.edit (module B0_unit) c us
let get format k us c = B0_b0.Def.get_meta_key (module B0_unit) c format k us
let build_dir us c =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* us = B0_unit.get_list_or_hint ~empty_means_all:true us in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let build_dir = B0_dir.build_dir ~b0_dir ~variant:"user" (* FIXME *) in
  let unit_dir u = B0_dir.unit_build_dir ~build_dir ~name:(B0_unit.name u) in
  let dirs = List.map unit_dir us in
  Log.app (fun m -> m "@[<v>%a@]" (Fmt.list Fpath.pp_unquoted) dirs);
  Ok B00_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let editor_envs = B00_editor.envs ()
let pager_envs = B00_pager.envs ()
let format = B00_cli.Arg.output_details ()
let units ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"UNIT")

let units_all = units ~right:(-1)

let sub_cmd name ~doc ~descr ~envs term =
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let man = [`S Manpage.s_description; `P descr; B0_b0.Cli.man_see_manual] in
  let term = B0_driver.with_b0_file ~driver:B0_b0.driver term in
  Cmd.v (Cmd.info name ~doc ~sdocs ~exits ~envs ~man) term

(* Sub commands *)

let list_term = Term.(const list $ format $ units_all)
let list =
  let doc = "List units (default command)" in
  let descr = "$(tname) lists build units. Use with $(b,-l) to output their \
               metadata."
  in
  sub_cmd "list" ~doc ~descr ~envs:pager_envs list_term

let edit =
  let doc = "Edit B0 files of units" in
  let descr = "$(tname) opens in your editor the B0 files of build units." in
  let term = Term.(const edit $ units_all) in
  sub_cmd "edit" ~doc ~descr ~envs:editor_envs term

let get =
  let doc = "Get metadata key values of units" in
  let descr = "$(tname) outputs the value of metadata $(i,KEY) of given or all \
               units."
  in
  let key =
    let doc = "The metadata key $(docv) to get" and docv = "KEY" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)
  in
  let term = Term.(const get $ format $ key $ units ~right:0) in
  sub_cmd "get" ~doc ~descr ~envs:pager_envs term

let build_dir =
  let doc = "Output build directories of units" in
  let descr = "$(tname) outputs unit build directories. The paths may not \
               exist."
  in
  let term = Term.(const build_dir $ units_all) in
  sub_cmd "build-dir" ~doc ~descr ~envs:[] term

let subs = [list; edit; get; build_dir]

(* Command *)

let cmd =
  let doc = "Operate on build units" in
  let descr = "$(tname) operates on build units. Invoked without arguments \
               this simply lists known build units."
  in
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let man = [`S Manpage.s_description; `P descr; B0_b0.Cli.man_see_manual] in
  let default = B0_driver.with_b0_file ~driver:B0_b0.driver list_term in
  let info = Cmd.info "unit" ~doc ~sdocs ~exits ~envs:pager_envs ~man in
  Cmd.group info ~default subs

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
