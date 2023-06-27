(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax


(* Dune command *)

let library_stanza ppf u =
  let requires = B0_unit.find_meta B0_ocaml.Meta.requires u in
  let requires = Option.value ~default:[] requires in
  let requires = List.map B0_ocaml.Lib.Name.to_string requires in
  let library = Option.get (B0_unit.find_meta B0_ocaml.Meta.library u) in
  let library = B0_ocaml.Lib.Name.to_string library in
  let mod_names = match B0_unit.find_meta B0_ocaml.Meta.mod_srcs u with
  | None -> []
  | Some m ->
      (* FIXME needs a story for easily looking up dynamic metadata
         for now this requires running the build *)
      if true then [] else
      let mod_name (_, src) = B0_ocaml.Mod.Src.mod_name src in
      List.map mod_name (B0_ocaml.Mod.Name.Map.bindings (Fut.sync m))
  in
  Fmt.pf ppf
    "@[<v1>(library@,(public_name %s)@,(wrapped false)@,@[<1>(libraries %a)@]@,\
     @[<1>(modules %a)@])@]"
    library Fmt.(list ~sep:sp string) requires Fmt.(list string) mod_names

let dune_file ppf libs =
  Fmt.pf ppf "@[<v>(include_subdirs unqualified)@,@[<v>%a@]@]"
    Fmt.(list library_stanza) libs

let write_dune_project ~file =
  let p = Fmt.str "(lang dune 3.8)\n(generate_opam_files false)\n" in
  let* () = Os.File.write ~force:true ~make_path:true file p in
  Log.app (fun m -> m ~header:"WROTE" "%a" Fpath.pp file);
  Ok ()

let dune us c =
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* us = B0_unit.get_list_or_hint ~empty_means_all:true us in
  let ocaml_libs =
    List.filter (B0_unit.has_meta B0_ocaml.Meta.library) us
  in
  dune_file Fmt.stdout ocaml_libs;
  Ok B0_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let units ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"UNIT")

let units_all = units ~right:(-1)

(* Commands *)

let dune =
  let doc = "Best-effort dune OCaml library boilerplate generation" in
  let descr = `P
      "$(iname) tries to derive dune boilerplate for OCaml \
       libraries defined in the project. This is a best-effort \
       procedure the files may need tweaking and since $(b,b0)'s build \
       model is more dynamic this may fail."
  in
  B0_tool_std.Cli.subcmd_with_b0_file "dune" ~doc ~descr @@
  Term.(const dune $ units_all)

let subs = [dune]

let cmd =
  let doc = "Export metadata to other systems" in
  let descr = `P "$(iname) provides a few commands to interact with other \
                  systems."
  in
  B0_tool_std.Cli.cmd_group_with_b0_file "export" ~doc ~descr subs
