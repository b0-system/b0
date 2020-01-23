(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let find_units_to_build packs units = match packs, units with
| [], [] -> Ok (false, B0_unit.list ()) (* FIXME do something smarter. *)
| _ ->
    let us = if units = [] then Ok [] else B0_unit.get_all units in
    let ps = if packs = [] then Ok [] else B0_pack.get_all packs in
    Result.bind us @@ fun us ->
    Result.bind ps @@ fun ps ->
    let locked = List.exists B0_pack.locked ps in
    let us = List.rev_append us (List.concat_map B0_pack.units ps) in
    let us = B0_unit.Set.of_list us in
    Ok (locked, B0_unit.Set.elements us)

let build locked packs units c =
  let build_error = B0_driver.Exit.Code 1 (* FIXME document *) in
  Log.if_error ~use:B0_driver.Exit.no_such_name @@
  Result.bind (find_units_to_build packs units) @@ function
  | _, [] ->
      Log.err (fun m -> m "Nothing found to build!");
      Ok build_error
  | locked_pack, units ->
      let locked = Option.value ~default:locked_pack locked (* cli *) in
      let b0_file = Option.get (B0_driver.Conf.b0_file c) in
      let root_dir = Fpath.parent b0_file in
      let b0_dir = B0_driver.Conf.b0_dir c in
      Result.bind (B0_driver.Conf.memo c) @@ fun memo ->
      (* FIXME We'll have to create our own memo here. *)
      let build = B0_build.create ~root_dir ~b0_dir memo ~locked units in
      Log.if_error' ~use:build_error @@
      Result.bind (B0_build.run build) @@ fun () ->
      Ok (B0_driver.Exit.ok)

(* Command line interface *)

open Cmdliner

let units =
  let doc = "Build the unit named $(docv)."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"UNIT")

let packs =
  let doc = "Build the pack named $(docv)" in
  Arg.(value & opt_all string [] & info ["p"; "pack"] ~doc ~docv:"PACK")

let locked =
  (* FIXME env var *)
  let lock =
    let doc = "Lock the build to units and packs specified on the cli." in
    Some true, Arg.info ["lock"] ~doc
  in
  let unlock =
    let doc = "Unlock a build that contains a locked pack." in
    Some false, Arg.info ["unlock"] ~doc
  in
  Arg.(value & vflag None [lock; unlock])


let doc = "Build (default)"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.Info.base_cmd
let man_xrefs = [ `Main; ]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) command builds the software.";
  `P "If no unit or pack is specified on the command line all of those \
      which are tagged as implied in the root b0 file are built.";
  `P "FIXME explain build locks or defer to manual";
  B0_b0.Cli.man_see_manual; ]

let cmd =
  let build_cmd =
    Term.(const build $ locked $ packs $ units)
  in
  B0_driver.with_b0_file ~driver:B0_b0.driver build_cmd,
  Term.info "build" ~doc ~sdocs ~exits ~man ~man_xrefs

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
