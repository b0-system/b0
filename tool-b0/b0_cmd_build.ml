(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let memo c =
  let hash_fun = B0_driver.Conf.hash_fun c in
  let cwd = B0_driver.Conf.cwd c in
  let cache_dir = B0_driver.Conf.cache_dir c in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let trash_dir = Fpath.(b0_dir / B00_ui.Memo.trash_dir_name) in
  let jobs = B0_driver.Conf.jobs c in
  let feedback =
    let op_howto ppf o = Fmt.pf ppf "b0 log --id %d" (B000.Op.id o) in
    let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
    B00_ui.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
      Fmt.stderr
  in
  B00.Memo.memo ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs ~feedback ()

let get_units packs units =
  let ps = B0_pack.get_list packs in
  let us = B0_unit.get_list units in
  Result.bind ps @@ fun ps ->
  Result.bind us @@ fun us ->
  let us = List.rev_append us (List.concat_map B0_pack.units ps) in
  let us = B0_unit.Set.of_list us in
  let has_locked_pack = List.exists B0_pack.locked ps in
  Ok (has_locked_pack, us)

let find_units_to_build lock packs x_packs units x_units =
  let must = match packs, units with
  | [], [] ->
      (* FIXME do something smarter to get a default build set *)
      Ok (false, B0_unit.(Set.of_list (list ())))
  | _ -> get_units packs units
  in
  Result.bind must @@ fun (has_locked_pack, must) ->
  Result.bind (get_units x_packs x_units) @@ fun (_, excluded) ->
  let all = B0_unit.Set.of_list (B0_unit.list ()) in
  let must = B0_unit.Set.diff must excluded in
  let lock = Option.value ~default:has_locked_pack lock (* cli *) in
  let may = if lock then must else (B0_unit.Set.diff all excluded) in
  Ok (may, must)

let build lock packs x_packs units x_units c =
  Log.if_error ~use:B0_driver.Exit.no_such_name @@
  Result.bind (find_units_to_build lock packs x_packs units x_units) @@
  fun (may_build, must_build) ->
  match B0_unit.Set.is_empty must_build with
  | true ->
      Log.err (fun m -> m "Nothing found to build!");
      Ok B0_driver.Exit.build_error
  | false ->
      let b0_file = Option.get (B0_driver.Conf.b0_file c) in
      let root_dir = Fpath.parent b0_file in
      let b0_dir = B0_driver.Conf.b0_dir c in
      Log.if_error' ~use:B0_driver.Exit.build_error @@
      Result.bind (memo c) @@ fun m ->
      let build = B0_build.create ~root_dir ~b0_dir m ~may_build ~must_build in
      match B0_build.run build with
      | Ok () -> Ok (B0_driver.Exit.ok)
      | Error () -> Ok (B0_driver.Exit.build_error)


(* Command line interface *)

open Cmdliner

let units =
  let doc = "Build the unit $(docv)."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"UNIT")

let packs =
  let doc = "Build the pack $(docv)" in
  Arg.(value & opt_all string [] & info ["p"; "pack"] ~doc ~docv:"PACK")

let x_units =
  let doc = "Exclude unit $(docv) from the build. Takes over inclusion and \
             locks the build." in
  Arg.(value & opt_all string [] & info ["x"; "x-unit"] ~doc ~docv:"UNIT")

let x_packs =
  let doc = "Exclude units of pack $(docv) from the build. \
             Takes over inclusion and locks the build." in
  Arg.(value & opt_all string [] & info ["X"; "x-pack"] ~doc ~docv:"PACK")

let lock =
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
  `P "FIXME explain excludes";
  `P "FIXME explain build locks or defer to manual";
  B0_b0.Cli.man_see_manual; ]

let cmd =
  let build_cmd =
    Term.(const build $ lock $ packs $ x_packs $ units $ x_units)
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
