(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Cmdliner

let output_details = B0_std_cli.output_details ()
let log_format = B0_memo_cli.Log.format_cli ()
let memo_op_query = B0_memo_cli.Op.query_cli ()
let no_pager = B0_pager.no_pager ()

let def_conv (module Def : B0_def.S) =
  let complete_defs _ctx ~token =
    let complete_def u =
      let name = Def.name u in
      if not (String.starts_with ~prefix:token name) then None else
      Some (Arg.Completion.string name ~doc:(Def.doc u))
    in
    Ok (List.filter_map complete_def (Def.list ()))
  in
  let completion = Arg.Completion.make complete_defs in
  Arg.Conv.of_conv Arg.string ~completion

(* Specifying units and packs *)

let get_excluded_units ~x_units ~x_packs =
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false x_units in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:false x_packs in
  let add_unit acc u = B0_unit.Set.add u acc in
  let add_pack_units p acc = List.fold_left add_unit acc (B0_pack.units p) in
  let packs = B0_pack.Set.of_list packs in
  Ok (B0_pack.Set.fold add_pack_units packs (B0_unit.Set.of_list units))

let unit_conv = def_conv (module B0_unit)
let pack_conv = def_conv (module B0_pack)

let use_units ?docs ?(doc = "Use unit $(docv).") () =
  Arg.(value & opt_all unit_conv [] &
       info ["u"; "unit"] ?docs ~doc ~docv:"UNIT")

let use_x_units
    ?docs ?(doc = "Exclude unit $(docv). Takes over inclusion.") ()
  =
  let docv = "UNIT" in
  Arg.(value & opt_all unit_conv [] & info ["x"; "x-unit"] ?docs ~doc ~docv)

let use_packs ?docs ?(doc = "Use pack $(docv).") () =
  Arg.(value & opt_all pack_conv [] &
       info ["p"; "pack"] ?docs ~doc ~docv:"PACK")

let use_x_packs
    ?docs ?(doc = "Exclude pack $(docv). Takes over inclusion.") ()
  =
  let docv = "PACK" in
  Arg.(value & opt_all pack_conv [] & info ["X"; "x-pack"] ?docs ~doc ~docv)

let build_units =
  use_units ~doc:"Build unit $(docv). Repeatable." ()

let build_x_units =
  let doc = "Exclude unit $(docv) from the build. Takes over inclusion." in
  use_x_units ~doc ()

let build_packs =
  use_packs ~doc:"Build pack $(docv). Repeteable." ()

let build_x_packs =
  let doc =
    "Exclude units in pack $(docv) from the build. Takes over inclusion."
  in
  use_x_packs ~doc ()

let act_on_units_posn
    ?(doc = "The $(docv) to act on. All of them if unspecified.")
    ~first ()
  =
  Arg.(value & pos_right (first - 1) unit_conv [] & info [] ~doc ~docv:"UNIT")

let act_on_units_pos0 = act_on_units_posn ~first:0 ()
let act_on_units_pos1 = act_on_units_posn ~first:1 ()

let act_on_packs_posn
    ?(doc = "The $(docv) to act on. All of them if unspecified.")
    ~first ()
  =
  Arg.(value & pos_right (first - 1)  pack_conv [] & info [] ~doc ~docv:"PACK")

let act_on_packs_pos0 = act_on_packs_posn ~first:0 ()
let act_on_packs_pos1 = act_on_packs_posn ~first:1 ()

(* Metadata keys *)

let required_metadata_key_pos0 =
  let doc = "The metadata key $(docv) to get." and docv = "KEY" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

(* b0 directory *)

let b0_dir_var = Cmd.Env.info "B0_DIR"
let b0_dirname = "_b0"
let b0_dir
    ?(opts = ["b0-dir"])
    ?(docs = Manpage.s_common_options)
    ?(doc = "Use $(docv) for the b0 directory.")
    ?doc_none:(absent = "$(b,_b0) in root directory")
    ?(env = b0_dir_var) ()
  =
  Arg.(value & opt (some B0_std_cli.dirpath) None &
       info opts ~env ~absent ~doc ~docs)

let get_b0_dir ~cwd ~root ~b0_dir = match b0_dir with
| None -> Fpath.(root / b0_dirname)
| Some d -> Fpath.(cwd // d)

let get_b0_dir_path ~cwd ~b0_dir default p = match p with
| None -> Fpath.(b0_dir / default)
| Some p -> Fpath.(cwd // p)

let find_dir_with_b0_dir ~start =
  let rec loop p =
    if Fpath.is_root p then None else
    match Os.Dir.exists Fpath.(p / b0_dirname) with
    | Error _ | Ok false -> loop (Fpath.parent p)
    | Ok true -> Some p
  in
  if Fpath.is_relative start then None else (loop start)

(* File cache directory *)

let get_cache_dir ~cwd ~b0_dir ~cache_dir =
  get_b0_dir_path ~cwd ~b0_dir B0_memo_cli.File_cache.dirname cache_dir
