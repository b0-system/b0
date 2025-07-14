(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let get_defs ~names ~with_lib_defs ~only_tests =
  let keep ~no_lib_def_test ~no_test_tag_test (B0_def.V ((module Def), v)) =
    (no_lib_def_test || not (String.starts_with ~prefix:"." (Def.name v)))
    &&
    (no_test_tag_test || Def.has_tag B0_meta.test v)
  in
  let defs = B0_tool.def_list and all_if_empty = true in
  let* vs = B0_tool.def_list_get_list_or_hint defs ~all_if_empty names in
  let no_lib_def_test = with_lib_defs || names <> [] in
  let no_test_tag_test = not only_tests in
  Ok (List.filter (keep ~no_lib_def_test ~no_test_tag_test) vs)

let edit ~names ~only_tests conf =
  let rec find_files not_found fs = function
  | [] -> not_found, Fpath.distinct fs
  | (B0_def.V ((module Def), def) as v) :: vs ->
      match B0_def.file (Def.def def) with
     | None -> find_files (v :: not_found) fs vs
     | Some f -> find_files not_found (f :: fs) vs
  in
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* vs = get_defs ~names ~with_lib_defs:false ~only_tests in
  let defs = B0_tool.def_list and all_if_empty = true in
  let* vs = B0_tool.def_list_get_list_or_hint defs ~all_if_empty names in
  let not_found, files = find_files [] [] vs in
  Log.if_error' ~use:Os.Exit.some_error @@
  let edit_all = names = [] in
  match not edit_all && not_found <> [] with
  | true ->
      let pp_name ppf (B0_def.V ((module Def), def)) = Def.pp_name ppf def in
      let plural = if (List.length not_found > 1) then "s" else "" in
      Fmt.error "Could not find b0 file for definition%s: @[%a@]"
        plural Fmt.(list ~sep:sp pp_name) not_found
  | false ->
      let* editor = B0_editor.find () in
      Result.bind (B0_editor.edit_files editor files) @@ function
      | `Exited 0 -> Ok Os.Exit.ok
      | _ -> Ok Os.Exit.some_error

let list ~output_details ~names ~all ~only_tests conf =
  let pp_v ppf (B0_def.V ((module Def), v)) = match output_details with
  | `Short -> Def.pp_name ppf v
  | `Normal -> Def.pp_synopsis ppf v
  | `Long -> Def.pp ppf v
  in
  let list conf vs =
    let sep = match output_details with
    | `Short | `Normal -> Fmt.cut | `Long -> Fmt.(cut ++ cut)
    in
    let don't = B0_driver.Conf.no_pager conf in
    let* pager = B0_pager.find ~don't () in
    let* () = B0_pager.page_stdout pager in
    if vs <> []
    then Log.stdout (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp_v) vs);
    Ok ()
  in
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* vs = get_defs ~names ~with_lib_defs:all ~only_tests in
  let* () = list conf vs in
  Ok Os.Exit.ok

let info ~output_details ~names ~all ~only_tests conf =
  let output_details =
    if output_details = `Normal then `Long else output_details
  in
  list ~output_details ~names ~all ~only_tests conf

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let all =
  let doc =
    "Include $(b,b0) library definitions when all definitions are listed."
  in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let only_tests =
  let doc = "List only tests. That is units tagged with $(b,B0_meta.test)." in
  Arg.(value & flag & info ["tests"] ~doc)

let names =
  let doc =
    "The $(docv) to act on. All of them except library definitions \
     if unspecified."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"DEF")

let cmd =
  let doc = "List definitions" in
  let descr =
    `P "$(cmd) list all or given b0 definitions. By default library \
        definitions are not listed, invoke with $(b,--all) include them."
  in
  B0_tool.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  let+ output_details = B0_tool.Cli.output_details
  and+ names and+ all and+ only_tests in
  list ~output_details ~names ~all ~only_tests


let cmd_info =
  let doc = "Display information about definitions" in
  let descr =
    `P "$(cmd) is $(tool) $(b,list -l). It outputs metadata of given
        definitions. By default library definitions are \
        not listed, invoke with $(b,--all) include them."
  in
  B0_tool.Cli.subcmd_with_b0_file "info" ~doc ~descr @@
  let+ output_details = B0_tool.Cli.output_details
  and+ names and+ all and+ only_tests in
  info ~output_details ~names ~all ~only_tests

let cmd_edit =
  let doc = "Edit definitions" in
  let descr =
    `P "$(cmd) opens in your editor the b0 files where given definitions are \
        defined."
  in
  let envs = B0_tool.Cli.editor_envs in
  B0_tool.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs @@
  let+ names and+ only_tests in
  edit ~names ~only_tests
