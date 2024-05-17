(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let get_defs names ~with_lib_defs =
  let keep_lib_defs ~keep (B0_def.V ((module Def), v)) =
    keep || not (String.starts_with ~prefix:"." (Def.name v))
  in
  let defs = B0_tool_std.def_list and all_if_empty = true in
  let* vs = B0_tool_std.def_list_get_list_or_hint defs ~all_if_empty names in
  Ok (List.filter (keep_lib_defs ~keep:(with_lib_defs || names <> [])) vs)

let edit names conf =
  let rec find_files not_found fs = function
  | [] -> not_found, Fpath.distinct fs
  | (B0_def.V ((module Def), def) as v) :: vs ->
      match B0_def.file (Def.def def) with
     | None -> find_files (v :: not_found) fs vs
     | Some f -> find_files not_found (f :: fs) vs
  in
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* vs = get_defs names ~with_lib_defs:false in
  let defs = B0_tool_std.def_list and all_if_empty = true in
  let* vs = B0_tool_std.def_list_get_list_or_hint defs ~all_if_empty names in
  let not_found, files = find_files [] [] vs in
  Log.if_error' ~use:Os.Exit.some_error @@
  let edit_all = names = [] in
  match not edit_all && not_found <> [] with
  | true ->
      let pp_name ppf (B0_def.V ((module Def), def)) = Def.pp_name ppf def in
      let plural = if (List.length not_found > 1) then "s" else "" in
      Fmt.error "Could not find B0 file for definition%s: @[%a@]"
        plural Fmt.(list ~sep:sp pp_name) not_found
  | false ->
      let* editor = B0_editor.find () in
      Result.bind (B0_editor.edit_files editor files) @@ function
      | `Exited 0 -> Ok Os.Exit.ok
      | _ -> Ok Os.Exit.some_error

let list format names all conf =
  let pp_v ppf (B0_def.V ((module Def), v)) = match format with
  | `Short -> Def.pp_name ppf v
  | `Normal -> Def.pp_synopsis ppf v
  | `Long -> Def.pp ppf v
  in
  let list conf vs =
    let sep = match format with
    | `Short | `Normal -> Fmt.cut | `Long -> Fmt.(cut ++ cut)
    in
    let don't = B0_driver.Conf.no_pager conf in
    let* pager = B0_pager.find ~don't () in
    let* () = B0_pager.page_stdout pager in
    if vs <> []
    then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp_v) vs);
    Ok ()
  in
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* vs = get_defs names ~with_lib_defs:all in
  let* () = list conf vs in
  Ok Os.Exit.ok

let show format names all conf =
  let format = if format = `Normal then `Long else format in
  list format names all conf

(* Command line interface *)

open Cmdliner

let all =
  let doc = "Include library definitions when all definitions are listed." in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let names =
  let doc =
    "The $(docv) to act on. All of them except library definitions \
     if unspecified."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"DEF")

let cmd =
  let doc = "List definitions" in
  let descr =
    `P "$(iname) list all or given b0 definitions. By default library \
        definitions are not listed, invoke with $(b,--all) include them."
  in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr @@
  Term.(const list $ B0_tool_std.Cli.format $ names $ all)

let cmd_show =
  let doc = "Show definition metadata" in
  let descr =
    `P "$(iname) is $(mname) $(b,list -l), it outputs metadata of given
        definitions. By default library definitions are \
        not listed, invoke with $(b,--all) include them."
  in
  B0_tool_std.Cli.subcmd_with_b0_file "show" ~doc ~descr @@
  Term.(const show $ B0_tool_std.Cli.format $ names $ all)

let cmd_edit =
  let doc = "Edit definitions" in
  let descr = `P "$(iname) opens in your editor the B0 files where given \
                  definitions are defined." in
  let envs = B0_tool_std.Cli.editor_envs in
  B0_tool_std.Cli.subcmd_with_b0_file "edit" ~doc ~descr ~envs @@
  Term.(const edit $ names)
