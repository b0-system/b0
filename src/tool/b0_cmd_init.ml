(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let find_copyright_holder ~cwd _meta = function
| Some holder -> Ok holder
| None ->
    (* We could look into B0_meta.authors but this tends to be unwiedly
       `git config user.name` could be something else. *)
    let* name = B0_init.find_project_name ~cwd () in
    match name with
    | Some name -> Ok (Fmt.str "The %s programmers" name)
    | None ->
        Fmt.error
          "@[<v>Could not find a project name to assign copyright.@,\
           Use option %a to specify a copyright holder.@]"
          Fmt.code' "--holder"

(* b0 file init *)

let b0_file file force _conf =
  (* TODO this needs to be made much better. *)
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let b0_file =
    "open B0_kit.V000\n\n\
    (* Library names *)\n\n\
    (* Libraries *)\n\n\
    (* Tools *)\n\n\
    (* Tests *)\n\n\
    (* Packs *)\n\n\
    let default =\n\
    \ let meta =\n\
    \   B0_meta.empty\n\
    \   |> B0_meta.tag B0_opam.tag\n\
    \ in\n\
    \ B0_pack.make \"default\" ~doc:\"Undocumented\" ~meta ~locked:true @@\n\
    \ B0_unit.list ()\n\
    "
  in
  let* () = Os.File.write ~force ~make_path:true file b0_file in
  Ok B0_cli.Exit.ok

(* Changes file init *)

let changes file force _conf =
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let* changes = B0_init.find_changes_generator file in
  let changes = changes () in
  let* () = Os.File.write ~force ~make_path:true file changes in
  Ok B0_cli.Exit.ok

(* License file init *)

let license years holder license file force conf =
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let cwd = B0_driver.Conf.cwd conf in
  let meta = B0_init.get_project_meta () in
  let years = B0_init.get_copyright_years years in
  let* holder = find_copyright_holder ~cwd meta holder in
  let license = B0_init.get_license meta license in
  let* text = B0_init.download_license_template ~strip_meta:true license in
  let license, warns = B0_init.license text ~years ~holder in
  List.iter (fun warn -> Log.warn (fun m -> m "%s" warn)) warns;
  let* () = Os.File.write ~force ~make_path:true file license in
  Ok B0_cli.Exit.ok

(* Readme file init *)

let get_readme_project_name ~cwd = function
| Some name -> Ok name
| None ->
    let* name = B0_init.find_project_name ~cwd () in
    match name with
    | Some name -> Ok name
    | None ->
        Fmt.error
          "@[<v>Could not find a project name to use.@,\
           Use option %a to specify a project name.@]" Fmt.code' "--name"

let readme name synopsis file force conf =
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let cwd = B0_driver.Conf.cwd conf in
  let meta = B0_init.get_project_meta () in
  let* project_name = get_readme_project_name ~cwd name in
  let* readme = B0_init.find_readme_generator file in
  let readme = readme ~project_name ~synopsis meta in
  let* () = Os.File.write ~force ~make_path:true file readme in
  Ok B0_cli.Exit.ok

(* Source file init *)

let get_lang ~file ~lang = match lang with
| Some lang -> Ok lang
| None when Fpath.equal file Fpath.dash -> Ok `Ocaml
| None ->
    let ext = Fpath.get_ext file in
    match B0_init.lang_of_file_ext ext with
    | Some lang -> Ok lang
    | None ->
        Fmt.error
          "@[<v>Could not find a language for extension %a@,\
           Use option %a to specify one.@]" Fmt.code' ext Fmt.code' "--lang"

let src years holder license lang files example force conf =
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let files = match files with [] -> [Fpath.dash] | files -> files in
  let cwd = B0_driver.Conf.cwd conf in
  let meta = B0_init.get_project_meta () in
  let years = B0_init.get_copyright_years years in
  let* holder = find_copyright_holder ~cwd meta holder in
  let license = B0_init.get_src_license meta ~example license in
  let write_file file =
    let* lang = get_lang ~file ~lang in
    let src = B0_init.src_generator lang in
    let src = src ~years ~holder ~license in
    Os.File.write ~force ~make_path:true file src
  in
  let* () = List.iter_stop_on_error write_file files in
  Ok B0_cli.Exit.ok

open Cmdliner

let force =
  let doc = "Overwrite any existing file rather than error." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let file =
  let doc = "Generate to file $(docv). Standard output if unspecified." in
  Arg.(value & pos 0 B0_cli.fpath Fpath.dash & info [] ~doc ~docv:"PATH")

let license_opt =
  let doc =
    "$(docv) is the SPDX license identifier. See <https://spdx.dev/ids/>."
  in
  let absent =
    "$(b,ISC) or the first license of $(b,.meta.licenses) in the default pack."
  in
  Arg.(value & opt (some string) None &
       info ["L"; "license"] ~doc ~docv:"SPDXID" ~absent)

let holder =
  let doc =
    "$(docv) is the copyright holder. If absent a $(b,project name) is \
     derived by finding a project root directory (by detecting a VCS \
     directory or a root build file) and the copyright is assigned to \
     the programmers of that project."
  in
  let absent = "The $(b,project name) programmers" in
  Arg.(value & opt (some string) None &
       info ["h"; "holder"] ~doc ~docv:"HOLDER" ~absent)

let years =
  let doc = "$(docv) is the copyright years." and docv = "YEARS" in
  let absent = "current year" in
  Arg.(value & opt (some string) None & info ["y"; "years"] ~doc ~docv ~absent)

let b0_file =
  let doc = "Generate a $(b,B0.ml) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,B0.ml) file \
          for a software project. For example:";
      `Pre "$(iname) $(b,> B0.ml)"; `Noblank;
      `Pre "$(iname) $(b,B0.ml)";
      `P "TODO unastisfactory for now."]
  in
  B0_tool_std.Cli.subcmd_with_b0_file_if_any "b0-file" ~doc ~descr @@
  Term.(const b0_file $ file $ force)

let changes =
  let doc = "Generate a $(b,CHANGES) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,CHANGES) file \
          for a software project. For example:";
      `Pre "$(iname) $(b,> CHANGES.md)"; `Noblank;
      `Pre "$(iname) $(b,CHANGES.md)"; ]
  in
  B0_tool_std.Cli.subcmd_with_b0_file_if_any "changes" ~doc ~descr @@
  Term.(const changes $ file $ force)

let license =
  let doc = "Generate a $(b,LICENSE) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,LICENSE) file \
          for a software project. It downloads the license text \
          from the $(b,choosealicense.com) project data and substitutes \
          the years and copyright holder in it. For example:";
      `Pre "$(iname) $(b,> LICENSE.md)"; `Noblank;
      `Pre "$(iname) $(b,-l ISC > LICENSE.md)"; `Noblank;
      `Pre "$(iname) $(b,-h holder -l ISC > LICENSE.md)"; `Noblank;
      `Pre "$(iname) $(b,LICENSE.md)";
      `P "If a B0 file with a default pack is available a license \
          for the first license of the $(b,.meta.licenses) key is generated \
          if $(b,--license) is unspecified." ]
  in
  B0_tool_std.Cli.subcmd_with_b0_file_if_any "license" ~doc ~descr @@
  Term.(const license $ years $ holder $ license_opt $ file $ force)

let readme =
  let doc = "Generate a $(b,README) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,README.md) file \
          for a software project. For example:";
      `Pre "$(iname) $(b,-s \"My project synopsis\" > README.md) "; `Noblank;
      `Pre "$(iname) $(b,-n myproject README.md) ";
      `P "If a B0 file with a default pack is available it may use some of \
          its metadata to generate boilerplate. In particular if it \
          has the $(b,.opam.tag) key, OCaml specific instructions are \
          generated." ]
  in
  let name' =
    let doc = "$(docv) is the project name. If absent \
               a $(b,project name) is derived by finding a project root \
               directory (by detecting a VCS directory or a root build file)"
    in
    let absent = "Name of the root directory" in
    Arg.(value & opt (some string) None &
         info ["n"; "name"] ~doc ~docv:"PROJECT_NAME" ~absent)
  in
  let synopsis =
    let doc = "$(docv) is a one line description for the project." in
    Arg.(value & opt (some string) None &
         info ["s"; "synopsis"] ~doc ~docv:"SYNOPSIS")
  in
  B0_tool_std.Cli.subcmd_with_b0_file_if_any "readme" ~doc ~descr @@
  Term.(const readme $ name' $ synopsis $ file $ force)

let src =
  let files =
    let doc =
      "Generate to file $(docv). Repeatable. Standard output if unspecified."
    in
    Arg.(value & pos_all B0_cli.fpath [] & info [] ~doc ~docv:"PATH")
  in
  let lang =
    let lang_conv = Arg.conv' (B0_init.(lang_of_id, pp_lang_id)) in
    let doc = "$(docv) is the source language." in
    let absent = "derived from file extension or $(b,ocaml) on stdout" in
    Arg.(value & opt (some lang_conv) None &
         info ["l"; "lang"] ~doc ~docv:"LANG" ~absent )
  in
  let example =
    let doc =
      "Example source code. If $(b,--license) is unspecified, uses \
       $(b,CC0-1.0) for the license."
    in
    Arg.(value & flag & info ["x"; "example"] ~doc)
  in
  let doc = "Generate a copyrighted source file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates an empty copyrighted source file. \
          For example:";
      `P "$(iname) $(b,mysrc.c)"; `Noblank;
      `P "$(iname) $(b,-x) $(b,example.c)"; `Noblank;
      `P "$(iname) $(b,-y 2038 mysrc.mli mysrc.ml) "; `Noblank;
      `P "$(iname) $(b,-h Unknown > mysrc.ml)"; `Noblank;
      `P "$(iname) $(b,-l c > mysrc.h)";
      `P "The command makes best-effort guesses to derive the file's language,
           the copyright year, the copyright holder and the SPDX license. \
          See the corresponding options for more details.";
      `P "The output format is fixed, it cannot be tweaked." ]
  in
  B0_tool_std.Cli.subcmd_with_b0_file_if_any "src" ~doc ~descr @@
  Term.(const src $ years $ holder $ license_opt $ lang $ files $ example $
        force)

let cmd =
  let doc = "Generate files from templates" in
  let descr = `P "The $(iname) command generates files from templates." in
  B0_tool_std.Cli.cmd_group "init" ~doc ~descr @@
  [b0_file; changes; license; readme; src]
