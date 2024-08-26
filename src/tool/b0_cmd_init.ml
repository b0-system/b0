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
          Fmt.code "--holder"

(* B0.ml file init *)

let b0_ml file force _conf =
  (* TODO this needs to be made much better. *)
  Log.if_error ~use:Os.Exit.some_error @@
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
  Ok Os.Exit.ok

(* Changes file init *)

let changes file force _conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let* changes = B0_init.find_changes_generator file in
  let changes = changes () in
  let* () = Os.File.write ~force ~make_path:true file changes in
  Ok Os.Exit.ok

(* [.gitignore] file init. *)

let gitignore file force _conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let gitignore = "_b0\n_build\ntmp\n*.install\n" in
  let* () = Os.File.write ~force ~make_path:true file gitignore in
  Ok Os.Exit.ok

(* License file init *)

let license years holder license file force conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let cwd = B0_driver.Conf.cwd conf in
  let meta = B0_init.get_project_meta () in
  let years = B0_init.get_copyright_years years in
  let* holder = find_copyright_holder ~cwd meta holder in
  let license = B0_init.get_license meta license in
  let* text = B0_init.download_license_template ~strip_meta:true license in
  let license, warns = B0_init.license text ~years ~holder in
  List.iter (fun warn -> Log.warn (fun m -> m "%s" warn)) warns;
  let* () = Os.File.write ~force ~make_path:true file license in
  Ok Os.Exit.ok

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
           Use option %a to specify a project name.@]" Fmt.code "--name"

let readme name synopsis file force conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let cwd = B0_driver.Conf.cwd conf in
  let meta = B0_init.get_project_meta () in
  let* project_name = get_readme_project_name ~cwd name in
  let* readme = B0_init.find_readme_generator file in
  let readme = readme ~project_name ~synopsis meta in
  let* () = Os.File.write ~force ~make_path:true file readme in
  Ok Os.Exit.ok

(* Source file templates. TODO a scheme like we had in caracass. *)

(* set at the end of this file to avoid cluttering *)

let templates = ref []

let src_template t = match List.assoc_opt t !templates with
| Some t -> Ok t
| None ->
    let names = List.map fst !templates in
    let suggestions = String.suggest names t in
    Fmt.error "@[%s: @[<v>No such template. %a@]"
      t Fmt.(did_you_mean string) suggestions

let template_list () conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let names = List.map fst !templates in
  Log.app (fun m -> m "@[<v>%a@]" Fmt.(code' (list string)) names);
  Ok Os.Exit.ok

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
           Use option %a to specify one.@]" Fmt.code ext Fmt.code "--lang"

let src ~years ~holder ~license ~lang ~files ~example ~force ~template conf =
  Log.if_error ~use:Os.Exit.some_error @@
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
    let* content = match template with
    | None -> Ok "" | Some t -> src_template t
    in
    let src = String.concat "\n" [src; content] in
    Os.File.write ~force ~make_path:true file src
  in
  let* () = List.iter_stop_on_error write_file files in
  Ok Os.Exit.ok

let snip ~files ~force ~template conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let files = match files with [] -> [Fpath.dash] | files -> files in
  let write_file file =
    let* content = src_template template in
    Os.File.write ~force ~make_path:true file content
  in
  let* () = List.iter_stop_on_error write_file files in
  Ok Os.Exit.ok

open Cmdliner
open Cmdliner.Term.Syntax

let force =
  let doc = "Overwrite any existing file rather than error." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let file =
  let doc = "Generate to file $(docv)." in
  let absent = "$(b,stdout)" in
  Arg.(value & pos 0 B0_std_cli.fpath Fpath.dash &
       info [] ~doc ~docv:"PATH" ~absent)

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

let template =
  let doc =
    "$(docv) is the template to use for the file content. \
     See $(b,b0 init template list) for a list";
  in
  let absent = "no content" in
  Arg.(value & opt (some string) None &
       info ["t"; "template"] ~doc ~docv:"NAME" ~absent)

let lang =
  let lang_conv = Arg.conv' (B0_init.(lang_of_id, pp_lang_id)) in
  let doc = "$(docv) is the source language." in
  let absent = "derived from file extension or $(b,ocaml) on stdout" in
  Arg.(value & opt (some lang_conv) None &
       info ["l"; "lang"] ~doc ~docv:"LANG" ~absent)

let b0_ml_cmd =
  let doc = "Generate a $(b,B0.ml) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,B0.ml) file \
          for a software project. For example:";
      `Pre "$(iname) $(b,> B0.ml)"; `Noblank;
      `Pre "$(iname) $(b,B0.ml)";
      `P "TODO unsatisfactory for now."]
  in
  B0_tool.Cli.subcmd_with_b0_file_if_any "B0.ml" ~doc ~descr @@
  Term.(const b0_ml $ file $ force)

let changes_cmd =
  let doc = "Generate a $(b,CHANGES) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,CHANGES) file \
          for a software project. For example:";
      `Pre "$(iname) $(b,> CHANGES.md)"; `Noblank;
      `Pre "$(iname) $(b,CHANGES.md)"; ]
  in
  B0_tool.Cli.subcmd_with_b0_file_if_any "CHANGES" ~doc ~descr @@
  Term.(const changes $ file $ force)

let gitignore_cmd =
  let doc = "Generate a $(b,.gitignore) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates $(b,.gitignore) file.
          For example:";
      `Pre "$(iname) $(b,> .gitignore)";
      `Pre "$(iname) $(b,.gitignore)"; ]
  in
  B0_tool.Cli.subcmd_with_b0_file_if_any ".gitignore" ~doc ~descr @@
  Term.(const gitignore $ file $ force)

let license_cmd =
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
      `P "If a b0 file with a default pack is available a license \
          for the first license of the $(b,.meta.licenses) key is generated \
          if $(b,--license) is unspecified." ]
  in
  B0_tool.Cli.subcmd_with_b0_file_if_any "LICENSE" ~doc ~descr @@
  Term.(const license $ years $ holder $ license_opt $ file $ force)

let readme_cmd =
  let doc = "Generate a $(b,README) file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates a $(b,README.md) file \
          for a software project. For example:";
      `Pre "$(iname) $(b,-s \"My project synopsis\" > README.md) "; `Noblank;
      `Pre "$(iname) $(b,-n myproject README.md) ";
      `P "If a b0 file with a default pack is available it may use some of \
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
  B0_tool.Cli.subcmd_with_b0_file_if_any "README" ~doc ~descr @@
  Term.(const readme $ name' $ synopsis $ file $ force)

let src_cmd =
  let doc = "Generate a copyrighted source file" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates copyrighted source file. The content \
          is empty unless the option $(b,-t) is specified. If you want \
          content without the copyright use $(b,b0 init snip). For example:";
      `Pre "$(iname) $(b,> mysrc.ml)  # Defaults has OCaml syntax"; `Noblank;
      `Pre "$(iname) $(b,-l c > mysrc.h)";`Noblank;
      `Pre "$(iname) $(b,mysrc.h) $(b,mysrc.c)"; `Noblank;
      `Pre "$(iname) $(b,-x) $(b,example.c)"; `Noblank;
      `Pre "$(iname) $(b,-y 2038 mysrc.mli mysrc.ml) "; `Noblank;
      `Pre "$(iname) $(b,-h Unknown > mysrc.ml)"; `Noblank;
      `Pre "$(iname) $(b,-t cmdliner > tool.ml)";
      `P "The command makes best-effort guesses to derive the file's language,
           the copyright year, the copyright holder and the SPDX license. \
          See the corresponding options for more details.";
      `P "The copyright header format is fixed, it cannot be tweaked." ]
  in
  B0_tool.Cli.subcmd_with_b0_file_if_any "src" ~doc ~descr @@
  let+ years and+ holder and+ license = license_opt and+ force and+ template
  and+ lang
  and+ files =
    let doc = "Generate to file $(docv). Repeatable." in
    let absent = "$(b,stdout)" in
    Arg.(value & pos_all B0_std_cli.fpath [] & info [] ~doc ~docv:"PATH" ~absent)
  and+ example =
    let doc =
      "Example source code. If $(b,--license) is unspecified, uses \
       $(b,CC0-1.0) for the license."
    in
    Arg.(value & flag & info ["x"; "example"] ~doc)
  in
  src ~years ~holder ~license ~lang ~files ~example ~force ~template

let snip_cmd =
  let doc = "Generate a snip of code" in
  let descr =
    `Blocks [
      `P "The $(iname) command generates an snip of code. It's exactly like
          the $(b,src) command but without the copyright headers."; ]
  in
  B0_tool.Cli.subcmd_with_b0_file_if_any "snip" ~doc ~descr @@
  let+ force
  and+ files =
    let doc = "Generate to file $(docv). Repeatable." in
    let absent = "$(b,stdout)" in
    Arg.(value & pos_right 0 B0_std_cli.fpath [] &
         info [] ~doc ~docv:"PATH" ~absent)
  and+ template =
    let doc =
      "$(docv) is the content template. \
       See $(b,b0 init template list) for a list";
    in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
  in
  snip ~files ~force ~template

let templates_cmd =
  let doc = "Operate on content templates" in
  let descr = `Blocks [
      `P "The $(iname) command operates on content templates.";
      `P "Note that for now templates are hard-coded."; ]
  in
  let template_list_cmd =
    let doc = "List templates" in
    let descr = `P "The $(iname) command lists available templates." in
    B0_tool.Cli.subcmd_with_b0_file_if_any "list" ~doc ~descr @@
    let+ () = Term.const () in
    template_list ()
  in
  B0_tool.Cli.cmd_group "template" ~doc ~descr @@
  [template_list_cmd]

let cmd =
  let doc = "Generate files from templates" in
  let descr = `P "The $(iname) command generates files from templates." in
  B0_tool.Cli.cmd_group "init" ~doc ~descr @@
  [b0_ml_cmd; changes_cmd; gitignore_cmd; license_cmd; readme_cmd;
   snip_cmd; src_cmd; templates_cmd;]


(* Templates, TODO eventually get rid of this with a file system based
   lookup mechanism. *)

let templates' = [
"cmdliner",
"let cmd ~flag = 0\n\n\
 open Cmdliner\n\
 open Cmdliner.Term.Syntax\n\n\
 let cmd =\n\
\  Cmd.v (Cmd.info \"TODO\" ~version:\"\x25%VERSION%%\") @@\n\
\  let+ flag = Arg.(value & flag & info [\"flag\"]) in\n\
\  cmd ~flag\n\n\
 let main () = Cmd.eval' cmd\n\
 let () = if !Sys.interactive then () else exit (main ())\n";

"b0.testing",
"open B0_testing\n\n\
 let test_that () =\n\
\  Test.test \"that\" @@ fun () ->\n\
\  Test.int ~__POS__ 1 1;\n\
\  ()\n\n\
 let main () =\n\
\  Test.main @@ fun () ->
\  test_that ();
\  ()\n\n\
let () = if !Sys.interactive then () else exit (main ())\n";]

let () = templates := templates'
