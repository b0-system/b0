(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "dune"

open B0_std
open Result.Syntax

let write_file ~dry_run file content =
  let* () =
    if dry_run
    then Ok ()
    else Os.File.write ~force:true ~make_path:true file content
  in
  Log.stdout (fun m -> m ~header:"WROTE" "%a" Fpath.pp file);
  if dry_run
  then Ok (Log.stdout (fun m -> m "%s@\n" content))
  else Ok ()

let library_stanza ppf u =
  let requires = B0_unit.find_meta B0_ocaml.requires u in
  let requires = Option.value ~default:[] requires in
  let requires = List.map B0_ocaml.Libname.to_string requires in
  let library = Option.get (B0_unit.find_meta B0_ocaml.library u) in
  let library = B0_ocaml.Libname.to_string library in
  let mod_names = match B0_unit.find_meta B0_ocaml.modsrcs u with
  | None -> []
  | Some m ->
      (* FIXME needs a story for easily looking up dynamic metadata
         for now this requires running the build *)
      if true then [] else
      let modname (_, src) = B0_ocaml.Modsrc.modname src in
      List.map modname (B0_ocaml.Modname.Map.bindings (Fut.sync m))
  in
  Fmt.pf ppf
    "@[<v1>(library@,(public_name %s)@,(wrapped false)@,@[<1>(libraries %a)@]@,\
     @[<1>(modules %a)@])@]"
    library Fmt.(list ~sep:sp string) requires Fmt.(list string) mod_names

let dune_file libs =
  Fmt.str "@[<v>(include_subdirs unqualified)@,@[<v>%a@]@]"
    Fmt.(list ~sep:Fmt.(cut ++ cut) library_stanza) libs

let write_dune_file ~dry_run (scope_dir, libs) =
  let file = Fpath.(scope_dir / "dune") in
  write_file ~dry_run file (dune_file libs)

let dune_project () =
  Fmt.str "@[<v>(lang dune 3.8)@,(generate_opam_files false)@]"

let ocaml_libs_by_scope us =
  let add_by_scope_dir acc u =
    if not (B0_unit.mem_meta B0_ocaml.library u) then acc else
    Log.if_error ~use:acc @@
    let* scope_dir = B0_unit.scope_dir' u in
    Ok (Fpath.Map.add_to_list scope_dir u acc)
  in
  List.fold_left add_by_scope_dir Fpath.Map.empty us

let file ~env ~units ~dry_run =
  let dry_run = true in
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* units = B0_unit.get_list_or_hint ~all_if_empty:true units in
  let ocaml_libs = Fpath.Map.bindings (ocaml_libs_by_scope units) in
  let root_dir = B0_env.root_dir env (* Do one dune file per scope dirs ? *) in
  let root_project = Fpath.(root_dir / "dune-project") in
  let* () = write_file ~dry_run root_project (dune_project ()) in
  let* () = List.iter_stop_on_error (write_dune_file ~dry_run) ocaml_libs in
  Ok Os.Exit.ok

(* .dune action command line interface  *)

open Cmdliner
open Cmdliner.Term.Syntax

let units ~right:r =
  let doc = "The $(docv) to act on. All of them if unspecified." in
  Arg.(value & pos_right r string [] & info [] ~doc ~docv:"UNIT")

let dune_cmd env u =
  let man =
    [ `S Manpage.s_see_also;
      `P "Consult $(b,odig doc b0) for the b0 dune manual."]
  in
  let export =
    let doc = "Best-effort dune file generation" in
    let man = [
      `P "$(iname) tries to derive dune files for OCaml \
          libraries defined in the project. This is a best-effort \
          procedure the files may need tweaking and since $(b,b0)'s build \
          model is more dynamic this may fail. But it should work well \
          on simple libraries.";
      `P "Use option $(b,--dry-run) to check the files that would be written \
          and their contents.";
      `Blocks man ]
    in
    Cmd.make (Cmd.info "file" ~doc ~man) @@
    let+ units = units ~right:(-1)
    and+ dry_run =
      let doc = "Do not write the files action, just print them." in
      Arg.(value & flag & info ["dry-run"] ~doc)
    in
    file ~env ~units ~dry_run
  in
  let man =
    [ `S Cmdliner.Manpage.s_description;
      `P "$(iname) helps with $(b,dune) \
          see the b0 dune manual in $(b,odig doc b0) and \
          invoke the subcommands with $(b,--help) for more \
          information.";
      `Blocks man]
  in
  let name = B0_unit.name u and doc = B0_unit.doc u in
  Cmd.group (Cmd.info name ~doc ~man) @@
  [export]

let unit =
  let doc = "dune support" in
  B0_unit.of_cmdliner_cmd "" dune_cmd ~doc

let () = B0_scope.close ()
