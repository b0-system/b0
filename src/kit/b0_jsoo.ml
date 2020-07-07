(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00
open B00_ocaml

(* FIXME good to have this but we can see a lot of things in b0 and
   b0_ocaml are not at their place yet. *)

let tag = B0_meta.Key.tag "jsoo" ~doc:"js_of_ocaml related entity"
module Meta = struct
end

(* FIXME B0_ocaml copycat *)

let compile_intfs ~and_cmti m ~comp ~opts ~requires ~mod_srcs =
  let compile _ src =
    Compile.mod_src_intf ~and_cmti m ~comp ~mod_srcs ~requires ~opts src
  in
  String.Map.iter compile mod_srcs

let compile_impls ~and_cmt m ~code ~opts ~requires ~mod_srcs =
  let compile _ src =
    Compile.mod_src_impl ~and_cmt m ~code ~opts ~mod_srcs ~requires src
  in
  String.Map.iter compile mod_srcs


let byte_comp set_mod_srcs srcs exe_name b =
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_root_dir b in
  let* mod_srcs = Mod.Src.map_of_files m ~build_dir ~src_root ~srcs in
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get B0_ocaml.Meta.requires meta in
  set_mod_srcs mod_srcs;
  let* conf = B0_build.get b B0_ocaml.conf in
  let* resolver = B0_build.get b B0_ocaml.lib_resolver in
  let* comp_requires = Lib.Resolver.get_list resolver requires in
  let exe_ext = B00_ocaml.Conf.exe_ext conf in
  let exe_name = B0_meta.get B0_meta.exe_name meta in
  let obyte = Fpath.(build_dir / (exe_name ^ ".byte" ^ exe_ext)) in
  let opts = Cmd.(arg "-g") (* TODO *) in
  let code = `Byte in
  let comp = Tool.ocamlc in
  compile_intfs ~and_cmti:true m ~comp ~opts ~requires:comp_requires ~mod_srcs;
  compile_impls ~and_cmt:true m ~code ~opts ~requires:comp_requires ~mod_srcs;
  let c_objs = [] in
  let mod_srcs = Mod.Src.sort (* for link *) ~deps:Mod.Src.ml_deps mod_srcs in
  let* link_requires = Lib.Resolver.get_list_and_deps resolver requires in
  let lib_objs = List.filter_map Lib.cma link_requires in
  let cobjs = List.filter_map (Mod.Src.impl_file ~code) mod_srcs  in
  let opts = Cmd.(opts % "-no-check-prims") in
  let cobjs = lib_objs @ cobjs in
  Link.code m ~conf ~code ~opts ~c_objs ~cobjs ~o:obyte;
  Fut.return (cobjs, obyte)

let _exe_proc set_exe_path set_mod_srcs srcs b =
  let opts = Cmd.empty in
  let source_map = Some `File in
  let build_dir = B0_build.current_build_dir b in
  let exe_name = B0_meta.get B0_meta.exe_name (B0_build.current_meta b) in
  let o = Fpath.(build_dir / (exe_name ^ ".js")) in
  set_exe_path o;
  let* _cobjs, obyte = byte_comp set_mod_srcs srcs exe_name b in
  B00_jsoo.compile (B0_build.memo b) ~opts ~source_map ~jss:[] ~byte:obyte ~o;
  Fut.return ()

let exe_proc set_exe_path set_mod_srcs srcs b =
  let* srcs = B0_srcs.select b srcs in
  _exe_proc set_exe_path set_mod_srcs srcs b

let copy_assets m srcs ~dst =
  let exts = String.Set.remove ".js" B00_fexts.www in
  let assets = B00_fexts.find_files exts srcs in
  let copy src =
    let dst = Fpath.(dst / Fpath.basename src) in
    Memo.copy m ~src dst
    (* FIXME for now this is flat.  I think we should expose a
       src_root in [srcs] (at that level because of watermarking) and
       redo the the hierarchy. *)
  in
  List.iter (Memo.file_ready m) assets;
  List.iter copy assets

let has_html file srcs =
  let has_file src = String.equal file (Fpath.basename src) in
  List.exists has_file (B00_fexts.find_files (B00_fexts.ext ".html") srcs)

let find_styles srcs =
  List.map Fpath.basename (B00_fexts.find_files B00_fexts.css srcs)


let web_assets set_exe_path srcs b =
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let meta = B0_build.current_meta b in
  let exe_name = B0_meta.get B0_meta.exe_name meta in
  let html_file = exe_name ^ ".html" in
  let js = exe_name ^ ".js" in
  let o = Fpath.(build_dir / html_file) in
  set_exe_path o;
  copy_assets m srcs ~dst:build_dir;
  if has_html html_file srcs then Fut.return () else
  let styles = find_styles srcs in
  B00_jsoo.write_page m ~styles ~scripts:[js] ~o;
  Fut.return ()


let web_proc set_exe_path set_mod_srcs srcs b =
  let* srcs = B0_srcs.select b srcs in
  let* () = _exe_proc (fun _ -> ()) set_mod_srcs srcs b in
  web_assets set_exe_path srcs b
(*
                     "--toplevel" % "--no-runtime" %
                     "--export" %% unstamp (path mod_names) %
                     "+runtime.js" % "+toplevel.js" % "+dynlink.js" %%
                     unstamp (path byte_exe))
*)

let top_proc set_exe_path set_mod_srcs srcs b =
  let* srcs = B0_srcs.select b srcs in
  let source_map = Some `File in
  let build_dir = B0_build.current_build_dir b in
  let exe_name = B0_meta.get B0_meta.exe_name (B0_build.current_meta b) in
  let o = Fpath.(build_dir / (exe_name ^ ".js")) in
  let* _cobjs, obyte = byte_comp set_mod_srcs srcs exe_name b in
  let opts = Cmd.(arg "--toplevel" % "+toplevel.js" % "+dynlink.js") in
  B00_jsoo.compile (B0_build.memo b) ~opts ~source_map ~jss:[] ~byte:obyte ~o;
  web_assets set_exe_path srcs b

(* FIXME lots to factorize *)

let node_action build u ~args:argl =
  let err e = Log.err (fun m -> m "%s" e); Fut.return B00_cli.Exit.some_error in
  match B0_unit.get_meta B0_meta.exe_file u with
  | Error e -> err e
  | Ok exe_file ->
      let* exe_file = exe_file in
      let node = Fpath.v "node" in
      match Os.Cmd.get_tool (* FIXME first search in build *) node with
      | Error e -> err e
      | Ok node_exe ->
          let cmd = Cmd.(path node %% path exe_file %% args argl) in
          B0_unit.Action.exec_file build u node_exe cmd

let show_uri_action build u ~args:argl =
  let err e = Log.err (fun m -> m "%s" e); Fut.return B00_cli.Exit.some_error in
  match B0_unit.get_meta B0_meta.exe_file u with
  | Error e -> err e
  | Ok exe_file ->
      let* exe_file = exe_file in
      let show_uri = Fpath.v "show-uri" in
      match Os.Cmd.get_tool (* FIXME search in build *) show_uri with
      | Error e -> err e
      | Ok show_uri_exe ->
          let cmd = Cmd.(path show_uri %% path exe_file %% args argl) in
          B0_unit.Action.exec_file build u show_uri_exe cmd

let unit_meta ~meta ~requires ~name ~mod_srcs ~exe_name ~exe_path =
  meta
  |> B0_meta.tag tag
  |> B0_meta.tag B0_ocaml.tag
  |> B0_meta.tag B0_meta.exe
  |> B0_meta.add B0_meta.exe_name exe_name
  |> B0_meta.add B0_ocaml.Meta.requires requires
  |> B0_meta.add B0_ocaml.Meta.mod_srcs mod_srcs
  |> B0_meta.add B0_meta.exe_file exe_path
  |> B0_meta.add B0_ocaml.Meta.supported_code `Byte
  |> B0_meta.add B0_ocaml.Meta.needs_code `Byte

let exe
    ?doc ?(meta = B0_meta.empty) ?(action = show_uri_action) ?(requires = [])
    ?name exe_name ~srcs
  =
  let name = Option.value ~default:exe_name name in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let meta = unit_meta ~meta ~requires ~name ~mod_srcs ~exe_name ~exe_path in
  B0_unit.v ?doc ~action ~meta name (exe_proc set_exe_path set_mod_srcs srcs)

let web
    ?doc ?(meta = B0_meta.empty) ?(action = show_uri_action) ?(requires = [])
    ?name page ~srcs
  =
  let name = Option.value ~default:page name in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let exe_name = page in
  let meta = unit_meta ~meta ~requires ~name ~mod_srcs ~exe_name ~exe_path in
  B0_unit.v ?doc ~action ~meta name (web_proc set_exe_path set_mod_srcs srcs)

let top
    ?doc ?(meta = B0_meta.empty) ?(action = show_uri_action) ?(requires = [])
    ?name page ~srcs
  =
  let name = Option.value ~default:page name in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let exe_name = page in
  let meta = unit_meta ~meta ~requires ~name ~mod_srcs ~exe_name ~exe_path in
  B0_unit.v ?doc ~action ~meta name (top_proc set_exe_path set_mod_srcs srcs)



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
