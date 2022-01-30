(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00
open B00_ocaml

let () = B0_def.Scope.lib "jsoo"

(* Metadata keys *)

let tag = B0_meta.Key.tag "jsoo" ~doc:"js_of_ocaml related entity"

let comp =
  let doc = "Options added to the jsoo compile command" in
  let pp_value = Cmd.pp in
  B0_meta.Key.v "jsoo-comp" ~doc ~pp_value

let link =
  let doc = "Options added to the jsoo link command" in
  let pp_value = Cmd.pp in
  B0_meta.Key.v "jsoo-link" ~doc ~pp_value

let source_map =
  let pp_kind ppf = function
  | `Inline -> Fmt.string ppf "inline" | `File -> Fmt.string ppf "file"
  in
  let pp_value = Fmt.option ~none:(Fmt.any "none") pp_kind in
  B0_meta.Key.v "jsoo-src-map" ~doc:"Source map desires" ~pp_value

let toplevel =
  let pp_value = Fmt.bool in
  B0_meta.Key.v "jsoo-toplevel" ~doc:"Compile with toplevel support" ~pp_value

type comp_mode = [ `Separate | `Whole ]

let comp_mode =
  let pp_value ppf = function
  | `Separate -> Fmt.string ppf "separate" | `Whole -> Fmt.string ppf "whole"
  in
  B0_meta.Key.v "jsoo-comp-mode" ~doc:"Compilation mode" ~pp_value

let assets_root =
  let doc = "Root path from which assets are rerooted." in
  let pp_value = Fpath.pp_unquoted in
  B0_meta.Key.v "jsoo-assets-root" ~doc ~pp_value

let meta
    ?(meta = B0_meta.empty) ?assets_root:ar ?comp:c
    ?comp_mode:(cm = `Whole) ?link:l ?(requires = [])
    ?source_map:(s = Some `File) ?toplevel:(t = false) ()
  =
  meta
  |> B0_meta.add_if_some assets_root ar
  |> B0_meta.add_if_some comp c
  |> B0_meta.add comp_mode cm
  |> B0_meta.add_if_some link l
  |> B0_meta.add B0_ocaml.Meta.requires requires
  |> B0_meta.add source_map s
  |> B0_meta.add toplevel t

(* Build fragments *)

let get_mod_srcs b ~srcs =
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_scope_dir b in
  Mod.Src.map_of_files (B0_build.memo b) ~build_dir ~src_root ~srcs

let get_link_objs ~code ~resolver ~requires ~mod_srcs =
  let mod_srcs = Mod.Src.sort (* for link *) ~deps:Mod.Src.ml_deps mod_srcs in
  let mod_objs = List.filter_map (Mod.Src.impl_file ~code) mod_srcs  in
  let* link_requires = Lib.Resolver.get_list_and_deps resolver requires in
  let lib_objs = List.filter_map Lib.cma link_requires in
  Fut.return (lib_objs, mod_objs)

let compile_byte m ~opts ~resolver ~requires ~mod_srcs =
  let code = `Byte in
  let comp = Tool.ocamlc in
  let* requires = Lib.Resolver.get_list resolver requires in
  Compile.intfs ~and_cmti:true m ~comp ~opts ~requires ~mod_srcs;
  Compile.impls ~and_cmt:true m ~code ~opts ~requires ~mod_srcs;
  Fut.return ()

let link_byte m ~conf ~opts ~resolver ~requires ~mod_srcs ~o =
  let code = `Byte in
  let* lib_objs, mod_objs = get_link_objs ~code ~resolver ~requires ~mod_srcs in
  let cobjs = lib_objs @ mod_objs in
  Link.code m ~conf ~code ~opts ~c_objs:[] ~cobjs ~o;
  Fut.return ()

let byte_exe ~mod_srcs ~o b =
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get B0_ocaml.Meta.requires meta in
  let* conf = B0_build.get b B0_ocaml.conf in
  let o = Fpath.(o + B00_ocaml.Conf.exe_ext conf) in
  let* resolver = B0_build.get b B0_ocaml.lib_resolver in
  let toplevel = Option.value ~default:false (B0_meta.find toplevel meta) in
  let global_opts = Cmd.(atom "-g") (* TODO *) in
  let opts = global_opts in
  let m = B0_build.memo b in
  let* () = compile_byte m ~opts ~resolver ~requires ~mod_srcs in
  let opts = Cmd.(global_opts %% if' toplevel (atom "-linkall")) in
  let* () = link_byte m ~conf ~opts ~resolver ~requires ~mod_srcs ~o in
  Fut.return o

let js_of_byte_exe ~jss ~mod_srcs ~o b =
  let* byte = byte_exe ~mod_srcs ~o:Fpath.(o -+ ".byte") b in
  let meta = B0_build.current_meta b in
  let source_map = Option.join (B0_meta.find source_map meta) in
  let opts = Option.value ~default:Cmd.empty (B0_meta.find comp meta) in
  let toplevel = Option.value ~default:false (B0_meta.find toplevel meta) in
  let opts = match toplevel with
  | true -> Cmd.(opts % "--toplevel" % "+toplevel.js" % "+dynlink.js")
  | false -> opts
  in
  B00_jsoo.compile (B0_build.memo b) ~opts ~source_map ~jss ~byte ~o;
  Fut.return ()

let js_of_byte_objs ~jss ~mod_srcs ~o b =
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get B0_ocaml.Meta.requires meta in
  let* conf = B0_build.get b B0_ocaml.conf in
  let* resolver = B0_build.get b B0_ocaml.lib_resolver in
  let m = B0_build.memo b in
  let global_opts = Cmd.(atom "-g") (* TODO *) in
  let opts = global_opts in
  let* () = compile_byte m ~opts ~resolver ~requires ~mod_srcs in
  let code = `Byte in
  let* lib_objs, mod_objs = get_link_objs ~code ~resolver ~requires ~mod_srcs in
  let source_map = Option.join (B0_meta.find source_map meta) in
  let toplevel = Option.value ~default:false (B0_meta.find toplevel meta) in
  let ocamlrt_js =
    let opts = Cmd.empty in
    let opts = match toplevel with
    | false -> opts
    | true -> Cmd.(opts % "+toplevel.js" % "+dynlink.js")
    in
    let build_dir = B0_build.current_build_dir b in
    let o = Fpath.(build_dir / "ocamlrt.js") in
    B00_jsoo.build_runtime m ~opts ~jss ~o;
    o
  in
  let opts = Option.value ~default:Cmd.empty (B0_meta.find comp meta) in
  let opts = if toplevel then Cmd.(opts % "--toplevel") else opts in
  let lib_jss, std_exit_js =
    (* We need a cache similar to B0_ocaml.lib_resolver for jsing libs.
       The ops will be cached across units but not the file writes which
       we should do in the shared dir *)
    let build_dir = B0_build.current_build_dir b in
    let compile obj =
      (* FIXME this won't work with lib convention we need to
         remember the lib_name and mangle. *)
      let o = Fpath.(build_dir / (Fpath.basename obj ^ ".js")) in
      let opts = Cmd.(opts %% atom "-I" %% path (Fpath.parent obj)) in
      B00_jsoo.compile m ~opts ~source_map ~jss:[] ~byte:obj ~o;
      o
    in
    let compile_lib acc obj = compile obj :: acc  in
    let jss = List.rev (List.fold_left compile_lib [] lib_objs) in
    (* FIXME at least stdlib should be looked up via resolver *)
    let stdlib_cma = Fpath.(Conf.where conf / "stdlib.cma") in
    let stdlib_js = Memo.file_ready m stdlib_cma; compile stdlib_cma in
    let std_exit_cmo = Fpath.(Conf.where conf / "std_exit.cmo") in
    let std_exit_js = Memo.file_ready m std_exit_cmo; compile std_exit_cmo in
    stdlib_js :: jss, std_exit_js
  in
  let mod_obj_jss =
    let compile_obj acc obj =
      let o = Fpath.(obj + ".js") in
      B00_jsoo.compile m ~opts ~source_map ~jss:[] ~byte:obj ~o;
      o :: acc
    in
    let objs = List.rev (List.fold_left compile_obj [] mod_objs) in
    List.rev objs
  in
  let jss = ocamlrt_js :: (lib_jss @ mod_obj_jss @ [std_exit_js]) in
  let opts = Option.value ~default:Cmd.empty (B0_meta.find link meta) in
  B00_jsoo.link m ~opts ~source_map ~jss ~o;
  Fut.return ()

let js_exe ~jss ~mod_srcs ~o b =
  let comp_mode = B0_meta.find comp_mode (B0_build.current_meta b) in
  match Option.value ~default:`Whole comp_mode with
  | `Whole -> js_of_byte_exe ~jss ~mod_srcs ~o b
  | `Separate -> js_of_byte_objs ~jss ~mod_srcs ~o b

(* XXX the assets root churn may indicate it would be
   better to have that in B00_srcs. (or the deploy stuff) *)

let build_setup ~srcs b = (* return a record maybe ? *)
  let* srcs = B0_srcs.(Fut.map by_ext @@ select b srcs) in
  let* mod_srcs = get_mod_srcs b ~srcs in
  (* FIXME the lookup should be ordered here: *)
  let jss = B00_fexts.find_files B00_fexts.js srcs in
  let build_dir = B0_build.current_build_dir b in
  let exe_name = B0_meta.get B0_meta.exe_name (B0_build.current_meta b) in
  let js = Fpath.(build_dir / exe_name) in
  let html = Fpath.(js -+ ".html") in
  let exe_html = Fpath.basename html in
  let htmls = B00_fexts.find_files B00_fexts.html srcs in
  let html = match htmls with
  | [] -> html (* This will be generated *)
  | htmls ->
      let base b f = Fpath.basename f = b in
      let file = match List.find_opt (base exe_html) htmls with
      | Some f -> f
      | None ->
          match List.find_opt (base "index.html") htmls with
          | Some f -> f
          | None -> List.hd htmls
      in
      match B0_meta.find assets_root (B0_build.current_meta b) with
      | Some r when Fpath.is_prefix r file ->
          Fpath.reroot ~root:r ~dst:build_dir file
      | _ -> Fpath.(build_dir / Fpath.basename file)
  in
  Fut.return (srcs, mod_srcs, jss, js, html)

(* Build js executables *)

let exe_proc set_exe_path set_mod_srcs srcs b =
  let* _srcs, mod_srcs, jss, js, _html = build_setup ~srcs b in
  set_mod_srcs mod_srcs;
  set_exe_path js;
  js_exe ~mod_srcs ~jss ~o:js b

(* Build html webs *)

let copy_assets m srcs ~exts ~assets_root ~dst =
  let assets = B00_fexts.find_files exts srcs in
  let copy acc src =
    let dst = match assets_root with
    | Some r when Fpath.is_prefix r src -> Fpath.reroot ~root:r ~dst src
    | _ -> Fpath.(dst / Fpath.basename src)
    in
    Memo.copy m ~src dst;
    Fpath.Set.add dst acc
  in
  List.iter (Memo.file_ready m) assets;
  List.fold_left copy Fpath.Set.empty assets

let web_exe ~srcs ~js ~o b =
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let assets_root =
    match B0_meta.find assets_root (B0_build.current_meta b) with
    | None -> None
    | Some r -> Some (Fpath.(B0_build.current_scope_dir b // r))
  in
  let exts = String.Set.remove ".js" B00_fexts.www in
  let assets = copy_assets m srcs ~exts ~assets_root ~dst:build_dir in
  if Fpath.Set.mem o assets then Fut.return () else
  let css = Fpath.Set.filter (Fpath.has_ext ".css") assets in
  let base f = Fpath.to_string (Option.get (Fpath.strip_prefix build_dir f)) in
  let styles = List.map base (Fpath.Set.elements css) in
  B00_jsoo.write_page m ~styles ~scripts:[Fpath.basename js] ~o;
  Fut.return ()

let web_proc set_exe_path set_mod_srcs srcs b =
  let* srcs, mod_srcs, jss, js, html = build_setup srcs b in
  set_mod_srcs mod_srcs;
  set_exe_path html;
  let* () = js_exe ~mod_srcs ~jss ~o:js b in
  web_exe ~srcs ~js ~o:html b

(* FIXME lots to factorize *)

let node_action build u ~args =
  let err e = Log.err (fun m -> m "%s" e); Fut.return B00_cli.Exit.some_error in
  match B0_unit.get_meta B0_meta.exe_file u with
  | Error e -> err e
  | Ok exe_file ->
      let* exe_file = exe_file in
      let node = Fpath.v "node" in
      match Os.Cmd.get_tool (* FIXME first search in build *) node with
      | Error e -> err e
      | Ok node_exe ->
          let cmd = Cmd.(path node %% path exe_file %% list args) in
          B0_unit.Action.exec_file build u node_exe cmd

let show_uri_action build u ~args =
  let err e = Log.err (fun m -> m "%s" e); Fut.return B00_cli.Exit.some_error in
  match B0_unit.get_meta B0_meta.exe_file u with
  | Error e -> err e
  | Ok exe_file ->
      let* exe_file = exe_file in
      let show_uri = Fpath.v "show-uri" in
      match Os.Cmd.get_tool (* FIXME search in build *) show_uri with
      | Error e -> err e
      | Ok show_uri_exe ->
          let cmd = Cmd.(path show_uri %% path exe_file %% list args) in
          B0_unit.Action.exec_file build u show_uri_exe cmd

let unit_meta ~meta ~name ~mod_srcs ~exe_name ~exe_path =
  meta
  |> B0_meta.tag tag
  |> B0_meta.tag B0_ocaml.tag
  |> B0_meta.tag B0_meta.exe
  |> B0_meta.add B0_meta.exe_name exe_name
  |> B0_meta.add B0_ocaml.Meta.mod_srcs mod_srcs
  |> B0_meta.add B0_meta.exe_file exe_path
  |> B0_meta.add B0_ocaml.Meta.supported_code `Byte
  |> B0_meta.add B0_ocaml.Meta.needs_code `Byte

let exe
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?(action = show_uri_action) ?name exe_name ~srcs
  =
  let name = match name with
  | None -> String.map (function '.' -> '-' | c -> c) exe_name
  | Some name -> name
  in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let meta = unit_meta ~meta ~name ~mod_srcs ~exe_name ~exe_path in
  let proc = wrap (exe_proc set_exe_path set_mod_srcs srcs) in
  B0_unit.v ?doc ~action ~meta name proc

let web
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?(action = show_uri_action) ?name page ~srcs
  =
  let name = Option.value ~default:page name in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let exe_name = page in
  let meta = unit_meta ~meta ~name ~mod_srcs ~exe_name ~exe_path in
  let proc = wrap (web_proc set_exe_path set_mod_srcs srcs) in
  B0_unit.v ?doc ~action ~meta name proc

let () = B0_def.Scope.close ()

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
