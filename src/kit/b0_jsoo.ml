(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

let () = B0_def.Scope.open_lib "jsoo"

let env_vars = ["BUILD_PATH_PREFIX_MAP"]
let tool = B0_memo.Tool.by_name ~vars:env_vars "js_of_ocaml"

(* Meta keys *)

let tag = B0_meta.Key.make_tag "jsoo" ~doc:"js_of_ocaml related entity"

let assets_root =
  let doc = "Root path from which assets are rerooted." in
  let pp_value = Fpath.pp_unquoted in
  B0_meta.Key.make "jsoo-assets-root" ~pp_value ~doc

type compilation_mode = [ `Separate | `Whole ]
let pp_compilation_mode ppf = function
| `Separate -> Fmt.string ppf "separate" | `Whole -> Fmt.string ppf "whole"

let compilation_mode =
  let doc = "Compilation mode" in
  let default = `Whole and pp_value = pp_compilation_mode in
  B0_meta.Key.make "jsoo-comp-mode" ~default ~pp_value ~doc

let compile_opts =
  let doc = "Options added to the js_of_ocaml compile command" in
  let pp_value = Cmd.pp in
  B0_meta.Key.make "jsoo-comp" ~default:Cmd.empty ~pp_value ~doc

let link_opts =
  let doc = "Options added to the js_of_ocaml link command" in
  let pp_value = Cmd.pp in
  B0_meta.Key.make "jsoo-link" ~default:Cmd.empty ~pp_value ~doc

type source_map = [`Inline | `File ] option
let pp_source_map ppf = function
| None -> Fmt.string ppf "none"
| Some `Inline -> Fmt.string ppf "inline"
| Some `File -> Fmt.string ppf "file"

let source_map =
  let pp_value = pp_source_map in
  B0_meta.Key.make "jsoo-src-map" ~pp_value ~doc:"Source map desires"

let toplevel =
  let doc = "Compile with toplevel support" in
  let pp_value = Fmt.bool in
  B0_meta.Key.make "jsoo-toplevel" ~default:false ~doc ~pp_value

(* Build fragments *)

let build_runtime m ~opts ~jss ~o =
  let jsoo = B0_memo.tool m tool in
  B0_memo.spawn m ~reads:jss ~writes:[o] @@
  jsoo Cmd.(arg "build-runtime" % "-o" %% (unstamp @@ path o) %% opts %%
            unstamp (paths jss))

let handle_source_map ~o = function
| None -> [o], Cmd.empty
| Some `Inline -> [o], Cmd.(arg "--source-map-inline")
| Some `File -> [o; Fpath.(o -+ ".map")], Cmd.(arg "--source-map")

let compile m ~opts ~source_map ~jss ~byte ~o =
  let jsoo = B0_memo.tool m tool in
  let writes, source_map = handle_source_map ~o source_map in
  B0_memo.spawn m ~reads:(byte :: jss) ~writes @@
  jsoo Cmd.(arg "compile" % "-o" %% (unstamp @@ path o) %% opts %%
            source_map %% (unstamp @@ paths jss %% path byte))

let link m ~opts ~source_map ~jss ~o =
  let jsoo = B0_memo.tool m tool in
  let writes, source_map = handle_source_map ~o source_map in
  B0_memo.spawn m ~reads:jss ~writes @@
  jsoo Cmd.(arg "link" % "-o" %% (unstamp @@ path o) %% opts %% source_map %%
            (unstamp @@ paths jss))

let write_page
    ?(lang = "") ?(generator = "") ?(styles = [])  ?(scripts = [])
    ?(title = "") m ~o
  =
  let title = if title = "" then Fpath.basename ~no_ext:true o else title in
  let stamp = List.rev_append styles scripts in
  let stamp = String.concat "" (lang :: generator :: title :: stamp) in
  B0_memo.write m ~stamp o @@ fun () ->
  let open B0_html in
  let body =
    let sorry = "Sorry, you need to enable JavaScript to see this page." in
    El.body El.[noscript [txt sorry]]
  in
  let page = El.basic_page ~generator ~lang ~scripts ~styles ~title body in
  Ok (El.to_string ~doc_type:true page)

(* Build fragments *)

let get_modsrcs b ~srcs =
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_scope_dir b in
  B0_ocaml.Modsrc.map_of_files (B0_build.memo b) ~build_dir ~src_root ~srcs

let get_link_objs ~code ~resolver ~requires ~modsrcs =
  let modsrcs =
    B0_ocaml.Modsrc.sort (* for link *) ~deps:B0_ocaml.Modsrc.ml_deps modsrcs in
  let mod_objs = List.filter_map (B0_ocaml.Modsrc.impl_file ~code) modsrcs  in
  let* link_requires =
    B0_ocaml.Libresolver.get_list_and_deps resolver requires
  in
  let lib_objs = List.filter_map B0_ocaml.Lib.cma link_requires in
  let lib_jss = List.concat_map B0_ocaml.Lib.js_stubs link_requires in
  Fut.return (lib_objs, mod_objs, lib_jss)

let compile_byte m ~opts ~resolver ~requires ~modsrcs =
  let code = `Byte in
  let comp = B0_ocaml.Tool.ocamlc in
  let* requires = B0_ocaml.Libresolver.get_list resolver requires in
  B0_ocaml.Compile.intfs ~and_cmti:true m ~comp ~opts ~requires ~modsrcs;
  B0_ocaml.Compile.impls ~and_cmt:true m ~code ~opts ~requires ~modsrcs;
  Fut.return ()

let link_byte m ~conf ~opts ~resolver ~requires ~modsrcs ~o =
  let code = `Byte in
  let* lib_objs, mod_objs, lib_jss =
    get_link_objs ~code ~resolver ~requires ~modsrcs
  in
  let cobjs = lib_objs @ mod_objs in
  B0_ocaml.Link.code m ~conf ~code ~opts ~c_objs:[] ~cobjs ~o;
  Fut.return lib_jss

let byte_exe ~modsrcs ~o b =
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get B0_ocaml.requires meta in
  let* conf = B0_build.get b B0_ocaml.Conf.key in
  let o = Fpath.(o + B0_ocaml.Conf.exe_ext conf) in
  let* resolver = B0_build.get b B0_ocaml.Libresolver.key in
  let toplevel = Option.value ~default:false (B0_meta.find toplevel meta) in
  let global_opts = Cmd.(arg "-g") (* TODO *) in
  let opts = global_opts in
  let m = B0_build.memo b in
  let* () = compile_byte m ~opts ~resolver ~requires ~modsrcs in
  let opts = Cmd.(global_opts %% if' toplevel (arg "-linkall")) in
  let* lib_jss = link_byte m ~conf ~opts ~resolver ~requires ~modsrcs ~o in
  Fut.return (o, lib_jss)

let js_of_byte_exe ~jss ~modsrcs ~o b =
  let* byte, lib_jss = byte_exe ~modsrcs ~o:Fpath.(o -+ ".byte") b in
  let meta = B0_build.current_meta b in
  let source_map = Option.join (B0_meta.find source_map meta) in
  let opts = Option.value ~default:Cmd.empty (B0_meta.find compile_opts meta) in
  let toplevel = Option.value ~default:false (B0_meta.find toplevel meta) in
  let opts = if toplevel then Cmd.(opts % "--toplevel") else opts in
  let jss = List.append lib_jss jss in
  compile (B0_build.memo b) ~opts ~source_map ~jss ~byte ~o;
  Fut.return ()

let js_of_byte_objs ~jss ~modsrcs ~o b =
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get B0_ocaml.requires meta in
  let* conf = B0_build.get b B0_ocaml.Conf.key in
  let* resolver = B0_build.get b B0_ocaml.Libresolver.key in
  let m = B0_build.memo b in
  let global_opts = Cmd.(arg "-g") (* TODO *) in
  let opts = global_opts in
  let* () = compile_byte m ~opts ~resolver ~requires ~modsrcs in
  let code = `Byte in
  let* lib_objs, mod_objs, lib_jss =
    get_link_objs ~code ~resolver ~requires ~modsrcs
  in
  let jss = List.append lib_jss jss in
  let source_map = Option.join (B0_meta.find source_map meta) in
  let toplevel = Option.value ~default:false (B0_meta.find toplevel meta) in
  let ocamlrt_js =
    let opts = Cmd.empty in
    let build_dir = B0_build.current_build_dir b in
    let o = Fpath.(build_dir / "ocamlrt.js") in
    build_runtime m ~opts ~jss ~o;
    o
  in
  let opts = B0_meta.find_or_default compile_opts meta in
  let opts = if toplevel then Cmd.(opts % "--toplevel") else opts in
  let lib_jss, std_exit_js =
    (* We need a cache similar to B0_ocaml.Libresolver for jsing libs.
       The ops will be cached across units but not the file writes which
       we should do in the shared dir *)
    let build_dir = B0_build.current_build_dir b in
    let compile obj =
      (* FIXME this won't work with lib convention we need to
         remember the lib_name and mangle. *)
      let o = Fpath.(build_dir / (Fpath.basename obj ^ ".js")) in
      let opts = Cmd.(opts %% arg "-I" %% path (Fpath.parent obj)) in
      compile m ~opts ~source_map ~jss:[] ~byte:obj ~o;
      o
    in
    let compile_lib acc obj = compile obj :: acc  in
    let jss = List.rev (List.fold_left compile_lib [] lib_objs) in
    (* FIXME at least stdlib should be looked up via resolver *)
    let stdlib_cma = Fpath.(B0_ocaml.Conf.where conf / "stdlib.cma") in
    let stdlib_js = B0_memo.file_ready m stdlib_cma; compile stdlib_cma in
    let std_exit_cmo = Fpath.(B0_ocaml.Conf.where conf / "std_exit.cmo") in
    let std_exit_js = B0_memo.file_ready m std_exit_cmo; compile std_exit_cmo in
    stdlib_js :: jss, std_exit_js
  in
  let mod_obj_jss =
    let compile_obj acc obj =
      let o = Fpath.(obj + ".js") in
      compile m ~opts ~source_map ~jss:[] ~byte:obj ~o;
      o :: acc
    in
    List.rev (List.fold_left compile_obj [] mod_objs)
  in
  let jss = ocamlrt_js :: (lib_jss @ mod_obj_jss @ [std_exit_js]) in
  let opts = B0_meta.find_or_default link_opts meta in
  link m ~opts ~source_map ~jss ~o;
  Fut.return ()

let js_exe ~jss ~modsrcs ~o b =
  let comp_mode = B0_meta.find compilation_mode (B0_build.current_meta b) in
  match Option.value ~default:`Whole comp_mode with
  | `Whole -> js_of_byte_exe ~jss ~modsrcs ~o b
  | `Separate -> js_of_byte_objs ~jss ~modsrcs ~o b

(* XXX the assets root churn may indicate it would be
   better to have that in B0_srcs. (or the deploy stuff) *)

let build_setup ~srcs b = (* return a record maybe ? *)
  let* srcs = B0_srcs.(Fut.map by_ext @@ select b srcs) in
  let* modsrcs = get_modsrcs b ~srcs in
  (* FIXME the lookup should be ordered here: *)
  let jss = B0_file_exts.find_files B0_file_exts.js srcs in
  let build_dir = B0_build.current_build_dir b in
  let tool_name = B0_meta.get B0_unit.tool_name (B0_build.current_meta b) in
  let js = Fpath.(build_dir / tool_name) in
  let html = Fpath.(js -+ ".html") in
  let exe_html = Fpath.basename html in
  let htmls = B0_file_exts.find_files B0_file_exts.html srcs in
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
  Fut.return (srcs, modsrcs, jss, js, html)

(* Build js executables *)

let exe_proc set_exe_path set_modsrcs srcs b =
  let* _srcs, modsrcs, jss, js, _html = build_setup ~srcs b in
  set_modsrcs modsrcs;
  set_exe_path js;
  js_exe ~modsrcs ~jss ~o:js b

(* Build html webs *)

let copy_assets m srcs ~exts ~assets_root ~dst =
  let assets = B0_file_exts.find_files exts srcs in
  let copy acc src =
    let dst = match assets_root with
    | Some r when Fpath.is_prefix r src -> Fpath.reroot ~root:r ~dst src
    | _ -> Fpath.(dst / Fpath.basename src)
    in
    B0_memo.copy m ~src dst;
    Fpath.Set.add dst acc
  in
  List.iter (B0_memo.file_ready m) assets;
  List.fold_left copy Fpath.Set.empty assets

let web_exe ~srcs ~js ~o b =
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let assets_root =
    match B0_meta.find assets_root (B0_build.current_meta b) with
    | None -> None
    | Some r -> Some (Fpath.(B0_build.current_scope_dir b // r))
  in
  let exts = String.Set.remove ".js" B0_file_exts.www in
  let assets = copy_assets m srcs ~exts ~assets_root ~dst:build_dir in
  if Fpath.Set.mem o assets then Fut.return () else
  let css = Fpath.Set.filter (Fpath.has_ext ".css") assets in
  let base f = Fpath.to_string (Option.get (Fpath.strip_prefix build_dir f)) in
  let styles = List.map base (Fpath.Set.elements css) in
  write_page m ~styles ~scripts:[Fpath.basename js] ~o;
  Fut.return ()

let web_proc set_exe_path set_modsrcs srcs b =
  let* srcs, modsrcs, jss, js, html = build_setup ~srcs b in
  (* FIXME review all this. It's a mess. web_proc is called
     by web which sets the executable to the page name. build_setup
     use it to define `js` which means it has no extension. So we readd
     it here *)
  let js = Fpath.(js + ".js") in
  set_modsrcs modsrcs;
  set_exe_path html;
  let* () = js_exe ~modsrcs ~jss ~o:js b in
  web_exe ~srcs ~js ~o:html b

(* FIXME lots to factorize *)

(*
let node_action build u ~args =
  let err e = Log.err (fun m -> m "%s" e); Fut.return B0_cli.Exit.some_error in
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
   *)

let show_uri_action build ?env ?cwd u ~args =
  let err e = Log.err (fun m -> m "%s" e); Fut.return B0_cli.Exit.some_error in
  match B0_unit.get_meta B0_unit.exe_file u with
  | Error e -> err e
  | Ok exe_file ->
      let* exe_file = exe_file in
      let show_uri = Fpath.v "show-uri" in
      match Os.Cmd.get_tool (* FIXME search in build *) show_uri with
      | Error e -> err e
      | Ok show_uri_exe ->
          let env = Option.map Os.Env.to_assignments env in
          let cmd = Cmd.(path show_uri_exe %% path exe_file %% args) in
          Fut.return (Os.Exit.exec ?env ?cwd show_uri_exe cmd)

let meta_base
    ~name ~modsrcs ~tool_name ~public ~requires ~assets_root:ar ~exe_path
  =
  B0_meta.empty
  |> B0_meta.tag tag
  |> B0_meta.add_some assets_root ar
  |> B0_meta.add B0_meta.public public
  |> B0_meta.tag B0_ocaml.tag
  |> B0_meta.add_some_or_default B0_ocaml.requires requires
  |> B0_meta.add B0_ocaml.modsrcs modsrcs
  |> B0_meta.add B0_ocaml.Code.supported `Byte
  |> B0_meta.add B0_ocaml.Code.needs `Byte
  |> B0_meta.tag B0_meta.exe
  |> B0_meta.add B0_unit.tool_name tool_name
  |> B0_meta.add B0_unit.exe_file exe_path
  |> B0_meta.add_if_undef B0_unit.exec ("show-uri", show_uri_action)

let exe
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?assets_root ?requires
    ?(public = false) ?name tool_name ~srcs
  =
  let name = match name with
  | None -> String.map (function '.' -> '-' | c -> c) tool_name
  | Some name -> name
  in
  let modsrcs, set_modsrcs = Fut.make () in
  let exe_path, set_exe_path = Fut.make () in
  let base =
    meta_base
      ~name ~modsrcs ~tool_name ~public ~requires ~assets_root ~exe_path
  in
  let meta = B0_meta.override base ~by:meta in
  let proc = wrap (exe_proc set_exe_path set_modsrcs srcs) in
  B0_unit.make ?doc ~meta name proc

let web
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?assets_root ?requires  ?(public = false) ?name page ~srcs
  =
  let name = Option.value ~default:page name in
  let modsrcs, set_modsrcs = Fut.make () in
  let exe_path, set_exe_path = Fut.make () in
  let tool_name = page in
  let base =
    meta_base ~name ~modsrcs ~tool_name ~requires ~assets_root
      ~public ~exe_path
  in
  let meta = B0_meta.override base ~by:meta in
  let proc = wrap (web_proc set_exe_path set_modsrcs srcs) in
  B0_unit.make ?doc ~meta name proc

let () = B0_def.Scope.close ()
