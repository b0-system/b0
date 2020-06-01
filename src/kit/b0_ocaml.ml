(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00
open B00_ocaml

let lib = Lib.Name.v

(* Build configuration *)

type built_code = [ `Byte | `Native | `Both ]

let wanted_code = Store.key (fun _ _ -> Fut.return `Auto)
let built_code =
  let of_wanted_code m = function
  | #built_code as v -> v
  | `Auto when Option.is_some (Memo.tool_opt m Tool.ocamlopt) -> `Native
  | `Auto -> `Byte
  in
  let det s m = Fut.map (of_wanted_code m) (Store.get s wanted_code) in
  Store.key det

let auto_comp s m =
  let of_built_code = function
  | `Native | `Both -> Tool.ocamlopt | `Byte -> Tool.ocamlc
  in
  Fut.map of_built_code (Store.get s built_code)

let conf : Tool.Conf.t B00.Store.key =
  let det s m =
    let* comp = auto_comp s m in
    let file = Fpath.(Store.dir s / Memo.mark m) in
    Tool.Conf.write m ~comp ~o:file;
    Tool.Conf.read m file
  in
  Store.key ~mark:"b00_ocaml.conf" det

let version b =
  Fut.map B00_ocaml.Tool.Conf.version (B0_build.get b conf)


module Meta = struct
  let tag =
    B0_meta.Key.tag "ocaml" ~doc:"OCaml related entity"

  let requires =
    let doc = "Required OCaml libraries" in
    let pp_value = Fmt.(box @@ list ~sep:sp Lib.Name.pp) in
    B0_meta.Key.v "ocaml-requires" ~doc ~pp_value

  let library =
    let pp_value = Fmt.using Lib.Name.to_string Fmt.string in
    B0_meta.Key.v "ocaml-library" ~doc:"Defined OCaml library name" ~pp_value

  let mod_srcs = (* FIXME don't do that. *)
    let pp_value = Fmt.any "<n/a>" in
    B0_meta.Key.v "ocaml-srcs" ~doc:"Module sources" ~pp_value
end

module Unit = struct
  type lib_resolver = Lib.Name.t -> Lib.t Fut.t

  let lib_of_unit b ~clib_ext u lib_name =
    let m = B0_build.memo b in
    let name = lib_name in
    let requires = match B0_meta.find Meta.requires (B0_unit.meta u) with
    | Some requires -> requires
    | None ->
        Memo.fail m "Built unit %a has no %a metadata"
          B0_unit.pp_name u B0_meta.Key.pp_name Meta.requires
    in
    let dir = B0_build.Unit.build_dir b u in
    let archive = Lib.Name.to_archive_name lib_name in
    let cma = Some (Fpath.(dir / (archive ^ ".cma"))) in
    let cmxa = Some (Fpath.(dir / (archive ^ ".cmxa"))) in
    let c_archive = Some (Fpath.(dir / (archive ^ clib_ext))) in
    let c_stubs = [] (* FIXME *) in
    B0_build.Unit.require b u;
    let* srcs = match B0_meta.find Meta.mod_srcs (B0_unit.meta u) with
    | Some srcs -> srcs
    | None ->
        Memo.fail m "Built unit %a has no %a metadata"
          B0_unit.pp_name u B0_meta.Key.pp_name Meta.requires
    in
    let cmis, cmxs =
      let rec loop cmis cmxs = function
      | [] -> cmis, cmxs
      | s :: ss ->
          let cmis = Mod.Src.cmi_file ~in_dir:dir s :: cmis in
          let cmxs = match Mod.Src.ml s with
          | None -> cmxs
          | Some _ -> Mod.Src.cmx_file ~in_dir:dir s :: cmxs
          in
          loop cmis cmxs ss
      in
      loop [] [] (Mod.Name.Map.fold (fun _ v acc -> v :: acc) srcs [])
    in
    Fut.return @@
    Lib.v ~name ~requires ~dir ~cmis ~cmxs ~cma ~cmxa ~c_archive ~c_stubs

  let buildable_libs b ~clib_ext : Lib.t Fut.t Lazy.t Lib.Name.Map.t =
    let add u acc = match B0_meta.find Meta.library (B0_unit.meta u) with
    | None -> acc
    | Some lib_name ->
        let lib = lazy (lib_of_unit b ~clib_ext u lib_name) in
        Lib.Name.Map.add lib_name lib acc
    in
    B0_unit.Set.fold add (B0_build.Unit.may_build b) Lib.Name.Map.empty

  let default_lib_resolver store m =
    let* b = Store.get store B0_build.current in
    let* ocaml_conf = B0_build.get b conf in
    let clib_ext = Tool.Conf.lib_ext ocaml_conf in
    let buildable_libs = buildable_libs b ~clib_ext in
    let memo_dir = B0_build.shared_build_dir b in
    let* ocamlpath = Ocamlpath.get m None in
    let r = Lib_resolver.create m ~memo_dir ~ocamlpath ~clib_ext in
    (* FIXME we likely want to plug buildable_libs in the Lib_resolver
       data structure. There's certainly some stuff to be done w.r.t.
       to better error handling, e.g. spell checking. *)
    let lib_resolver n = match Lib.Name.Map.find_opt n buildable_libs with
    | None -> Lib_resolver.find r n
    | Some (lazy (lib)) -> lib
    in
    Fut.return lib_resolver

  let lib_resolver = Store.key ~mark:"b0.ocaml-lib"  default_lib_resolver

  let get_libs b libs =
    let* resolve = B0_build.get b lib_resolver in
    Fut.of_list (List.map resolve libs)

  let get_recursive_libs b libs =
    let* resolve = B0_build.get b lib_resolver in
    let rec loop seen acc = function
    | [] -> Fut.return (seen, acc)
    | l :: ls  ->
        if Lib.Name.Set.mem l seen then loop seen acc ls else
        let seen = Lib.Name.Set.add l seen in
        let* lib = resolve l in
        let not_seen n = not (Lib.Name.Set.mem n seen) in
        let deps = List.filter not_seen (Lib.requires lib) in
        let* seen, acc = loop seen acc deps in
        loop seen (lib :: acc) ls
    in
    let* _, acc = loop Lib.Name.Set.empty [] libs in
    Fut.return (List.rev acc)

  let compile_c_srcs b ~in_dir ~srcs =
    (* XXX Maybe better things could be done here once we have a good C
       domain. *)
    let opts = Cmd.arg "-g" in
    let* ocaml_conf = B0_build.get b conf in
    let obj_ext = Tool.Conf.obj_ext ocaml_conf in
    let m = B0_build.memo b in
    let rec loop os cunits hs = function
    | [] -> List.rev os
    | c :: cs ->
        let cname = Fpath.basename ~no_ext:true c in
        match String.Map.find cname cunits with
        | exception Not_found ->
            let o = Fpath.(in_dir / Fmt.str "%s%s" cname obj_ext) in
            Compile.c_to_o m opts ~reads:hs ~c ~o;
            loop (o :: os) (String.Map.add cname c cunits) hs cs
        | f ->
            B00.Memo.notify m `Warn
              "@[<v>%a:@,File ignored. %s's compilation unit already defined \
               by file:@,%a:@]"
              Fpath.pp_unquoted c cname Fpath.pp_unquoted f;
            loop os cunits hs cs
    in
    let hs = B00_fexts.(find_files (ext ".h") srcs) in
    let cs = B00_fexts.(find_files (ext ".c") srcs) in
    let os = loop [] String.Map.empty hs cs in
    Fut.return os

  let get_mod_srcs ?(only_mlis = false) m ~in_dir ~src_root ~srcs =
    let exts = B00_fexts.v (".mli" :: if only_mlis then [] else [".ml"]) in
    let srcs = B00_fexts.find_files exts srcs in
    let o = Fpath.(in_dir / "ocaml-srcs.deps") in
    Mod.Src.Deps.write m ~src_root ~srcs ~o;
    let* src_deps = Mod.Src.Deps.read m ~src_root o in
    Fut.return (Mod.Src.of_srcs m ~srcs ~src_deps)

  let compile_intf m ~in_dir ~requires ~mod_srcs src =
    match Mod.Src.mli src with
    | None -> ()
    | Some mli ->
        let opts = Cmd.arg "-g" in
        let o = Mod.Src.cmi_file ~in_dir src in
        let deps = Mod.Src.mli_deps src in
        let local_deps, _remain = Mod.Src.find_local_deps mod_srcs deps in
        let add_dep_objs acc dep =
          Mod.Src.as_intf_dep_files ~init:acc ~in_dir dep
        in
        let local_objs = List.fold_left add_dep_objs [] local_deps in
        let ext_objs =
          (* TODO could be more precise *)
          let add_lib acc l = List.rev_append (Lib.cmis l) acc in
          List.fold_left add_lib [] requires
        in
        let reads = List.rev_append local_objs ext_objs in
        Compile.mli_to_cmi m opts ~reads ~mli ~o ~and_cmti:true

  let compile_impl m ~code ~in_dir ~requires ~mod_srcs src =
    match Mod.Src.ml src with
    | None -> ()
    | Some ml ->
        let opts = Cmd.arg "-g" in
        let o = Mod.Src.impl_file ~code ~in_dir src in
        let deps = Mod.Src.ml_deps src in
        let local_deps, _remain = Mod.Src.find_local_deps mod_srcs deps in
        let add_dep_objs acc dep =
          Mod.Src.as_impl_dep_files ~code ~init:acc ~in_dir dep
        in
        let local_objs = List.fold_left add_dep_objs [] local_deps in
        let ext_objs =
          let add_lib acc l =
            List.rev_append (Lib.cmxs l) @@
            List.rev_append (Lib.cmis l) acc
          in
          List.fold_left add_lib [] requires
        in
        let has_cmi, local_objs = match Mod.Src.mli src with
        | None -> false, local_objs
        | Some _ -> true, Mod.Src.cmi_file ~in_dir src :: local_objs
        in
        let reads = List.rev_append ext_objs local_objs in
        Compile.ml_to_impl m ~code opts ~reads ~has_cmi ~ml ~o ~and_cmt:true

  let compile_intfs m ~in_dir ~requires ~mod_srcs =
    let compile _ src = compile_intf m ~mod_srcs ~requires ~in_dir src in
    String.Map.iter compile mod_srcs

  let compile_impls b ~code ~in_dir ~requires ~mod_srcs =
    let compile _ src = compile_impl b ~code ~mod_srcs ~requires ~in_dir src in
    String.Map.iter compile mod_srcs

  let compile_srcs b set_mod_srcs ~code ~in_dir ~src_root ~requires ~srcs =
    let m = B0_build.memo b in
    let* mod_srcs = get_mod_srcs m ~in_dir ~src_root ~srcs in
    set_mod_srcs mod_srcs;
    compile_intfs m ~in_dir ~requires ~mod_srcs;
    compile_impls m ~code ~in_dir ~requires ~mod_srcs;
    let* c_objs = compile_c_srcs b ~in_dir ~srcs in
    let cobj src = match Mod.Src.ml src with
    | None -> None
    | Some _ -> Some (Mod.Src.impl_file ~code ~in_dir src)
    in
    let deps = Mod.Src.ml_deps in
    let mod_srcs = Mod.Src.sort ~deps mod_srcs in
    let cobjs = List.filter_map cobj mod_srcs (* sorted for link *) in
    Fut.return (c_objs, cobjs)

  let exe_proc set_exe_path set_mod_srcs srcs b =
    let* ocaml_conf = Store.get (B0_build.store b) conf in
    let u = B0_build.Unit.current b in
    let build_dir = B0_build.Unit.build_dir b u in
    let src_root = B0_build.Unit.root_dir b u in
    let meta = B0_unit.meta u in
    let requires = B0_meta.get Meta.requires meta in
    let exe_name = B0_meta.get B0_meta.exe_name meta in
    let exe_ext = Tool.Conf.exe_ext ocaml_conf
                  (* FIXME solve that in Link.exe *)
    in
    let code = `Native (* TODO *) in
    let m = B0_build.memo b in
    let* srcs = B0_srcs.select b srcs in
    let* req_libs = get_libs b requires in
    let* (c_objs, cobjs) =
      compile_srcs b set_mod_srcs
        ~code ~in_dir:build_dir ~src_root ~requires:req_libs ~srcs
    in
    let o = Fpath.(build_dir / (exe_name ^ exe_ext)) in
    set_exe_path o;
    let* requires = get_recursive_libs b requires in
    let lib_objs =
      let ar = match code with
      | `Native -> Lib.cmxa
      | `Byte -> Lib.cma
      in
      List.filter_map ar requires
    in
    Link.code m ~code ~c_objs ~cobjs:(lib_objs @ cobjs) ~o;
    Fut.return ()

  let exe ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name exe_name ~srcs =
    let name = Option.value ~default:exe_name name in
    let mod_srcs, set_mod_srcs = Fut.create () in
    let exe_path, set_exe_path = Fut.create () in
    let meta =
      B0_meta.tag Meta.tag @@
      B0_meta.tag B0_meta.exe @@
      B0_meta.add B0_meta.exe_name exe_name @@
      B0_meta.add Meta.requires requires @@
      B0_meta.add Meta.mod_srcs mod_srcs @@
      B0_meta.add B0_meta.exe_path exe_path @@
      meta
    in
    B0_unit.v ?doc ~meta name (exe_proc set_exe_path set_mod_srcs srcs)

  let lib_proc set_mod_srcs srcs b =
    let u = B0_build.Unit.current b in
    let build_dir = B0_build.Unit.build_dir b u in
    let src_root = B0_build.Unit.root_dir b u in
    let meta = B0_unit.meta u in
    let requires = B0_meta.get Meta.requires meta in
    let lib_name = B0_meta.get Meta.library meta in
    let archive_name = Lib.Name.to_archive_name lib_name in
    let code = `Native (* TODO *) in
    let m = B0_build.memo b in
    let* srcs = B0_srcs.select b srcs in
    let* requires = get_libs b requires in
    let* c_objs, cobjs =
      compile_srcs b set_mod_srcs
        ~code ~in_dir:build_dir ~src_root ~requires ~srcs
    in
    let odir = build_dir and oname = archive_name in
    let has_cstubs = c_objs <> [] in
    if has_cstubs then Archive.cstubs m ~c_objs ~odir ~oname;
    Archive.code m ~code ~has_cstubs ~cobjs ~odir ~oname;
    Fut.return ()

  let lib ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name lib_name ~srcs =
    let name = match name with
    | None -> Lib.Name.undot ~rep:'-' lib_name
    | Some name -> name
    in
    let mod_srcs, set_mod_srcs = Fut.create () in
    let meta =
      B0_meta.tag Meta.tag @@
      B0_meta.tag B0_meta.lib @@
      B0_meta.add Meta.library lib_name @@
      B0_meta.add Meta.requires requires @@
      B0_meta.add Meta.mod_srcs mod_srcs @@
      meta
    in
    B0_unit.v ?doc ~meta name (lib_proc set_mod_srcs srcs)
end

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
