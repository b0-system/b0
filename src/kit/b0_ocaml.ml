(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00
open B00_ocaml

let libname = Lib.Name.v

type built_code = [ `Byte | `Native | `All ]
let pp_built_code ppf c = Fmt.string ppf (match c with
| `Byte -> "byte" | `Native -> "native" | `All -> "all")

(* Metadata *)

let tag = B0_meta.Key.tag "ocaml" ~doc:"OCaml related entity"

module Meta = struct
  let requires =
    let doc = "Required OCaml libraries" in
    let pp_value = Fmt.(box @@ list ~sep:sp B00_ocaml.Lib.Name.pp) in
    B0_meta.Key.v "ocaml-requires" ~doc ~pp_value

  let library =
    let pp_value = Fmt.using B00_ocaml.Lib.Name.to_string Fmt.string in
    B0_meta.Key.v "ocaml-library" ~doc:"Defined OCaml library name" ~pp_value

  let mod_srcs = (* FIXME don't do that. *)
    let pp_value = Fmt.any "<n/a>" in
    B0_meta.Key.v "srcs" ~doc:"Module sources" ~pp_value

  let supported_code =
    let pp_value = pp_built_code in
    B0_meta.Key.v "ocaml-supported-code" ~doc:"Supported built code" ~pp_value

  let needs_code =
    let pp_value = pp_built_code in
    B0_meta.Key.v "ocaml-needs-code" ~doc:"Needed built code" ~pp_value
end

(* Build configuration *)

let needed_code s m =
  let find_need u acc =
    let need = B0_unit.find_meta Meta.needs_code u in
    match acc, need with
    | need, None | None, need -> need
    | Some `Byte, Some `Byte -> acc (* jsoo use case *)
    | Some `All, _ -> acc | _, Some `All -> acc
    | Some `Byte, Some `Native -> Some `All
    | Some `Native, Some `Native -> acc
    | Some `Native, Some `Byte -> Some `All
  in
  let* b = Store.get s B0_build.self in
  Fut.return (B0_unit.Set.fold find_need (B0_build.may_build b) None)

let wanted_code = Store.key (fun _ _ -> Fut.return `Auto)
let built_code =
  let of_wanted_code s m = function
  | #built_code as v -> Fut.return v
  | `Auto ->
      let* need = needed_code s m in
      Fut.return @@
      match need with
      | None when Option.is_some (Memo.tool_opt m Tool.ocamlopt) -> `Native
      | None -> `Byte
      | Some need -> need
  in
  let det s m =
    let* wanted = Store.get s wanted_code in
    of_wanted_code s m wanted
  in
  Store.key det

let conf : B00_ocaml.Conf.t B00.Store.key =
  let conf_comp s m =
    let of_built_code = function
    | `Native | `All -> Tool.ocamlopt | `Byte -> Tool.ocamlc
    in
    Fut.map of_built_code (Store.get s built_code)
  in
  let det s m =
    let* comp = conf_comp s m in
    let file = Fpath.(Store.dir s / Memo.mark m) in
    B00_ocaml.Conf.write m ~comp ~o:file;
    B00_ocaml.Conf.read m file
  in
  Store.key ~mark:"ocaml.conf" det

let version b = Fut.map B00_ocaml.Conf.version (B0_build.get b conf)

(* Library resolution *)

let lib_of_unit b ocaml_conf u =
  (* TODO presence of archives should depend on built_code. *)
  B0_build.require b u;
  let m = B0_build.memo b in
  let build_dir = B0_build.build_dir b u in
  let name = B0_unit.get_meta Meta.library u |> Memo.fail_if_error m in
  let requires = B0_unit.get_meta Meta.requires u |> Memo.fail_if_error m in
  let archive = Lib.Name.to_archive_name name in
  let base = Fpath.(build_dir / archive) in
  let cma = Some Fpath.(base + ".cma") in
  let cmxa = Some Fpath.(base + ".cmxa") in
  let c_archive = Some Fpath.(base + (B00_ocaml.Conf.lib_ext ocaml_conf)) in
  let c_stubs = [] (* FIXME *) in
  let* srcs = B0_unit.get_meta Meta.mod_srcs u |> Memo.fail_if_error m in
  let cmis, cmxs =
    let rec loop cmis cmxs = function
    | [] -> cmis, cmxs
    | s :: ss ->
        let cmis = Mod.Src.cmi_file s :: cmis in
        let cmxs = match Mod.Src.cmx_file s with
        | None -> cmxs | Some cmx -> cmx :: cmxs
        in
        loop cmis cmxs ss
    in
    loop [] [] (Mod.Name.Map.fold (fun _ v acc -> v :: acc) srcs [])
  in
  Fut.return @@
  Some (Lib.v ~name ~requires ~dir:build_dir ~cmis ~cmxs ~cma ~cmxa ~c_archive
          ~c_stubs)

let libs_in_build
    b ~conf : (B0_unit.t * (Lib.t option Fut.t Lazy.t)) Lib.Name.Map.t
  =
  let add u acc = match B0_unit.find_meta Meta.library u with
  | None -> acc
  | Some lib_name ->
      match Lib.Name.Map.find_opt lib_name acc with
      | None ->
          let lib = lazy (lib_of_unit b conf u) in
          Lib.Name.Map.add lib_name (u, lib) acc
      | Some (lib_u, _) ->
          Memo.notify (B0_build.memo b)
            `Warn "@[OCaml library %a already defined in unit %a.@,\
                   Ignoring definition in unit %a@]"
            Lib.Name.pp lib_name B0_unit.pp_name lib_u B0_unit.pp_name u;
          acc
  in
  B0_unit.Set.fold add (B0_build.may_build b) Lib.Name.Map.empty

let lib_resolver_build_scope b conf =
  let name = "build" in
  let libs_in_build = libs_in_build b ~conf in
  let find ocaml_conf m n = match Lib.Name.Map.find_opt n libs_in_build with
  | None -> Fut.return None
  | Some (_, lazy lib) -> lib
  in
  let suggest ocaml_conf m n = Fut.return None in
  Lib.Resolver.scope ~name ~find ~suggest

let default_lib_resolver store m =
  let* b = Store.get store B0_build.self in
  let* ocaml_conf = B0_build.get b conf in
  let build_scope = lib_resolver_build_scope b ocaml_conf in
  let* ocamlpath = Ocamlpath.get m None in
  let cache_dir = Fpath.(B0_build.shared_build_dir b / "ocamlib") in
  let ocamlpath = Lib.Resolver.ocamlpath ~cache_dir ~ocamlpath in
  let ocamlfind = Lib.Resolver.ocamlfind ~cache_dir in
  Fut.return @@
  Lib.Resolver.create m ocaml_conf [build_scope; ocamlpath; ocamlfind]

let lib_resolver = Store.key ~mark:"b0.ocamlib"  default_lib_resolver

(* Compile *)

let compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs =
  (* XXX Maybe better things could be done here once we have a good C domain. *)
  let obj_ext = B00_ocaml.Conf.obj_ext conf in
  let rec loop os cunits hs = function
  | [] -> List.rev os
  | c :: cs ->
      let cname = Fpath.basename ~no_ext:true c in
      match String.Map.find cname cunits with
      | exception Not_found ->
          let o = Fpath.(build_dir / Fmt.str "%s%s" cname obj_ext) in
          Compile.c_to_o m ~comp ~opts ~reads:hs ~c ~o;
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

let unit_code b m meta =
  let* built_code = B0_build.get b built_code in
  let _supported_code = B0_meta.find Meta.supported_code meta in
  let _needs_code = B0_meta.find Meta.needs_code meta in
  (* TODO *)
  Fut.return built_code

let exe_proc set_exe_path set_mod_srcs srcs b =
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_root_dir b in
  let* srcs = B0_srcs.select b srcs in
  let* mod_srcs = Mod.Src.map_of_files m ~build_dir ~src_root ~srcs in
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get Meta.requires meta in  set_mod_srcs mod_srcs;
  let* unit_code = unit_code b m meta in
  let* conf = B0_build.get b conf in
  let* resolver = B0_build.get b lib_resolver in
  let* comp_requires = Lib.Resolver.get_list resolver requires in
  let exe_name = B0_meta.get B0_meta.exe_name meta in
  let exe_ext = B00_ocaml.Conf.exe_ext conf in
  let opts = Cmd.(arg "-g") (* TODO *) in
  let o = Fpath.(build_dir / (exe_name ^ exe_ext)) in
  set_exe_path o;  (* FIXME introduce a general mecanism for that *)
  let code = match unit_code with `All | `Native -> `Native |`Byte -> `Byte in
  let all_code = match unit_code with `All -> true | _ -> false in
  let comp = match unit_code with
  | `Native | `All -> Tool.ocamlopt | `Byte -> Tool.ocamlc
  in
  compile_intfs ~and_cmti:true m ~comp ~opts ~requires:comp_requires ~mod_srcs;
  compile_impls ~and_cmt:true m ~code ~opts ~requires:comp_requires ~mod_srcs;
  if all_code then begin
    compile_impls
      ~and_cmt:false m ~code:`Byte ~opts ~requires:comp_requires ~mod_srcs;
  end;
  let* c_objs = compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs in
  let mod_srcs = Mod.Src.sort (* for link *) ~deps:Mod.Src.ml_deps mod_srcs in
  let* link_requires = Lib.Resolver.get_list_and_deps resolver requires in
  let archive ~code = match code with `Native -> Lib.cmxa | `Byte -> Lib.cma in
  let lib_objs = List.filter_map (archive ~code) link_requires in
  let cobjs = List.filter_map (Mod.Src.impl_file ~code) mod_srcs  in
  Link.code m ~conf ~code ~opts ~c_objs ~cobjs:(lib_objs @ cobjs) ~o;
  if all_code then begin
    let o = Fpath.(build_dir / (exe_name ^ ".byte" ^ exe_ext)) in
    let lib_objs = List.filter_map (archive ~code:`Byte) link_requires in
    let cobjs = List.filter_map (Mod.Src.impl_file ~code:`Byte) mod_srcs in
    Link.code m ~conf ~code:`Byte ~opts ~c_objs ~cobjs:(lib_objs @ cobjs) ~o
  end;
  Fut.return ()

let lib_proc set_mod_srcs srcs b =
  (* XXX we are still missing cmxs here *)
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_root_dir b in
  let* srcs = B0_srcs.select b srcs in
  let* mod_srcs = Mod.Src.map_of_files m ~build_dir ~src_root ~srcs in
  set_mod_srcs mod_srcs;
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get Meta.requires meta in
  let library = B0_meta.get Meta.library meta in
  let archive_name = Lib.Name.to_archive_name library in
  let opts = Cmd.(arg "-g") (* TODO *) in
  let* built_code = B0_build.get b built_code in
  let* conf = B0_build.get b conf in
  let* resolver = B0_build.get b lib_resolver in
  let* requires = Lib.Resolver.get_list resolver requires in
  let code = match built_code with `All | `Native -> `Native |`Byte -> `Byte in
  let all_code = match built_code with `All -> true | _ -> false in
  let comp = match built_code with
  | `Native | `All -> Tool.ocamlopt | `Byte -> Tool.ocamlc
  in
  compile_intfs ~and_cmti:true m ~comp ~opts ~requires ~mod_srcs;
  compile_impls ~and_cmt:true m ~code ~opts ~requires ~mod_srcs;
  if all_code
  then (compile_impls ~and_cmt:true m ~code:`Byte ~opts ~requires ~mod_srcs);
  let* c_objs = compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs in
  let mod_srcs = Mod.Src.sort (* for link *) ~deps:Mod.Src.ml_deps mod_srcs in
  let cobjs = List.filter_map (Mod.Src.impl_file ~code) mod_srcs  in
  let odir = build_dir and oname = archive_name in
  let has_cstubs = c_objs <> [] in
  if has_cstubs then Archive.cstubs m ~conf ~opts ~c_objs ~odir ~oname;
  Archive.code m ~conf ~code ~opts ~has_cstubs ~cobjs ~odir ~oname;
  if all_code then begin
    let cobjs = List.filter_map (Mod.Src.impl_file ~code:`Byte) mod_srcs in
    Archive.code m ~conf ~code:`Byte ~opts ~has_cstubs ~cobjs ~odir ~oname
  end;
  Fut.return ()

let exe ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name exe_name ~srcs =
  let name = Option.value ~default:exe_name name in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let meta =
    meta
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.exe
    |> B0_meta.add B0_meta.exe_name exe_name
    |> B0_meta.add Meta.requires requires
    |> B0_meta.add Meta.mod_srcs mod_srcs
    |> B0_meta.add B0_meta.exe_path exe_path
  in
  B0_unit.v ?doc ~meta name (exe_proc set_exe_path set_mod_srcs srcs)

let lib ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name lib_name ~srcs =
  let name = match name with
  | None -> Lib.Name.undot ~rep:'-' lib_name
  | Some name -> name
  in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let meta =
    meta
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.lib
    |> B0_meta.add Meta.library lib_name
    |> B0_meta.add Meta.requires requires
    |> B0_meta.add Meta.mod_srcs mod_srcs
  in
  B0_unit.v ?doc ~meta name (lib_proc set_mod_srcs srcs)

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
