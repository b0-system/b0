(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00
open B00_ocaml

let lib = Lib.Name.v

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
end

module Unit = struct
  type lib_resolver = Lib.Name.t -> Lib.t Memo.fiber

  let buildable_libs b : Lib.t Lazy.t B00_ocaml.Lib.Name.Map.t =
    let lib_of_unit b u lib_name =
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
      let lib = Lib.v ~installed:false ~archive m ~name ~requires ~dir in
      B0_build.Unit.require b u;
      lib
    in
    let add u acc = match B0_meta.find Meta.library (B0_unit.meta u) with
    | None -> acc
    | Some lib_name ->
        let lib = lazy (lib_of_unit b u lib_name) in
        B00_ocaml.Lib.Name.Map.add lib_name lib acc
    in
    B0_unit.Set.fold add (B0_build.Unit.may_build b)
      B00_ocaml.Lib.Name.Map.empty

  let default_lib_resolver store m k =
    Store.get store B0_build.current @@ fun b ->
    let buildable_libs = buildable_libs b in
    let memo_dir = B0_build.shared_build_dir b in
    B00_ocaml.Ocamlpath.get m None @@ fun ocamlpath ->
    let resolver = B00_ocaml.Lib_resolver.create m ~memo_dir ~ocamlpath in
    (* FIXME we likely want to plug buildable_libs in the Lib_resolver
       data structure. There's certainly some stuff to be done w.r.t.
       to better error handling, e.g. spell checking. *)
    let lib_resolver n k =
      match B00_ocaml.Lib.Name.Map.find_opt n buildable_libs with
      | Some (lazy (lib)) -> k lib
      | None -> B00_ocaml.Lib_resolver.find resolver n k
    in
    k lib_resolver

  let lib_resolver = Store.key ~mark:"b0.ocaml-lib"  default_lib_resolver

  let get_libs b libs k =
    B0_build.get b lib_resolver @@ fun resolve ->
    Memo.Fiber.of_list (List.map resolve libs) k

  let get_recursive_libs b libs k =
    B0_build.get b lib_resolver @@ fun resolve ->
    let rec loop seen acc ls k = match ls with
    | [] -> k (seen, acc)
    | l :: ls  ->
        if Lib.Name.Set.mem l seen then loop seen acc ls k else
        let seen = Lib.Name.Set.add l seen in
        resolve l @@ fun lib ->
        let not_seen n = not (Lib.Name.Set.mem n seen) in
        let deps = List.filter not_seen (Lib.requires lib) in
        loop seen acc deps @@ fun (seen, acc) ->
        loop seen (lib :: acc) ls k
    in
    loop Lib.Name.Set.empty [] libs @@ fun (_, acc) ->
    k (List.rev acc)

  let sync_built_requires m ~code ~requires k =
    (* FIXME we want more fine grained syncing for parallelism.
       cmi and archive. We need something in Build.t per unit maybe
       a fut dict. *)
    let built l = not (Lib.installed l) in
    let built_libs = List.filter built requires in
    let to_sync = List.map (Lib.archive ~code) built_libs in
    Memo.wait_files m to_sync k

  let compile_c_srcs b ~in_dir ~srcs k =
    (* XXX Maybe better things could be done here once we have a good C
       domain. *)
    B0_build.get b B00_ocaml.Conf.key @@ fun ocaml_conf ->
    let obj_ext = B00_ocaml.Conf.obj_ext ocaml_conf in
    let m = B0_build.memo b in
    let rec loop os cunits hs = function
    | [] -> List.rev os
    | c :: cs ->
        let cname = Fpath.basename ~no_ext:true c in
        match String.Map.find cname cunits with
        | exception Not_found ->
            let o = Fpath.(in_dir / Fmt.str "%s%s" cname obj_ext) in
            B00_ocaml.Compile.c_to_o m ~hs ~c ~o;
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
    k (loop [] String.Map.empty hs cs)

  let get_mod_srcs ?(only_mlis = false) m ~in_dir ~src_root ~srcs k =
    let exts = B00_fexts.v (".mli" :: if only_mlis then [] else [".ml"]) in
    let srcs = B00_fexts.find_files exts srcs in
    let o = Fpath.(in_dir / "ocaml-srcs.deps") in
    B00_ocaml.Mod_src.Deps.write m ~src_root ~srcs ~o;
    B00_ocaml.Mod_src.Deps.read m ~src_root o @@ fun src_deps ->
    k (B00_ocaml.Mod_src.of_srcs m ~srcs ~src_deps)

  let compile_intf m ~in_dir ~requires ~mod_srcs src =
    match Mod_src.mli src with
    | None -> ()
    | Some mli ->
        let o = Mod_src.cmi_file ~in_dir src in
        let deps = Mod_src.mli_deps src in
        let local_deps, _remain = Mod_src.find_local_deps mod_srcs deps in
        let add_dep_objs acc dep =
          Mod_src.as_intf_dep_files ~init:acc ~in_dir dep
        in
        let local_objs = List.fold_left add_dep_objs [] local_deps in
        let ext_objs =
          (* TODO could be more precise *)
          let add_lib acc l = List.rev_append (Lib.cmis l) acc in
          List.fold_left add_lib [] requires
        in
        let reads = List.rev_append local_objs ext_objs in
        Compile.mli_to_cmi m ~reads ~mli ~o

  let compile_impl m ~code ~in_dir ~requires ~mod_srcs src =
    match Mod_src.ml src with
    | None -> ()
    | Some ml ->
        let o = Mod_src.impl_file ~code ~in_dir src in
        let deps = Mod_src.ml_deps src in
        let local_deps, _remain = Mod_src.find_local_deps mod_srcs deps in
        let add_dep_objs acc dep =
          Mod_src.as_impl_dep_files ~code ~init:acc ~in_dir dep
        in
        let local_objs = List.fold_left add_dep_objs [] local_deps in
        let ext_objs =
          (* FIXME add cmx *)
          let add_lib acc l = List.rev_append (Lib.cmis l) acc in
          List.fold_left add_lib [] requires
        in
        let has_cmi, local_objs = match Mod_src.mli src with
         | None -> false, local_objs
        | Some _ -> true, Mod_src.cmi_file ~in_dir src :: local_objs
        in
        let reads = List.rev_append ext_objs local_objs in
        Compile.ml_to_impl m ~code ~has_cmi ~reads ~ml ~o

  let compile_intfs m ~in_dir ~requires ~mod_srcs =
    let compile _ src = compile_intf m ~mod_srcs ~requires ~in_dir src in
    String.Map.iter compile mod_srcs

  let compile_impls b ~code ~in_dir ~requires ~mod_srcs =
    let compile _ src = compile_impl b ~code ~mod_srcs ~requires ~in_dir src in
    String.Map.iter compile mod_srcs

  let compile_srcs b ~code ~in_dir ~src_root ~requires ~srcs k =
    let m = B0_build.memo b in
    compile_c_srcs b ~in_dir ~srcs @@ fun c_objs ->
    get_mod_srcs m ~in_dir ~src_root ~srcs @@ fun mod_srcs ->
    sync_built_requires m ~code ~requires @@ fun () ->
    compile_intfs m ~in_dir ~requires ~mod_srcs;
    compile_impls m ~code ~in_dir ~requires ~mod_srcs;
    let cobj src = match Mod_src.ml src with
    | None -> None
    | Some _ -> Some (Mod_src.impl_file ~code ~in_dir src)
    in
    let deps = B00_ocaml.Mod_src.ml_deps in
    let mod_srcs = B00_ocaml.Mod_src.sort ~deps mod_srcs in
    let cobjs = List.filter_map cobj mod_srcs (* sorted for link *) in
    k (c_objs, cobjs)

  let exe_proc srcs b k =
    Store.get (B0_build.store b) B00_ocaml.Conf.key @@ fun ocaml_conf ->
    let u = B0_build.Unit.current b in
    let build_dir = B0_build.Unit.build_dir b u in
    let src_root = B0_build.Unit.root_dir b u in
    let meta = B0_unit.meta u in
    let requires = B0_meta.get Meta.requires meta in
    let exe_name = B0_meta.get B0_meta.exe_name meta in
    let exe_ext = B00_ocaml.Conf.exe_ext ocaml_conf
                  (* FIXME solve that in Link.exe *)
    in
    let code = B00_ocaml.Cobj.Native (* TODO *) in
    let m = B0_build.memo b in
    B0_srcs.select b srcs @@ fun srcs ->
    get_libs b requires @@ fun req_libs ->
    compile_srcs b ~code ~in_dir:build_dir ~src_root ~requires:req_libs ~srcs @@
    fun (c_objs, cobjs) ->
    let o = Fpath.(build_dir / (exe_name ^ exe_ext)) in
    get_recursive_libs b requires @@ fun requires ->
    let lib_objs = List.map (Lib.archive ~code) requires in
    Link.exe m ~code ~c_objs ~cobjs:(lib_objs @ cobjs) ~o;
    k ()

  let exe ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name exe_name ~srcs =
    let name = Option.value ~default:exe_name name in
    let meta =
      B0_meta.tag Meta.tag @@
      B0_meta.tag B0_meta.exe @@
      B0_meta.add B0_meta.exe_name exe_name @@
      B0_meta.add Meta.requires requires @@
      meta
    in
    B0_unit.v ?doc ~meta name (exe_proc srcs)

  let lib_proc srcs b k =
    let u = B0_build.Unit.current b in
    let build_dir = B0_build.Unit.build_dir b u in
    let src_root = B0_build.Unit.root_dir b u in
    let meta = B0_unit.meta u in
    let requires = B0_meta.get Meta.requires meta in
    let lib_name = B0_meta.get Meta.library meta in
    let archive_name = B00_ocaml.Lib.Name.to_archive_name lib_name in
    let code = B00_ocaml.Cobj.Native (* TODO *) in
    let m = B0_build.memo b in
    B0_srcs.select b srcs @@ fun srcs ->
    get_libs b requires @@ fun requires ->
    compile_srcs b ~code ~in_dir:build_dir ~src_root ~requires ~srcs @@
    fun (c_objs, cobjs) ->
    let odir = build_dir and oname = archive_name in
    let has_cstubs = c_objs <> [] in
    if has_cstubs then Compile.cstubs_archives m ~c_objs ~odir ~oname;
    Compile.archive m ~code ~has_cstubs ~cobjs ~odir ~oname;
    k ()

  let lib ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name lib_name ~srcs =
    let name = match name with
    | None -> B00_ocaml.Lib.Name.undot ~rep:'-' lib_name
    | Some name -> name
    in
    let meta =
      B0_meta.tag Meta.tag @@
      B0_meta.tag B0_meta.lib @@
      B0_meta.add Meta.library lib_name @@
      B0_meta.add Meta.requires requires @@
      meta
    in
    B0_unit.v ?doc ~meta name (lib_proc srcs)
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
