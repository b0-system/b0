(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let lib = B00_ocaml.Lib_name.v

module Meta = struct
  let tag =
    B0_meta.Key.tag "ocaml" ~doc:"OCaml related entity"

  let requires =
    let doc = "Required OCaml libraries" in
    let pp_value = Fmt.(box @@ list ~sep:sp B00_ocaml.Lib_name.pp) in
    B0_meta.Key.v "ocaml-requires" ~doc ~pp_value

  let library =
    let pp_value = Fmt.using B00_ocaml.Lib_name.to_string Fmt.string in
    B0_meta.Key.v "ocaml-library" ~doc:"Defined OCaml library" ~pp_value
end

module Unit = struct

  (* XXX brzo copy cats ! *)

  let compile_c_srcs m ~in_dir ~srcs k =
    (* XXX Maybe better things could be done here once we have a good C
       domain. *)
    (* FIXME conf *)
    let obj_ext = ".o" in
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

  let local_mods m ~in_dir ~src_root ~srcs k =
    let mli_only = false in
    let exts = B00_fexts.v (".mli" :: if mli_only then [] else [".ml"]) in
    let srcs = B00_fexts.find_files exts srcs in
    let o = Fpath.(in_dir / "ocaml.compdeps") in
    B00_ocaml.Mod_src.Deps.write m ~src_root ~srcs ~o;
    B00_ocaml.Mod_src.Deps.read m ~src_root o @@
    fun src_deps -> k (B00_ocaml.Mod_src.of_srcs m ~src_deps ~srcs)

  let compile_intf m ~in_dir ~requires ~local_mods msrc =
    let open B00_ocaml in
    match Mod_src.mli msrc with
    | None -> None
    | Some mli ->
        let o = Mod_src.cmi_file ~in_dir msrc in
        begin
          let deps = Mod_src.mli_deps msrc in
          let local_deps, _remain = Mod_src.find_local_deps local_mods deps in
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
        end;
        Some o

  let compile_impl m ~code ~in_dir ~requires ~local_mods msrc =
    let open B00_ocaml in
    match Mod_src.ml msrc with
    | None -> None
    | Some ml ->
        let o = Mod_src.impl_file ~code ~in_dir msrc in
        begin
          let deps = Mod_src.ml_deps msrc in
          let local_deps, _remain = Mod_src.find_local_deps local_mods deps in
          let add_dep_objs acc dep =
            Mod_src.as_impl_dep_files ~code ~init:acc ~in_dir dep
          in
          let local_objs = List.fold_left add_dep_objs [] local_deps in
          let ext_objs = [] in
          let has_cmi, local_objs = match Mod_src.mli msrc with
          | None -> false, local_objs
          | Some _ -> true, Mod_src.cmi_file ~in_dir msrc :: local_objs
          in
          let reads = List.rev_append ext_objs local_objs in
          Compile.ml_to_impl m ~code ~has_cmi ~reads ~ml ~o;
        end;
        Some o

  let compile_intfs m ~in_dir ~requires ~local_mods =
    let compile _ msrc acc =
      match compile_intf m ~local_mods ~requires ~in_dir msrc with
      | None -> acc | Some cmi -> cmi :: acc
    in
    String.Map.fold compile local_mods []

  let compile_impls b ~code ~in_dir ~requires ~local_mods =
    let compile _ m acc =
      match compile_impl b ~code ~local_mods ~requires ~in_dir m with
      | None -> acc | Some o -> o :: acc
    in
    String.Map.fold compile local_mods []

  let compile_srcs m ~in_dir ~src_root ~requires ~srcs k =
    let _code = B00_ocaml.Cobj.Native in
    compile_c_srcs m ~in_dir ~srcs @@ fun c_objs ->
    local_mods m ~in_dir ~src_root ~srcs @@ fun local_mods ->
    let _cmis = compile_intfs m ~in_dir ~requires ~local_mods in
    (*
    let _cobjs = compile_impls m ~code ~in_dir ~requires ~local_mods in
*)
    k ()

  (* FIXME share resolver and resolutions between units. *)
  let get_resolver m ~build_dir k =
    B00_ocaml.Ocamlpath.get m None @@ fun ocamlpath ->
    k (B00_ocaml.Lib_resolver.create m ~memo_dir:build_dir ~ocamlpath)

  let get_libs r libs k =
    let rec loop k acc = function
    | [] -> k (List.rev acc)
    | l :: ls ->
        B00_ocaml.Lib_resolver.find r l @@ fun l -> loop k (l :: acc) ls
    in
    loop k [] libs

  let exe_proc srcs b k =
    let u = B0_build.Unit.current b in
    let build_dir = B0_build.Unit.build_dir b u in
    let src_root = B0_build.Unit.root_dir b u in
    let meta = B0_unit.meta u in
    let requires = B0_meta.get Meta.requires meta in
    let _exe_name = B0_meta.get B0_meta.exe_name meta in
    let m = B0_build.memo b in
    B0_srcs.select b srcs @@ fun srcs ->
    get_resolver m ~build_dir @@ fun r ->
    get_libs r requires @@ fun requires ->
    compile_srcs m ~in_dir:build_dir ~src_root ~requires ~srcs k

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
    let _lib_name = B00_ocaml.Lib_name.to_string lib_name in
    let m = B0_build.memo b in
    B0_srcs.select b srcs @@ fun srcs ->
    get_resolver m ~build_dir @@ fun r ->
    get_libs r requires @@ fun requires ->
    compile_srcs m ~in_dir:build_dir ~src_root ~requires ~srcs k

  let lib ?doc ?(meta = B0_meta.empty) ?(requires = []) ?name lib_name ~srcs =
    let name = match name with
    | Some n -> n
    | None ->
        let dot_to_dash = function '.' -> '-' | c -> c in
        let lib_name = B00_ocaml.Lib_name.to_string ~no_legacy:true lib_name in
        String.map dot_to_dash lib_name
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
