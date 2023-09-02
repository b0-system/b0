(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

let add_if c v l = if c then v :: l else l

module Tool = struct

  (* Compilers *)

  let comp_env_vars =
    [ "CAMLLIB"; "CAMLSIGPIPE"; "CAML_DEBUG_FILE"; "CAML_DEBUG_SOCKET";
      "CAML_LD_LIBRARY_PATH"; "BUILD_PATH_PREFIX_MAP"; "OCAMLDEBUG"; "OCAMLLIB";
      "OCAMLPROF_DUMP"; "OCAMLRUNPARAM"; "OCAML_COLOR"; "OCAML_FLEXLINK";
      "OCAML_INSTR_FILE"; "OCAML_INSTR_START"; "OCAML_INSTR_STOP";
      "OCAML_SPACETIME_INTERVAL"; "OCAML_SPACETIME_SNAPSHOT_DIR"; "PATH";
      "TERM"; "__AFL_SHM_ID";

      (* XXX For cc for now we add them in bulk but we could make
         them depend on the conffiguration. *)
      "LD_LIBRARY_PATH"; "LIBRARY_PATH"; "C_INCLUDE_PATH";

      (* XXX These are Windows specific and needed by cl.exe *)
      "SystemRoot"; "INCLUDE"; "LIB"; ]

  let ocamlc = B0_memo.Tool.by_name ~vars:comp_env_vars "ocamlc"
  let ocamlopt = B0_memo.Tool.by_name ~vars:comp_env_vars "ocamlopt"
  let ocamldep = B0_memo.Tool.by_name ~vars:comp_env_vars "ocamldep"
  let ocamlmklib =
    B0_memo.Tool.by_name ~vars:("OCAML_FLEXLINK" :: comp_env_vars) "ocamlmklib"

  let ocamlobjinfo = B0_memo.Tool.by_name ~vars:comp_env_vars "ocamlobjinfo"

  (* Toplevels *)

  let top_env_vars =
    [ "CAML_LD_LIBRARY_PATH"; "CAMLRUNPARAM";
      "OCAMLTOP_INCLUDE_PATH";
      "HOME"; "OCAMLLIB"; "OCAMLRUN_PARAM"; "OCAMLTOP_UTF_8"; "PATH"; "TERM"; ]

  let ocaml = B0_memo.Tool.by_name ~vars:top_env_vars "ocaml"
  let ocamlnat = B0_memo.Tool.by_name ~vars:top_env_vars "ocamlnat"
end

module Conf = struct
  type code = [ `Byte | `Native ]
  type t =
    { fields : string String.Map.t;
      version : int * int * int * string option;
      where : Fpath.t;
      asm_ext : string;
      dll_ext : string;
      exe_ext : string;
      lib_ext : string;
      obj_ext : string;
      has_dynlink : bool; }

  let find k c = String.Map.find_opt k c.fields
  let version c = c.version
  let where c = c.where
  let asm_ext c = c.asm_ext
  let exe_ext c = c.exe_ext
  let dll_ext c = c.dll_ext
  let lib_ext c = c.lib_ext
  let obj_ext c = c.obj_ext
  let has_dynlink c = c.has_dynlink
  let to_string_map c = c.fields
  let of_string_map fields = try
    let err = Fmt.failwith in
    let err_key k = err "key %a not found." Fmt.(code string) k in
    let find k fs = match String.Map.find_opt k fs with
    | None -> err_key k | Some v -> v
    in
    let version =
      let v = find "version" fields in
      match String.to_version v with
      | None -> err "could not parse version string %S" v
      | Some v -> v
    in
    let where = Fpath.of_string (find "standard_library" fields) in
    let where = where |> Result.error_to_failure in
    let asm_ext = find "ext_asm" fields in
    let dll_ext = find "ext_dll" fields in
    let exe_ext = find "ext_exe" fields in
    let lib_ext = find "ext_lib" fields in
    let obj_ext = find "ext_obj" fields in
    let has_dynlink =
      let k = "supports_shared_libraries" in
      let s = find k fields in
      match bool_of_string_opt s with
      | None -> err "key %a cound not parse bool from %S" Fmt.(code string) k s
      | Some b -> b
    in
    Ok { fields; version; where; asm_ext; dll_ext; exe_ext; lib_ext; obj_ext;
         has_dynlink; }
  with
  | Failure e -> Error e

  (* IO *)

  let of_string ?file s =
    let parse_line _ acc l = match String.cut_left ~sep:":" l with
    | None -> acc (* XXX report an error *)
    | Some (k, v) -> String.Map.add (String.trim k) (String.trim v) acc
    in
    try
      let s = String.trim s and strip_newlines = true in
      let fields =
        String.fold_ascii_lines ~strip_newlines parse_line String.Map.empty s
      in
      Ok (of_string_map fields |> Result.error_to_failure)
    with Failure e -> Fpath.error ?file " OCaml config: %s" e

  let write m ~comp ~o =
    let comp = B0_memo.tool m comp in
    B0_memo.spawn m ~writes:[o] ~stdout:(`File o) @@
    comp (Cmd.arg "-config")

  let read m file =
    let* s = B0_memo.read m file in
    Fut.return (of_string ~file s |> B0_memo.fail_if_error m)
end
module Mod = struct
  module Name = struct
    type t = string
    let of_filename f = String.Ascii.capitalize (Fpath.basename ~no_ext:true f)
    let v n = String.Ascii.capitalize n
    let equal = String.equal
    let compare = String.compare
    let pp = Fmt.tty_string [`Bold]
    module Set = String.Set
    module Map = String.Map

    (* Filename mangling *)

    let of_mangled_filename s =
      let rem_ocaml_ext s = match String.cut_right ~sep:"." s with
      | None -> s | Some (s, ("ml" | ".mli")) -> s | Some _ -> s
      in
      let mangle s =
        let char_len = function
        | '-' | '.' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> 1
        | _ -> 2
        in
        let set_char b i c = match c with
        | '.' | '-' -> Bytes.set b i '_'; i + 1
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' as c ->
            Bytes.set b i c; i + 1
        | c ->
            let c = Char.code c in
            Bytes.set b (i    ) (Char.Ascii.upper_hex_digit (c lsr 4));
            Bytes.set b (i + 1) (Char.Ascii.upper_hex_digit (c      ));
            i + 2
        in
        String.byte_replacer char_len set_char s
      in
      let s = mangle (rem_ocaml_ext s) in
      let s = match String.head s with
      | Some c when Char.Ascii.is_letter c -> s
      | None | Some _ -> "M" ^ s
      in
      String.Ascii.capitalize s
  end

  module Ref = struct
    type t = Name.t * Digest.t
    let v n d = (String.Ascii.capitalize n, d)
    let name = fst
    let digest = snd
    let equal (_, d0) (_, d1) = Digest.equal d0 d1
    let compare (n0, d0) (n1, d1) = match Name.compare n0 n1 with
    | 0 -> Digest.compare d0 d1
    | c -> c

    let pp ppf (n, d) = Fmt.pf ppf "@[%s %a@]" (Digest.to_hex d) Name.pp n

    module T = struct type nonrec t = t let compare = compare end
    module Set = struct
      include Set.Make (T)

      let dump ppf rs =
        Fmt.pf ppf "@[<1>{%a}@]" (Fmt.iter ~sep:Fmt.comma iter pp) rs

      let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
    end

    module Map = struct
      include Map.Make (T)

      let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
      let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

      let add_to_list k v m = match find k m with
      | exception Not_found -> add k [v] m
      | l -> add k (v :: l) m

      let add_to_set
          (type set) (type elt)
          (module S : Stdlib.Set.S with type elt = elt and type t = set)
          k v m = match find k m with
      | exception Not_found -> add k (S.singleton v) m
      | set -> add k (S.add v set) m

      let dump pp_v ppf m =
        let pp_binding ppf (k, v) =
          Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp k pp_v v
        in
        Fmt.pf ppf "@[<1>{%a}@]"
          (Fmt.iter_bindings ~sep:Fmt.sp iter pp_binding) m

      let pp ?sep pp_binding = Fmt.iter_bindings ?sep iter pp_binding
    end
  end

  module Src = struct
    module Deps = struct
      let of_string ?(file = Fpath.dash) ?src_root s =
        (* Parse ocamldep's [-slash -modules], a bit annoying to parse.
           ocamldep shows its Makefile legacy. *)
        let parse_path n p = (* ocamldep escapes spaces as "\ ",
                                a bit annoying *)
          let char_len_at s i = match s.[i] with
          | '\\' when i + 1 < String.length s && s.[i+1] = ' ' -> 2
          | _ -> 1
          in
          let set_char b k s i = match char_len_at s i with
          | 2 -> Bytes.set b k ' '; 2
          | 1 -> Bytes.set b k s.[i]; 1
          | _ -> assert false
          in
          match String.byte_unescaper char_len_at set_char p with
          | Error j -> Fmt.failwith_line n "%d: illegal escape" j
          | Ok p ->
              match Fpath.of_string p with
              | Error e -> Fmt.failwith_line n " %s" e
              | Ok p -> p
        in
        let parse_line ~src_root n acc line =
          if line = "" then acc else
          match String.cut_right (* right, windows drives *) ~sep:":" line with
          | None -> Fmt.failwith_line n " cannot parse line: %S" line
          | Some (file, mods) ->
              let file = parse_path n file in
              let file = match src_root with
              | None -> file
              | Some src_root -> Fpath.(src_root // file)
              in
              let add_mod acc m = Name.Set.add m acc in
              let mods = String.cuts_left ~drop_empty:true ~sep:" " mods in
              let start = Name.Set.singleton "Stdlib" in
              let mods = List.fold_left add_mod start mods in
              Fpath.Map.add file mods acc
        in
        try
          let strip_newlines = true and parse = parse_line ~src_root in
          Ok (String.fold_ascii_lines ~strip_newlines parse Fpath.Map.empty s)
        with
        | Failure e -> Fpath.error ~file "%s" e

      let write ?src_root m ~srcs ~o =
        let ocamldep = B0_memo.tool m Tool.ocamldep in
        let srcs', cwd = match src_root with
        | None -> srcs, None
        | Some root ->
            (* XXX unfortunately this doesn't report parse error
               at the right place. So we don't do anything for now
               the output thus depends on the path location and can't
               be cached across machines.
               let rem_prefix src = Fpath.rem_prefix root src |> Option.get in
               List.map rem_prefix srcs, Some root
            *)
            srcs, None
        in
        B0_memo.spawn m ?cwd ~reads:srcs ~writes:[o] ~stdout:(`File o) @@
        ocamldep Cmd.(arg "-slash" % "-modules" %% paths srcs')

      let read ?src_root m file =
        let* s = B0_memo.read m file in
        Fut.return (of_string ?src_root ~file s |> B0_memo.fail_if_error m)
    end

    type t =
      { mod_name : Name.t;
        opaque : bool;
        mli : Fpath.t option;
        mli_deps : Name.Set.t;
        ml : Fpath.t option;
        ml_deps : Name.Set.t;
        build_dir : Fpath.t;
        build_base : Fpath.t }

    let v ~mod_name ~opaque ~mli ~mli_deps ~ml ~ml_deps ~build_dir =
      let build_base = Fpath.(build_dir / String.Ascii.uncapitalize mod_name) in
      { mod_name; opaque; mli; mli_deps; ml; ml_deps; build_dir; build_base }

    let mod_name m = m.mod_name
    let opaque m = m.opaque
    let mli m = m.mli
    let mli_deps m = m.mli_deps
    let ml m = m.ml
    let ml_deps m = m.ml_deps
    let build_dir m = m.build_dir
    let built_file m ~ext = Fpath.(m.build_base + ext)
    let cmi_file m = built_file m ~ext:".cmi"
    let cmo_file m = match ml m with
    | None -> None | Some _ -> Some (built_file m ~ext:".cmo")

    let cmx_file m = match ml m with
    | None -> None | Some _ -> Some (built_file m ~ext:".cmx")

    let pp =
      let path_option = Fmt.option ~none:Fmt.none Fpath.pp_unquoted in
      let deps = Name.Set.pp ~sep:Fmt.sp Fmt.string in
      Fmt.record Fmt.[
          field "mod-name" mod_name Name.pp;
          field "opaque" opaque bool;
          field "mli" mli path_option;
          field "mli-deps" mli_deps deps;
          field "ml" ml path_option;
          field "ml-deps" ml_deps deps;
          field "build-dir" build_dir Fpath.pp_unquoted ]

    let impl_file ~code m =
      let file = match code with `Byte -> cmo_file | `Native -> cmx_file in
      file m

    let as_intf_dep_files ?(init = []) m = cmi_file m :: init
    let as_impl_dep_files ?(init = []) ~code m = match code with
    | `Byte -> cmi_file m :: init
    | `Native ->
        match ml m with
        | None -> cmi_file m :: init
        | Some _ when m.opaque -> cmi_file m :: init
        | Some _ -> cmi_file m :: Option.get (cmx_file m) :: init

    let mod_name_map m ~kind files =
      let add acc f =
        let mname = Name.of_filename f in
        match Name.Map.find_opt mname acc with
        | None -> Name.Map.add mname f acc
        | Some f' ->
            B0_memo.notify m `Warn
              "@[<v>%a:@,File ignored. %a's module %s defined by file:@,%a:@]"
              Fpath.pp f Name.pp mname kind Fpath.pp f';
            acc
      in
      List.fold_left add Name.Map.empty files

    let map_of_srcs m ~build_dir ~srcs ~src_deps  =
      let get_src_deps = function
      | None -> Name.Set.empty
      | Some file ->
          match Fpath.Map.find file src_deps with
          | exception Not_found -> Name.Set.empty
          | deps -> deps
      in
      let mlis, mls = List.partition (Fpath.has_ext ".mli") srcs in
      let mlis = mod_name_map m ~kind:"interface" mlis in
      let mls = mod_name_map m ~kind:"implementation" mls in
      let mod' mod_name mli ml =
        let mli_deps = get_src_deps mli in
        let ml_deps = get_src_deps ml in
        Some (v ~mod_name ~opaque:false ~mli ~mli_deps ~ml ~ml_deps ~build_dir)
      in
      Name.Map.merge mod' mlis mls

    let sort ?stable ~deps name_map =
      (* FIXME do something better, on cycles this lead to link failure
         we should detect it. *)
      let rec loop seen acc = function
      | [] -> seen, acc
      | src :: srcs ->
          if Name.Set.mem src.mod_name seen then loop seen acc srcs else
          let seen = Name.Set.add src.mod_name seen in
          let add_src_dep n acc = match Name.Set.mem n seen with
          | true -> acc
          | false ->
              match Name.Map.find_opt n name_map with
              | None -> acc
              | Some src -> src :: acc
          in
          let deps = Name.Set.fold add_src_dep (deps src) [] in
          let seen, acc = loop seen acc deps in
          loop seen (src :: acc) srcs
      in
      let add_src _ src acc = src :: acc in
      let stable = Option.value ~default:[] stable in
      let todo = stable @ Name.Map.fold add_src name_map [] in
      let _, acc = loop Name.Set.empty [] todo in
      List.rev acc

    let find ns map =
      let rec loop res remain deps = match Name.Set.choose deps with
      | exception Not_found -> res, remain
      | dep ->
          let deps = Name.Set.remove dep deps in
          match Name.Map.find dep map with
          | m -> loop (m :: res) remain deps
          | exception Not_found ->
              loop res (Name.Set.add dep remain) deps
      in
      loop [] Name.Set.empty ns

    let map_of_files ?(only_mlis = false) m ~build_dir ~src_root ~srcs =
      let exts = B0_file_exts.v (".mli" :: if only_mlis then [] else [".ml"]) in
      let srcs = B0_file_exts.find_files exts srcs in
      let o = Fpath.(build_dir / "ocaml-srcs.deps") in
      Deps.write m ~src_root ~srcs ~o;
      let* src_deps = Deps.read m ~src_root o in
      Fut.return (map_of_srcs m ~build_dir ~srcs ~src_deps)
  end
end

module Cobj = struct
  let archive_ext_of_code = function `Byte -> ".cma" | `Native -> ".cmxa"
  let object_ext_of_code = function `Byte -> ".cmo" | `Native -> ".cmx"

  type t =
    { file : Fpath.t;
      defs : Mod.Ref.Set.t;
      deps : Mod.Ref.Set.t;
      link_deps : Mod.Ref.Set.t; (* deps whose name appear in required
                                    globals/implementations imported *) }

  let file c = c.file
  let defs c = c.defs
  let deps c = c.deps
  let link_deps c = c.link_deps
  let equal c0 c1 = Fpath.equal c0.file c1.file
  let compare c0 c1 = Fpath.compare c0.file c1.file
  let pp =
    Fmt.record @@
    [ Fmt.field "file" file Fpath.pp_quoted;
      Fmt.field "defs" defs Mod.Ref.Set.dump;
      Fmt.field "deps" deps Mod.Ref.Set.dump;
      Fmt.field "link-deps" link_deps Mod.Ref.Set.dump; ]

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  let sort ?(deps = link_deps) cobjs =
    let rec loop cobjs_defs seen ext_deps cobjs = function
    | (c :: cs as l) :: todo ->
        begin match Mod.Ref.Set.subset (defs c) seen with
        | true -> loop cobjs_defs seen ext_deps cobjs (cs :: todo)
        | false ->
            let seen = Mod.Ref.Set.union (defs c) seen in
            let add_dep d (local_deps, ext_deps as acc) =
              if Mod.Ref.Set.mem d seen then acc else
              match Mod.Ref.Map.find d cobjs_defs with
              | exception Not_found -> local_deps, Mod.Ref.Set.add d ext_deps
              | dep_cobj -> dep_cobj :: local_deps, ext_deps
            in
            let start = [], ext_deps in
            let local_deps, ext_deps =
              Mod.Ref.Set.fold add_dep (deps c) start
            in
            match local_deps with
            | [] -> loop cobjs_defs seen ext_deps (c :: cobjs) (cs :: todo)
            | deps -> loop cobjs_defs seen ext_deps cobjs (deps :: l :: todo)
        end
    | [] :: (c :: cs) :: todo ->
        loop cobjs_defs seen ext_deps (c :: cobjs) (cs :: todo)
    | [] :: ([] :: todo) ->
        loop cobjs_defs seen ext_deps cobjs todo
    | [] :: [] ->
        let sorted = List.rev cobjs in
        sorted, ext_deps
    | [] -> assert false
    in
    let add_def c d acc = Mod.Ref.Map.add d c acc in
    let add_defs acc c = Mod.Ref.Set.fold (add_def c) (defs c) acc in
    let cobjs_defs = List.fold_left add_defs Mod.Ref.Map.empty cobjs in
    loop cobjs_defs Mod.Ref.Set.empty Mod.Ref.Set.empty [] (cobjs :: [])

  (* ocamlobjinfo output parsing, could be easier... *)

  let make_cobj file defs deps ldeps =
    let deps = Mod.Ref.Set.diff deps defs in
    let link_deps =
      let keep m = String.Set.mem (Mod.Ref.name m) ldeps in
      Mod.Ref.Set.filter keep deps
    in
    { file; defs; deps; link_deps; }

  let file_prefix = "File "
  let parse_file_path (n, line) =
    let len = String.length file_prefix in
    match Fpath.of_string (String.drop_left len line) with
    | Ok file -> file
    | Error e -> Fmt.failwith_line n " %s" e

  let rec parse_ldeps acc file defs deps ldeps name = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | ((n, l) :: ls) as data ->
      match String.cut_right ~sep:"\t" l with
      | None -> parse_file acc file defs deps ldeps data
      | Some (_, ldep) ->
          let ldeps = String.Set.add (String.trim ldep) ldeps in
          parse_ldeps acc file defs deps ldeps name ls

  and parse_deps acc file defs deps ldeps name = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | ((n, l) :: ls) as data ->
      match String.cut_right ~sep:"\t" l with
      | None ->
          begin match l with
          | l
            when String.starts_with ~prefix:"Implementations imported:" l ||
                 String.starts_with ~prefix:"Required globals:" l ->
              parse_ldeps acc file defs deps ldeps name ls
          | _ ->
              parse_file acc file defs deps ldeps data
          end
      | Some (dhex, dname) ->
          let dhex = String.trim dhex in
          let dname = String.trim dname in
          match Digest.from_hex dhex with
          | digest ->
              let mref = Mod.Ref.v dname digest in
              let defs, deps = match String.equal dname name with
              | true -> Mod.Ref.Set.add mref defs, deps
              | false -> defs, Mod.Ref.Set.add mref deps
              in
              parse_deps acc file defs deps ldeps name ls
          | exception Invalid_argument _ ->
              (* skip undigested deps *)
              match dhex <> "" && dhex.[0] = '-' with
              | true -> parse_deps acc file defs deps ldeps name ls
              | false -> Fmt.failwith_line n " %S: could not parse digest" dhex

  and parse_unit acc file defs deps ldeps name = function
  | [] -> Fmt.failwith "unexpected end of input"
  | (_, l) :: rest when String.starts_with ~prefix:"Interfaces imported:" l ->
      parse_deps acc file defs deps ldeps name rest
  | _ :: rest -> parse_unit acc file defs deps ldeps name rest

  and parse_file acc file defs deps ldeps = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | (n, l) :: ls when String.starts_with ~prefix:"Unit name" l ||
                 String.starts_with ~prefix:"Name" l ->
      begin match String.cut_left ~sep:":" l with
      | None -> assert false
      | Some (_, name) ->
          parse_unit acc file defs deps ldeps (String.trim name) ls
      end
  | (n, l as line) :: ls when String.starts_with ~prefix:file_prefix l ->
      let acc = make_cobj file defs deps ldeps :: acc in
      let file = parse_file_path line in
      parse_file
        acc file Mod.Ref.Set.empty Mod.Ref.Set.empty String.Set.empty ls
  | _ :: ls -> parse_file acc file defs deps ldeps ls

  and parse_files acc = function
  | [] -> acc
  | (n, l as line) :: ls when String.starts_with ~prefix:file_prefix l ->
      let file = parse_file_path line in
      parse_file
        acc file Mod.Ref.Set.empty Mod.Ref.Set.empty String.Set.empty ls
  | l :: ls -> parse_files acc ls

  let of_string ?file data =
    let line num acc l = (num, l) :: acc in
    let rev_lines = String.fold_ascii_lines ~strip_newlines:true line [] data in
    try Ok (parse_files [] (List.rev rev_lines)) with
    | Failure e -> Fpath.error ?file "%s" e

  let write m ~cobjs ~o =
    (* FIXME add [src_root] so that we can properly unstamp. *)
    let ocamlobjinfo = B0_memo.tool m Tool.ocamlobjinfo in
    B0_memo.spawn m ~reads:cobjs ~writes:[o] ~stdout:(`File o) @@
    ocamlobjinfo Cmd.(arg "-no-approx" % "-no-code" %% paths cobjs)

  let read m file =
    let* s = B0_memo.read m file in
    Fut.return (of_string ~file s |> B0_memo.fail_if_error m)
end

(* Libraries *)

module Lib = struct

  (* Library names. *)

  module Name = struct
    let fpath_to_name ?(sep = '.') s =
      let b = Bytes.of_string (Fpath.to_string s) in
      for i = 0 to Bytes.length b - 1 do
        if Bytes.get b i = Fpath.dir_sep_char then Bytes.set b i sep;
      done;
      Bytes.unsafe_to_string b

    let name_to_fpath s =
      let err s exp = Fmt.error "%S: not a library name, %s" s exp in
      let err_start s = err s "expected a starting lowercase ASCII letter" in
      let b = Bytes.of_string s in
      let max = String.length s - 1 in
      let rec loop i ~id_start = match i > max with
      | true ->
          if id_start then err_start s else
          Ok (Fpath.v (Bytes.unsafe_to_string b))
      | false when id_start ->
          begin match Bytes.get b i with
          | 'a' .. 'z' -> loop (i + 1) ~id_start:false
          | _ -> err_start s
          end
      | false ->
          begin match Bytes.get b i with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' ->
              loop (i + 1) ~id_start:false
          | '.' -> Bytes.set b i Fpath.dir_sep_char; loop (i + 1) ~id_start:true
          | c -> err s (Fmt.str "illegal character %C" c)
          end
      in
      loop 0 ~id_start:true

    type t = Fpath.t (* dots are Fpath.dir_sep_char *)

    let first n =
      let n = Fpath.to_string n in
      match String.cut_right ~sep:Fpath.dir_sep n with
      | None -> n | Some (_, n) -> n

    let last n =
      let n = Fpath.to_string n in
      match String.cut_left ~sep:Fpath.dir_sep n with
      | None -> n | Some (n, _) -> n

    let to_archive_name n = fpath_to_name ~sep:'_' n
    let undot ~rep n = fpath_to_name ~sep:rep n

    let of_string s = Result.bind (name_to_fpath s) @@ fun name -> Ok name
    let to_string n = fpath_to_name n
    let to_fpath n = n
    let v s = match of_string s with Ok v -> v | Error e -> invalid_arg e
    let equal = Fpath.equal
    let compare = Fpath.compare
    let pp = Fmt.using to_string (Fmt.code Fmt.string)

    module T = struct type nonrec t = t let compare = compare end
    module Set = Set.Make (T)
    module Map = Map.Make (T)
  end

  (* Libraries *)

  type t =
    { name : Name.t;
      requires : Name.t list;
      dir : Fpath.t;
      cmis : Fpath.t list;
      cmxs : Fpath.t list;
      cma : Fpath.t option;
      cmxa : Fpath.t option;
      c_archive : Fpath.t option;
      c_stubs : Fpath.t list;
      js_stubs : Fpath.t list;  }

  let v
      ~name ~requires ~dir ~cmis ~cmxs ~cma ~cmxa ~c_archive ~c_stubs ~js_stubs
    =
    { name; requires; dir; cmis; cmxs; cma; cmxa; c_archive; c_stubs; js_stubs }

  let of_dir m ~clib_ext ~name ~requires ~dir ~archive ~js_stubs =
    Fut.return @@
    Result.map_error (fun e -> Fmt.str "library %a: %s" Name.pp name e) @@
    Result.bind (Os.Dir.exists dir) @@ function
    | false ->
        B0_memo.notify m `Warn "library %a: no directory %a"
          Name.pp name Fpath.pp dir;
        Ok (v ~name ~requires ~dir ~cmis:[] ~cmxs:[] ~cma:None ~cmxa:None
              ~c_archive:None ~c_stubs:[] ~js_stubs:[])
    | true ->
        Result.bind (Os.Dir.fold_files ~recurse:false Os.Dir.path_list dir [])
        @@ fun fs ->
        let js_stubs = List.map (fun f -> Fpath.(dir // f)) js_stubs in
        let () = List.iter (B0_memo.file_ready m) js_stubs in
        let rec loop cmis cmxs cma cmxa c_archive c_stubs = function
        | [] ->
            v ~name ~requires ~dir ~cmis ~cmxs ~cma ~cmxa ~c_archive ~c_stubs
              ~js_stubs
        | f :: fs ->
            let is_lib_archive f = match archive with
            | None -> false
            | Some a -> String.equal (Fpath.basename ~no_ext:true f) a
            in
            match Fpath.get_ext f with
            | ".cmi" ->
                B0_memo.file_ready m f;
                loop (f :: cmis) cmxs cma cmxa c_archive c_stubs fs
            | ".cmx" ->
                B0_memo.file_ready m f;
                loop cmis (f :: cmxs) cma cmxa c_archive c_stubs fs
            | ".cma" ->
                let cma = match is_lib_archive f with
                | true -> B0_memo.file_ready m f; Some f
                | false -> cma
                in
                loop cmis cmxs cma cmxa c_archive c_stubs fs
            | ".cmxa" ->
                let cmxa = match is_lib_archive f with
                | true -> B0_memo.file_ready m f; Some f
                | false -> cmxa
                in
                loop cmis cmxs cma cmxa c_archive c_stubs fs
            | ext when String.equal ext clib_ext ->
                B0_memo.file_ready m f;
                let c_archive, c_stubs = match is_lib_archive f with
                | true -> Some f, c_stubs
                | false -> c_archive, (f :: c_stubs)
                in
                loop cmis cmxs cma cmxa c_archive c_stubs fs
            | _ ->
                loop cmis cmxs cma cmxa c_archive c_stubs fs
        in
        Ok (loop [] [] None None None [] fs)

  let name l = l.name
  let requires l = l.requires
  let dir l = l.dir
  let cmis l = l.cmis
  let cmxs l = l.cmxs
  let cma l = l.cma
  let cmxa l = l.cmxa
  let c_archive l = l.c_archive
  let c_stubs l = l.c_stubs
  let js_stubs l = l.js_stubs

  (* Resolvers *)

  module Resolver = struct

    (* FIXME rework erroring, for now we are not using the mecanisms
       and they likely need to be tweaked. *)

    type lib = t

    (* Resolution scopes *)

    type scope_find = Conf.t -> B0_memo.t -> Name.t -> lib option Fut.t
    type scope_suggest =
      Conf.t -> B0_memo.t -> Name.t -> string option Fut.t

    type scope = { name : string; find : scope_find; suggest : scope_suggest; }

    let scope_name s = s.name
    let scope_find s = s.find
    let scope_suggest s = s.suggest
    let scope ~name ~find ~suggest = { name; find; suggest }

    module Ocamlpath = struct
      (* Stubbed at the moment *)
      let find ~cache_dir conf m n = Fut.return None
      let suggest conf m n = Fut.return None
      let scope ~cache_dir =
        let find = find ~cache_dir in
        { name = "OCAMLPATH"; find; suggest }
    end

    module Ocamlfind = struct
      let tool = B0_memo.Tool.by_name "ocamlfind"

      let parse_info m ?(file = Fpath.dash) ~name s =
        let parse_requires requires =
          let to_libname s =
            Result.error_to_failure @@
            Result.map_error (Fmt.str "required library: %s") @@
            Name.of_string s
          in
          if requires = "" then [] else
          (* ocamlfind does not normalize *)
          let skip_ws = String.lose_left Char.Ascii.is_white in
          let get_tok = String.span_left (Fun.negate Char.Ascii.is_white) in
          let rec rev_toks acc s =
            let s = (skip_ws s) in
            match get_tok s with
            | "", rest -> if rest = "" then acc else rest :: acc (* will err *)
            | tok, rest -> rev_toks (tok :: acc) rest
          in
          List.rev_map to_libname (rev_toks [] requires)
        in
        let parse_archive a =
          if a = "" then None else
          match String.cut_right ~sep:"." a with
          | None -> Some a | Some (a, _ext) -> Some a
        in
        let parse_js_stubs js_stubs =
          let stubs = String.cuts_left ~drop_empty:true ~sep:"," js_stubs in
          let to_path s =
            Result.error_to_failure @@
            Result.map_error (Fmt.str "js stubs: %s") @@
            Fpath.of_string s
          in
          List.map to_path stubs
        in
        try
          match String.split_on_char ':' (String.trim s) with
          | [meta; dir; archive; requires; js_stubs] ->
              let requires = parse_requires requires in
              let archive = parse_archive archive in
              let dir =
                Result.error_to_failure @@
                Result.map_error (Fmt.str "library directory: %s") @@
                Fpath.of_string dir
              in
              let js_stubs = parse_js_stubs js_stubs in
              Ok (meta, requires, dir, archive, js_stubs)
          | _ -> Fmt.failwith "could not parse %S" s
        with
        | Failure e -> Fmt.error "@[<v>%a: %s@]" Fpath.pp file e

      (* FIXME need to solve the META file read.
         FIXME post exec is still super messy, check if we can make it
         to use Memo.t *)

      let write_info ~cache_dir conf m n =
        (* FIXME better [n] not found error *)
        let ocamlfind = B0_memo.tool m tool in
        let fname, lib, predicates = match Name.to_string n with
        | "ocaml.threads" | "threads" | "threads.posix" ->
            if Conf.version conf < (5, 0, 0, None)
            then "threads", "threads.posix", "byte,native,mt,mt_posix"
            else "threads", "threads", "byte,native"
        | n -> n, n, "byte,native"
        in
        let post_exec op = match B0_zero.Op.status op with
        | B0_zero.Op.Done ->
            begin match
              Option.get (B0_zero.Op.Spawn.exit (B0_zero.Op.Spawn.get op))
            with
            | `Exited 2 ->
                (* FIXME checktypo *)
                let err = Fmt.str "OCaml library %a not found" Name.pp n in
                B0_zero.Op.set_status op (B0_zero.Op.(Failed (Exec (Some err))))
            | _ -> ()
            end
        | _ -> ()
        in
        let success_exits = [0; 2 (* not found *) ] in
        let info =
          (* We use %A otherwise whith %a we get a blank line if there's
             no archive. Technically though we only support single library
             archives *)
          "%m:%d:%A:%(requires):%(jsoo_runtime)"
        in
        let fname = Fmt.str "ocamlfind.%s" fname in
        let o = Fpath.(cache_dir / fname) in
        let stdout = `File o in
        B0_memo.spawn m
          ~success_exits ~reads:[] ~writes:[o] ~stdout ~post_exec @@
        ocamlfind Cmd.(arg "query" % lib % "-predicates" % predicates %
                       "-format" % info);
        o

      let read_info m clib_ext name file =
        let* s = B0_memo.read m file in
        match parse_info ~file m ~name s with
        | Error _ as e -> B0_memo.fail_if_error m e
        | Ok (_meta, requires, dir, archive, js_stubs) ->
            let* lib =
              of_dir m ~clib_ext ~name ~requires ~dir ~archive ~js_stubs
            in
            Fut.return (Some (B0_memo.fail_if_error m lib))

      let find ~cache_dir conf m n =
        (* This never returns None we should factor error reporting
           in *)
        let clib_ext = Conf.lib_ext conf in
        let o = write_info ~cache_dir conf m n in
        read_info m clib_ext n o

      let suggest conf m n = Fut.return None
      let scope ~cache_dir =
        let find = find ~cache_dir in
        { name = "ocamlfind"; find; suggest }
    end

    let cache_dir_name = "ocaml-lib"
    let ocamlpath = Ocamlpath.scope
    let ocamlfind = Ocamlfind.scope

    type t =
      { memo : B0_memo.t;
        conf : Conf.t;
        scopes : scope list;
        mutable libs : lib option Fut.t Name.Map.t; }

    let create memo conf scopes =
      let memo = B0_memo.with_mark memo "ocaml-resolver" in
      { memo; conf; scopes; libs = Name.Map.empty }

    let ocaml_conf r = r.conf

    let find_in_scopes r set n =
      let rec loop r set n = function
      | [] -> set None
      | s :: ss ->
          Fut.await (scope_find s r.conf r.memo n) @@ function
          | None -> loop r set n ss
          | Some _ as lib -> set lib
      in
      loop r set n r.scopes

    let find r n = match Name.Map.find_opt n r.libs with
    | Some v -> v
    | None ->
        let fut, set = Fut.create () in
        r.libs <- Name.Map.add n fut r.libs;
        find_in_scopes r set n;
        fut

    let get r n = Fut.bind (find r n) @@ function
    | None -> B0_memo.fail r.memo "No OCaml library %a found" Name.pp n
    | Some lib -> Fut.return lib

    let get_list r ns = Fut.of_list (List.map (get r) ns)
    let get_list_and_deps r ns =
      let rec loop seen acc = function
      | [] -> Fut.return (seen, acc)
      | l :: ls  ->
          if Name.Set.mem l seen then loop seen acc ls else
          let seen = Name.Set.add l seen in
          let* lib = get r l in
          let not_seen n = not (Name.Set.mem n seen) in
          let deps = List.filter not_seen (requires lib) in
          let* seen, acc = loop seen acc deps in
          loop seen (lib :: acc) ls
      in
      let* _, libs = loop Name.Set.empty [] ns in
      Fut.return (List.rev libs)
  end
end

(* FIXME likely remove that *)

module Ocamlpath = struct
  let get m ps = match ps with
  | Some ps -> Fut.return ps
  | None ->
      let empty_is_none = true in
      match
        B0_memo.Env.find' ~empty_is_none Fpath.list_of_search_path "OCAMLPATH" m
      with
      | Some ps -> Fut.return ps
      | None ->
          match
            B0_memo.Env.find' ~empty_is_none
              Fpath.of_string "OPAM_SWITCH_PREFIX" m
          with
          | Some p -> Fut.return [Fpath.(p / "lib")]
          | None ->
              B0_memo.fail m
                "Could not determine an %a in the build environment."
                Fmt.(code string) "OCAMLPATH"
end

module Compile = struct

  (* XXX We should properly investigate how to use BUILD_PATH_PREFIX_MAP.
     However for some reasons that were never not really answered by @gasche in
     https://github.com/ocaml/ocaml/pull/1515, the map does not affect
     absolute paths which severly limits its applicability.

     XXX At some point we would had -o OBJ src [-I inc...] this worked
     at least in 4.07 but not in 4.03, where apparently the order mattered.

     XXX thread conf/version at that level ? E.g. if `-inc` becomes a
     reality. We'd like to slip `-inc` in incs_of_file. *)

  type code = [ `Byte | `Native ]

  let incs_of_files files =
    Cmd.paths ~slip:"-I" @@ Fpath.distinct @@ List.map Fpath.parent files

  let c_to_o ?post_exec ?k m ~comp ~opts ~reads ~c ~o =
    let cwd = Fpath.parent o
      (* We can't use `-c` and `-o` on C files see
         https://github.com/ocaml/ocaml/issues/7677 so we cwd to the
         output directory to perform the spawn. *)
    in
    let incs = incs_of_files reads in
    B0_memo.spawn m ?post_exec ?k ~reads:(c :: reads) ~writes:[o] ~cwd @@
    (B0_memo.tool m comp)
      Cmd.(arg "-c" %% opts %% unstamp (incs %% path c))

  let mli_to_cmi ?post_exec ?k ~and_cmti m ~comp ~opts ~reads ~mli ~o =
    let base = Fpath.strip_ext o in
    let stamp = Fpath.basename base in
    let reads = mli :: reads in
    let writes = o :: if and_cmti then [Fpath.(base + ".cmti")] else [] in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmti (Cmd.arg "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path mli)) in
    B0_memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    (B0_memo.tool m comp) Cmd.(arg "-c" %% bin_annot %% opts % "-o" %% io)

  let ml_to_cmo ?post_exec ?k ~and_cmt m ~opts ~reads ~has_cmi ~ml ~o =
    let ocamlc = B0_memo.tool m Tool.ocamlc in
    let base = Fpath.strip_ext o in
    let stamp = Fpath.basename base (* output depends on mod name *) in
    let reads = ml :: reads in
    let writes =
      o :: (add_if and_cmt Fpath.(base + ".cmt") @@
            add_if (not has_cmi) Fpath.(base + ".cmi") [])
    in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmt (Cmd.arg "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path ml)) in
    B0_memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlc Cmd.(arg "-c" %% bin_annot %% opts % "-o" %% io)

  let ml_to_cmx ?post_exec ?k ~and_cmt m ~opts ~reads ~has_cmi ~ml ~o =
    let ocamlopt = B0_memo.tool m Tool.ocamlopt in
    let base = Fpath.strip_ext o in
    let stamp = Fpath.basename base (* output depends on mod name *) in
    let reads = ml :: reads in
    let writes =
      o :: Fpath.(base + ".o") ::
      (add_if and_cmt Fpath.(base + ".cmt") @@
       add_if (not has_cmi) Fpath.(base + ".cmi") [])
    in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmt (Cmd.arg "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path ml)) in
    B0_memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlopt Cmd.(arg "-c" %% bin_annot %% opts % "-o" %% io)

  let ml_to_impl ?post_exec ?k m ~code ~opts ~reads ~has_cmi ~ml ~o ~and_cmt =
    let ml_to_obj = match code with `Byte -> ml_to_cmo | `Native -> ml_to_cmx in
    ml_to_obj ?post_exec ?k m ~opts ~reads ~has_cmi ~ml ~o ~and_cmt

  (* Mod.Src convenience *)

  let mod_src_intf ~and_cmti m ~comp ~opts ~requires ~mod_srcs src =
    match Mod.Src.mli src with
    | None -> ()
    | Some mli ->
        let o = Mod.Src.cmi_file src in
        let deps = Mod.Src.mli_deps src in
        let src_deps, _remain = Mod.Src.find deps mod_srcs in
        let add_src_dep_objs acc dep = Mod.Src.as_intf_dep_files ~init:acc dep
        in
        let src_deps_objs = List.fold_left add_src_dep_objs [] src_deps in
        let ext_objs =
          (* XXX could be more precise with [_remain] *)
          let add_lib acc l = List.rev_append (Lib.cmis l) acc in
          List.fold_left add_lib [] requires
        in
        let reads = List.rev_append src_deps_objs ext_objs in
        mli_to_cmi ~and_cmti m ~comp ~opts ~reads ~mli ~o

  let mod_src_impl ~and_cmt m ~code ~opts ~requires ~mod_srcs src =
    match Mod.Src.ml src with
    | None -> ()
    | Some ml ->
        let o = Option.get (Mod.Src.impl_file ~code src) in
        let deps = Mod.Src.ml_deps src in
        let src_deps, _remain = Mod.Src.find deps mod_srcs in
        let add_src_dep_objs acc dep =
          Mod.Src.as_impl_dep_files ~code ~init:acc dep
        in
        let src_deps_objs = List.fold_left add_src_dep_objs [] src_deps in
        let ext_objs =
          let add_lib acc l = match code with
          | `Native ->
              List.rev_append (Lib.cmxs l) @@
              List.rev_append (Lib.cmis l) acc
          | `Byte ->
              List.rev_append (Lib.cmis l) acc
          in
          List.fold_left add_lib [] requires
        in
        let has_cmi, src_deps_objs = match Mod.Src.mli src with
        | None -> false, src_deps_objs
        | Some _ -> true, Mod.Src.cmi_file src :: src_deps_objs
        in
        let reads = List.rev_append ext_objs src_deps_objs in
        ml_to_impl ~and_cmt m ~code ~opts ~reads ~has_cmi ~ml ~o

  let intfs ~and_cmti m ~comp ~opts ~requires ~mod_srcs =
    let compile _ src =
      mod_src_intf ~and_cmti m ~comp ~mod_srcs ~requires ~opts src
    in
    String.Map.iter compile mod_srcs

  let impls ~and_cmt m ~code ~opts ~requires ~mod_srcs =
    let compile _ src =
      mod_src_impl ~and_cmt m ~code ~opts ~mod_srcs ~requires src
    in
    String.Map.iter compile mod_srcs
end

module Archive = struct
  let cstubs_name name = Fmt.str "%s_stubs" name
  let cstubs_clib name ext_lib = Fmt.str "lib%s_stubs%s" name ext_lib
  let cstubs_dll name ext_dll = Fmt.str "dll%s_stubs%s" name ext_dll
  let cstubs ?post_exec ?k m ~conf ~opts ~c_objs ~odir ~oname =
    let lib_ext = Conf.lib_ext conf in
    let dll_ext = Conf.dll_ext conf in
    let ocamlmklib = B0_memo.tool m Tool.ocamlmklib in
    let o = Fpath.(odir / cstubs_name oname) in
    let writes =
      Fpath.(odir / cstubs_clib oname lib_ext) ::
      add_if (Conf.has_dynlink conf) Fpath.(odir / cstubs_dll oname dll_ext) []
    in
    B0_memo.spawn ?post_exec ?k m ~reads:c_objs ~writes @@
    ocamlmklib
      Cmd.(arg "-o" %% unstamp (path o) %% opts %% unstamp (paths c_objs))

  let byte ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname =
    let ocamlc = B0_memo.tool m Tool.ocamlc in
    let cstubs_opts =
      if not has_cstubs then Cmd.empty else
      let lib = Fmt.str "-l%s" (cstubs_name oname) in
      Cmd.(arg "-cclib" % lib %%
           if' (Conf.has_dynlink conf) (arg "-dllib" % lib))
    in
    let cma = Fpath.(odir / Fmt.str "%s.cma" oname) in
    B0_memo.spawn m ~reads:cobjs ~writes:[cma] @@
    ocamlc Cmd.(arg "-a" % "-o" %% unstamp (path cma) %% opts %% cstubs_opts %%
                unstamp (paths cobjs))

  let native ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname =
    let ocamlopt = B0_memo.tool m Tool.ocamlopt in
    let lib_ext = Conf.lib_ext conf in
    let obj_ext = Conf.obj_ext conf in
    let cstubs_opts =
      if not has_cstubs then Cmd.empty else
      Cmd.(arg "-cclib" % Fmt.str "-l%s" (cstubs_name oname))
    in
    let cmxa_clib =
      if cobjs = [] && Conf.version conf >= (4, 13, 0, None)
      then []
      else [Fpath.(odir / Fmt.str "%s%s" oname lib_ext)]
    in
    let cmxa = Fpath.(odir / Fmt.str "%s.cmxa" oname) in
    let writes = cmxa :: cmxa_clib in
    let c_objs = List.rev_map (Fpath.set_ext obj_ext) cobjs in
    let reads = List.rev_append c_objs cobjs in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes @@
    ocamlopt Cmd.(arg "-a" % "-o" %% unstamp (path cmxa) %% opts %%
                  cstubs_opts %% unstamp (paths cobjs))

  let code ?post_exec ?k m ~conf ~opts ~code ~has_cstubs ~cobjs ~odir ~oname =
    let archive = match code with `Byte -> byte | `Native -> native in
    archive ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname

  let native_dynlink ?post_exec ?k m ~conf ~opts ~has_cstubs ~cmxa ~o =
    let lib_ext = Conf.lib_ext conf in
    let ocamlopt = B0_memo.tool m Tool.ocamlopt in
    let cmxa_clib = Fpath.(cmxa -+ lib_ext) in
    let cstubs_opts, reads =
      if not has_cstubs then Cmd.empty, [cmxa; cmxa_clib] else
      (* Fixme do this on a cstubs path *)
      let oname = Fpath.basename ~no_ext:true cmxa in
      let cstubs_dir = Fpath.(parent cmxa) in
      let cstubs = Fpath.(cstubs_dir / cstubs_clib oname lib_ext) in
      let inc = Cmd.(arg "-I" %% unstamp (path cstubs_dir)) in
      Cmd.(inc %% unstamp (path cstubs)), [cstubs; cmxa; cmxa_clib]
    in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlopt Cmd.(arg "-shared" % "-linkall" % "-o" %% unstamp (path o) %%
                  opts %% cstubs_opts %% unstamp (path cmxa))
end

module Link = struct

  (* FIXME Add cstubs archives of cm[x]a to [reads] ? Do we need it ?
     that would entail an ocamlobjinfo + C library lookup *)

  let cstubs_incs objs =
    let add_inc acc obj = Fpath.Set.add (Fpath.parent obj) acc in
    let incs = List.fold_left add_inc Fpath.Set.empty objs in
    Cmd.paths ~slip:"-I" (Fpath.Set.elements incs)

  let distinct_cobjs cobjs =
    (* Formally two ocamlfind package name can point to the same objects
       (we also have aliases hardcoded in the resolver to handle the thread
       package mess) so we might have the same archive twice. *)
    Fpath.distinct cobjs

  let byte ?post_exec ?k m ~conf ~opts ~c_objs ~cobjs ~o =
    let ocamlc = B0_memo.tool m Tool.ocamlc in
    let cobjs = distinct_cobjs cobjs in
    let reads = List.rev_append cobjs c_objs in
    let incs = cstubs_incs cobjs in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlc Cmd.(arg "-o" %% unstamp (path o) %% opts %%
                unstamp (incs %% paths c_objs %% paths cobjs))

  let native ?post_exec ?k m ~conf ~opts ~c_objs ~cobjs ~o =
    let ocamlopt = B0_memo.tool m Tool.ocamlopt in
    let obj_ext = Conf.obj_ext conf in
    let cobjs = distinct_cobjs cobjs in
    let incs = cstubs_incs cobjs in
    let reads, cobjs =
      let rec loop rsides rcobjs = function
      | [] ->
          List.rev_append rcobjs (List.rev_append rsides c_objs),
          List.rev rcobjs
      | cobj :: cobjs ->
          match Fpath.has_ext ".cmx" cobj with
          | true ->
              (* Add the side `.o` C object to read files. *)
              let rsides = Fpath.set_ext obj_ext cobj :: rsides in
              loop rsides (cobj :: rcobjs) cobjs
          | false ->
              match Fpath.has_ext ".cmxa" cobj with
              | true ->
                  loop rsides (cobj :: rcobjs) cobjs
              | false ->
                  (* This should be the `cmxa`s C library archives *)
                  loop (cobj :: rsides) rcobjs cobjs
      in
      loop [] [] cobjs
    in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlopt Cmd.(arg "-o" %% unstamp (path o) %% opts %%
                  unstamp (incs %% paths c_objs %% paths cobjs))

  let code ?post_exec ?k m ~conf ~opts ~code ~c_objs ~cobjs ~o =
    let linker = match code with `Byte -> byte | `Native -> native in
    linker ?post_exec ?k m ~conf ~opts ~c_objs ~cobjs ~o
end

module Crunch = struct
  let string_to_string ~id ~data:s =
    let len = String.length s in
    let len = len * 4 + (len / 18) * (3 + 2) in
    let b = Buffer.create (len + String.length id + 3) in
    let adds = Buffer.add_string in
    adds b "let "; adds b id; adds b " =\n  \"";
    for i = 0 to String.length s - 1 do
      if i mod 18 = 0 && i <> 0 then adds b "\\\n   ";
      let c = Char.code (String.get s i) in
      adds b "\\x";
      Buffer.add_char b (Char.Ascii.lower_hex_digit ((c lsr 4) land 0xF));
      Buffer.add_char b (Char.Ascii.lower_hex_digit (c land 0xF))
    done;
    adds b "\"\n";
    Buffer.contents b
end

let () = B0_def.Scope.open_lib "ocaml"

let libname = Lib.Name.v

type built_code = [ `Byte | `Native | `All ]
let pp_built_code ppf c = Fmt.string ppf (match c with
| `Byte -> "byte" | `Native -> "native" | `All -> "all")

(* Metadata *)

let tag = B0_meta.Key.tag "ocaml" ~doc:"OCaml related entity"

module Meta = struct
  let c_requires =
    let doc = "Required C libraries" in
    let pp_value = Cmd.pp in
    B0_meta.Key.v "c-requires" ~doc ~pp_value

  let requires =
    let doc = "Required OCaml libraries" in
    let pp_value = Fmt.(box @@ list ~sep:sp Lib.Name.pp) in
    B0_meta.Key.v "ocaml-requires" ~doc ~pp_value

  let library =
    let pp_value = Fmt.using Lib.Name.to_string Fmt.string in
    B0_meta.Key.v "ocaml-library" ~doc:"Defined OCaml library name" ~pp_value

  let mod_srcs = (* FIXME don't do that. *)
    let pp_value = Fmt.any "<dynamic>" in
    B0_meta.Key.v "mod-srcs" ~doc:"Module sources" ~pp_value

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
  let* b = B0_store.get s B0_build.self in
  Fut.return (B0_unit.Set.fold find_need (B0_build.may_build b) None)

let wanted_code = B0_store.key (fun _ _ -> Fut.return `Auto)
let built_code =
  let of_wanted_code s m = function
  | #built_code as v -> Fut.return v
  | `Auto ->
      let* need = needed_code s m in
      match need with
      | Some need -> Fut.return need
      | None ->
          let* ocamlopt = B0_memo.tool_opt m Tool.ocamlopt in
          Fut.return @@ if Option.is_some ocamlopt then `Native else `Byte
  in
  let det s m =
    let* wanted = B0_store.get s wanted_code in
    of_wanted_code s m wanted
  in
  B0_store.key det

let conf : Conf.t B0_store.key =
  let conf_comp s m =
    let of_built_code = function
    | `Native | `All -> Tool.ocamlopt | `Byte -> Tool.ocamlc
    in
    Fut.map of_built_code (B0_store.get s built_code)
  in
  let det s m =
    let* comp = conf_comp s m in
    let file = Fpath.(B0_store.dir s / B0_memo.mark m) in
    Conf.write m ~comp ~o:file;
    Conf.read m file
  in
  B0_store.key ~mark:"ocaml.conf" det

let version b = Fut.map Conf.version (B0_build.get b conf)

(* Library resolution *)

let lib_of_unit b ocaml_conf u =
  (* TODO presence of archives should depend on built_code. *)
  B0_build.require b u;
  let m = B0_build.memo b in
  let build_dir = B0_build.build_dir b u in
  let name = B0_unit.get_meta Meta.library u |> B0_memo.fail_if_error m in
  let requires =
    B0_unit.get_meta Meta.requires u |> B0_memo.fail_if_error m
  in
  let archive = Lib.Name.to_archive_name name in
  let base = Fpath.(build_dir / archive) in
  let cma = Some Fpath.(base + ".cma") in
  let cmxa = Some Fpath.(base + ".cmxa") in
  let c_archive = Some Fpath.(base + (Conf.lib_ext ocaml_conf)) in
  let c_stubs = [] (* FIXME *) in
  let* srcs = B0_unit.get_meta Meta.mod_srcs u |> B0_memo.fail_if_error m in
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
  let js_stubs = [] (* FIXME *) in
  Fut.return @@
  Some (Lib.v ~name ~requires ~dir:build_dir ~cmis ~cmxs ~cma ~cmxa ~c_archive
          ~c_stubs ~js_stubs)

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
          B0_memo.notify (B0_build.memo b)
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
  let* b = B0_store.get store B0_build.self in
  let* ocaml_conf = B0_build.get b conf in
  let build_scope = lib_resolver_build_scope b ocaml_conf in
  let cache_dir =
    Fpath.(B0_build.shared_build_dir b / Lib.Resolver.cache_dir_name)
  in
(*  let ocamlpath = Lib.Resolver.ocamlpath ~cache_dir in *)
  let ocamlfind = Lib.Resolver.ocamlfind ~cache_dir in
  Fut.return @@
  Lib.Resolver.create m ocaml_conf [build_scope; (* ocamlpath; *) ocamlfind]

let lib_resolver = B0_store.key ~mark:"b0.ocamlib"  default_lib_resolver

(* Compile *)

let compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs =
  (* XXX Maybe better things could be done here once we have a good C domain. *)
  let obj_ext = Conf.obj_ext conf in
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
          B0_memo.notify m `Warn
            "@[<v>%a:@,File ignored. %s's compilation unit already defined \
             by file:@,%a:@]"
            Fpath.pp c cname Fpath.pp f;
          loop os cunits hs cs
  in
  let hs = B0_file_exts.(find_files (ext ".h") srcs) in
  let cs = B0_file_exts.(find_files (ext ".c") srcs) in
  let os = loop [] String.Map.empty hs cs in
  Fut.return os

let unit_code b m meta =
  let* built_code = B0_build.get b built_code in
  let _supported_code = B0_meta.find Meta.supported_code meta in
  let _needs_code = B0_meta.find Meta.needs_code meta in
  (* TODO *)
  Fut.return built_code

let exe_proc set_exe_path set_mod_srcs srcs b =
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_scope_dir b in
  let* srcs = B0_srcs.(Fut.map by_ext @@ select b srcs) in
  let* mod_srcs = Mod.Src.map_of_files m ~build_dir ~src_root ~srcs in
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get Meta.requires meta in  set_mod_srcs mod_srcs;
  let* unit_code = unit_code b m meta in
  let* conf = B0_build.get b conf in
  let* resolver = B0_build.get b lib_resolver in
  let* comp_requires = Lib.Resolver.get_list resolver requires in
  let exe_name = B0_meta.get B0_meta.exe_name meta in
  let exe_ext = Conf.exe_ext conf in
  let opts = Cmd.(arg "-g") (* TODO *) in
  let o = Fpath.(build_dir / (exe_name ^ exe_ext)) in
  set_exe_path o;  (* FIXME introduce a general mecanism for that *)
  let code = match unit_code with `All | `Native -> `Native |`Byte -> `Byte in
  let all_code = match unit_code with `All -> true | _ -> false in
  let comp = match unit_code with
  | `Native | `All -> Tool.ocamlopt | `Byte -> Tool.ocamlc
  in
  Compile.intfs ~and_cmti:true m ~comp ~opts ~requires:comp_requires ~mod_srcs;
  Compile.impls ~and_cmt:true m ~code ~opts ~requires:comp_requires ~mod_srcs;
  if all_code then begin
    Compile.impls
      ~and_cmt:false m ~code:`Byte ~opts ~requires:comp_requires ~mod_srcs;
  end;
  let* c_objs = compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs in
  let mod_srcs =
    Mod.Src.sort (* for link *) ~deps:Mod.Src.ml_deps mod_srcs
  in
  let* link_requires = Lib.Resolver.get_list_and_deps resolver requires in
  let archive ~code lib = match code with
  | `Byte -> (match Lib.cma lib with None -> [] | Some cma -> [cma])
  | `Native ->
      let add v l = match v with None -> l | Some v -> v :: l in
      add (Lib.cmxa lib) (add (Lib.c_archive lib) [])
  in
  let lib_objs = List.concat_map (archive ~code) link_requires in
  let cobjs = List.filter_map (Mod.Src.impl_file ~code) mod_srcs in
  let opts =
    let c_requires = B0_meta.get Meta.c_requires meta in
    Cmd.(opts %% (Cmd.list ~slip:"-ccopt" (Cmd.to_list c_requires)))
  in
  Link.code m ~conf ~code ~opts ~c_objs ~cobjs:(lib_objs @ cobjs) ~o;
  if all_code then begin
    let o = Fpath.(build_dir / (exe_name ^ ".byte" ^ exe_ext)) in
    let lib_objs = List.concat_map (archive ~code:`Byte) link_requires in
    let cobjs = List.filter_map (Mod.Src.impl_file ~code:`Byte) mod_srcs in
    Link.code m ~conf ~code:`Byte ~opts ~c_objs ~cobjs:(lib_objs @ cobjs) ~o
  end;
  Fut.return ()

let lib_proc set_mod_srcs srcs b =
  (* XXX we are still missing cmxs here *)
  let m = B0_build.memo b in
  let build_dir = B0_build.current_build_dir b in
  let src_root = B0_build.current_scope_dir b in
  let* srcs = B0_srcs.(Fut.map by_ext @@ select b srcs) in
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
  Compile.intfs ~and_cmti:true m ~comp ~opts ~requires ~mod_srcs;
  Compile.impls ~and_cmt:true m ~code ~opts ~requires ~mod_srcs;
  if all_code
  then (Compile.impls ~and_cmt:true m ~code:`Byte ~opts ~requires ~mod_srcs);
  let* c_objs = compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs in
  let mod_srcs = Mod.Src.sort (* for link *) ~deps:Mod.Src.ml_deps mod_srcs in
  let cobjs = List.filter_map (Mod.Src.impl_file ~code) mod_srcs  in
  let odir = build_dir and oname = archive_name in
  let has_cstubs = c_objs <> [] in
  if has_cstubs then Archive.cstubs m ~conf ~opts ~c_objs ~odir ~oname;
  let opts =
    let c_requires = B0_meta.get Meta.c_requires meta in
    Cmd.(opts %% (Cmd.list ~slip:"-ccopt" (Cmd.to_list c_requires)))
  in
  Archive.code m ~conf ~code ~opts ~has_cstubs ~cobjs ~odir ~oname;
  if all_code then begin
    let cobjs = List.filter_map (Mod.Src.impl_file ~code:`Byte) mod_srcs in
    Archive.code m ~conf ~code:`Byte ~opts ~has_cstubs ~cobjs ~odir ~oname
  end;
  Fut.return ()

let exe
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty) ?action
    ?(c_requires = Cmd.empty) ?(requires = []) ?name exe_name ~srcs
  =
  let name = Option.value ~default:exe_name name in
  let mod_srcs, set_mod_srcs = Fut.create () in
  let exe_path, set_exe_path = Fut.create () in
  let meta =
    meta
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.exe
    |> B0_meta.add B0_meta.exe_name exe_name
    |> B0_meta.add Meta.c_requires c_requires
    |> B0_meta.add Meta.requires requires
    |> B0_meta.add Meta.mod_srcs mod_srcs
    |> B0_meta.add B0_meta.exe_file exe_path
  in
  let action = match action with None -> B0_unit.Action.exec | Some a -> a in
  let proc = wrap (exe_proc set_exe_path set_mod_srcs srcs) in
  B0_unit.v ?doc ~meta ~action name proc

let lib
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty) ?action
    ?(c_requires = Cmd.empty) ?(requires = []) ?name lib_name ~srcs
  =
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
    |> B0_meta.add Meta.c_requires c_requires
    |> B0_meta.add Meta.requires requires
    |> B0_meta.add Meta.mod_srcs mod_srcs
  in
  let proc = wrap (lib_proc set_mod_srcs srcs) in
  B0_unit.v ?doc ~meta ?action name proc

let () = B0_def.Scope.close ()
