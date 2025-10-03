(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "ocaml"

let ocaml_tag = B0_meta.Key.make_tag "tag" ~doc:"OCaml related entity"

open B0_std
open Fut.Syntax

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

module Code = struct
  type t = Byte | Native | Wasm
  let pp ppf = function
  | Byte -> Fmt.code ppf "byte"
  | Native -> Fmt.code ppf "native"
  | Wasm -> Fmt.code ppf "wasm"

  module T = struct type nonrec t = t let compare = Stdlib.compare end
  module Set = struct
    include Set.Make (T)

    let to_list = elements (* only in >= 5.1 *)
    let set_iter = iter
    let pp_human ppf set = match cardinal set with
    | 0 -> Fmt.string ppf "no code"
    | n ->
        let code = Fmt.cardinal ~one:(Fmt.any "code") () in
        Fmt.pf ppf "@[%a@ %a@]"
          code n Fmt.(iter ~sep:comma set_iter pp) set

    let pp = Fmt.(box ~indent:1 @@ braces (iter ~sep:comma set_iter pp))

  end
  let none = Set.empty
  let byte = Set.singleton Byte
  let native = Set.singleton Native
  let wasm = Set.singleton Wasm
  let traditional = Set.of_list [Byte; Native]
  let all = Set.of_list [Byte; Native; Wasm]

  type wanted = Auto | Wanted of t list
  let pp_wanted ppf = function
  | Auto -> Fmt.string ppf "auto"
  | Wanted wanted -> Set.pp ppf (Set.of_list wanted)

  type restrict = string * (Set.t -> Set.t)

  let restrict =
    let doc = "Built codes restrictions" in
    let pp_value = Fmt.(using fst string) in
    let default = "No restriction", Fun.id in
    B0_meta.Key.make "code-restrict" ~default ~doc ~pp_value

  let unique_favour_native =
    "Unique code and favour native",
    fun built ->
      if Set.mem Native built then native else
      (* This will be Byte if it's in the set. *)
      Set.singleton (Set.min_elt built)

  (* Store keys *)

  let get_restrictions available store memo =
    let find_restrictions u acc =
      if not (B0_unit.(has_tag ocaml_tag u && has_tag B0_meta.exe u))
      then acc else
      let _doc, restrict = B0_unit.find_or_default_meta restrict u in
      Set.union acc (restrict available)
    in
    let* build = B0_store.get store B0_build.self in
    let must = B0_build.must_build build in
    let built = B0_unit.Set.fold find_restrictions must Set.empty in
    if Set.is_empty built (* no exe *)
    then Fut.return available
    else Fut.return built

  let get_available store memo =
    (* FIXME we should rely on Conf.t, however it's a bit unclear whether
       we want to rely on the output of -config or probe executables. *)
    let* ocamlopt = B0_memo.tool_opt memo Tool.ocamlopt in
    let set = Set.singleton Byte (* Really always available ? *) in
    Fut.return (if Option.is_some ocamlopt then Set.add Native set else set)

  let wanted = B0_store.key (fun _ _ -> Fut.return Auto)
  let built =
    let det store memo =
      let* wanted = B0_store.get store wanted in
      let* available = get_available store memo in
      match wanted with
      | Auto -> get_restrictions available store memo
      | Wanted codes ->
          let codes = Set.of_list codes in
          let unavailable = Set.diff codes available in
          if Set.is_empty unavailable then Fut.return codes else
          B0_memo.fail memo
            "OCaml %a requested but not available."
            Set.pp_human unavailable
    in
    B0_store.key det

  let check_any ~supported ~by:build =
    let* built = B0_build.get build built in
    if not (Set.is_empty (Set.inter supported built)) then Fut.return () else
    let memo = B0_build.memo build in
    B0_memo.fail memo
      "@[<v>@[Not@ buildable.@ The@ build@ procedure@ supports@ %a@ \
       but@ the@ build@ has@ %a.@ \
       @[Try@ to@ add@ %a %a@ to@ the@ build.@]@]"
      Set.pp_human supported Set.pp_human built
      Fmt.code "-u" Fmt.code (B0_unit.name (B0_build.current build))

  let check_all ~supported ~by:build =
    let* built = B0_build.get build built in
    if not (Set.subset supported built) then Fut.return () else
    let memo = B0_build.memo build in
    (* FIXME the error advice here may not be sufficient but we don't have
       the ui for that yet. *)
    B0_memo.fail memo
      "@[<v>@[Not@ buildable.@ The@ build@ procedure@ requires@ %a@ \
       but@ the@ build@ has@ %a.@ \
       @[Try@ to@ add@ %a %a@ to@ the@ build.@]@]"
      Set.pp_human supported Set.pp_human built
      Fmt.code "-u" Fmt.code (B0_unit.name (B0_build.current build));
end

module Conf = struct
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
    let err_key k = err "key %a not found." Fmt.code k in
    let find k fs = match String.Map.find_opt k fs with
    | None -> err_key k | Some v -> v
    in
    let version =
      let v = find "version" fields in
      match B0_version.of_string v with
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
      | None -> err "key %a cound not parse bool from %S" Fmt.code k s
      | Some b -> b
    in
    Ok { fields; version; where; asm_ext; dll_ext; exe_ext; lib_ext; obj_ext;
         has_dynlink; }
  with
  | Failure e -> Error e

  (* IO *)

  let of_string ?(file = Fpath.dash) s =
    let parse_line _ acc l = match String.split_first ~sep:":" l with
    | None -> acc (* XXX report an error *)
    | Some (k, v) -> String.Map.add (String.trim k) (String.trim v) acc
    in
    try
      let s = String.trim s and strip_newlines = true in
      let fields =
        String.fold_ascii_lines ~strip_newlines parse_line String.Map.empty s
      in
      Ok (of_string_map fields |> Result.error_to_failure)
    with Failure e -> Fpath.error file "OCaml config: %s" e

  let write m ~comp ~o =
    let comp = B0_memo.tool m comp in
    B0_memo.spawn m ~writes:[o] ~stdout:(`File o) @@
    comp (Cmd.arg "-config")

  let read m file =
    let* s = B0_memo.read m file in
    Fut.return (of_string ~file s |> B0_memo.fail_if_error m)

  let key : t B0_store.key =
    let conf_comp s m =
      let of_built_code built =
        if Code.Set.mem Native built then Tool.ocamlopt else Tool.ocamlc
      in
      Fut.map of_built_code (B0_store.get s Code.built)
    in
    let det s m =
      let* comp = conf_comp s m in
      let file = Fpath.(B0_store.dir s / B0_memo.mark m) in
      write m ~comp ~o:file;
      read m file
    in
    B0_store.key ~mark:"ocaml.conf" det

  let version' build = Fut.map version (B0_build.get build key)
end

(* Modules *)

module Modname = struct
  type t = string
  let of_path_filename f =
    String.Ascii.capitalize (Fpath.basename ~strip_exts:true f)

  let v n = String.Ascii.capitalize n
  let equal = String.equal
  let compare = String.compare
  let pp = Fmt.st [`Bold]
  module Set = String.Set
  module Map = String.Map

  (* Filename mangling *)

  let mangle_filename s =
    let rem_ocaml_ext s = match String.split_last ~sep:"." s with
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
          Bytes.set b (i    ) (Char.Ascii.upper_hex_digit_of_int (c lsr 4));
          Bytes.set b (i + 1) (Char.Ascii.upper_hex_digit_of_int (c      ));
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

module Modref = struct
  type t = Modname.t * Digest.t
  let make n d = (String.Ascii.capitalize n, d)
  let name = fst
  let digest = snd
  let equal (_, d0) (_, d1) = Digest.equal d0 d1
  let compare (n0, d0) (n1, d1) = match Modname.compare n0 n1 with
  | 0 -> Digest.compare d0 d1
  | c -> c

  let pp ppf (n, d) = Fmt.pf ppf "@[%s %a@]" (Digest.to_hex d) Modname.pp n

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

module Modsrc = struct
  module Deps = struct
    let of_string ?(file = Fpath.dash) ?src_root s =
      (* Parse ocamldep's [-slash -modules], a bit annoying to parse.
         ocamldep shows its Makefile legacy. *)
      let parse_path n p = (* ocamldep escapes spaces as "\ ", a bit annoying *)
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
        match String.split_last (* last, windows drives *) ~sep:":" line with
        | None -> Fmt.failwith_line n " cannot parse line: %S" line
        | Some (file, mods) ->
            let file = parse_path n file in
            let file = match src_root with
            | None -> file
            | Some src_root -> Fpath.(src_root // file)
            in
            let add_mod acc m = Modname.Set.add m acc in
            let mods = String.split_all ~drop:String.is_empty ~sep:" " mods in
            let start = Modname.Set.singleton "Stdlib" in
            let mods = List.fold_left add_mod start mods in
            Fpath.Map.add file mods acc
      in
      try
        let strip_newlines = true and parse = parse_line ~src_root in
        Ok (String.fold_ascii_lines ~strip_newlines parse Fpath.Map.empty s)
      with
      | Failure e -> Fpath.error file "%s" e

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
    { modname : Modname.t;
      opaque : bool;
      mli : Fpath.t option;
      mli_deps : Modname.Set.t;
      ml : Fpath.t option;
      ml_deps : Modname.Set.t;
      build_dir : Fpath.t;
      build_base : Fpath.t }

  let make ~modname ~opaque ~mli ~mli_deps ~ml ~ml_deps ~build_dir =
    let build_base = Fpath.(build_dir / String.Ascii.uncapitalize modname) in
    { modname; opaque; mli; mli_deps; ml; ml_deps; build_dir; build_base }

  let modname m = m.modname
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
    let deps = Modname.Set.pp ~sep:Fmt.sp Fmt.string in
    Fmt.record Fmt.[
        field "modname" modname Modname.pp;
        field "opaque" opaque bool;
        field "mli" mli path_option;
        field "mli-deps" mli_deps deps;
        field "ml" ml path_option;
        field "ml-deps" ml_deps deps;
        field "build-dir" build_dir Fpath.pp_unquoted ]

  let impl_file ~code m = match code with
  | Code.Byte -> cmo_file m
  | Code.Native -> cmx_file m
  | Code.Wasm -> assert false


  let as_intf_dep_files ?(init = []) m = cmi_file m :: init
  let as_impl_dep_files ?(init = []) ~code m = match code with
  | Code.Byte -> cmi_file m :: init
  | Code.Native ->
      begin match ml m with
      | None -> cmi_file m :: init
      | Some _ when m.opaque -> cmi_file m :: init
      | Some _ -> cmi_file m :: Option.get (cmx_file m) :: init
      end
  | Code.Wasm -> assert false

  let modname_map m ~kind files =
    let add acc f =
      let mname = Modname.of_path_filename f in
      match Modname.Map.find_opt mname acc with
      | None -> Modname.Map.add mname f acc
      | Some f' ->
          B0_memo.notify m `Warn
            "@[<v>%a:@,File ignored. %a's module %s defined by file:@,%a:@]"
            Fpath.pp f Modname.pp mname kind Fpath.pp f';
          acc
    in
    List.fold_left add Modname.Map.empty files

  let map_of_srcs m ~build_dir ~srcs ~src_deps  =
    let get_src_deps = function
    | None -> Modname.Set.empty
    | Some file ->
        match Fpath.Map.find file src_deps with
        | exception Not_found -> Modname.Set.empty
        | deps -> deps
    in
    let mlis, mls = List.partition (Fpath.has_ext ".mli") srcs in
    let mlis = modname_map m ~kind:"interface" mlis in
    let mls = modname_map m ~kind:"implementation" mls in
    let mod' modname mli ml =
      let mli_deps = get_src_deps mli in
      let ml_deps = get_src_deps ml in
        let opaque = false in
      Some (make ~modname ~opaque ~mli ~mli_deps ~ml ~ml_deps ~build_dir)
    in
    Modname.Map.merge mod' mlis mls

  let sort ?stable ~deps name_map =
    (* FIXME do something better, on cycles this lead to link failure
         we should detect it. *)
    let rec loop seen acc = function
    | [] -> seen, acc
    | src :: srcs ->
        if Modname.Set.mem src.modname seen then loop seen acc srcs else
        let seen = Modname.Set.add src.modname seen in
        let add_src_dep n acc = match Modname.Set.mem n seen with
        | true -> acc
        | false ->
            match Modname.Map.find_opt n name_map with
            | None -> acc
            | Some src -> src :: acc
        in
        let deps = Modname.Set.fold add_src_dep (deps src) [] in
        let seen, acc = loop seen acc deps in
        loop seen (src :: acc) srcs
    in
    let add_src _ src acc = src :: acc in
    let stable = Option.value ~default:[] stable in
    let todo = stable @ Modname.Map.fold add_src name_map [] in
    let _, acc = loop Modname.Set.empty [] todo in
    List.rev acc

  let find ns map =
    let rec loop res remain deps = match Modname.Set.choose deps with
    | exception Not_found -> res, remain
    | dep ->
        let deps = Modname.Set.remove dep deps in
        match Modname.Map.find dep map with
        | m -> loop (m :: res) remain deps
        | exception Not_found ->
            loop res (Modname.Set.add dep remain) deps
    in
    loop [] Modname.Set.empty ns

  let map_of_files ?(only_mlis = false) m ~build_dir ~src_root ~srcs =
    let exts = ".mli" :: if only_mlis then [] else [".ml"] in
    let exts = B0_file_exts.make exts in
    let srcs = B0_file_exts.find_files exts srcs in
    let o = Fpath.(build_dir / "ocaml-srcs.deps") in
    Deps.write m ~src_root ~srcs ~o;
      let* src_deps = Deps.read m ~src_root o in
      Fut.return (map_of_srcs m ~build_dir ~srcs ~src_deps)
end

(* Libraries *)

module Libname = struct
  open Result.Syntax

  (* Note. As it stands library name dots are represented by
     [Fpath.dir_sep_char].  Not sure it makes sense it was done at
     some point while seeking a simpler library model so that we could
     directly lookup in directories without having to
     convert. Review. *)

  type t = { name : Fpath.t }

  let fpath_to_name ?(sep = '.') s =
    let b = Bytes.of_string (Fpath.to_string s) in
    for i = 0 to Bytes.length b - 1 do
      if Bytes.get b i = Fpath.natural_dir_sep_char then Bytes.set b i sep;
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
        | '.' ->
            Bytes.set b i Fpath.natural_dir_sep_char;
            loop (i + 1) ~id_start:true
        | c -> err s (Fmt.str "illegal character %C" c)
        end
    in
    loop 0 ~id_start:true

  let basename n =
    let n = Fpath.to_string n.name in
    match String.split_last ~sep:Fpath.natural_dir_sep n with
    | None -> n | Some (_, n) -> n

  let name n = fpath_to_name n.name

  let root n =
    let n = Fpath.to_string n.name in
    match String.split_first ~sep:Fpath.natural_dir_sep n with
    | None -> n | Some (n, _) -> n

  let segments n = Fpath.to_segments n.name
  let to_archive_name n = fpath_to_name ~sep:'_' n.name
  let undot ~rep n = fpath_to_name ~sep:rep n.name

  let of_string s =
    let* name = name_to_fpath s in
    Ok { name }

  let to_string n = fpath_to_name n.name
  let to_fpath { name } = name
  let v s = match of_string s with Ok n -> n | Error e -> invalid_arg e
  let equal n0 n1 = Fpath.equal n0.name n1.name
  let compare n0 n1 = Fpath.compare n0.name n1.name
  let pp = Fmt.using to_string Fmt.code

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

let libname = Libname.v

(* Metadata *)

let tag = ocaml_tag

let c_requires =
  let doc = "Required C libraries" in
  let pp_value = Cmd.pp in
  B0_meta.Key.make "c-requires" ~default:Cmd.empty ~doc ~pp_value

let library =
  let doc = "Defined OCaml library name" in
  let pp_value = Fmt.using Libname.to_string Fmt.string in
  B0_meta.Key.make "library" ~doc ~pp_value

let modsrcs = (* FIXME don't do that. *)
  let pp_value = Fmt.any "<dynamic>" in
  B0_meta.Key.make "mod-srcs" ~doc:"Module sources" ~pp_value

let pp_libname_list = Fmt.(box @@ list ~sep:sp Libname.pp)

let requires =
  let doc = "Required OCaml libraries" in
  let pp_value = pp_libname_list in
  B0_meta.Key.make "requires" ~default:[] ~doc ~pp_value

let exports =
  let doc = "(Re-)exported OCaml libraries" in
  let pp_value = pp_libname_list in
  B0_meta.Key.make "exports" ~default:[] ~doc ~pp_value

module Lib = struct
  type t =
    { libname : Libname.t;
      requires : Libname.t list;
      exports : Libname.t list;
      dir : Fpath.t;
      cmis : Fpath.t list;
      cmxs : Fpath.t list;
      cma : Fpath.t option;
      cmxa : Fpath.t option;
      c_archive : Fpath.t option;
      c_stubs : Fpath.t list;
      js_stubs : Fpath.t list;
      warning : string option }

  let make
      ~libname ~requires ~exports ~dir ~cmis ~cmxs ~cma ~cmxa ~c_archive
      ~c_stubs ~js_stubs ~warning
    =
    { libname; requires; exports; dir; cmis; cmxs; cma; cmxa; c_archive;
      c_stubs; js_stubs; warning }

  let of_dir m
      ~clib_ext ~libname ~requires ~exports ~dir ~archive ~js_stubs ~warning
    =
    Result.map_error (fun e -> Fmt.str "Library %a: %s" Libname.pp libname e) @@
    Result.bind (Os.Dir.exists dir) @@ function
    | false ->
        B0_memo.notify m `Warn "Library %a: %a: no such directory"
          Libname.pp libname Fpath.pp dir;
        Ok (make ~libname ~requires ~exports ~dir ~cmis:[] ~cmxs:[]
              ~cma:None ~cmxa:None ~c_archive:None ~c_stubs:[] ~js_stubs:[]
              ~warning)
    | true ->
        Result.bind (Os.Dir.fold_files ~recurse:false Os.Dir.path_list dir [])
        @@ fun fs ->
        let js_stubs = List.map (fun f -> Fpath.(dir // f)) js_stubs in
        let () = B0_memo.ready_files m js_stubs in
        let rec loop cmis cmxs cma cmxa c_archive c_stubs = function
        | [] ->
            make ~libname ~requires ~exports ~dir ~cmis ~cmxs ~cma ~cmxa
              ~c_archive ~c_stubs ~js_stubs ~warning
        | f :: fs ->
            let is_lib_archive f = match archive with
            | None -> false
            | Some a -> String.equal (Fpath.basename ~strip_exts:true f) a
            in
            match Fpath.get_ext ~multi:false f with
            | ".cmi" ->
                B0_memo.ready_file m f;
                loop (f :: cmis) cmxs cma cmxa c_archive c_stubs fs
            | ".cmx" ->
                B0_memo.ready_file m f;
                loop cmis (f :: cmxs) cma cmxa c_archive c_stubs fs
            | ".cma" ->
                let cma = match is_lib_archive f with
                | true -> B0_memo.ready_file m f; Some f
                | false -> cma
                in
                loop cmis cmxs cma cmxa c_archive c_stubs fs
            | ".cmxa" ->
                let cmxa = match is_lib_archive f with
                | true -> B0_memo.ready_file m f; Some f
                | false -> cmxa
                in
                loop cmis cmxs cma cmxa c_archive c_stubs fs
            | ext when String.equal ext clib_ext ->
                (* XXX note that this won't get us the stub dlls which
                   are not in the library directory. *)
                B0_memo.ready_file m f;
                let c_archive, c_stubs = match is_lib_archive f with
                | true -> Some f, c_stubs
                | false -> c_archive, (f :: c_stubs)
                in
                loop cmis cmxs cma cmxa c_archive c_stubs fs
            | _ ->
                loop cmis cmxs cma cmxa c_archive c_stubs fs
        in
        Ok (loop [] [] None None None [] fs)

  let key : t Fut.t B0_meta.key =
    let pp_value = Fmt.any "<dynamic>" in
    B0_meta.Key.make "lib-def" ~doc:"OCaml library definition" ~pp_value

  let of_unit b u =
    B0_build.require_unit b u;
    let m = B0_build.memo b in
    let lib = B0_unit.get_meta key u |> B0_memo.fail_if_error m in
    Fut.map Option.some lib

  let libname l = l.libname
  let requires l = l.requires
  let exports l = l.exports
  let dir l = l.dir
  let cmis l = l.cmis
  let cmxs l = l.cmxs
  let cma l = l.cma
  let cmxa l = l.cmxa
  let c_archive l = l.c_archive
  let c_stubs l = l.c_stubs
  let js_stubs l = l.js_stubs
  let warning l = l.warning
  let handle_warning m l = match l.warning with
  | None -> ()
  | Some w ->
      B0_memo.notify m `Warn "@[<v>Library %a: %a@]"
        Libname.pp l.libname Fmt.lines w
end

(* Library resolvers *)

module Libresolver = struct

  (* FIXME rework erroring, for now we are not using the mecanisms
     and they likely need to be tweaked. *)

  (* Resolution scopes *)

  module Scope = struct
    type find = Conf.t -> B0_memo.t -> Libname.t -> Lib.t option Fut.t
    type suggest = Conf.t -> B0_memo.t -> Libname.t -> string list option Fut.t
    type t = { name : string; find : find; suggest : suggest; }

    let name s = s.name
    let find s = s.find
    let suggest s = s.suggest
    let make ~name ~find ~suggest = { name; find; suggest }

    module Ocamlpath = struct
      (* Stubbed at the moment *)
      let find ~cache_dir conf m n = Fut.return None
      let suggest conf m n = Fut.return None
      let scope ~cache_dir =
        let find = find ~cache_dir in
        { name = "OCAMLPATH"; find; suggest }
    end

    module Ocamlfind = struct
      let tool =
        let vars =
          (* From https://projects.camlcity.org/projects/dl/findlib-1.9.6/\
             doc/ref-html/r865.html#AEN897 *)
          [ "OCAMLFIND_CONF"; "OCAMLFIND_TOOLCHAIN"; "OCAMLPATH";
            "OCAMLFIND_DESTDIR"; "OCAMLFIND_METADIR"; "OCAMLFIND_COMMANDS";
            "CAMLLIB"; "OCAMLLIB"; "OCAMLFIND_LDCONF";
            "OCAMLFIND_IGNORE_DUPS_IN"; ]
        in
        B0_memo.Tool.by_name ~vars "ocamlfind"

      let lib_info_query =
        (* We use %A otherwise whith %a we get a blank line if there's
           no archive. Technically though we only support single library
           archives *)
        "%m:%d:%A:%(requires):%(exports):%(jsoo_runtime):%(warning)"

      let parse_field field parse s = match parse s with
      | Error e -> Fmt.failwith "%s: %s" field e
      | Ok v -> v

      let parse_requires = function
      | "" -> []
      | requires ->
          let to_libname = parse_field "required library" Libname.of_string in
          (* ocamlfind does not normalize *)
          let skip_ws = String.drop_first_while Char.Ascii.is_white in
          let get_tok =
            String.cut_first_while (Fun.negate Char.Ascii.is_white)
          in
          let rec rev_toks acc s =
            let s = skip_ws s in
            match get_tok s with
            | "", rest -> if rest = "" then acc else rest :: acc (* will err *)
            | tok, rest -> rev_toks (tok :: acc) rest
          in
          List.rev_map to_libname (rev_toks [] requires)

      let parse_exports = parse_requires
      let parse_archive = function
      | "" -> None
      | a ->
          match String.split_last ~sep:"." a with
          | None -> Some a | Some (a, _ext) -> Some a

      let parse_dir dir = parse_field "library directory" Fpath.of_string dir
      let parse_js_stubs js_stubs =
        let to_path s = parse_field "js stubs" Fpath.of_string s in
        let stubs = String.split_all ~drop:String.is_empty ~sep:"," js_stubs in
        List.map to_path stubs

      let parse_warning = function "" -> None | w -> Some w

      let get_meta_file data = match String.split_first ~sep:":" data with
      | None -> None
      | Some (m, _) -> Result.to_option (Fpath.of_string m)

      let lib_of_info m ~conf ~libname ~file info =
        let clib_ext = Conf.lib_ext conf in
        try match String.split_on_char ':' (String.trim info) with
        | [_meta_file; dir; archive; requires; exports; js_stubs; warning] ->
            let requires = parse_requires requires in
            let exports = parse_exports exports in
            let archive = parse_archive archive in
            let dir = parse_dir dir in
            let js_stubs = parse_js_stubs js_stubs in
            let warning = parse_warning warning in
            Lib.of_dir m
              ~clib_ext ~libname ~requires ~exports
              ~dir ~archive ~js_stubs ~warning
        | _ -> Fmt.failwith "could not parse %S" info
        with
        | Failure e -> Fpath.error file "%s" e

      let query_result _o _set_res op = match B0_zero.Op.status op with
      | B0_zero.Op.Success ->
          (* Ideally we could launch the read of the [o] file here.
             Once we get effects we might. For now we are not
             allowed to use memo. So we use the `Tee hack and don't use
             set_res because it may trigger memo (the op's k does it). *)
          let spawn = B0_zero.Op.Spawn.get op in
          begin match Option.get (B0_zero.Op.Spawn.exit spawn) with
          | `Exited 0 ->
              begin match B0_zero.Op.Spawn.stdo_ui spawn with
              | None | Some Error _ ->
                  B0_zero.Op.disable_reviving op
              | Some Ok data ->
                  match get_meta_file data with
                  | None ->
                      B0_zero.Op.disable_reviving op
                  | Some _file ->
                      ()
                      (*
                      let reads = file :: B0_zero.Op.reads op in
                      B0_zero.Op.set_reads op reads; *)
              end;
              B0_zero.Op.Spawn.set_stdo_ui spawn None;
          | `Exited 2 ->
              (* It could become available in another run and we don't
                 have a file-based way to invalidate the cache. *)
              B0_zero.Op.Spawn.set_stdo_ui spawn None;
              B0_zero.Op.disable_reviving op
          | _ -> ()
          end
      | _ -> ()

      let write_info ~cache_dir conf m name =
        (* FIXME better [name] not found error
           FIXME need to solve the META file read.
           FIXME post exec is still super messy, check if we can make it
           to use Memo.t
           FIXME why this mess ?
        *)
        let ocamlfind = B0_memo.tool m tool in
        let fname, lib, predicates = match Libname.to_string name with
        | "ocaml.threads" | "threads" | "threads.posix" ->
            if Conf.version conf < (5, 0, 0, None)
            then "threads", "threads.posix", "byte,native,mt,mt_posix"
            else "threads", "threads", "byte,native"
        | n -> n, n, "byte,native"
        in
        let fname = Fmt.str "ocamlfind.%s" fname in
        let o = Fpath.(cache_dir / fname) in
        let stdout = `Tee (* hack *) o and stderr = `File Fpath.null in
        let res, set_res = Fut.make () in
        let post_exec = query_result o set_res in
        let success_exits = [0; 2 (* not found *) ] in
        let k _ = function (* Ideally we could do that in post_exec *)
        | 0 -> set_res (Some o)
        | 2 -> set_res None | _ -> assert false
        in
        B0_memo.spawn
          m ~reads:[] ~writes:[o] ~stdout ~stderr ~post_exec ~success_exits ~k
        @@
        ocamlfind Cmd.(arg "query" % lib % "-predicates" % predicates %
                       "-format" % lib_info_query);
        res

      let read_info m conf libname file =
        let* info = B0_memo.read m file in
        let lib = lib_of_info m ~conf ~libname ~file info in
        Fut.return (Some (B0_memo.fail_if_error m lib))

      let find ~cache_dir conf m name =
        let* outf = write_info ~cache_dir conf m name in
        match outf with
        | None -> Fut.return None
        | Some o -> read_info m conf name o

      let suggest conf m name = Fut.return None (* TODO *)
      let scope ~cache_dir =
        let find = find ~cache_dir in
        { name = "ocamlfind"; find; suggest }
    end

    module Build = struct
      let libs_in_build
          b ~conf : (B0_unit.t * (Lib.t option Fut.t Lazy.t)) Libname.Map.t
        =
        let add u acc = match B0_unit.find_meta library u with
        | None -> acc
        | Some lib_name ->
            match Libname.Map.find_opt lib_name acc with
            | None ->
                let lib = lazy (Lib.of_unit b u) in
                Libname.Map.add lib_name (u, lib) acc
            | Some (lib_u, _) ->
                B0_memo.notify (B0_build.memo b)
                  `Warn "@[Library %a: already defined in unit %a.@,\
                         Ignoring definition in unit %a@]"
                  Libname.pp lib_name B0_unit.pp_name lib_u B0_unit.pp_name u;
                acc
        in
        B0_unit.Set.fold add (B0_build.may_build b) Libname.Map.empty

      let scope b conf =
        let name = "build" in
        let libs_in_build = libs_in_build b ~conf in
        let find ocaml_conf m n =
          match Libname.Map.find_opt n libs_in_build with
          | None -> Fut.return None
          | Some (_, lazy lib) -> lib
        in
        let suggest ocaml_conf m n = Fut.return None (* TODO *) in
        make ~name ~find ~suggest
    end

    let cache_dir_name = "ocaml-libresolver"
    let ocamlpath = Ocamlpath.scope
    let ocamlfind = Ocamlfind.scope
    let build = Build.scope
  end

  type t =
    { memo : B0_memo.t;
      conf : Conf.t;
      scopes : Scope.t list;
      mutable lookups : Lib.t option Fut.t Libname.Map.t; }

  let make memo conf scopes =
    let memo = B0_memo.with_mark memo "ocaml-libresolver" in
    { memo; conf; scopes; lookups = Libname.Map.empty }

  let default store m =
    let* b = B0_store.get store B0_build.self in
    let* ocaml_conf = B0_build.get b Conf.key in
    let build_scope = Scope.build b ocaml_conf in
    let cache_dir = Fpath.(B0_build.shared_dir b / Scope.cache_dir_name)in
    (*  let ocamlpath = Lib.Resolver.ocamlpath ~cache_dir in *)
    let ocamlfind = Scope.ocamlfind ~cache_dir in
    let scopes = [build_scope; (* ocamlpath; *) ocamlfind] in
    let resolver = make m ocaml_conf scopes in
    Fut.return resolver

  let key = B0_store.key ~mark:"ocaml.libresolver" default

  (* Properties *)

  let memo r = r.memo
  let lookups r = r.lookups
  let ocaml_conf r = r.conf
  let scopes r = r.scopes

  (* Lookups *)

  let find_in_scopes r set name =
    let rec loop r set name = function
    | [] -> set None
    | s :: ss ->
        Fut.await (Scope.find s r.conf r.memo name) @@ function
        | None -> loop r set name ss
        | Some _ as lib -> set lib
    in
    loop r set name r.scopes

  let find m r name = match Libname.Map.find_opt name r.lookups with
  | Some v -> v
  | None ->
      let fut, set = Fut.make () in
      r.lookups <- Libname.Map.add name fut r.lookups;
      find_in_scopes r set name;
      fut

  let get m r name =
    let* lib = find m r name in
    match lib with
    | None -> B0_memo.fail m "@[Library %a: Not found@]" Libname.pp name
    | Some lib -> Fut.return lib

  let get_list_and_exports m r ns =
    (* In the future we want to get `requires` aswell to
       get them into -H options (OCaml >= 5.2) *)
    let rec loop seen acc = function
    | [] -> Fut.return (seen, acc)
    | l :: ls ->
        if Libname.Set.mem l seen then loop seen acc ls else
        let seen = Libname.Set.add l seen in
        let* lib = get m r l in
        let () = Lib.handle_warning m lib in
        let not_seen n = not (Libname.Set.mem n seen) in
        let exports = List.filter not_seen (Lib.exports lib) in
        let* seen, acc = loop seen acc exports in
        loop seen (lib :: acc) ls
    in
    let* _, libs = loop Libname.Set.empty [] ns in
    Fut.return (List.rev libs)

  let get_list_and_deps m r ns =
    let rec loop seen acc = function
    | [] -> Fut.return (seen, acc)
    | l :: ls  ->
        if Libname.Set.mem l seen then loop seen acc ls else
        let seen = Libname.Set.add l seen in
        let* lib = get m r l in
        let deps = Lib.exports lib in
        let* seen, acc = loop seen acc deps in
        let deps = Lib.requires lib in
        let* seen, acc = loop seen acc deps in
        loop seen (lib :: acc) ls
    in
    let* _, libs = loop Libname.Set.empty [] ns in
    Fut.return (List.rev libs)
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
    let writes = [o] in
    B0_memo.spawn m ?post_exec ?k ~reads:(c :: reads) ~writes ~cwd @@
    (B0_memo.tool m comp)
      Cmd.(arg "-c" %% opts %% unstamp (incs %% path c));
    writes

  let mli_to_cmi ?post_exec ?k ~and_cmti m ~comp ~opts ~reads ~mli ~o =
    let base = Fpath.strip_ext ~multi:false o in
    let stamp = Fpath.basename base in
    let reads = mli :: reads in
    let writes = o :: if and_cmti then [Fpath.(base + ".cmti")] else [] in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmti (Cmd.arg "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path mli)) in
    B0_memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    (B0_memo.tool m comp) Cmd.(arg "-c" %% bin_annot %% opts % "-o" %% io);
    writes

  let ml_to_cmo ?post_exec ?k ~and_cmt m ~opts ~reads ~has_cmi ~ml ~o =
    let ocamlc = B0_memo.tool m Tool.ocamlc in
    let base = Fpath.strip_ext ~multi:false o in
    let stamp = Fpath.basename base (* output depends on mod name *) in
    let reads = ml :: reads in
    let writes =
      o :: (List.cons_if and_cmt Fpath.(base + ".cmt") @@
            List.cons_if (not has_cmi) Fpath.(base + ".cmi") [])
    in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmt (Cmd.arg "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path ml)) in
    B0_memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlc Cmd.(arg "-c" %% bin_annot %% opts % "-o" %% io);
    writes

  let ml_to_cmx ?post_exec ?k ~and_cmt m ~opts ~reads ~has_cmi ~ml ~o =
    let ocamlopt = B0_memo.tool m Tool.ocamlopt in
    let base = Fpath.strip_ext ~multi:false o in
    let stamp = Fpath.basename base (* output depends on mod name *) in
    let reads = ml :: reads in
    let writes =
      o :: Fpath.(base + ".o") ::
      (List.cons_if and_cmt Fpath.(base + ".cmt") @@
       List.cons_if (not has_cmi) Fpath.(base + ".cmi") [])
    in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmt (Cmd.arg "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path ml)) in
    B0_memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlopt Cmd.(arg "-c" %% bin_annot %% opts % "-o" %% io);
    writes

  let ml_to_impl ?post_exec ?k m ~code ~opts ~reads ~has_cmi ~ml ~o ~and_cmt =
    let ml_to_obj = match code with
    | Code.Byte -> ml_to_cmo | Code.Native -> ml_to_cmx
    | Code.Wasm -> assert false
    in
    ml_to_obj ?post_exec ?k m ~opts ~reads ~has_cmi ~ml ~o ~and_cmt

  (* Modsrc convenience *)

  let modsrc_intf ~and_cmti m ~comp ~opts ~requires ~modsrcs src =
    match Modsrc.mli src with
    | None -> []
    | Some mli ->
        let o = Modsrc.cmi_file src in
        let deps = Modsrc.mli_deps src in
        let src_deps, _remain = Modsrc.find deps modsrcs in
        let add_src_dep_objs acc dep = Modsrc.as_intf_dep_files ~init:acc dep
        in
        let src_deps_objs = List.fold_left add_src_dep_objs [] src_deps in
        let ext_objs =
          (* XXX could be more precise with [_remain] *)
          let add_lib acc l = List.rev_append (Lib.cmis l) acc in
          List.fold_left add_lib [] requires
        in
        let reads = List.rev_append src_deps_objs ext_objs in
        mli_to_cmi ~and_cmti m ~comp ~opts ~reads ~mli ~o

  let modsrc_impl ~and_cmt m ~code ~opts ~requires ~modsrcs src =
    match Modsrc.ml src with
    | None -> []
    | Some ml ->
        let o = Option.get (Modsrc.impl_file ~code src) in
        let deps = Modsrc.ml_deps src in
        let src_deps, _remain = Modsrc.find deps modsrcs in
        let add_src_dep_objs acc dep =
          Modsrc.as_impl_dep_files ~code ~init:acc dep
        in
        let src_deps_objs = List.fold_left add_src_dep_objs [] src_deps in
        let ext_objs =
          let add_lib acc l = match code with
          | Code.Native ->
              List.rev_append (Lib.cmxs l) @@
              List.rev_append (Lib.cmis l) acc
          | Code.Byte ->
              List.rev_append (Lib.cmis l) acc
          | Code.Wasm ->
              assert false
          in
          List.fold_left add_lib [] requires
        in
        let has_cmi, src_deps_objs = match Modsrc.mli src with
        | None -> false, src_deps_objs
        | Some _ -> true, Modsrc.cmi_file src :: src_deps_objs
        in
        let reads = List.rev_append ext_objs src_deps_objs in
        ml_to_impl ~and_cmt m ~code ~opts ~reads ~has_cmi ~ml ~o

  let intfs ~and_cmti m ~comp ~opts ~requires ~modsrcs =
    let compile _ src acc =
      List.rev_append
        (modsrc_intf ~and_cmti m ~comp ~modsrcs ~requires ~opts src) acc
    in
    String.Map.fold compile modsrcs []

  let impls ~and_cmt m ~code ~opts ~requires ~modsrcs =
    let compile _ src acc =
      List.rev_append
        (modsrc_impl ~and_cmt m ~code ~opts ~modsrcs ~requires src) acc
    in
    String.Map.fold compile modsrcs []
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
      List.cons_if
        (Conf.has_dynlink conf) Fpath.(odir / cstubs_dll oname dll_ext) []
    in
    B0_memo.spawn ?post_exec ?k m ~reads:c_objs ~writes @@
    ocamlmklib
      Cmd.(arg "-o" %% unstamp (path o) %% opts %% unstamp (paths c_objs));
    writes

  let byte ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname =
    let ocamlc = B0_memo.tool m Tool.ocamlc in
    let cstubs_opts =
      if not has_cstubs then Cmd.empty else
      let lib = Fmt.str "-l%s" (cstubs_name oname) in
      Cmd.(arg "-cclib" % lib %%
           if' (Conf.has_dynlink conf) (arg "-dllib" % lib))
    in
    let cma = Fpath.(odir / Fmt.str "%s.cma" oname) in
    let writes = [cma] in
    B0_memo.spawn m ~reads:cobjs ~writes @@
    ocamlc Cmd.(arg "-a" % "-o" %% unstamp (path cma) %% cstubs_opts %% opts %%
                unstamp (paths cobjs));
    writes

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
    let c_objs = List.rev_map (Fpath.set_ext ~multi:false obj_ext) cobjs in
    let reads = List.rev_append c_objs cobjs in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes @@
    ocamlopt Cmd.(arg "-a" % "-o" %% unstamp (path cmxa) %% cstubs_opts %%
                  opts %% unstamp (paths cobjs));
    writes

  let code ?post_exec ?k m ~conf ~opts ~code ~has_cstubs ~cobjs ~odir ~oname =
    let archive = match code with
    | Code.Byte -> byte | Code.Native -> native | Code.Wasm -> assert false
    in
    archive ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname

  let native_dynlink ?post_exec ?k m ~conf ~opts ~has_cstubs ~cmxa ~o =
    let lib_ext = Conf.lib_ext conf in
    let ocamlopt = B0_memo.tool m Tool.ocamlopt in
    let cmxa_clib = Fpath.(cmxa -+ lib_ext) in
    let cstubs_opts, reads =
      if not has_cstubs then Cmd.empty, [cmxa; cmxa_clib] else
      (* Fixme do this on a cstubs path *)
      let oname = Fpath.basename ~strip_exts:true cmxa in
      let cstubs_dir = Fpath.(parent cmxa) in
      let cstubs = Fpath.(cstubs_dir / cstubs_clib oname lib_ext) in
      let inc = Cmd.(arg "-I" %% unstamp (path cstubs_dir)) in
      Cmd.(inc %% unstamp (path cstubs)), [cstubs; cmxa; cmxa_clib]
    in
    let writes = [o] in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes @@
    ocamlopt Cmd.(arg "-shared" % "-linkall" % "-o" %% unstamp (path o) %%
                  opts %% cstubs_opts %% unstamp (path cmxa));
    writes
end

module Link = struct
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
    let reads, cobjs =
      let rec loop rsides rcobjs = function
      | [] ->
          List.rev_append rcobjs (List.rev_append rsides c_objs),
          List.rev rcobjs
      | cobj :: cobjs ->
          match Fpath.has_ext ".cmo" cobj with
          | true -> loop rsides (cobj :: rcobjs) cobjs
          | false ->
              match Fpath.has_ext ".cma" cobj with
              | true -> loop rsides (cobj :: rcobjs) cobjs
              | false ->
                  (* This should be the cma's dll archive *)
                  loop (cobj :: rsides) rcobjs cobjs
      in
      loop [] [] cobjs
    in
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
              let rsides = Fpath.set_ext ~multi:false obj_ext cobj :: rsides in
              loop rsides (cobj :: rcobjs) cobjs
          | false ->
              match Fpath.has_ext ".cmxa" cobj with
              | true ->
                  loop rsides (cobj :: rcobjs) cobjs
              | false ->
                  (* This should be the `cmxa`s C library archives or
                     C stubs archives *)
                  loop (cobj :: rsides) rcobjs cobjs
      in
      loop [] [] cobjs
    in
    B0_memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlopt Cmd.(arg "-o" %% unstamp (path o) %% opts %%
                  unstamp (incs %% paths c_objs %% paths cobjs))

  let code ?post_exec ?k m ~conf ~opts ~code ~c_objs ~cobjs ~o =
    let linker = match code with
    | Code.Byte -> byte | Code.Native -> native | Code.Wasm -> assert false
    in
    linker ?post_exec ?k m ~conf ~opts ~c_objs ~cobjs ~o
end

(* Build units. *)

let compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs =
  (* XXX Maybe better things could be done here once we have a good C domain. *)
  let obj_ext = Conf.obj_ext conf in
  let rec loop os cunits hs = function
  | [] -> List.rev os
  | c :: cs ->
      let cname = Fpath.basename ~strip_exts:true c in
      match String.Map.find cname cunits with
      | exception Not_found ->
          let o = Fpath.(build_dir / Fmt.str "%s%s" cname obj_ext) in
          ignore (Compile.c_to_o m ~comp ~opts ~reads:hs ~c ~o);
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
  let* built = B0_build.get b Code.built in
  let _doc, restrict = B0_meta.find_or_default Code.restrict meta in
  Fut.return (restrict built)

let exe_proc set_exe_path set_modsrcs srcs b =
  let* () = Code.check_any ~supported:Code.traditional ~by:b in
  let m = B0_build.memo b in
  let build_dir = B0_build.current_dir b in
  let src_root = B0_build.scope_dir b in
  let* srcs = B0_srcs.(Fut.map by_ext @@ select b srcs) in
  let* modsrcs = Modsrc.map_of_files m ~build_dir ~src_root ~srcs in
  let meta = B0_build.current_meta b in
  let requires = B0_meta.get requires meta in
  set_modsrcs modsrcs;
  let* unit_code = unit_code b m meta in
  let* conf = B0_build.get b Conf.key in
  let* resolver = B0_build.get b Libresolver.key in
  let* comp_requires = Libresolver.get_list_and_exports m resolver requires in
  let tool_name = B0_meta.get B0_unit.tool_name meta in
  let exe_ext = Conf.exe_ext conf in
  let opts = Cmd.(arg "-g") (* TODO *) in
  let o = Fpath.(build_dir / (tool_name ^ exe_ext)) in
  set_exe_path o;  (* FIXME introduce a general mecanism for that *)
  (* FIXME this can likely be streamlined  *)
  let code =
    if Code.Set.mem Native unit_code then Code.Native else Code.Byte
  in
  let all_code = code = Code.Native && Code.Set.mem Code.Byte unit_code in
  let comp = if code = Code.Native then Tool.ocamlopt else Tool.ocamlc in
  ignore @@
  Compile.intfs ~and_cmti:true m ~comp ~opts ~requires:comp_requires ~modsrcs;
  ignore @@
  Compile.impls ~and_cmt:true m ~code ~opts ~requires:comp_requires ~modsrcs;
  if all_code then begin
    ignore @@ Compile.impls
      ~and_cmt:false m ~code:Byte ~opts ~requires:comp_requires ~modsrcs;
  end;
  let* c_objs = compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs in
  let modsrcs =
    Modsrc.sort (* for link *) ~deps:Modsrc.ml_deps modsrcs
  in
  let* link_requires = Libresolver.get_list_and_deps m resolver requires in
  let archive ~code lib = match code with
  | Code.Byte ->
      let c_stubs =
        (* Note we only get something with Libraries from
           the build, the ocamlfind resolver doesn't give us the dlls here
           for now *)
        List.find_all (Fpath.has_ext (Conf.dll_ext conf)) (Lib.c_stubs lib)
      in
      (match Lib.cma lib with None -> c_stubs | Some cma -> cma :: c_stubs)
  | Code.Native ->
      let add v l = match v with None -> l | Some v -> v :: l in
      let c_stubs =
        List.find_all (Fpath.has_ext (Conf.lib_ext conf)) (Lib.c_stubs lib)
      in
      add (Lib.cmxa lib) (add (Lib.c_archive lib) c_stubs)
  | Code.Wasm ->
      assert false
  in
  let lib_objs = List.concat_map (archive ~code) link_requires in
  let cobjs = List.filter_map (Modsrc.impl_file ~code) modsrcs in
  let opts =
    let c_requires = B0_meta.get c_requires meta in
    Cmd.(opts %% (Cmd.list ~slip:"-cclib" (Cmd.to_list c_requires)))
  in
  Link.code m ~conf ~code ~opts ~c_objs ~cobjs:(lib_objs @ cobjs) ~o;
  if all_code then begin
    let o = Fpath.(build_dir / (tool_name ^ ".byte" ^ exe_ext)) in
    let lib_objs = List.concat_map (archive ~code:Byte) link_requires in
    let cobjs = List.filter_map (Modsrc.impl_file ~code:Byte) modsrcs in
    Link.code m ~conf ~code:Byte ~opts ~c_objs ~cobjs:(lib_objs @ cobjs) ~o
  end;
  Fut.return ()

let script_proc set_exe_path file b =
  let m = B0_build.memo b in
  let scope_dir = B0_build.scope_dir b in
  let exe_file = Fpath.(scope_dir // file) in
  B0_memo.ready_file m exe_file;
  set_exe_path exe_file;
  let ocaml = B0_memo.tool m Tool.ocaml in
  let stdout_file = Fpath.(B0_build.current_dir b / "ocaml.stdout") in
  let stdin = Fpath.null and stderr = `File Fpath.null in
  let stdout = `File stdout_file in
  let nocolor = "-color=never" (* If this changes [find_error] must too *) in
  let post_exec op = match Os.File.read stdout_file with
  | Error _ as e -> Log.if_error ~use:() e
  | Ok log ->
      let find_error _ () s =
        if String.starts_with ~prefix:"Error:" s then raise Exit else ()
      in
      try String.fold_ascii_lines ~strip_newlines:false find_error () log
      with Exit -> B0_zero.Op.set_status op (Failed (Exec (Some log)))
  in
  let reads = [exe_file] and writes = [stdout_file] in
  B0_memo.spawn m ~reads ~writes ~stdin ~stdout ~stderr ~post_exec @@
  ocaml Cmd.(arg nocolor % "-init" %% path exe_file);
  Fut.return ()

let lib_proc set_modsrcs set_lib srcs b =
  let* () = Code.check_any ~supported:Code.traditional ~by:b in
  (* XXX we are still missing cmxs here
     XXX not sure the Archive.code makes the logic easier to understand.
  *)
  let m = B0_build.memo b in
  let build_dir = B0_build.current_dir b in
  let src_root = B0_build.scope_dir b in
  let* srcs = B0_srcs.(Fut.map by_ext @@ select b srcs) in
  let* modsrcs = Modsrc.map_of_files m ~build_dir ~src_root ~srcs in
  set_modsrcs modsrcs;
  let meta = B0_build.current_meta b in
  let librequires = B0_meta.get requires meta in
  let libname = B0_meta.get library meta in
  let archive_name = Libname.to_archive_name libname in
  let opts = Cmd.(arg "-g") (* TODO *) in
  let* unit_code = unit_code b m meta in
  let* conf = B0_build.get b Conf.key in
  let* resolver = B0_build.get b Libresolver.key in
  let* requires = Libresolver.get_list_and_exports m resolver librequires in
  (* FIXME this can likely be streamlined  *)
  let code =
    if Code.Set.mem Native unit_code then Code.Native else Code.Byte
  in
  let all_code = code = Code.Native && Code.Set.mem Code.Byte unit_code in
  let comp = if code = Code.Native then Tool.ocamlopt else Tool.ocamlc in
  let intfs = Compile.intfs ~and_cmti:true m ~comp ~opts ~requires ~modsrcs in
  let impls = Compile.impls ~and_cmt:true m ~code ~opts ~requires ~modsrcs in
  let impls =
    if not all_code then impls else
    let bimpls =
      Compile.impls ~and_cmt:true m ~code:Byte ~opts ~requires ~modsrcs
    in
    List.rev_append bimpls impls
  in
  let* c_objs =
    let comp = Tool.ocamlc (* https://github.com/ocaml/ocaml/issues/13798 *) in
    compile_c_srcs m ~conf ~comp ~opts ~build_dir ~srcs
  in
  let modsrcs = Modsrc.sort (* for link *) ~deps:Modsrc.ml_deps modsrcs in
  let cobjs = List.filter_map (Modsrc.impl_file ~code) modsrcs  in
  let odir = build_dir and oname = archive_name in
  let has_cstubs = c_objs <> [] in
  let c_requires = B0_meta.get c_requires meta in
  let c_stubs = match has_cstubs with
  | false -> []
  | true ->
      let opts = Cmd.(opts %% c_requires) in
      Archive.cstubs m ~conf ~opts ~c_objs ~odir ~oname
  in
  let opts =
    Cmd.(opts %% (Cmd.list ~slip:"-cclib" (Cmd.to_list c_requires)))
  in
  let ars = Archive.code m ~conf ~code ~opts ~has_cstubs ~cobjs ~odir ~oname in
  let ars =
    if not all_code then ars else
    let cobjs = List.filter_map (Modsrc.impl_file ~code:Byte) modsrcs in
    let bars =
      Archive.code m ~conf ~code:Byte ~opts ~has_cstubs ~cobjs ~odir ~oname
    in
    List.rev_append ars bars
  in
  let lib =
    let exports = B0_meta.get exports meta in
    let warning = B0_meta.find B0_meta.warning meta in
    let cma = List.find_opt (Fpath.has_ext ".cma") ars in
    let cmxa = List.find_opt (Fpath.has_ext ".cmxa") ars in
    let cmis = List.find_all (Fpath.has_ext ".cmi") intfs in
    let cmxs = List.find_all (Fpath.has_ext ".cmx") impls in
    let js_stubs = B0_file_exts.(find_files js) srcs in
    let c_archive = List.find_opt (Fpath.has_ext (Conf.lib_ext conf)) ars in
    Lib.make ~libname ~requires:librequires
      ~exports ~dir:build_dir ~cmis ~cmxs ~cma ~cmxa
      ~c_archive ~c_stubs ~js_stubs ~warning
  in
  set_lib lib;
  Fut.return ()

let exe
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?c_requires:c_reqs ?requires:reqs
    ?(public = false) ?name tool_name ~srcs
  =
  let name = Option.value ~default:tool_name name in
  let doc = match doc with
  | Some _ as doc -> doc
  | None when public -> Some (Fmt.str "The %s tool" tool_name)
  | None -> None
  in
  let fut_modsrcs, set_modsrcs = Fut.make () in
  let exe_path, set_exe_path = Fut.make () in
  let base =
    B0_meta.empty
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.exe
    |> B0_meta.add B0_unit.tool_name tool_name
    |> B0_meta.add B0_meta.public public
    |> B0_meta.add_some_or_default c_requires c_reqs
    |> B0_meta.add_some_or_default requires reqs
    |> B0_meta.add modsrcs fut_modsrcs
    |> B0_meta.add B0_unit.exe_file exe_path
    |> B0_meta.add Code.restrict Code.unique_favour_native
  in
  let meta = B0_meta.override base ~by:meta in
  let proc = wrap (exe_proc set_exe_path set_modsrcs srcs) in
  B0_unit.make ?doc ~meta name proc

let script
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?(public = false) ?name file
  =
  let script_name = Fpath.basename file in
  let basename = B0_unit.mangle_basename script_name in
  let name = Option.value ~default:basename name in
  let doc = match doc with
  | Some _ as doc -> doc
  | None when public -> Some (Fmt.str "The %s script" script_name)
  | None -> None
  in
  let exe_path, set_exe_path = Fut.make () in
  let base =
    B0_meta.empty
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.exe
    |> B0_meta.add B0_unit.tool_name script_name
    |> B0_meta.add B0_meta.public public
    |> B0_meta.add B0_unit.exe_file exe_path
  in
  let meta = B0_meta.override base ~by:meta in
  let proc = wrap (script_proc set_exe_path file) in
  B0_unit.make ?doc ~meta name proc

let test
    ?wrap ?doc ?(meta = B0_meta.empty) ?requires ?name ?(run = true)
    ?(long  = false) ?(srcs = []) src
  =
  let srcs = (`File src) :: srcs in
  let meta =
    B0_meta.override ~by:meta @@
    B0_meta.empty
    |> B0_meta.tag B0_meta.test
    |> B0_meta.add B0_meta.run run
    |> B0_meta.add B0_meta.long long
    |> B0_meta.add B0_unit.Action.cwd `Scope_dir
  in
  let name = match name with
  | None -> Fpath.basename ~strip_exts:true src
  | Some name -> name
  in
  exe ?wrap ?doc name ~srcs ?requires ~meta


let unit_name_for_lib ~libname ~name = match name with
| Some name -> name | None -> Libname.undot ~rep:'-' libname

let unit_doc_for_lib ~deprecated ~libname ~doc = match doc with
| Some doc -> doc
| None ->
    let pp_depr ppf d = if d then Fmt.string ppf " (deprecated)" else () in
    Fmt.str "The %s library%a" (Libname.to_string libname) pp_depr deprecated

let unit_warning_for_deprecated_lib ~exports ~warning = match warning with
| Some warning -> warning
| None ->
    let pp_use_exports ppf = function
    | None | Some [] -> ()
    | Some [l] -> Fmt.pf ppf ", use the %s library." (Libname.to_string l)
    | Some ls ->
        let pp_libs = Fmt.list Fmt.string in
        let ls = List.map Libname.to_string ls in
        Fmt.pf ppf ", use the %a libraries." pp_libs ls
    in
    Fmt.str "Deprecated%a" pp_use_exports exports

let don't_load =
  Libname.Set.of_list [
    libname "compiler-libs.common";
    libname "compiler-libs.bytecomp";
    libname "compiler-libs.optcomp";
    libname "compiler-libs.toplevel";
    libname "compiler-libs.native-toplevel"; ]


let byte_code_build_load_args b ~x_units units =
  (* This is first good step. Also we don't have a notion of stable
     order for sort_mods, but we don't have one in the first place in b0
     OCaml exe specification yet I think. *)
  let sort_mods mods =
    let mods = Modname.Map.map snd mods in
    let mods = Modsrc.sort ~deps:Modsrc.ml_deps mods in
    List.filter_map Modsrc.cmo_file mods
  in
  let rec loop libs_acc inc_mods mods = function
  | [] -> List.rev (snd libs_acc), inc_mods, sort_mods mods
  | unit :: units ->
      let add_lib (seen, libs as acc) name =
        if Libname.Set.mem name seen then acc else
        (Libname.Set.add name seen, name :: libs)
      in
      let libs_acc, inc_mods, mods =
        match B0_unit.find_meta library unit with
        | Some name -> (* N.B. libs' requires will be added by lib resolution *)
            add_lib libs_acc name, inc_mods, mods
        | None ->
            let libs_acc = match B0_unit.find_meta requires unit with
            | None -> libs_acc
            | Some requires -> List.fold_left add_lib libs_acc requires
            in
            let mods = match B0_unit.find_meta modsrcs unit with
            | None -> mods | Some srcs ->
                let add n src acc =
                  let update = function
                  | None -> Some (unit, src)
                  | Some (u, src) ->
                      Fmt.failwith
                      "@[<v>Cannot load build: module %a defined by@,\
                       units %a and %a@]"
                      Modname.pp n B0_unit.pp_name u B0_unit.pp_name unit
                  in
                  Modname.Map.update n update acc
                in
                Modname.Map.fold add (Fut.sync srcs) mods
            in
            let inc_mods = Fpath.Set.add (B0_build.unit_dir b unit) inc_mods in
            libs_acc, inc_mods, mods
      in
      loop libs_acc inc_mods mods units
  in
  try
    let units = B0_unit.Set.diff units x_units in
    let don't_load =
      (* Note perhaps this could be a bit more subtle and we should
         exclude *build* lookups a the resolver level (i.e. let
         the other resolvers quick in if needed *)
      let add_unit_lib xu acc = match B0_unit.find_meta library xu with
      | None -> acc | Some libname -> Libname.Set.add libname acc
      in
      B0_unit.Set.fold add_unit_lib x_units don't_load
    in
    let units = B0_unit.Set.elements units in
    let add_lib_opts cmd lib =
      let cma = Option.map Cmd.path (Lib.cma lib) in
      Cmd.(cmd % "-I" %% path (Lib.dir lib) %% if_some cma)
    in
    let libs, inc_mods, mods =
      loop (Libname.Set.empty, []) Fpath.Set.empty Modname.Map.empty units
    in
    let m = B0_build.memo b in
    let libs =
      let* resolver = B0_build.get b Libresolver.key in
      let libs = Libresolver.get_list_and_deps m resolver libs in
      libs
    in
    B0_memo.stir ~block:true m;
    let libs = Fut.sync libs in
    let don't_load lib = Libname.Set.mem (Lib.libname lib) don't_load in
    let libs = List.filter (Fun.negate don't_load) libs in
    let lib_opts = List.fold_left add_lib_opts Cmd.empty libs in
    let inc_mods = Cmd.paths ~slip:"-I" (Fpath.Set.elements inc_mods) in
    Ok Cmd.(lib_opts %% inc_mods %% paths mods)
  with
  | Failure e -> Error e

let run_ocaml
    env ~use_utop ~dry_run ~args ~x_units (* TODO in resolver *) units
  =
  let open Result.Syntax in
  let b = B0_env.build env in
  let top = Cmd.tool (if use_utop then "utop" else "ocaml") in
  let* exe = B0_env.get_cmd env top in
  let* load_args = byte_code_build_load_args b ~x_units units in
  let args = Cmd.of_list Fun.id args in
  let top = Cmd.(exe %% load_args %% args) in
  match dry_run with
  | false -> Ok (Os.Exit.execv top)
  | true ->
      Log.stdout (fun m -> m "%a" Cmd.pp_shell top);
      Ok Os.Exit.ok

let run_ocaml_term func env =
  let open Cmdliner in
  let open Cmdliner.Term.Syntax in
  let+ dry_run =
    let doc = "Output $(b,ocaml) invocation rather than executing it." in
    Arg.(value & flag & info ["dry-run"] ~doc)
  and+ use_utop =
    let doc = "Use $(b,utop) rather than $(b,ocaml)." in
    Arg.(value & flag & info ["utop"] ~doc)
  and+ x_units =
    let doc = "Exclude objects of $(docv) from loading." in
    B0_cli.use_x_units ~doc ()
  and+ x_packs =
    let doc = "Exclude objects of units of $(docv) from loading." in
    B0_cli.use_x_packs ~doc ()
  and+ args =
    let doc =
      "Arguments for the $(b,ocaml) executable. Specify them after $(b,--)."
    in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"ARG")
  in
  func env use_utop dry_run x_units x_packs args

let load_lib =
  let open Result.Syntax in
  B0_unit.Action.of_cmdliner_term @@ fun env u ->
  let run env use_utop dry_run x_units x_packs args =
    Log.if_error ~use:Os.Exit.some_error @@
    let* x_units = B0_cli.get_excluded_units ~x_units ~x_packs in
    let x_units = B0_unit.Set.filter (fun u -> B0_unit.has_tag tag u) x_units in
    run_ocaml env ~use_utop ~dry_run ~args ~x_units (B0_unit.Set.singleton u)
  in
  run_ocaml_term run env

let lib
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?c_requires:c_reqs ?requires:reqs ?exports:exps ?(public = true)
    ?name libname ~srcs
  =
  let name = unit_name_for_lib ~libname ~name in
  let doc = unit_doc_for_lib ~deprecated:false ~libname ~doc in
  let fut_modsrcs, set_modsrcs = Fut.make () in
  let fut_lib, set_lib = Fut.make () in
  let base =
    B0_meta.empty
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.lib
    |> B0_meta.add library libname
    |> B0_meta.add B0_meta.public public
    |> B0_meta.add_some_or_default c_requires c_reqs
    |> B0_meta.add_some_or_default requires reqs
    |> B0_meta.add_some_or_default exports exps
    |> B0_meta.add modsrcs fut_modsrcs
    |> B0_meta.add Lib.key fut_lib
    |> B0_meta.add B0_unit.Action.key (`Fun ("ocaml", load_lib))
    |> B0_meta.add B0_unit.Action.store
       (* XXX the store should be able to depend on the action's
          args so that we can add a `--nat` option. *)
      B0_store.[B (Code.built, Fut.return (Code.Set.singleton Code.Byte))]
  in
  let meta = B0_meta.override base ~by:meta in
  let proc = wrap (lib_proc set_modsrcs set_lib srcs) in
  B0_unit.make ~doc ~meta name proc

let deprecated_lib
    ?(wrap = fun proc b -> proc b) ?doc ?(meta = B0_meta.empty)
    ?exports:exps ?warning ?(public = true) ?name libname
  =
  let name = unit_name_for_lib ~libname ~name in
  let doc = unit_doc_for_lib ~deprecated:true ~libname ~doc in
  let warning = unit_warning_for_deprecated_lib ~exports:exps ~warning in
  let fut_lib, set_lib = Fut.make () in
  let meta =
    B0_meta.override ~by:meta @@
    B0_meta.empty
    |> B0_meta.tag tag
    |> B0_meta.tag B0_meta.lib
    |> B0_meta.tag B0_meta.deprecated
    |> B0_meta.add library libname
    |> B0_meta.add B0_meta.public public
    |> B0_meta.add B0_meta.warning warning
    |> B0_meta.add_some_or_default requires None
    |> B0_meta.add_some_or_default exports exps
    |> B0_meta.add modsrcs (Fut.return Modname.Map.empty)
    |> B0_meta.add Lib.key fut_lib
  in
  let proc =
    wrap @@ fun b ->
    let lib =
      let meta = B0_build.current_meta b in
      let exports = B0_meta.get exports meta in
      let warning = B0_meta.find B0_meta.warning meta in
      let build_dir = B0_build.current_dir b in
      Lib.make ~libname ~requires:[]
        ~exports ~dir:build_dir ~cmis:[] ~cmxs:[] ~cma:None ~cmxa:None
        ~c_archive:None ~c_stubs:[] ~js_stubs:[] ~warning
    in
    set_lib lib;
    Fut.return ()
  in
  B0_unit.make ~meta ~doc name proc

(* Compiled object information *)

module Cobj = struct
  let archive_ext_of_code = function
  | Code.Byte -> ".cma" | Code.Native -> ".cmxa" | Code.Wasm -> assert false

  let object_ext_of_code = function
  | Code.Byte -> ".cmo" | Code.Native -> ".cmx" | Code.Wasm -> assert false

  type t =
    { file : Fpath.t;
      defs : Modref.Set.t;
      deps : Modref.Set.t;
      link_deps : Modref.Set.t; (* deps whose name appear in required
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
      Fmt.field "defs" defs Modref.Set.dump;
      Fmt.field "deps" deps Modref.Set.dump;
      Fmt.field "link-deps" link_deps Modref.Set.dump; ]

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  let sort ?(deps = link_deps) cobjs =
    let rec loop cobjs_defs seen ext_deps cobjs = function
    | (c :: cs as l) :: todo ->
        begin match Modref.Set.subset (defs c) seen with
        | true -> loop cobjs_defs seen ext_deps cobjs (cs :: todo)
        | false ->
            let seen = Modref.Set.union (defs c) seen in
            let add_dep d (local_deps, ext_deps as acc) =
              if Modref.Set.mem d seen then acc else
              match Modref.Map.find d cobjs_defs with
              | exception Not_found -> local_deps, Modref.Set.add d ext_deps
              | dep_cobj -> dep_cobj :: local_deps, ext_deps
            in
            let start = [], ext_deps in
            let local_deps, ext_deps =
              Modref.Set.fold add_dep (deps c) start
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
    let add_def c d acc = Modref.Map.add d c acc in
    let add_defs acc c = Modref.Set.fold (add_def c) (defs c) acc in
    let cobjs_defs = List.fold_left add_defs Modref.Map.empty cobjs in
    loop cobjs_defs Modref.Set.empty Modref.Set.empty [] (cobjs :: [])

  (* ocamlobjinfo output parsing, could be easier... *)

  let make_cobj file defs deps ldeps =
    let deps = Modref.Set.diff deps defs in
    let link_deps =
      let keep m = String.Set.mem (Modref.name m) ldeps in
      Modref.Set.filter keep deps
    in
    { file; defs; deps; link_deps; }

  let file_prefix = "File "
  let parse_file_path (n, line) =
    let len = String.length file_prefix in
    match Fpath.of_string (String.drop_first len line) with
    | Ok file -> file
    | Error e -> Fmt.failwith_line n " %s" e

  let rec parse_ldeps acc file defs deps ldeps name = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | ((n, l) :: ls) as data ->
      match String.split_last ~sep:"\t" l with
      | None -> parse_file acc file defs deps ldeps data
      | Some (_, ldep) ->
          let ldeps = String.Set.add (String.trim ldep) ldeps in
          parse_ldeps acc file defs deps ldeps name ls

  and parse_deps acc file defs deps ldeps name = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | ((n, l) :: ls) as data ->
      match String.split_last ~sep:"\t" l with
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
              let mref = Modref.make dname digest in
              let defs, deps = match String.equal dname name with
              | true -> Modref.Set.add mref defs, deps
              | false -> defs, Modref.Set.add mref deps
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
      begin match String.split_first ~sep:":" l with
      | None -> assert false
      | Some (_, name) ->
          parse_unit acc file defs deps ldeps (String.trim name) ls
      end
  | (n, l as line) :: ls when String.starts_with ~prefix:file_prefix l ->
      let acc = make_cobj file defs deps ldeps :: acc in
      let file = parse_file_path line in
      parse_file
        acc file Modref.Set.empty Modref.Set.empty String.Set.empty ls
  | _ :: ls -> parse_file acc file defs deps ldeps ls

  and parse_files acc = function
  | [] -> acc
  | (n, l as line) :: ls when String.starts_with ~prefix:file_prefix l ->
      let file = parse_file_path line in
      parse_file
        acc file Modref.Set.empty Modref.Set.empty String.Set.empty ls
  | l :: ls -> parse_files acc ls

  let of_string ?(file = Fpath.dash) data =
    let line num acc l = (num, l) :: acc in
    let rev_lines = String.fold_ascii_lines ~strip_newlines:true line [] data in
    try Ok (parse_files [] (List.rev rev_lines)) with
    | Failure e -> Fpath.error file "%s" e

  let ocamlobjinfo_args = Cmd.(arg "-no-approx" % "-no-code")

  let write m ~cobjs ~o =
    (* FIXME add [src_root] so that we can properly unstamp. *)
    let ocamlobjinfo = B0_memo.tool m Tool.ocamlobjinfo in
    B0_memo.spawn m ~reads:cobjs ~writes:[o] ~stdout:(`File o) @@
    ocamlobjinfo Cmd.(ocamlobjinfo_args %% paths cobjs)

  let read m file =
    let* s = B0_memo.read m file in
    Fut.return (of_string ~file s |> B0_memo.fail_if_error m)
end

module Crunch = struct
  let id_of_filename s =
    String.Ascii.uncapitalize (Modname.mangle_filename s)

  let string_to_string ~id ~data:s =
    let len = String.length s in
    let len = len * 4 + (len / 18) * (3 + 2) in
    let b = Buffer.create (len + String.length id + 3) in
    let adds = Buffer.add_string in
    adds b "let "; adds b id; adds b " =\n  \"";
    for i = 0 to String.length s - 1 do
      if i mod 18 = 0 && i <> 0 then adds b "\\\n   ";
      let c = String.get_uint8 s i in
      adds b "\\x";
      Buffer.add_char b (Char.Ascii.lower_hex_digit_of_int (c lsr 4));
      Buffer.add_char b (Char.Ascii.lower_hex_digit_of_int (c      ))
    done;
    adds b "\"\n";
    Buffer.contents b
end

(* Actions *)

open Result.Syntax

let crunch id file =
  Log.if_error ~use:Os.Exit.some_error @@
  let* data = Os.File.read file in
  let id = match id with
  | Some id -> id
  | None when Fpath.equal file Fpath.dash -> "stdin"
  | None -> Crunch.id_of_filename (Fpath.basename file)
  in
  let crunch = Crunch.string_to_string ~id ~data in
  let* () = Os.File.write ~force:false ~make_path:false Fpath.dash crunch in
  Ok Os.Exit.ok

let list format pager_don't =
  let keep u =
    let base = B0_unit.has_tag B0_meta.lib u && B0_unit.has_tag tag u in
    if not base then None else
    match B0_unit.find_meta library u with
    | None -> None
    | Some libname -> Some (libname, u)
  in
  let us = List.filter_map keep (B0_unit.list ()) in
  let pp_normal ppf (libname, u) =
    Fmt.pf ppf "%a (%s) %s" Libname.pp libname (B0_unit.name u) (B0_unit.doc u)
  in
  let pp_long ppf (libname, u) =
    let requires = B0_unit.find_meta requires u in
    let requires = Option.value ~default:[] requires in
    Fmt.pf ppf
      "@[<v>Libary %a (%s)@,%a@,%a@]"
      Libname.pp libname (B0_unit.name u)
      Fmt.(field "doc" B0_unit.doc string) u
      Fmt.(field "requires" Fun.id (Fmt.box (Fmt.(list ~sep:sp Libname.pp))))
      requires
  in
  let pp_lib, sep = match format with
  | `Short -> Fmt.using fst Libname.pp, Fmt.cut
  | `Normal -> pp_normal, Fmt.cut
  | `Long -> pp_long, Fmt.(cut ++ cut)
  in
  Log.stdout (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp_lib) us);
  Os.Exit.ok

module Digest_dag = struct
  (* Maps digest to the digests of their dependencies. *)
  type t = String.Set.t String.Map.t

  let transitive_reduction graph =
    (* For each vertex [v] of [graph], walk the direct successors of
       [v] and remove the visited elements from the successors of [v]. *)
    let rec prune_reachable graph ~seen ~to_prune ~todo =
      match String.Set.choose_opt todo with
      | None -> seen, to_prune
      | Some u ->
          let todo = String.Set.remove u todo in
          let to_prune = String.Set.remove u to_prune in
          if String.Set.mem u seen
          then prune_reachable graph ~seen ~to_prune ~todo else
            let seen = String.Set.add u seen in
            match String.Map.find_opt u graph with
            | None -> prune_reachable graph ~seen ~to_prune ~todo
            | Some u_succs ->
                let add_succ succ todo =
                  if String.Set.mem succ seen then todo else
                  String.Set.add succ todo
                in
                let todo = String.Set.fold add_succ u_succs todo in
                prune_reachable graph ~seen ~to_prune ~todo
    in
    let rec walk_direct_succs graph ~seen ~to_prune ~todo =
      match String.Set.choose_opt todo with
      | None -> to_prune
      | Some u ->
          let todo = String.Set.remove u todo in
          if String.Set.mem u seen
          then walk_direct_succs graph ~seen ~to_prune ~todo else
          let seen = String.Set.add u seen in
          match String.Map.find_opt u graph with
          | None -> walk_direct_succs graph ~seen ~to_prune ~todo
          | Some u_succs ->
              let seen, to_prune =
                prune_reachable graph ~seen ~to_prune ~todo:u_succs
              in
              walk_direct_succs graph ~seen ~to_prune ~todo
    in
    let reduce_vertex v succs graph =
      let seen = String.Set.empty and to_prune = succs and todo = succs in
      let pruned = walk_direct_succs graph ~seen ~to_prune ~todo in
      String.Map.add v pruned graph
    in
    String.Map.fold reduce_vertex graph graph
end

module Cobj_index = struct
  let ocamlobjinfo = Cmd.(tool "ocamlobjinfo" %% Cobj.ocamlobjinfo_args)
  let cobjs_of_file ocamlobjinfo file =
    let* data = Os.Cmd.run_out ~trim:true Cmd.(ocamlobjinfo %% path file) in
    Cobj.of_string data

  type t =
    { by_entity : ([`Unit of B0_unit.t | `Lib of Lib.t] * Cobj.t list) list;
      by_digest : Cobj.t list String.Map.t; }

  let by_entity i = i.by_entity
  let by_digest i = i.by_digest
  let empty = { by_entity = []; by_digest = String.Map.empty }

  let find_env_libnames env ~x_libs ocaml_units =
    let add_requires u (in_build, acc) =
      let in_build = match B0_unit.find_meta library u with
      | None -> in_build | Some l -> Libname.Set.add l in_build
      in
      let acc = match B0_unit.find_meta requires u with
      | None -> acc
      | Some libs -> List.fold_left (Fun.flip Libname.Set.add) acc libs
      in
      (in_build, acc)
    in
    let in_build, acc =
      B0_unit.Set.fold add_requires ocaml_units Libname.Set.(empty, empty)
    in
    let exclude = Libname.Set.union x_libs in_build in
    Libname.Set.diff acc exclude

  let lookup_env_libs env libnames =
    let build = B0_env.build env in
    let store = B0_build.store build in
    let memo = B0_build.memo build in
    let resolver = Fut.sync (B0_store.get store Libresolver.key) in
    let libnames = Libname.Set.elements libnames in
    Fut.sync (Libresolver.get_list_and_exports memo resolver libnames)

  let find_built_ocaml_units ~x_units ~x_packs ~x_libs env =
    let units = B0_build.did_build (B0_env.build env) in
    let* x_units = B0_cli.get_excluded_units ~x_units ~x_packs in
    let keep u =
      B0_unit.has_tag ocaml_tag u &&
      match B0_unit.find_meta library u with
      | None -> true | Some n -> not (Libname.Set.mem n x_libs)
    in
    let ocaml_units = B0_unit.Set.filter keep units in
    if B0_unit.Set.cardinal units = 1 (* .ocaml action *) then begin
      Log.warn (fun m ->
          m "The build is empty.@ Use@ option %a@ or %a to@ specify@ what@ to@ \
             build." Fmt.code "-u" Fmt.code "-p");
      Ok B0_unit.Set.empty
    end else if B0_unit.Set.is_empty ocaml_units then begin
      Log.warn (fun m ->
          m "No units@ with %a found@ in@ the@ build." Fmt.code ".ocaml.tag");
      Ok B0_unit.Set.empty
    end else begin
      let ocaml_units = B0_unit.Set.diff ocaml_units x_units in
      if B0_unit.Set.is_empty ocaml_units then begin
        Log.warn @@ fun m ->
        m "All %a tagged units were excluded." Fmt.code ".ocaml.tag"
      end;
      Ok ocaml_units
    end

  let of_build ~kind ~x_units ~x_packs ~env_libs ~x_libs env =
    let* ocamlobjinfo = B0_env.get_cmd env ocamlobjinfo in
    let add_dig cobj m d = String.Map.add_to_list (Modref.digest m) cobj d in
    let add_cobj d cobj = Modref.Set.fold (add_dig cobj) (Cobj.defs cobj) d in
    let add_cobj_file (entity_cobjs, by_digest) file  =
      let cobjs = cobjs_of_file ocamlobjinfo file |> Result.error_to_failure in
      let entity_cobjs = List.rev_append cobjs entity_cobjs in
      let by_digest = List.fold_left add_cobj by_digest cobjs in
      entity_cobjs, by_digest
    in
    let add_unit_cobjs u index =
      let add_unit_file _stat _fname file acc =
        if not (Fpath.has_ext kind file) then acc else add_cobj_file acc file
      in
      let dir = B0_build.unit_dir (B0_env.build env) u in
      let entity_cobjs, by_digest =
        Os.Dir.fold_files ~recurse:true add_unit_file dir ([], index.by_digest)
        |> Result.error_to_failure
      in
      let by_entity = (`Unit u, entity_cobjs) :: index.by_entity in
      { by_entity; by_digest }
    in
    let add_lib_cobjs index lib =
      let entity_cobjs, by_digest =
        assert (kind = ".cmx"); (* Needs to be adapted *)
        let cmxs = Lib.cmxs lib in
        List.fold_left add_cobj_file ([], index.by_digest) cmxs
      in
      let by_entity = (`Lib lib, entity_cobjs) :: index.by_entity in
      { by_entity; by_digest }
    in
    let x_libs = Libname.Set.of_list (List.map Libname.v x_libs) in
    let* ocaml_units = find_built_ocaml_units ~x_units ~x_packs ~x_libs env in
    try
      let index = B0_unit.Set.fold add_unit_cobjs ocaml_units empty in
      if not env_libs then Ok index else
      let libnames = find_env_libnames env ~x_libs ocaml_units in
      let libs = lookup_env_libs env libnames in
      Ok (List.fold_left add_lib_cobjs index libs)
    with Failure e -> Error e

  let to_digest_dag index : Digest_dag.t =
    let rec loop seen acc todo = match String.Map.choose_opt todo with
    | None -> acc
    | Some (digest, cobjs) ->
        (* Just pick one we only about the module name *)
        let cobj = List.hd cobjs in
        (* XXX if one day we want to use this on archives the link_deps
           here is to coarse grained, every modref defined by the archive
           gets the same deps *)
        let cobj_deps = Cobj.link_deps cobj in
        let dep_digests =
          let inside_index digest = String.Map.mem digest index.by_digest in
          let add_digest m acc =
            let digest = Modref.digest m in
            if inside_index digest then digest :: acc else acc
          in
          Modref.Set.fold add_digest cobj_deps []
        in
        let add_modref_deps modref acc =
          if String.Set.mem digest seen then acc else
          let add_dep acc dep_digest =
            String.Map.add_to_set (module String.Set) digest dep_digest acc
          in
          List.fold_left add_dep acc dep_digests
        in
        let acc = Modref.Set.fold add_modref_deps (Cobj.defs cobj) acc in
        let todo = String.Map.remove digest todo in
        let seen = String.Set.add digest seen in
        loop seen acc todo
    in
    loop String.Set.empty String.Map.empty index.by_digest
end

module Module_deps = struct
  let show_url env ~filename:fname data =
    let* show_url = B0_env.get_cmd env Cmd.(tool "show-url" % "-f" % fname) in
    let stdin = Os.Cmd.in_string data in
    Os.Cmd.run ~stdin show_url

  let dot_to_svg env dot_graph =
    let* dot = B0_env.get_cmd env Cmd.(tool "dot" % "-Tsvg") in
    let stdin = Os.Cmd.in_string dot_graph in
    Os.Cmd.run_out ~stdin ~trim:false dot

  let dot_edges graph =
    let add_edge digest deps acc =
      let digest = Digest.to_hex digest in
      let add_dep dep acc = B0_dot.(acc ++ edge digest (Digest.to_hex dep)) in
      String.Set.fold add_dep deps acc
    in
    String.Map.fold add_edge graph B0_dot.empty

  let dot_nodes index =
    let add_entity acc (entity, cobjs) =
      let add_node modref acc =
        let name = Modref.name modref in
        let id = Digest.to_hex (Modref.digest modref) in
        B0_dot.(acc ++ node ~atts:(label name) id)
      in
      let add_nodes acc cobj =
        Modref.Set.fold add_node (Cobj.defs cobj) acc
      in
      let nodes = List.fold_left add_nodes B0_dot.empty cobjs in
      let name = match entity with
      | `Unit u -> String.concat "" ["<b>"; B0_unit.name u; "</b>"]
      | `Lib l -> String.concat "" ["<i>"; Libname.name (Lib.libname l); "</i>"]
      in
      let gatts =
        B0_dot.(atts `Graph @@
                att_html "label" name ++
                att "style" "dotted" ++
                att "fontsize" "20.0" ++
                att "labeljust" "l")
      in
      B0_dot.(acc ++ subgraph ~id:("cluster_" ^ name) (gatts ++ nodes))
    in
    List.fold_left add_entity B0_dot.empty (Cobj_index.by_entity index)

  let dot_graph ~rankdir ~deps index =
    let rankdir = match rankdir with `LR -> "LR" | `TB -> "TB" in
    let font = B0_dot.att "fontname" "monospace" in
    let graph_atts = B0_dot.(atts `Graph @@ att "rankdir" rankdir ++ font) in
    let edge_atts =
      B0_dot.(atts `Edge @@ att "color" "#777777" ++ att "arrowsize" "0.75")
    in
    let node_atts =
      let box_color = "#dadada" in
      B0_dot.(atts `Node @@
              font ++
              att "shape" "rectangle" ++
              att "style" "filled" ++
              att "color" box_color ++
              att "fillcolor" box_color)
    in
    let edges = dot_edges deps in
    let nodes = dot_nodes index in
    let g = B0_dot.(graph_atts ++ edge_atts ++ node_atts ++ edges ++ nodes) in
    B0_dot.(graph ~id:"module_deps" `Digraph g)

  let json ~deps index =
    let open B0_json in
    let json_digest digest = Jsong.string (Digest.to_hex digest) in
    let json_node (name, digest, entity_name, entity_kind, deps) =
      Jsong.obj
      |> Jsong.mem "name" (Jsong.string name)
      |> Jsong.mem "digest" (json_digest digest)
      |> Jsong.mem "entity-name" (Jsong.string entity_name)
      |> Jsong.mem "entity-kind" (Jsong.string entity_kind)
      |> Jsong.mem "deps" (Jsong.list json_digest deps)
      |> Jsong.obj_end
    in
    let add_entity acc (entity, cobjs) =
      let entity_name, entity_kind = match entity with
      | `Unit u -> B0_unit.name u, "unit"
      | `Lib l -> Libname.name (Lib.libname l), "lib"
      in
      let add_modref modref acc =
        let name = Modref.name modref in
        let digest = Modref.digest modref in
        let deps = match String.Map.find_opt digest deps with
        | None -> []
        | Some deps -> String.Set.elements deps
        in
        (name, digest, entity_name, entity_kind, deps) :: acc
      in
      let add_cobj acc cobj =
        Modref.Set.fold add_modref (Cobj.defs cobj) acc
      in
      List.fold_left add_cobj acc cobjs
    in
    let nodes = List.fold_left add_entity [] (Cobj_index.by_entity index) in
    Jsong.to_string (Jsong.list json_node nodes)

  let of_build env
      ~format ~reduce_deps:reduce ~x_units ~x_packs ~env_libs ~x_libs ~show
    =
    Log.if_error ~use:Os.Exit.some_error @@
    let* index =
      let kind =
        (* If we want to change this to lookup archives we need to review
           Cobj_index.to_digest_dag and Cobj which lumps individual archive
           member dependencies into a single set *)
        ".cmx"
      in
      Cobj_index.of_build ~kind env ~x_units ~x_packs ~env_libs ~x_libs
    in
    let deps = Cobj_index.to_digest_dag index in
    let deps = if reduce then Digest_dag.transitive_reduction deps else deps in
    let* () = match format with
    | `Dot rankdir ->
        let g = B0_dot.to_string (dot_graph ~rankdir ~deps index) in
        begin match show with
        | None -> Fmt.pr "%s@." g; Ok ()
        | Some filename ->
            let* svg = dot_to_svg env g in
            show_url env ~filename svg
        end
    | `Json ->
        let data = json ~deps index in
        begin match show with
        | None -> Fmt.pr "%s@." data; Ok ()
        | Some filename -> show_url env ~filename data
        end
    in
    Ok Os.Exit.ok
end

(* OCamlfind META files (for generation) *)

module Meta = struct
  type lib_info =
    { archive_name : string; (* ignored in case warning = Some _ *)
      description : string;
      libname : Libname.t;
      requires : string list;
      exports : string list;
      unit : B0_unit.t;
      warning : string option; }

  type lib =
    { basename : string;
      children : lib String.Map.t;
      lib_info : lib_info option; (* None, if there's no actual lib *) }

  type t =
    { version : string;
      root : lib;
      root_doc : string; (* Used if there's no lib_info in root *)  }

  let lib_info_of_unit unit =
    let libname = B0_unit.find_meta library unit |> Option.get in
    let archive_name = Libname.to_archive_name libname in
    let description = B0_unit.doc unit in
    let requires = B0_unit.find_or_default_meta requires unit in
    let requires = List.map Libname.to_string requires in
    let warning = B0_unit.find_meta B0_meta.warning unit in
    let requires, exports = match B0_unit.find_meta exports unit with
    | None -> requires, []
    | Some exps ->
        let exports = List.map Libname.to_string exps in
        let requires =
          List.distinct String.compare (requires @ exports) (* for compat *)
        in
        requires, exports
    in
    { archive_name; description; libname; requires; exports; unit; warning }

  let empty_node basename =
    { basename; children = String.Map.empty; lib_info = None }

  let add_lib lib unit =
    let rec loop unit lib = function
    | [] ->
        begin match lib.lib_info with
        | None -> { lib with lib_info = Some (lib_info_of_unit unit) }
        | Some i ->
            Fmt.failwith "@[<v>Library %a both defined by unit %a and %a@]"
              Libname.pp i.libname B0_unit.pp_name i.unit B0_unit.pp_name unit
        end
    | n :: ns ->
        let lib' = match String.Map.find_opt n lib.children with
        | None -> empty_node n | Some lib -> lib
        in
        let lib' = loop unit lib' ns in
        { lib with children = String.Map.add n lib' lib.children }
    in
    let libname = B0_unit.find_meta library unit |> Option.get in
    let root = Libname.root libname in
    let names = List.tl (Libname.segments libname) in
    if root = lib.basename then loop unit lib names else
    Fmt.failwith "@[<v>Library %a defined by unit %a not rooted in %a@,\
                  All libraries must belong to the same root.@]"
      Libname.pp libname B0_unit.pp_name unit Fmt.code root

  let of_units ~root_doc ~version units =
    try
      let root = match units with
      | [] -> empty_node ""
      | u :: _ as units ->
          let root = Libname.root (B0_unit.find_meta library u |> Option.get) in
          List.fold_left add_lib (empty_node root) units
      in
      Ok { version; root; root_doc }
    with
    | Failure e -> Error e

  let to_string meta =
    let line ls fmt = Printf.ksprintf (fun s -> s :: ls) fmt in
    let open' ls ~indent name = line ls "%spackage %S (" indent name in
    let close ls ~indent = line ls "%s)" indent in
    let field ls ~indent field v = line ls "%s%s = %S" indent field v in
    let rec loop meta indent ls lib =
      let is_top = lib.basename = meta.root.basename in
      let is_deprecated = match lib.lib_info with
      | None -> false | Some i -> Option.is_some i.warning
      in
      let ls = if is_top then ls else line ls "" in
      let ls, indent, close =
        if is_top then ls, indent, Fun.id else
        open' ls ~indent lib.basename,
        indent ^ "  ",
        fun ls -> close ls ~indent
      in
      let description =
        if is_top then meta.root_doc else
        match lib.lib_info with None -> "" | Some i -> i.description
      in
      let requires = match lib.lib_info with
      | None -> "" | Some i -> (String.concat " " i.requires)
      in
      let exports = match lib.lib_info with
      | None -> "" | Some i -> (String.concat " " i.exports)
      in
      let ls =
        if is_top || is_deprecated
        then ls
        else field ls ~indent "directory" lib.basename
      in
      let ls = field ls ~indent "description" description in
      let ls = field ls ~indent "version" meta.version in
      let ls = field ls ~indent "requires" requires in
      let ls =
        if exports = "" then ls else
        field ls ~indent "exports" exports
      in
      let ls = match lib.lib_info with
      | None -> ls
      | Some i ->
          match i.warning with
          | None ->
              let cma = i.archive_name ^ ".cma" in
              let cmxa = i.archive_name ^ ".cmxa" in
              let cmxs = i.archive_name ^ ".cmxs" in
              let ls = field ls ~indent "archive(byte)" cma in
              let ls = field ls ~indent "archive(native)" cmxa in
              let ls = field ls ~indent "plugin(byte)" cma in
              let ls = field ls ~indent "plugin(native)" cmxs in
              let ls = field ls ~indent "exists_if" (cma ^ " " ^ cmxa)in
              ls
          | Some msg -> field ls ~indent "warning" msg
      in
      let add_children _ lib ls = loop meta indent ls lib in
      let ls = String.Map.fold add_children lib.children ls in
      close ls
    in
    let ls = loop meta "" [] meta.root in
    String.concat "\n" (List.rev ls)
end

let meta_file_of_pack pack =
  match List.filter (B0_unit.mem_meta library) (B0_pack.units pack) with
  | [] -> Fmt.error "No OCaml library found in pack %a" B0_pack.pp_name pack
  | us ->
      let version = "\x25\x25VERSION_NUM\x25\x25" in
      let root_doc =
        B0_pack.derive_synopsis_and_description pack (B0_pack.meta pack)
        |> B0_meta.find_or_default B0_meta.synopsis
      in
      let* meta = Meta.of_units ~version ~root_doc us in
      Ok (Meta.to_string meta)

let meta env pack =
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* pack = B0_pack.get_or_hint pack in
  Log.if_error' ~use:Os.Exit.some_error @@
  let* meta = meta_file_of_pack pack in
  Log.stdout (fun m -> m "%s" meta);
  Ok Os.Exit.ok

open Cmdliner

let unit =
  let open Cmdliner.Term.Syntax in
  let doc = "OCaml support" in
  B0_unit.of_cmdliner_cmd "" ~doc @@ fun env u ->
  let man =
    [ `S Manpage.s_see_also;
      `P "Consult $(b,odig doc b0) for the b0 OCaml manual." ]
  in
  let crunch_cmd =
    let doc = "Crunch bytes into an OCaml string" in
    let exits = B0_std_cli.Exit.infos in
    let infile =
      let doc = "Input bytes from file $(docv). Use $(b,-) for $(b,stdin)." in
      Arg.(value & pos 0 B0_std_cli.filepath Fpath.dash & info [] ~doc)
    in
    let id =
      let doc = "OCaml identifier to use for the crunch." in
      let docv = "ID" and absent = "Derived from the basename of $(i,FILE)" in
      Arg.(value & opt (some string) None & info ["id"] ~doc ~docv ~absent)
    in
    Cmd.make (Cmd.info "crunch" ~doc ~man ~exits) @@
    Term.(const crunch $ id $ infile)
  in
  let list_cmd =
    let doc = "List buildable OCaml libraries" in
    let man =
      [ `S Manpage.s_description;
        `P "$(cmd) lists buildable OCaml libraries." ]
    in
    let no_pager = B0_pager.no_pager () in
    let exits = B0_std_cli.Exit.infos in
    let details = B0_cli.output_details in
    Cmd.make (Cmd.info "libs" ~doc ~man ~exits) @@
    Term.(const list $ details $ no_pager)
  in
  let meta_cmd =
    let doc = "Output ocamlfind META files" in
    let man =
      [ `S Manpage.s_description;
        `P "$(cmd) outputs OCaml META files for a given pack. This \
            generates a META file assuming an install with one library \
            per directory." ]
    in
    let pack =
      let doc = "The pack to use to generate the META file from. All the \
                 libraries in the pack must share the same root whose name \
                 is implied in the META file."
      in
      Arg.(value & pos 0 string "default" & info [] ~doc ~docv:"PACK")
    in
    Cmd.make (Cmd.info "META" ~doc ~man) @@
    Term.(const meta $ const env $ pack)
  in
  let module_deps_cmd =
    let doc = "Output module dependency graphs" in
    let man =
      [ `S Cmdliner.Manpage.s_synopsis;
        `P "$(b,b0) [$(b,-p) $(i,PACK)]… [$(b,-u) $(i,UNIT)]… \
            -- $(cmd) [$(i,OPTION)]…";
        `S Manpage.s_description;
        `P "$(cmd) outputs the dependency graph of built or used OCaml \
            modules. A pack or unit must be explicitely specified otherwise \
            the build is empty.";
        `Pre "$(b,b0 -p default --) \
              $(cmd) $(b,| dot -Tsvg | show-url -f graph.svg)"; `Noblank;
        `Pre "$(b,b0 -p default --) $(cmd) $(b,--show graph.svg)";
        `S "WARNING TODO";
        `P "For now $(cmd) is a bit simplistic, it only performs analysis on \
            $(b,.cmx) files. This needs to be extended to handle byte code \
            and mixed native and byte code builds."
      ]
    in
    Cmd.make (Cmd.info "module-deps" ~doc ~man) @@
    let+ format =
      let formats =
        [ "dot-lr", `Dot `LR; "dot-tb", `Dot `TB; "json", `Json ]
      in
      let doc = Printf.sprintf
          "Output format. $(docv) Must be %s. $(b,dot-*) are for dot \
           graphs with given rank directions; pipe to $(b,dot -Tsvg) to \
           generate an SVG file. See also option $(b,--show)."
          (Arg.doc_alts_enum formats)
      in
      let docv = "FMT" in
      Arg.(value & opt (enum formats) (`Dot `LR) &
           info ["f"; "format"] ~doc ~docv)
    and+ reduce_deps =
      let doc = "$(docv) indicates if transitive reduction of dependencies \
                 is performed."
      in
      Arg.(value & opt bool true & info ["reduce-deps"] ~doc)
    and+ x_units =
      let doc = "Exclude modules of unit $(docv)." in
      B0_cli.use_x_units ~doc ()
    and+ x_packs =
      let doc = "Exclude modules of units of $(docv)." in
      B0_cli.use_x_packs ~doc ()
    and+ x_libs =
      let doc =
        "Exclude modules of library $(docv). Whether in the build, or in \
         the environment."
      in
      let docv = "LIB" in
      Arg.(value & opt_all string [] & info ["x-lib"] ~doc ~docv)
    and+ show =
      let doc =
        "Open output in a viewer. $(docv) is a temporary \
         file name which must have a correct file extension ($(b,.svg) for \
         dot). It allows reloads accross invocations. For dot \
         output using this option is equivalent to pipe to \
         $(b,dot -Tsvg | show-url -f) $(docv)."
      in
      let docv = "FILENAME" in
      Arg.(value & opt (some string) None & info ["s"; "show"] ~doc ~docv)
    and+ env_libs =
      let doc = "Include library dependencies from the environment." in
      Arg.(value & flag & info ["e"; "env-libs"] ~doc)
    in
    Module_deps.of_build env
      ~format ~reduce_deps ~x_units ~x_packs ~env_libs ~x_libs ~show
  in
  let man =
    [ `S Manpage.s_description;
      `P "$(cmd) has a few tools for OCaml";
      `Blocks man ]
  in
  let name = B0_unit.name u in
  Cmd.group (Cmd.info name ~doc ~man) @@
  [ crunch_cmd; list_cmd; meta_cmd; module_deps_cmd]

let ocaml env use_utop dry_run x_units x_packs args =
  Log.if_error ~use:Os.Exit.some_error @@
  let b = B0_env.build env in
  let units = B0_build.did_build b in
  let* x_units = B0_cli.get_excluded_units ~x_units ~x_packs in
  let x_units = B0_unit.Set.filter (B0_unit.has_tag tag) x_units in
  let units = B0_unit.Set.filter (B0_unit.has_tag tag) units in
  begin match B0_unit.Set.is_empty units with
  | true -> Log.warn (fun m -> m "The build has no OCaml entities to load.")
  | false ->
      Log.info @@ fun m ->
      m "Did build: @[%a@]"
        Fmt.(iter B0_unit.Set.iter ~sep:sp B0_unit.pp_name) units;
  end;
  run_ocaml env ~use_utop ~dry_run ~args ~x_units units

let ocaml_ocaml_cmd env u =
  (* N.B. We have that separately for now because we can't
     specify separate store arguments for cmdliner subcommands *)
  let man =
    [ `S Cmdliner.Manpage.s_synopsis;
      `P "$(b,b0) [$(b,-p) $(i,PACK)]… [$(b,-u) $(i,UNIT)]… \
          -- .ocaml.repl [$(i,OPTION)]… -- $(i,ARG)…";
      `S Cmdliner.Manpage.s_description;
      `P "$(cmd) loads the build you specify for the action \
          in the $(b,ocaml) interactive toplevel. This also \
          gives access to modules that may end up being private \
          at install time.";
      `S Cmdliner.Manpage.s_arguments;
    ]
  in
  let name = B0_unit.name u and doc = B0_unit.doc u in
  Cmd.make (Cmd.info name ~doc ~man) @@
  run_ocaml_term ocaml env

let unit_repl =
  let doc = "Load your build in the ocaml REPL" in
  let store = B0_store.[B (Code.built, Fut.return (Code.Set.singleton Byte))] in
  B0_unit.of_cmdliner_cmd ~store "repl" ocaml_ocaml_cmd ~doc

let () = B0_scope.close ()
