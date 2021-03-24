(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Fut.Syntax
open B00

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

      (* XXX These are Windows specific and needed by cl.exe
         For cc is likely that we need to add LD_LIBRARY_PATH,
         C_INCLUDE_PATH, LIBRARY_PATH. Either we add them in bulk
         or we make them depend on the configuration. *)
      "SystemRoot"; "INCLUDE"; "LIB"; ]

  let ocamlc = Tool.by_name ~vars:comp_env_vars "ocamlc"
  let ocamlopt = Tool.by_name ~vars:comp_env_vars "ocamlopt"
  let ocamldep = Tool.by_name ~vars:comp_env_vars "ocamldep"
  let ocamlmklib =
    Tool.by_name ~vars:("OCAML_FLEXLINK" :: comp_env_vars) "ocamlmklib"

  let ocamlobjinfo = Tool.by_name ~vars:comp_env_vars "ocamlobjinfo"

  (* Toplevels *)

  let top_env_vars =
    [ "CAML_LD_LIBRARY_PATH"; "CAMLRUNPARAM";
      "OCAMLTOP_INCLUDE_PATH";
      "HOME"; "OCAMLLIB"; "OCAMLRUN_PARAM"; "OCAMLTOP_UTF_8"; "PATH"; "TERM"; ]

  let ocaml = Tool.by_name ~vars:top_env_vars "ocaml"
  let ocamlnat = Tool.by_name ~vars:top_env_vars "ocamlnat"
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
    let where = where |> Result.to_failure in
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
    let parse_line i l acc = match String.cut_left ~sep:":" l with
    | None -> acc
    | Some (k, v) -> String.Map.add (String.trim k) (String.trim v) acc
    in
    let s = String.trim s in
    Result.bind (B00_lines.fold ?file s parse_line String.Map.empty) @@
    fun fields -> match of_string_map fields with
    | Ok v -> Ok v
    | Error e -> B00_lines.err_file ?file (Fmt.str "OCaml config: %s" e)

  let write m ~comp ~o =
    let comp = Memo.tool m comp in
    Memo.spawn m ~writes:[o] ~stdout:(`File o) @@
    comp (Cmd.atom "-config")

  let read m file =
    let* s = Memo.read m file in
    Fut.return (of_string ~file s |> Memo.fail_if_error m)
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
          (module S : B00_std.Stdlib_set.S with type elt = elt and type t = set)
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
      let of_string ?(file = Fpath.dash) ?src_root data =
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
          | Error j -> B00_lines.err n "%d: illegal escape" j
          | Ok p ->
              match Fpath.of_string p with
              | Error e -> B00_lines.err n "%s" e
              | Ok p -> p
        in
        let parse_line ~src_root n line acc =
          if line = "" then acc else
          match String.cut_right (* right, windows drives *) ~sep:":" line with
          | None -> B00_lines.err n "cannot parse line: %S" line
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
        B00_lines.fold ~file data (parse_line ~src_root) Fpath.Map.empty

      let write ?src_root m ~srcs ~o =
        let ocamldep = Memo.tool m Tool.ocamldep in
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
        Memo.spawn m ?cwd ~reads:srcs ~writes:[o] ~stdout:(`File o) @@
        ocamldep Cmd.(atom "-slash" % "-modules" %% paths srcs')

      let read ?src_root m file =
        let* s = Memo.read m file in
        Fut.return (of_string ?src_root ~file s |> Memo.fail_if_error m)
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
            Memo.notify m `Warn
              "@[<v>%a:@,File ignored. %a's module %s defined by file:@,%a:@]"
              Fpath.pp_unquoted f Name.pp mname kind Fpath.pp_unquoted f';
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
      let exts = B00_fexts.v (".mli" :: if only_mlis then [] else [".ml"]) in
      let srcs = B00_fexts.find_files exts srcs in
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
  let parse_file_path n line =
    let len = String.length file_prefix in
    match Fpath.of_string (String.drop_left len line) with
    | Ok file -> file
    | Error e -> B00_lines.err n "%s" e

  let rec parse_ldeps acc file defs deps ldeps name n = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | (l :: ls) as data ->
      match String.cut_right ~sep:"\t" l with
      | None -> parse_file acc file defs deps ldeps n data
      | Some (_, ldep) ->
          let ldeps = String.Set.add (String.trim ldep) ldeps in
          parse_ldeps acc file defs deps ldeps name (n + 1) ls

  and parse_deps acc file defs deps ldeps name n = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | (l :: ls) as data ->
      match String.cut_right ~sep:"\t" l with
      | None ->
          begin match l with
          | l when String.starts_with "Implementations imported:" l ||
                   String.starts_with "Required globals:" l ->
              parse_ldeps acc file defs deps ldeps name (n + 1) ls
          | _ ->
              parse_file acc file defs deps ldeps n data
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
              parse_deps acc file defs deps ldeps name (n + 1) ls
          | exception Invalid_argument _ ->
              (* skip undigested deps *)
              match dhex <> "" && dhex.[0] = '-' with
              | true -> parse_deps acc file defs deps ldeps name (n + 1) ls
              | false -> B00_lines.err n "%S: could not parse digest" dhex

  and parse_unit acc file defs deps ldeps name n = function
  | [] -> B00_lines.err n "unexpected end of input"
  | line :: rest when String.starts_with "Interfaces imported:" line ->
      parse_deps acc file defs deps ldeps name (n + 1) rest
  | _ :: rest -> parse_unit acc file defs deps ldeps name (n + 1) rest

  and parse_file acc file defs deps ldeps n = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | l :: ls when String.starts_with "Unit name" l ||
                 String.starts_with "Name" l ->
      begin match String.cut_left ~sep:":" l with
      | None -> assert false
      | Some (_, name) ->
          parse_unit acc file defs deps ldeps (String.trim name) (n + 1) ls
      end
  | l :: ls when String.starts_with file_prefix l ->
      let acc = make_cobj file defs deps ldeps :: acc in
      let file = parse_file_path n l in
      parse_file
        acc file Mod.Ref.Set.empty Mod.Ref.Set.empty String.Set.empty (n + 1) ls
  | _ :: ls -> parse_file acc file defs deps ldeps (n + 1) ls

  and parse_files acc n = function
  | [] -> acc
  | l :: ls when String.starts_with file_prefix l ->
      let file = parse_file_path n l in
      parse_file
        acc file Mod.Ref.Set.empty Mod.Ref.Set.empty String.Set.empty (n + 1) ls
  | l :: ls -> parse_files acc (n + 1) ls

  let of_string ?file data =
    try Ok (parse_files [] 1 (B00_lines.of_string data)) with
    | Failure e -> B00_lines.err_file ?file e

  let write m ~cobjs ~o =
    (* FIXME add [src_root] so that we can properly unstamp. *)
    let ocamlobjinfo = Memo.tool m Tool.ocamlobjinfo in
    Memo.spawn m ~reads:cobjs ~writes:[o] ~stdout:(`File o) @@
    ocamlobjinfo Cmd.(atom "-no-approx" % "-no-code" %% paths cobjs)

  let read m file =
    let* s = B00.Memo.read m file in
    Fut.return (of_string ~file s |> Memo.fail_if_error m)
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
    let v s = of_string s |> Result.to_invalid_arg
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
      c_stubs : Fpath.t list; }

  let v ~name ~requires ~dir ~cmis ~cmxs ~cma ~cmxa ~c_archive ~c_stubs =
    { name; requires; dir; cmis; cmxs; cma; cmxa; c_archive; c_stubs }

  let of_dir m ~clib_ext ~name ~requires ~dir ~archive =
    let rec loop cmis cmxs cma cmxa c_archive c_stubs = function
    | [] -> v ~name ~requires ~dir ~cmis ~cmxs ~cma ~cmxa ~c_archive ~c_stubs
    | f :: fs ->
        let is_lib_archive f = match archive with
        | None -> false
        | Some a -> String.equal (Fpath.basename ~no_ext:true f) a
        in
        match Fpath.get_ext f with
        | ".cmi" ->
            Memo.file_ready m f;
            loop (f :: cmis) cmxs cma cmxa c_archive c_stubs fs
        | ".cmx" ->
            Memo.file_ready m f;
            loop cmis (f :: cmxs) cma cmxa c_archive c_stubs fs
        | ".cma" ->
            let cma = match is_lib_archive f with
            | true -> Memo.file_ready m f; Some f
            | false -> cma
            in
            loop cmis cmxs cma cmxa c_archive c_stubs fs
        | ".cmxa" ->
            let cmxa = match is_lib_archive f with
            | true -> Memo.file_ready m f; Some f
            | false -> cmxa
            in
            loop cmis cmxs cma cmxa c_archive c_stubs fs
        | ext when String.equal ext clib_ext ->
            Memo.file_ready m f;
            let c_archive, c_stubs = match is_lib_archive f with
            | true -> Some f, c_stubs
            | false -> c_archive, (f :: c_stubs)
            in
            loop cmis cmxs cma cmxa c_archive c_stubs fs
        | _ ->
            loop cmis cmxs cma cmxa c_archive c_stubs fs
    in
    Fut.return @@
    Result.map_error (fun e -> Fmt.str "library %a: %s" Name.pp name e) @@
    Result.bind (Os.Dir.fold_files ~recurse:false Os.Dir.path_list dir []) @@
    fun fs -> Ok (loop [] [] None None None [] fs)

  let name l = l.name
  let requires l = l.requires
  let dir l = l.dir
  let cmis l = l.cmis
  let cmxs l = l.cmxs
  let cma l = l.cma
  let cmxa l = l.cmxa
  let c_archive l = l.c_archive
  let c_stubs l = l.c_stubs

  (* Resolvers *)

  module Resolver = struct

    (* FIXME rework erroring, for now we are not using the mecanisms
       and they likely need to be tweaked. *)

    type lib = t

    (* Resolution scopes *)

    type scope_find = Conf.t -> B00.Memo.t -> Name.t -> lib option Fut.t
    type scope_suggest = Conf.t -> B00.Memo.t -> Name.t -> string option Fut.t
    type scope = { name : string; find : scope_find; suggest : scope_suggest; }

    let scope_name s = s.name
    let scope_find s = s.find
    let scope_suggest s = s.suggest
    let scope ~name ~find ~suggest = { name; find; suggest }

    module Ocamlpath = struct
      (* Stubbed at the moment *)

      let find ~cache_dir ~ocamlpath conf m n = Fut.return None
      let suggest conf m n = Fut.return None
      let scope ~cache_dir ~ocamlpath =
        let find = find ~cache_dir ~ocamlpath in
        { name = "OCAMLPATH"; find; suggest }
    end

    let ocamlpath = Ocamlpath.scope

    module Ocamlfind = struct
      let tool = B00.Tool.by_name "ocamlfind"

      let parse_info m ?(file = Fpath.dash) ~name s =
        let parse_requires requires =
          let to_libname s =
            Result.to_failure @@
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
          match String.cut_right "." a with
          | None -> Some a | Some (a, _ext) -> Some a
        in
        try
          match String.split_on_char ':' (String.trim s) with
          | [meta; dir; archive; requires] ->
              let requires = parse_requires requires in
              let archive = parse_archive archive in
              let dir =
                Result.to_failure @@
                Result.map_error (Fmt.str "library directory: %s") @@
                Fpath.of_string dir
              in
              Ok (meta, requires, dir, archive)
          | _ -> Fmt.failwith "could not parse %S" s
        with
        | Failure e -> Fmt.error "@[<v>%a: %s@]" Fpath.pp_unquoted file e

      (* FIXME need to solve the META file read.
         FIXME post exec is still super messy, check if we can make it
         to use Memo.t *)

      let write_info m n ~o =
        (* FIXME better [n] not found error *)
        let ocamlfind = Memo.tool m tool in
        let lib, predicates = match Name.to_string n with
        | "ocaml.threads" | "threads" | "threads.posix" ->
            "threads.posix", "byte,native,mt,mt_posix"
        | n -> n, "byte,native"
        in
        let post_exec op = match B000.Op.status op with
        | B000.Op.Done ->
            begin match Option.get (B000.Op.Spawn.exit (B000.Op.Spawn.get op))
            with
            | `Exited 2 ->
                (* FIXME checktypo *)
                let err = Fmt.str "OCaml library %a not found" Name.pp n in
                B000.Op.set_status op (B000.Op.(Failed (Exec (Some err))))
            | _ -> ()
            end
        | _ -> ()
        in
        let success_exits = [0; 2 (* not found *) ] in
        let info =
          (* We use %A otherwise whith %a we get a blank line if there's
             no archive. Technically though we only support single library
             archives *)
          "%m:%d:%A:%(requires)"
        in
        let stdout = `File o in
        Memo.spawn m ~success_exits ~reads:[] ~writes:[o] ~stdout ~post_exec @@
        ocamlfind Cmd.(atom "query" % lib % "-predicates" % predicates %
                       "-format" % info)

      let read_info m clib_ext name file =
        let* s = Memo.read m file in
        match parse_info ~file m ~name s with
        | Error _ as e -> Memo.fail_if_error m e
        | Ok (_meta, requires, dir, archive) ->
            let* lib = of_dir m ~clib_ext ~name ~requires ~dir ~archive in
            Fut.return (Some (Memo.fail_if_error m lib))

      let find ~cache_dir ~ocamlpath conf m n =
        (* This never returns None we should factor error reporting
           in *)
        let clib_ext = Conf.lib_ext conf in
        let fname = Fmt.str "ocamlfind.%s" (Name.to_string n) in
        let o = Fpath.(cache_dir / fname) in
        write_info m n o;
        read_info m clib_ext n o

      let suggest conf m n = Fut.return None
      let scope ~cache_dir =
        let find = find ~cache_dir ~ocamlpath in
        { name = "ocamlfind"; find; suggest }
    end

    let ocamlfind = Ocamlfind.scope

    type t =
      { memo : Memo.t;
        conf : Conf.t;
        scopes : scope list;
        mutable libs : lib option Fut.t Name.Map.t; }

    let create memo conf scopes =
      let memo = B00.Memo.with_mark memo "ocamlib" in
      { memo; conf; scopes; libs = Name.Map.empty }

    let ocaml_conf r = r.conf
    let find r n = match Name.Map.find_opt n r.libs with
    | Some v -> v
    | None ->
        let rec loop r n = function
        | [] -> Fut.return None
        | s :: ss ->
            let fut = scope_find s r.conf r.memo n in
            let* l = fut in
            match l with
            | None -> loop r n ss
            | Some _ -> r.libs <- Name.Map.add n fut r.libs; fut
        in
        loop r n r.scopes

    let get r n = Fut.bind (find r n) @@ function
    | None -> Memo.fail r.memo "No OCaml library %a found" Name.pp n
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
  let get_var parse var m = (* FIXME move that to Memo.env ? *)
    let env = Env.env (Memo.env m) in
    match String.Map.find_opt var env with
    | None | Some "" -> None
    | Some v ->
        match parse v with
        | Error e -> Memo.fail m "parsing %a: %s" Fmt.(code string) var v
        | Ok v -> Some v

  let get m ps = match ps with
  | Some ps -> Fut.return ps
  | None ->
      match get_var Fpath.list_of_search_path "OCAMLPATH" m with
      | Some ps -> Fut.return ps
      | None ->
          match get_var Fpath.of_string "OPAM_SWITCH_PREFIX" m with
          | Some p -> Fut.return [Fpath.(p / "lib")]
          | None ->
              Memo.fail m
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
    Cmd.paths ~slip:"-I" @@ Fpath.uniquify @@ List.map Fpath.parent files

  let c_to_o ?post_exec ?k m ~comp ~opts ~reads ~c ~o =
    let cwd = Fpath.parent o
      (* We can't use `-c` and `-o` on C files see
         https://github.com/ocaml/ocaml/issues/7677 so we cwd to the
         output directory to perform the spawn. *)
    in
    let incs = incs_of_files reads in
    Memo.spawn m ?post_exec ?k ~reads:(c :: reads) ~writes:[o] ~cwd @@
    (Memo.tool m comp) Cmd.(atom "-c" %% opts %% unstamp (incs %% path c))

  let mli_to_cmi ?post_exec ?k ~and_cmti m ~comp ~opts ~reads ~mli ~o =
    let base = Fpath.strip_ext o in
    let stamp = Fpath.basename base in
    let reads = mli :: reads in
    let writes = o :: if and_cmti then [Fpath.(base + ".cmti")] else [] in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmti (Cmd.atom "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path mli)) in
    Memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    (Memo.tool m comp) Cmd.(atom "-c" %% bin_annot %% opts % "-o" %% io)

  let ml_to_cmo ?post_exec ?k ~and_cmt m ~opts ~reads ~has_cmi ~ml ~o =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let base = Fpath.strip_ext o in
    let stamp = Fpath.basename base (* output depends on mod name *) in
    let reads = ml :: reads in
    let writes =
      o :: (add_if and_cmt Fpath.(base + ".cmt") @@
            add_if (not has_cmi) Fpath.(base + ".cmi") [])
    in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmt (Cmd.atom "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path ml)) in
    Memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlc Cmd.(atom "-c" %% bin_annot %% opts % "-o" %% io)

  let ml_to_cmx ?post_exec ?k ~and_cmt m ~opts ~reads ~has_cmi ~ml ~o =
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let base = Fpath.strip_ext o in
    let stamp = Fpath.basename base (* output depends on mod name *) in
    let reads = ml :: reads in
    let writes =
      o :: Fpath.(base + ".o") ::
      (add_if and_cmt Fpath.(base + ".cmt") @@
       add_if (not has_cmi) Fpath.(base + ".cmi") [])
    in
    let incs = incs_of_files reads in
    let bin_annot = Cmd.if' and_cmt (Cmd.atom "-bin-annot") in
    let io = Cmd.(unstamp (path o %% incs %% path ml)) in
    Memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlopt Cmd.(atom "-c" %% bin_annot %% opts % "-o" %% io)

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
    let ocamlmklib = Memo.tool m Tool.ocamlmklib in
    let o = Fpath.(odir / cstubs_name oname) in
    let writes =
      Fpath.(odir / cstubs_clib oname lib_ext) ::
      add_if (Conf.has_dynlink conf) Fpath.(odir / cstubs_dll oname dll_ext) []
    in
    Memo.spawn ?post_exec ?k m ~reads:c_objs ~writes @@
    ocamlmklib
      Cmd.(atom "-o" %% unstamp (path o) %% opts %% unstamp (paths c_objs))

  let byte ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let cstubs_opts =
      if not has_cstubs then Cmd.empty else
      let lib = Fmt.str "-l%s" (cstubs_name oname) in
      Cmd.(atom "-cclib" % lib %%
           if' (Conf.has_dynlink conf) (atom "-dllib" % lib))
    in
    let cma = Fpath.(odir / Fmt.str "%s.cma" oname) in
    Memo.spawn m ~reads:cobjs ~writes:[cma] @@
    ocamlc Cmd.(atom "-a" % "-o" %% unstamp (path cma) %% opts %% cstubs_opts %%
                unstamp (paths cobjs))

  let native ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname =
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let lib_ext = Conf.lib_ext conf in
    let obj_ext = Conf.obj_ext conf in
    let cstubs_opts =
      if not has_cstubs then Cmd.empty else
      Cmd.(atom "-cclib" % Fmt.str "-l%s" (cstubs_name oname))
    in
    let cmxa = Fpath.(odir / Fmt.str "%s.cmxa" oname) in
    let cmxa_clib = Fpath.(odir / Fmt.str "%s%s" oname lib_ext) in
    let c_objs = List.rev_map (Fpath.set_ext obj_ext) cobjs in
    let reads = List.rev_append c_objs cobjs in
    Memo.spawn m ?post_exec ?k ~reads ~writes:[cmxa; cmxa_clib] @@
    ocamlopt Cmd.(atom "-a" % "-o" %% unstamp (path cmxa) %% opts %%
                  cstubs_opts %% unstamp (paths cobjs))

  let code ?post_exec ?k m ~conf ~opts ~code ~has_cstubs ~cobjs ~odir ~oname =
    let archive = match code with `Byte -> byte | `Native -> native in
    archive ?post_exec ?k m ~conf ~opts ~has_cstubs ~cobjs ~odir ~oname

  let native_dynlink ?post_exec ?k m ~conf ~opts ~has_cstubs ~cmxa ~o =
    let lib_ext = Conf.lib_ext conf in
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let cmxa_clib = Fpath.(cmxa -+ lib_ext) in
    let cstubs_opts, reads =
      if not has_cstubs then Cmd.empty, [cmxa; cmxa_clib] else
      (* Fixme do this on a cstubs path *)
      let oname = Fpath.basename ~no_ext:true cmxa in
      let cstubs_dir = Fpath.(parent cmxa) in
      let cstubs = Fpath.(cstubs_dir / cstubs_clib oname lib_ext) in
      let inc = Cmd.(atom "-I" %% unstamp (path cstubs_dir)) in
      Cmd.(inc %% unstamp (path cstubs)), [cstubs; cmxa; cmxa_clib]
    in
    Memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlopt Cmd.(atom "-shared" % "-linkall" % "-o" %% unstamp (path o) %%
                  opts %% cstubs_opts %% unstamp (path cmxa))
end

module Link = struct

  (* FIXME Add cstubs archives of cm[x]a to [reads] ? Do we need it ?
     that would entail an ocamlobjinfo + C library lookup *)

  let cstubs_incs objs =
    let add_inc acc obj = Fpath.Set.add (Fpath.parent obj) acc in
    let incs = List.fold_left add_inc Fpath.Set.empty objs in
    Cmd.paths ~slip:"-I" (Fpath.Set.elements incs)

  let byte ?post_exec ?k m ~conf ~opts ~c_objs ~cobjs ~o =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let reads = List.rev_append cobjs c_objs in
    let incs = cstubs_incs cobjs in
    Memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlc Cmd.(atom "-o" %% unstamp (path o) %% opts %%
                unstamp (incs %% paths c_objs %% paths cobjs))

  let native ?post_exec ?k m ~conf ~opts ~c_objs ~cobjs ~o =
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let obj_ext = Conf.obj_ext conf in
    let lib_ext = Conf.lib_ext conf in
    let cobj_side_obj cobj =
      let ext = if Fpath.has_ext ".cmx" cobj then obj_ext else lib_ext in
      Fpath.set_ext ext cobj
    in
    let incs = cstubs_incs cobjs in
    let reads =
      let sides = List.rev_map cobj_side_obj cobjs in
      List.rev_append cobjs (List.rev_append sides c_objs)
    in
    Memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlopt Cmd.(atom "-o" %% unstamp (path o) %% opts %%
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

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
