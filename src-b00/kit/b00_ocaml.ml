(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

module Tool = struct
  let comp_env_vars =
    [ "CAMLLIB"; "CAMLSIGPIPE"; "CAML_DEBUG_FILE"; "CAML_DEBUG_SOCKET";
      "CAML_LD_LIBRARY_PATH"; "BUILD_PATH_PREFIX_MAP"; "OCAMLDEBUG"; "OCAMLLIB";
      "OCAMLPROF_DUMP"; "OCAMLRUNPARAM"; "OCAML_COLOR"; "OCAML_FLEXLINK";
      "OCAML_INSTR_FILE"; "OCAML_INSTR_START"; "OCAML_INSTR_STOP";
      "OCAML_SPACETIME_INTERVAL"; "OCAML_SPACETIME_SNAPSHOT_DIR"; "PATH";
      "TERM"; "__AFL_SHM_ID";

      (* FIXME These are Windows specific and needed by cl.exe
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

  let top_env_vars =
    [ "CAML_LD_LIBRARY_PATH"; "CAMLRUNPARAM";
      "OCAMLTOP_INCLUDE_PATH";
      "HOME"; "OCAMLLIB"; "OCAMLRUN_PARAM"; "OCAMLTOP_UTF_8"; "PATH"; "TERM"; ]

  let ocaml = Tool.by_name ~vars:top_env_vars "ocaml"
  let ocamlnat = Tool.by_name ~vars:top_env_vars "ocamlnat"
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
      obj_ext : string; }

  let find k c = match String.Map.find k c.fields with
  | exception Not_found -> None | v -> Some v

  let version c = c.version
  let where c = c.where
  let asm_ext c = c.asm_ext
  let exe_ext c = c.exe_ext
  let dll_ext c = c.dll_ext
  let lib_ext c = c.lib_ext
  let obj_ext c = c.obj_ext
  let to_string_map c = c.fields
  let of_string_map fields =
    try
      let err = Fmt.failwith in
      let err_key k = err "key %a not found." Fmt.(code string) k in
      let find k fs = match String.Map.find k fs with
      | exception Not_found -> err_key k | v -> v
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
      Ok { fields; version; where; asm_ext; dll_ext; exe_ext; lib_ext; obj_ext }
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
    comp (Cmd.arg "-config")

  let read m file k =
    Memo.read m file @@ fun s ->
    k (of_string ~file s |> Memo.fail_if_error m)

  (* Build orchestration *)

  type built_code = [ `Byte | `Native | `Both ]

  let build_code =
    let det _ _ k = k `Auto in
    Store.key det

  let built_code =
    let det s m k =
      Store.get s build_code @@ function
      | #built_code as v -> k v
      | `Auto when Option.is_some (Memo.tool_opt m Tool.ocamlopt) -> k `Native
      | `Auto  -> k `Byte

    in
    Store.key det

  let auto_comp s m k =
    Store.get s built_code @@ function
    | `Native | `Both -> k Tool.ocamlopt
    | `Byte -> k Tool.ocamlc

  let key : t B00.Store.key =
    let mark = "b00_ocaml.conf" in
    let det s m k =
      auto_comp s m @@ fun comp ->
      let file = Fpath.(Store.dir s / mark) in
      write m ~comp ~o:file;
      read m file k
    in
    Store.key ~mark det

  (* TODO remove that *)

  let ocamlc_bin = Cmd.arg "ocamlc"
  let exists m k = match Os.Cmd.find ocamlc_bin |> Memo.fail_if_error m with
  | None -> k false | Some _ -> k true

  let if_exists m f k =
    exists m @@ function false -> k None | true -> f () (fun v -> k (Some v))

  let run m cmd k =
    let ocamlc = Os.Cmd.must_find ocamlc_bin |> Memo.fail_if_error m in
    k (Os.Cmd.run_out ~trim:true Cmd.(ocamlc %% cmd))

  let stdlib_dir m () k =
    run m (Cmd.arg "-where") @@ fun r ->
    k @@ Memo.fail_if_error m @@ Result.bind r @@ fun s -> Fpath.of_string s

end

module Mod_name = struct
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

module Mod_ref = struct
  type t = Mod_name.t * Digest.t
  let v n d = (String.Ascii.capitalize n, d)
  let name = fst
  let digest = snd
  let equal (_, d0) (_, d1) = Digest.equal d0 d1
  let compare (n0, d0) (n1, d1) = match Mod_name.compare n0 n1 with
  | 0 -> Digest.compare d0 d1
  | c -> c

  let pp ppf (n, d) = Fmt.pf ppf "@[%s %a@]" (Digest.to_hex d) Mod_name.pp n

  module T = struct
    type nonrec t = t
    let compare = compare
  end

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
      Fmt.pf ppf "@[<1>{%a}@]" (Fmt.iter_bindings ~sep:Fmt.sp iter pp_binding) m

    let pp ?sep pp_binding = Fmt.iter_bindings ?sep iter pp_binding
  end
end

module Cobj = struct
  type code = Byte | Native
  let archive_ext_of_code = function Byte -> ".cma" | Native -> ".cmxa"
  let object_ext_of_code = function Byte -> ".cmo" | Native -> ".cmx"

  type t =
    { file : Fpath.t;
      defs : Mod_ref.Set.t;
      deps : Mod_ref.Set.t;
      link_deps : Mod_ref.Set.t; (* deps whose name appear in required
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
      Fmt.field "defs" defs Mod_ref.Set.dump;
      Fmt.field "deps" deps Mod_ref.Set.dump;
      Fmt.field "link-deps" link_deps Mod_ref.Set.dump; ]

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make (T)
  module Map = Map.Make (T)

  let sort ?(deps = link_deps) cobjs =
    let rec loop cobjs_defs seen ext_deps cobjs = function
    | (c :: cs as l) :: todo ->
        begin match Mod_ref.Set.subset (defs c) seen with
        | true -> loop cobjs_defs seen ext_deps cobjs (cs :: todo)
        | false ->
            let seen = Mod_ref.Set.union (defs c) seen in
            let add_dep d (local_deps, ext_deps as acc) =
              if Mod_ref.Set.mem d seen then acc else
              match Mod_ref.Map.find d cobjs_defs with
              | exception Not_found -> local_deps, Mod_ref.Set.add d ext_deps
              | dep_cobj -> dep_cobj :: local_deps, ext_deps
            in
            let start = [], ext_deps in
            let local_deps, ext_deps =
              Mod_ref.Set.fold add_dep (deps c) start
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
    let add_def c d acc = Mod_ref.Map.add d c acc in
    let add_defs acc c = Mod_ref.Set.fold (add_def c) (defs c) acc in
    let cobjs_defs = List.fold_left add_defs Mod_ref.Map.empty cobjs in
    loop cobjs_defs Mod_ref.Set.empty Mod_ref.Set.empty [] (cobjs :: [])

  (* ocamlobjinfo output parsing, could be easier... *)

  let make_cobj file defs deps ldeps =
    let deps = Mod_ref.Set.diff deps defs in
    let link_deps =
      let keep m = String.Set.mem (Mod_ref.name m) ldeps in
      Mod_ref.Set.filter keep deps
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
          | l when String.is_prefix "Implementations imported:" l ||
                   String.is_prefix "Required globals:" l ->
              parse_ldeps acc file defs deps ldeps name (n + 1) ls
          | _ ->
              parse_file acc file defs deps ldeps n data
          end
      | Some (dhex, dname) ->
          let dhex = String.trim dhex in
          let dname = String.trim dname in
          match Digest.from_hex dhex with
          | digest ->
              let mref = Mod_ref.v dname digest in
              let defs, deps = match String.equal dname name with
              | true -> Mod_ref.Set.add mref defs, deps
              | false -> defs, Mod_ref.Set.add mref deps
              in
              parse_deps acc file defs deps ldeps name (n + 1) ls
          | exception Invalid_argument _ ->
              (* skip undigested deps *)
              match dhex <> "" && dhex.[0] = '-' with
              | true -> parse_deps acc file defs deps ldeps name (n + 1) ls
              | false -> B00_lines.err n "%S: could not parse digest" dhex

  and parse_unit acc file defs deps ldeps name n = function
  | [] -> B00_lines.err n "unexpected end of input"
  | line :: rest when String.is_prefix "Interfaces imported:" line ->
      parse_deps acc file defs deps ldeps name (n + 1) rest
  | _ :: rest -> parse_unit acc file defs deps ldeps name (n + 1) rest

  and parse_file acc file defs deps ldeps n = function
  | [] -> make_cobj file defs deps ldeps :: acc
  | l :: ls when String.is_prefix "Unit name" l || String.is_prefix "Name" l ->
      begin match String.cut_left ~sep:":" l with
      | None -> assert false
      | Some (_, name) ->
          parse_unit acc file defs deps ldeps (String.trim name) (n + 1) ls
      end
  | l :: ls when String.is_prefix file_prefix l ->
      let acc = make_cobj file defs deps ldeps :: acc in
      let file = parse_file_path n l in
      parse_file
        acc file Mod_ref.Set.empty Mod_ref.Set.empty String.Set.empty (n + 1) ls
  | _ :: ls -> parse_file acc file defs deps ldeps (n + 1) ls

  and parse_files acc n = function
  | [] -> acc
  | l :: ls when String.is_prefix file_prefix l ->
      let file = parse_file_path n l in
      parse_file
        acc file Mod_ref.Set.empty Mod_ref.Set.empty String.Set.empty (n + 1) ls
  | l :: ls -> parse_files acc (n + 1) ls

  let of_string ?file data =
    try Ok (parse_files [] 1 (B00_lines.of_string data)) with
    | Failure e -> B00_lines.err_file ?file e

  (* FIXME add [src_root] so that we can properly unstamp. *)

  let write m ~cobjs ~o =
    let ocamlobjinfo = Memo.tool m Tool.ocamlobjinfo in
    Memo.spawn m ~reads:cobjs ~writes:[o] ~stdout:(`File o) @@
    ocamlobjinfo Cmd.(arg "-no-approx" % "-no-code" %% paths cobjs)

  let read m file k =
    B00.Memo.read m file @@ fun s ->
    k (of_string ~file s |> Memo.fail_if_error m)
end

module Mod_src = struct
  module Deps = struct
    let of_string ?(file = Fpath.dash) ?src_root data =
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
            let add_mod acc m = Mod_name.Set.add m acc in
            let mods = String.cuts_left ~drop_empty:true ~sep:" " mods in
            let start = Mod_name.Set.singleton "Stdlib" in
            let mods = List.fold_left add_mod start mods in
            Fpath.Map.add file mods acc
      in
      B00_lines.fold ~file data (parse_line ~src_root) Fpath.Map.empty

    let write ?src_root m ~srcs ~o =
      let ocamldep = Memo.tool m Tool.ocamldep in
      let srcs', cwd = match src_root with
      | None -> srcs, None
      | Some root ->
          (* FIXME unfortunately this doesn't report parse error
             at the right place. So we don't do anything for now
             the output thus depends on the path location and can't
             be cached across machines.
          let rem_prefix src = Fpath.rem_prefix root src |> Option.get in
          List.map rem_prefix srcs, Some root
          *)
          srcs, None
      in
      Memo.spawn m ?cwd ~reads:srcs ~writes:[o] ~stdout:(`File o) @@
      ocamldep Cmd.(arg "-slash" % "-modules" %% paths srcs')

    let read ?src_root m file k =
      Memo.read m file @@ fun contents ->
      k (of_string ?src_root ~file contents |> Memo.fail_if_error m)
  end

  type t =
    { mod_name : Mod_name.t;
      opaque : bool;
      mli : Fpath.t option;
      mli_deps : Mod_name.Set.t;
      ml : Fpath.t option;
      ml_deps : Mod_name.Set.t }

  let v ~mod_name ~opaque ~mli ~mli_deps ~ml ~ml_deps =
    { mod_name; opaque; mli; mli_deps; ml; ml_deps }

  let mod_name m = m.mod_name
  let opaque m = m.opaque
  let mli m = m.mli
  let mli_deps m = m.mli_deps
  let ml m = m.ml
  let ml_deps m = m.ml_deps

  let file ~in_dir m ~ext =
    let fname = String.Ascii.uncapitalize (mod_name m) in
    Fpath.(in_dir / Fmt.str "%s%s" fname ext)

  let cmi_file ~in_dir m = file ~in_dir m ~ext:".cmi"
  let cmo_file ~in_dir m = file ~in_dir m ~ext:".cmo"
  let cmx_file ~in_dir m = file ~in_dir m ~ext:".cmx"
  let impl_file ~code ~in_dir m =
    (match code with
    | Cobj.Byte -> cmo_file
    | Cobj.Native -> cmx_file) ~in_dir m

  let as_intf_dep_files ?(init = []) ~in_dir m = cmi_file ~in_dir m :: init
  let as_impl_dep_files ?(init = []) ~code ~in_dir m = match code with
  | Cobj.Byte -> cmi_file ~in_dir m :: init
  | Cobj.Native ->
      match ml m with
      | None -> cmi_file ~in_dir m :: init
      | Some _ when m.opaque -> cmi_file ~in_dir m :: init
      | Some _ -> cmi_file ~in_dir m :: cmx_file ~in_dir m :: init

  let mod_name_map m ~kind files =
    let add acc f =
      let mname = Mod_name.of_filename f in
      match Mod_name.Map.find mname acc with
      | exception Not_found -> Mod_name.Map.add mname f acc
      | f' ->
          Memo.notify m `Warn
            "@[<v>%a:@,File ignored. %a's module %s defined by file:@,%a:@]"
            Fpath.pp_unquoted f Mod_name.pp mname kind Fpath.pp_unquoted f';
          acc
    in
    List.fold_left add Mod_name.Map.empty files

  let of_srcs m ~srcs ~src_deps  =
    let get_src_deps = function
    | None -> Mod_name.Set.empty
    | Some file ->
        match Fpath.Map.find file src_deps with
        | exception Not_found -> Mod_name.Set.empty
        | deps -> deps
    in
    let mlis, mls = List.partition (Fpath.has_ext ".mli") srcs in
    let mlis = mod_name_map m ~kind:"interface" mlis in
    let mls = mod_name_map m ~kind:"implementation" mls in
    let mod' mod_name mli ml =
      let mli_deps = get_src_deps mli in
      let ml_deps = get_src_deps ml in
      Some (v ~mod_name ~opaque:false ~mli ~mli_deps ~ml ~ml_deps)
    in
    Mod_name.Map.merge mod' mlis mls

  let sort ?stable ~deps name_map =
    let rec loop seen acc = function
    | [] -> seen, acc
    | src :: srcs ->
        if Mod_name.Set.mem src.mod_name seen then loop seen acc srcs else
        let seen = Mod_name.Set.add src.mod_name seen in
        let add_src_dep n acc = match Mod_name.Set.mem n seen with
        | true -> acc
        | false ->
            match Mod_name.Map.find_opt n name_map with
            | None -> acc
            | Some src -> src :: acc
        in
        let deps = Mod_name.Set.fold add_src_dep (deps src) [] in
        let seen, acc = loop seen acc deps in
        loop seen (src :: acc) srcs
    in
    let add_src _ src acc = src :: acc in
    let stable = Option.value ~default:[] stable in
    let todo = stable @ Mod_name.Map.fold add_src name_map [] in
    let _, acc = loop Mod_name.Set.empty [] todo in
    List.rev acc

  let find_local_deps map ns =
    let rec loop res remain deps = match Mod_name.Set.choose deps with
    | exception Not_found -> res, remain
    | dep ->
        let deps = Mod_name.Set.remove dep deps in
        match Mod_name.Map.find dep map with
        | m -> loop (m :: res) remain deps
        | exception Not_found ->
            loop res (Mod_name.Set.add dep remain) deps
    in
    loop [] Mod_name.Set.empty ns
end

module Compile = struct

  (* XXX We should properly investigate how to use BUILD_PATH_PREFIX_MAP.
     However for some reasons that were never not really answered by @gasche in
     https://github.com/ocaml/ocaml/pull/1515, the map does not affect
     absolute paths which severly limits its applicability.

     XXX At some point we would had -o OBJ src [-I inc...] this worked
     at least in 4.07 but not in 4.03, where apparently the order mattered. *)

  let add_if c v l = if c  then v :: l else l
  let debug = Cmd.arg "-g"

  let c_to_o ?post_exec ?k m ~hs ~c ~o =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let cwd =
      (* We can't use `-c` and `-o` on C files see
         https://github.com/ocaml/ocaml/issues/7677 so we cwd to the
         output directory to perform the spawn. *)
      Fpath.parent o
    in
    let incs = Fpath.uniquify @@ List.map Fpath.parent hs in
    let incs = Cmd.paths ~slip:"-I" incs in
    Memo.spawn m ?post_exec ?k ~reads:(c :: hs) ~writes:[o] ~cwd @@
    ocamlc Cmd.(debug % "-c" %% unstamp (incs %% path c))

  let mli_to_cmi
      ?post_exec ?k ?args:(more_args = Cmd.empty) ?(with_cmti = true) m
      ~reads ~mli ~o
    =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let incs = Fpath.uniquify @@ List.map Fpath.parent reads in
    let incs = Cmd.paths ~slip:"-I" incs in
    let bin_annot = Cmd.if' with_cmti (Cmd.arg "-bin-annot") in
    let stamp = Fpath.basename o in
    let reads = mli :: reads in
    let writes = o :: if with_cmti then [Fpath.(o -+ ".cmti")] else [] in
    Memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlc Cmd.(debug %% bin_annot % "-c" % "-o" %%
                unstamp (path o %% incs) %% more_args %% unstamp (path mli))

  let ml_to_cmo
      ?post_exec ?k ?args:(more_args = Cmd.empty) ?(with_cmt = true) m
      ~has_cmi ~reads ~ml ~o
    =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let incs = Fpath.uniquify @@ List.map Fpath.parent reads in
    let incs = Cmd.paths ~slip:"-I" incs in
    let bin_annot = Cmd.if' with_cmt (Cmd.arg "-bin-annot") in
    let stamp = Fpath.basename o in
    let reads = ml :: reads in
    let writes =
      o :: (add_if with_cmt Fpath.(o -+ ".cmt") @@
            add_if (not has_cmi) Fpath.(o -+ ".cmi") [])
    in
    Memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlc Cmd.(debug %% bin_annot % "-c" % "-o" %% unstamp (path o) %%
                unstamp incs %% more_args %% unstamp (path ml))

  let ml_to_cmx
      ?post_exec ?k ?args:(more_args = Cmd.empty) ?(with_cmt = true) m ~has_cmi
      ~reads ~ml ~o
    =
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let incs = Fpath.uniquify @@ List.map Fpath.parent reads in
    let incs = Cmd.paths ~slip:"-I" incs in
    let bin_annot = Cmd.if' with_cmt (Cmd.arg "-bin-annot") in
    let reads = ml :: reads in
    let stamp = Fpath.basename o in
    let base = Fpath.rem_ext o in
    let writes = [o; Fpath.(base + ".o")] in
    let writes = add_if with_cmt Fpath.(base + ".cmt") writes in
    let writes = add_if (not has_cmi) Fpath.(base + ".cmi") writes in
    Memo.spawn m ?post_exec ?k ~stamp ~reads ~writes @@
    ocamlopt Cmd.(debug %% bin_annot % "-c" % "-o" %% unstamp (path o) %%
                  unstamp incs %% more_args %% unstamp (path ml))

  let ml_to_impl
      ?post_exec ?k ?args ?with_cmt m ~code ~has_cmi ~reads ~ml ~o
    =
    (match code with Cobj.Byte -> ml_to_cmo | Cobj.Native -> ml_to_cmx)
      ?post_exec ?k ?args ?with_cmt m ~has_cmi ~reads ~ml ~o

  let cstubs_name name = Fmt.str "%s_stubs" name
  let cstubs_clib name ext_lib = Fmt.str "lib%s_stubs%s" name ext_lib
  let cstubs_dll name ext_dll = Fmt.str "dll%s_stubs%s" name ext_dll
  let cstubs_archives
      ?post_exec ?k ?args:(more_args = Cmd.empty) m ~c_objs ~odir ~oname
    =
    (* FIXME Conf *)
    let lib_ext = ".a" in
    let dll_ext = ".so" in
    let ocamlmklib = Memo.tool m Tool.ocamlmklib in
    let o = Fpath.(odir / cstubs_name oname) in
    let writes =
      [ Fpath.(odir / cstubs_clib oname lib_ext);
        Fpath.(odir / cstubs_dll oname dll_ext)] (* FIXME dynlink cond. *)
    in
    Memo.spawn ?post_exec ?k m ~reads:c_objs ~writes @@
    ocamlmklib Cmd.(debug % "-o" %% unstamp (path o) %% unstamp (paths c_objs))

  let byte_archive
      ?post_exec ?k ?args:(more_args = Cmd.empty) m ~has_cstubs ~cobjs ~odir
      ~oname
    =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let cstubs_opts = match has_cstubs with
    | false -> Cmd.empty
    | true ->
        let lib = Fmt.str "-l%s" (cstubs_name oname) in
        let cclib = Cmd.(arg "-cclib" % lib) in
        let dllib = Cmd.(arg "-dllib" % lib) in (* FIXME dynlink cond. *)
        Cmd.(cclib %% dllib)
    in
    let cma = Fpath.(odir / Fmt.str "%s.cma" oname) in
    Memo.spawn m ~reads:cobjs ~writes:[cma] @@
    ocamlc Cmd.(debug % "-a" % "-o" %% unstamp (path cma) %% cstubs_opts %%
                unstamp (paths cobjs))

  let native_archive
      ?post_exec ?k ?args:(more_args = Cmd.empty) m ~has_cstubs ~cobjs ~odir
      ~oname
    =
    (* FIXME Conf *)
    let lib_ext = ".a" in
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let cstubs_opts = match has_cstubs with
    | false -> Cmd.empty
    | true -> Cmd.(arg "-cclib" % Fmt.str "-l%s" (cstubs_name oname))
    in
    let cmxa = Fpath.(odir / Fmt.str "%s.cmxa" oname) in
    let cmxa_clib = Fpath.(odir / Fmt.str "%s%s" oname lib_ext) in
    Memo.spawn m ?post_exec ?k ~reads:cobjs ~writes:[cmxa; cmxa_clib] @@
    ocamlopt Cmd.(debug % "-a" % "-o" %% unstamp (path cmxa) %% cstubs_opts %%
                  unstamp (paths cobjs))

  let archive ?post_exec ?k ?args m ~code ~has_cstubs ~cobjs ~odir ~oname =
    (match code with Cobj.Byte -> byte_archive | Cobj.Native -> native_archive)
    ?post_exec ?k ?args m ~has_cstubs ~cobjs ~odir ~oname

  let native_dynlink_archive
      ?post_exec ?k ?args:(more_args = Cmd.empty) m ~has_cstubs ~cmxa ~o
    =
    let lib_ext = ".a" in
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    let cmxa_clib = Fpath.(cmxa -+ lib_ext) in
    let cstubs_opts, reads = match has_cstubs with
    | false -> Cmd.empty, [cmxa; cmxa_clib]
    | true ->
        (* Fixme do this on a cstubs path *)
        let oname = Fpath.basename ~no_ext:true cmxa in
        let cstubs_dir = Fpath.(parent cmxa) in
        let cstubs = Fpath.(cstubs_dir / cstubs_clib oname lib_ext) in
        let inc = Cmd.(arg "-I" %% unstamp (path cstubs_dir)) in
        Cmd.(inc %% unstamp (path cstubs)), [cstubs; cmxa; cmxa_clib]
    in
    Memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlopt Cmd.(debug % "-shared" % "-linkall" % "-o" %% unstamp (path o) %%
                  cstubs_opts %% unstamp (path cmxa))
end

module Link = struct
  module Deps = struct
    let write = Cobj.write
    let read m file k = Cobj.read m file @@ fun cobjs -> k (Cobj.sort cobjs)
  end

  (* FIXME How to add cstubs archives of cm[x]a to [reads].
     Do we need it ? *)

  let cstubs_incs objs =
    let add_inc acc obj = Fpath.Set.add (Fpath.parent obj) acc in
    let incs = List.fold_left add_inc Fpath.Set.empty objs in
    Cmd.paths ~slip:"-I" (Fpath.Set.elements incs)

  let byte_exe ?post_exec ?k ?args:(more_args = Cmd.empty) m ~c_objs ~cobjs ~o =
    let ocamlc = Memo.tool m Tool.ocamlc in
    let reads = List.rev_append cobjs c_objs in
    let incs = cstubs_incs cobjs in
    Memo.spawn m ?post_exec ?k ~reads ~writes:[o] @@
    ocamlc Cmd.(Compile.debug % "-custom" % "-o" %%
                unstamp (path o %% incs %% paths c_objs %% paths cobjs))

  let native_exe
      ?post_exec ?k ?args:(more_args = Cmd.empty) m ~c_objs ~cobjs ~o
    =
    let ocamlopt = Memo.tool m Tool.ocamlopt in
    (* FIXME lift that as args or ask for OCaml.Conf.t *)
    let obj_ext = ".o" in
    let lib_ext = ".a" in
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
    ocamlopt Cmd.(Compile.debug % "-o" %%
                  unstamp (path o %% incs %% paths c_objs %% paths cobjs))

  let exe ?post_exec ?k ?args m ~code ~c_objs ~cobjs ~o =
    (match code with Cobj.Byte -> byte_exe | Cobj.Native -> native_exe)
    ?post_exec ?k ?args m ~c_objs ~cobjs ~o
end

(* Libraries *)

module Ocamlpath = struct
  let get_var parse var m = (* FIXME move that to Memo.env ? *)
    let env = Env.env (Memo.env m) in
    match String.Map.find_opt var env with
    | None | Some "" -> None
    | Some v ->
        match parse v with
        | Error e -> Memo.fail m "parsing %a: %s" Fmt.(code string) var v
        | Ok v -> Some v

  let get m ps k = match ps with
  | Some ps -> k ps
  | None ->
      match get_var Fpath.list_of_search_path "OCAMLPATH" m with
      | Some ps -> k ps
      | None ->
          match get_var Fpath.of_string "OPAM_SWITCH_PREFIX" m with
          | Some p -> k [Fpath.(p / "lib")]
          | None ->
              Memo.fail m
                "Could not determine an %a in the build environment."
                Fmt.(code string) "OCAMLPATH"
end

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
          | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> loop (i + 1) ~id_start:false
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

  (* This doesn't work well for built libraries and needs fixing.
     Should work but for reasons that are subtle, also doesn't
     maxmimize // (in B0_ocaml sync on archive ready rather more fine
     grained on ifaces). *)

  type t =
    { name : Name.t;
      requires : Name.t list;
      dir : Fpath.t;
      archive : string;
      dir_files_by_ext : B00_fexts.map Lazy.t;
      installed : bool; (* this should not be needed. *) }


  let dir_files_by_ext m ~installed dir =
    Memo.fail_if_error m @@
    let add _ _ p acc =
      if installed then Memo.file_ready m p;
      String.Map.add_to_list (Fpath.get_ext p) p acc
    in
    Os.Dir.fold_files ~recurse:false add dir String.Map.empty

  let v ?(installed = true) ?(archive = "lib") m ~name ~requires ~dir =
    let dir_files_by_ext = lazy (dir_files_by_ext m ~installed dir) in
    { name; requires; dir; archive; dir_files_by_ext; installed }

  let name l = l.name
  let requires l = l.requires
  let dir l = l.dir
  let archive ~code l =
    let a = l.archive ^ Cobj.archive_ext_of_code code in
    Fpath.(l.dir / a)

  let installed l = l.installed
  let cmis l = match String.Map.find ".cmi" (Lazy.force l.dir_files_by_ext) with
  | exception Not_found -> [] | cmis -> cmis
end

(* Resolver *)

module Lib_convention = struct

  let find m ~ocamlpath n = failwith "TODO"
    (*
    let rec loop n = function
    | d :: ds ->
        let libdir = Fpath.(d // Lib_name.to_path n) in
        begin match Log.if_error ~use:false (Os.Dir.exists libdir) with
        | false -> loop n ds
        | true ->
            let requires = Libname.Set.empty in
            let lib = Lib.v ~name ~requies ~dir:d in
            r.libs <- Lib_name.Map.add n lib r.libs;
            k lib
        end
    | [] ->
        Memo.fail r.memo
          "@[<v>@[OCaml library %a not found in OCAMLPATH@]:@, \
           @[<v>%a@]@]"
          Lib_name.pp n (Fmt.list Fpath.pp_unquoted) r.ocamlpath
    in
    loop n r.ocamlpath
*)
end


module Ocamlfind = struct
  let tool = B00.Tool.by_name "ocamlfind"
  let parse_info m ?(file = Fpath.dash) n s =
    let parse_requires requires =
      let to_libname s =
        Result.to_failure @@
        Result.map_error (Fmt.str "required library: %s") @@
        Lib.Name.of_string s
      in
      if requires = "" then [] else
      List.map to_libname (String.split_on_char ' ' requires)
    in
    let parse_archive a = match String.cut_right "." a with
    | None -> a | Some (a, _ext) -> a
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
          Ok (meta, Lib.v m ~name:n ~requires ~dir ~archive)
      | _ -> Fmt.failwith "could not parse %S" s
    with
    | Failure e -> Fmt.error "@[<v>%a: %s@]" Fpath.pp_unquoted file e


  (* FIXME need to solve the META file read.
     FIXME post exec is still messy, check if we can make it to use Memo.t *)

  let write_info m n ~o =
    (* FIXME better [n] not found error *)
    let ocamlfind = Memo.tool m tool in
    let lib = Lib.Name.to_string n in
    let post_exec op = match B000.Op.status op with
    | B000.Op.Done ->
        begin match Option.get (B000.Op.Spawn.exit (B000.Op.Spawn.get op)) with
        | `Exited 2 ->
            (* FIXME checktypo *)
            let err = Fmt.str "OCaml library %a not found" Lib.Name.pp n in
            B000.Op.set_status op (B000.Op.(Failed (Exec (Some err))))
        | _ -> ()
        end
    | _ -> ()
    in
    let success_exits = [0; 2 (* not found *) ] in
    Memo.spawn m ~success_exits ~reads:[] ~writes:[o] ~stdout:(`File o)
      ~post_exec @@
    ocamlfind Cmd.(arg "query" % lib % "-predicates" % "byte,native" %
                   "-format" % "%m:%d:%a:%(requires)")

  let read_info m n f k =
    Memo.read m f @@ fun s ->
    let _meta_file, lib = parse_info ~file:f m n s |> Memo.fail_if_error m in
    k lib
end

module Lib_resolver = struct

  type t =
    { memo : B00.Memo.t;
      memo_dir : Fpath.t;
      ocamlpath : Fpath.t list;
      mutable libs : Lib.t B00.Memo.Fut.t Lib.Name.Map.t; }

  let create memo ~memo_dir ~ocamlpath =
    let memo = B00.Memo.with_mark memo "b00.ocaml.lib.resolver" in
    { memo; memo_dir; ocamlpath; libs = Lib.Name.Map.empty }

  let find r n = match Lib.Name.Map.find n r.libs with
  | lib -> Memo.Fut.await lib
  | exception Not_found ->
      let fname = Fmt.str "ocamlib-%s.ocamlfind" (Lib.Name.to_string n) in
      let o = Fpath.(r.memo_dir / fname) in
      Ocamlfind.write_info r.memo n o;
      let fib = Ocamlfind.read_info r.memo n o in
      let fut = Memo.Fut.of_fiber r.memo fib in
      r.libs <- Lib.Name.Map.add n fut r.libs;
      fib

(*
  let find_in_ocamlpath r n =

*)
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
