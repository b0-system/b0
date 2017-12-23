(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let loc = Def.Loc.lib "ocamlc"
let group = Conf.Group.v ~loc "ocaml" ~doc:"OCaml language support"

(* OCaml unit metadata keys *)

let tag =
  let doc = "A unit with OCaml related outcomes" in
  Unit.Meta.Key.v ~loc "ocaml" Conv.bool () ~doc

let lib_deps =
  let doc = "OCaml libraries needed by the unit" in
  Unit.Meta.Key.v ~loc "ocaml.lib_deps" Conv.(list string) () ~doc

let lib_name =
  let doc = "OCaml library name produced by the unit" in
  Unit.Meta.Key.v ~loc "ocaml.lib_name" Conv.string () ~doc

(* Configuration keys *)

module Tool = struct
  let env_vars =
    [ "CAMLLIB"; "CAMLSIGPIPE"; "CAML_DEBUG_FILE"; "CAML_DEBUG_SOCKET";
      "CAML_LD_LIBRARY_PATH"; "OCAMLDEBUG"; "OCAMLLIB";
      "OCAMLPROF_DUMP"; "OCAMLRUNPARAM"; "OCAML_COLOR"; "OCAML_FLEXLINK";
      "OCAML_INSTR_FILE"; "OCAML_INSTR_START"; "OCAML_INSTR_STOP";
      "OCAML_SPACETIME_INTERVAL"; "OCAML_SPACETIME_SNAPSHOT_DIR"; "PATH";
      "TERM"; "__AFL_SHM_ID" ]

  let tool tool ~doc =
    let tools = [Fpath.v (tool ^ ".opt"); Fpath.v tool ] in
    Tool.key ~loc ~doc ~group ~env_vars ~tools tool

  let ocamldep = tool "ocamldep" ~doc:"The OCaml dependency analyzer"
  let ocamlc = tool "ocamlc" ~doc:"The OCaml byte-code compiler"
  let ocamlopt = tool "ocamlopt" ~doc:"The OCaml native-code compiler"
  let ocamlmktop = tool "ocamlmktop" ~doc:"The OCaml custom toplevel builder"
  let ocamlmklib = tool "ocamlmklib" ~doc:"The mixed C/OCaml library builder"
end

module Key = struct
  let build_byte =
    let default = Conf.const true (* FIXME *) in
    let doc = "Build bytecode artefacts" in
    Conf.key "ocaml.build_byte" Conv.bool ~loc ~doc ~default ~group

  let build_native =
    let default = Conf.const true (* FIXME *) in
    let doc = "Build native code artefacts" in
    Conf.key "ocaml.build_native" Conv.bool ~loc ~doc ~default ~group

  let build_native_dynlink =
    let default = Conf.const true (* FIXME *) in
    let doc = "Build native code dynamically linkable artefacts" in
    Conf.key "ocaml.build_native_dynlink" Conv.bool ~loc ~doc ~default ~group

  let build_cmtis =
    let default = Conf.const true (* FIXME *) in
    let doc = "Build cmti file artefacts (for documentation)" in
    Conf.key "ocaml.build_cmtis" Conv.bool ~loc ~doc ~default ~group

  let stacktraces =
    let default = Conf.const true in
    let doc = "Build with stacktrace support" in
    Conf.key "ocaml.stacktraces" Conv.bool ~loc ~doc ~default ~group
end

(* OCaml configuration *)

type conf =
  { build_byte : bool;
    build_native : bool;
    build_native_dynlink : bool;
    build_cmtis : bool;
    byte_compiler : Cmd.t -> Build.run;
    native_compiler : Cmd.t -> Build.run;
    cmi_compiler : Cmd.t -> Build.run;
    mklib : Cmd.t -> Build.run;
    stacktraces : Cmd.t;
    byte_ext : Fpath.ext;
    native_ext : Fpath.ext;
    exe_ext : Fpath.ext;
    cobj_ext : Fpath.ext;
    clib_ext : Fpath.ext;
    cdll_ext : Fpath.ext; }

let conf b =
  let build_byte = Build.conf b Key.build_byte in
  let build_native = Build.conf b Key.build_native in
  let build_native_dynlink = Build.conf b Key.build_native_dynlink in
  let build_cmtis = Build.conf b Key.build_cmtis in
  let byte_compiler = Build.conf_tool b Tool.ocamlc in
  let native_compiler = Build.conf_tool b Tool.ocamlopt in
  let mklib = Build.conf_tool b Tool.ocamlmklib in
  let cmi_compiler = match build_byte with
  | true -> byte_compiler
  | false -> native_compiler
  in
  let stacktraces = match Build.conf b Key.stacktraces with
  | true -> Cmd.v "-g"
  | false -> Cmd.empty
  in
  let exe_ext = Build.conf b B0_care.OS.exe_ext in
  let byte_ext = ".byte" ^ exe_ext in
  let native_ext = exe_ext in
  let cobj_ext = Build.conf b B0_c.obj_ext in
  let clib_ext = Build.conf b B0_c.lib_ext in
  let cdll_ext = Build.conf b B0_c.dll_ext in
  { build_byte; build_native; build_native_dynlink;
    build_cmtis; byte_compiler; native_compiler;
    cmi_compiler; mklib; stacktraces; exe_ext; byte_ext; native_ext;
    cobj_ext; clib_ext; cdll_ext; }

module Lib = struct

  type name = string

  let archive_of_name name = match String.cut ~sep:"." name with
  | None -> name
  | Some (pkg, name) ->
      (* FIXME try to further decompose name, treat @variant *)
      strf "%s_%s" pkg name

  (* Library sets *)

  type t =
    { name : name;
      loc : [ `Env | `Build of Unit.t ];
      inc : Fpath.t option;
      base : Fpath.t option;
      deps : name list; }

  let lib_of_unit b u =
    let bdir = Build.unit_build_dir b u in
    let name = Unit.meta_get lib_name u in
    let loc = `Build u in
    let inc = Some bdir in
    let base = Some Fpath.(bdir / archive_of_name name) in
    let deps = match Unit.meta_find lib_deps u with
    | None -> [] | Some deps -> deps
    in
    { name; loc; inc; base; deps }

  let get_lib libs n = match String.Map.find n libs with
  | exception Not_found -> assert false
  | lib -> lib

  let sort_libs ?(rev = false) libs names =
    (* [names] stable topo sort by depth first exploration of the DAG. Assumes
       names are in lib_map and lib_map is closed under dependencies. *)
    let rec loop seen acc = function
    | (n :: ns as names) :: todo ->
        begin match String.Set.mem n seen with
        | true -> loop seen acc (ns :: todo)
        | false ->
            let seen = String.Set.add n seen in
            let lib = get_lib libs n in
            match lib.deps with
            | [] -> loop seen (lib :: acc) (ns :: todo)
            | deps -> loop seen acc (deps :: names :: todo)
        end
    | [] :: (n :: ns) :: todo -> loop seen (get_lib libs n :: acc) (ns :: todo)
    | [] :: ([] :: todo) -> loop seen acc todo
    | [] :: [] -> if rev then acc else List.rev acc
    | [] -> assert false
    in
    loop String.Set.empty [] [names]

  type set =
    { root_libs : t list;
      libs : t list;
      incs_comp : Fpath.t list;
      incs_link : Fpath.t list;
      cmas : Fpath.t list;
      cmxas : Fpath.t list;
      cmxss : Fpath.t list; }

  let empty =
    { root_libs = []; libs = []; incs_comp = []; incs_link = [];
      cmas = []; cmxas = []; cmxss = [] }

  let is_empty s = s.root_libs = []
  let root_libs s = s.root_libs
  let libs s = s.libs
  let incs_comp s = s.incs_comp
  let incs_link s = s.incs_link
  let cmas s = s.cmas
  let cmxas s = s.cmxas
  let cmxss s = s.cmxss

  let ars_of_rev_libs doit b fext rev_libs =
    let add_env_file f fs = match R.failwith_error_msg @@ OS.File.exists f with
    | true -> Build.ready b f; f :: fs | false -> fs
    in
    let add_lib acc lib = match lib.base with
    | None -> acc
    | Some base ->
        let ar = Fpath.(base + fext) in
        match lib.loc with
        | `Env -> add_env_file ar acc
        | `Build _ ->
            (* FIXME we should check if the unit builds the file *)
            ar :: acc
    in
    if doit then List.fold_left add_lib [] rev_libs else []

  let pp_name ppf l = Fmt.string ppf l.name

  let set_of_libs b conf libs names =
    let add_inc acc l = match l.inc with None -> acc | Some i -> i :: acc in
    let rev_root_libs = List.rev_map (get_lib libs) names in
    let rev_libs = sort_libs ~rev:true libs names in
    let incs_comp = List.fold_left add_inc [] rev_root_libs in
    let incs_link = List.fold_left add_inc [] rev_libs in
    let cmas = ars_of_rev_libs conf.build_byte b ".cma" rev_libs in
    let cmxas = ars_of_rev_libs conf.build_native b ".cmxa" rev_libs in
    let cmxss =
      (* FIXME This is not needed for building
      ars_of_rev_libs conf.build_native_dynlink b ".cmxs" rev_libs *) []
    in
    let root_libs = List.rev rev_root_libs in
    let libs = List.rev rev_libs in
    { root_libs; libs; incs_comp; incs_link; cmas; cmxas; cmxss }

  (* Resolvers *)

  type resolver =
    { resolve : Build.t -> conf -> name list -> Fpath.t -> set Build.fiber }

  let resolve b r conf names file f = r.resolve b conf names file f

  (* Default build resolver, resolve libraries in the build or
     via ocamlfind *)

  let buildable_libs b =
    let add_lib acc u = match Unit.meta_find lib_name u with
    | None -> acc | Some n -> String.Map.add n u acc
    in
    (* FIXME for better error reports it might be better to
       have all declared units from Unit.list here and then
       report if one is not in the build/env. *)
    List.fold_left add_lib String.Map.empty (Build.units b)

  let find_lib_locs ~buildable b names =
    let rec loop seen env built = function
    | [] -> env, built
    | n :: ns ->
        match String.Set.mem n seen with
        | true -> loop seen env built ns
        | false ->
            let seen = String.Set.add n seen in
            match String.Map.find n buildable with
            | exception Not_found -> loop seen (String.Set.add n env) built ns
            | u ->
                let built = String.Map.add n (lib_of_unit b u) built in
                match Unit.meta_find lib_deps u with
                | None | Some [] -> loop seen env built ns
                | Some deps -> loop seen env built (List.rev_append deps ns)
    in
    loop String.Set.empty String.Set.empty String.Map.empty names

  (* ocamlfind resolver *)

  let ocamlfind =
    let env_vars = (* As found in findlib.conf(5) *)
      [ "OCAMLFIND_CONF"; "OCAMLFIND_TOOLCHAIN"; "OCAMLPATH";
        "OCAMLFIND_DESTDIR"; "OCAMLFIND_METADIR"; "OCAMLFIND_COMMANDS";
        "CAMLLIB"; "OCAMLLIB"; "OCAMLFIND_LDCONF"; "OCAMLFIND_IGNORE_DUPS_IN" ]
    in
    B0.Tool.key ~loc ~group ~env_vars "ocamlfind" ~doc:"The ocamlfind binary"

  let parse_ocamlfind_libs ~known data =
    let parse_ocamlfind_lib known l = match String.cuts ~sep:"|" l with
    | [name; inc; ar; deps] ->
        begin match String.Map.mem name known with
        | true -> known
        | false ->
            let inc = match inc with
            | "" -> None
            | s -> Some (R.failwith_error_msg @@ Fpath.of_string s)
            in
            let base = match ar with
            | "" -> None
            | s -> Some Fpath.(rem_ext @@ R.failwith_error_msg @@ of_string s)
            in
            let deps = String.cuts ~empty:false ~sep:" " deps in
            let lib = { name; loc = `Env; inc; base; deps } in
            String.Map.add name lib known
        end
    | _ ->
        failwith @@ strf
          "%S: could not parse ocamlfind output into name, include, lib, deps" l
    in
    let lines = String.cuts ~empty:false ~sep:"\n" data in
    try Ok (List.fold_left parse_ocamlfind_lib known lines)
    with Failure f -> R.error_msg f

  let ocamlfind_libs b ~built names file f =
    (* ocamlfind lookup uses ocamlfind query and makes the following
       assumptions.

     1. The byte and native includes of a library are in the %d directory.
     2. Its archives are all in the dirname of %+A with the same base name.
        We query both byte and native predicates. This gives the cma
        if both are present and cmxa if only the cmxa is present. We redo
        the suffixing ourselves. There's only one archive per package.

       FIXME it's a bit unclear what we want to do here. Either
       we prevent memo altogether. Or we query and post-read the META files
       for the spawn (need support for this in Build). *)
    let ocamlfind = Build.conf_tool b ocamlfind in
    let preds =
      let t n = n = "threads" || n = "threads.posix" || n = "threads.vm" in
      match List.find t names with
      | "threads" -> "byte native mt"
      | "threads.posix" -> "byte native mt mt_posix"
      | "threads.vm" -> "byte native mt mt_vm"
      | exception Not_found -> "byte native"
      | _ -> assert false
    in
    let query =
      Cmd.(v "query" % "-predicates" % preds % "-r" % "-format" %
           "%p|%d|%+A|%(requires)" %% of_list names)
    in
    Build.spawn b ~writes:[file] ~stdout:(`File file) @@
    ocamlfind query;
    Build.read b file begin fun data ->
      f (Build.fail_on_error_msg @@ parse_ocamlfind_libs ~known:built data)
    end

  let default_resolve b conf names file f =
    let buildable = buildable_libs b in
    let env, built = find_lib_locs b ~buildable names in
    let env_names = String.Set.elements env in
    ocamlfind_libs b ~built:built env_names file begin fun libs ->
      f (set_of_libs b conf libs names)
    end

  let default_resolver = { resolve = default_resolve }

  let get_resolver b = function
  | Some r -> r
  | None -> (* FIXME we could lookup in conf of [b] *) default_resolver
end

module Conf = struct
  let build_byte c = c.build_byte
  let build_native c = c.build_native
  let build_native_dynlink c = c.build_native_dynlink
  let build_cmtis c = c.build_cmtis
  let byte_compiler c = c.byte_compiler
  let native_compiler c = c.native_compiler
  let cmi_compiler c = c.cmi_compiler
  let mklib c = c.mklib
  let stacktraces c = c.stacktraces
  let exe_ext c = c.exe_ext
  let byte_ext c = c.byte_ext
  let native_ext c = c.native_ext
  let cobj_ext c = c.cobj_ext
  let clib_ext c = c.clib_ext
  let cdll_ext c = c.cdll_ext
end

type mod_spec =
  { name : string;
    (* Only mutated during construction. *)
    mutable mli : (Fpath.t * string list) option;
    mutable ml : (Fpath.t * string list) option; }

let spec name = { name; mli = None; ml = None; }

let set_mli spec mli =
  if spec.mli <> None then Log.warn (fun m -> m "TODO multi");
  spec.mli <- Some mli

let set_ml spec ml =
  if spec.ml <> None then Log.warn (fun m -> m "TODO multi");
  spec.ml <- Some ml

let add_mod dst f deps acc =
  let warn () =
    Log.warn (fun m -> m "Not an OCaml source: %a" Fpath.pp f); acc
  in
  match String.cut ~rev:true ~sep:"." (Fpath.filename f) with
  | None -> warn ()
  | Some (m, ext) ->
      let acc, spec = match String.Map.find m acc with
      | spec -> acc, spec
      | exception Not_found ->
          let spec = spec m in
          String.Map.add m spec acc, spec
      in
      match ext with
      | "ml" -> set_ml spec (f, deps); acc
      | "mli" -> set_mli spec (f, deps); acc
      | _ -> warn ()

module Deps = struct
  type t = mod_spec String.map

  let parse_file f = (* ocamldep escapes spaces as "\ " *)
    let rec unescape_len max i len = match i > max with
    | true -> len
    | false -> unescape_len max (i + 1) (if f.[i] = ' ' then len else len + 1)
    in
    let len = String.length f in
    let max = len - 1 in
    let ulen = unescape_len max 0 0 in
    match ulen = len with
    | true -> f
    | false ->
        let b = Bytes.create ulen in
        let rec loop i k = match i > max with
        | true -> Bytes.unsafe_to_string b
        | false ->
            match f.[i] with
            | ' ' -> Bytes.set b (k - 1) ' '; loop (i + 1) k
            | c -> Bytes.set b k c; loop (i + 1) (k + 1)
      in
      loop 0 0

  let parse_dep_line n l f acc =
    let rev = true (* because windows drives... *) in
    let l = String.trim l (* because ocamldep may output \r\n *) in
    match String.cut ~rev ~sep:":" l with
    | None -> failwith (strf ":%d: Could not parse line %S" n l)
    | Some (file, mods) ->
        let file = match Fpath.of_string @@ parse_file file with
        | Error (`Msg m) -> failwith (strf ":%d: %s" n m)
        | Ok p -> p
        in
        let mods = String.cuts ~empty:false ~sep:" " mods in
        let mods = List.map String.lowercase_ascii mods in
        f acc file mods

  let of_ocamldep_output ~src data f acc =
    try
      let rec loop acc n data = match String.cut ~sep:"\n" data with
      | Some (line, rest) -> loop (parse_dep_line n line f acc) (n + 1) rest
      | None when data = "" -> acc
      | None -> parse_dep_line n data f acc
      in
      Ok (loop acc 1 data)
    with Failure msg -> R.error_msgf "%a%s" Fpath.pp src msg

  let write ?(opts = Cmd.empty) b ~srcs file =
    (* -slash is used because file paths with spaces are escaped
       with backslashes and is ambiguous on windows. *)
    let ocamldep = Build.conf_tool b Tool.ocamldep in
    Build.spawn b ~reads:srcs ~writes:[file] ~stdout:(`File file) @@
    ocamldep Cmd.(v "-slash" % "-modules" %% opts %% of_values p srcs)

  let read b file ~dst k =
    let f acc fn deps = add_mod dst fn deps acc in
    let parse data = of_ocamldep_output ~src:file data f String.Map.empty in
    Build.read b file (fun data -> k (Build.fail_on_error_msg @@ parse data))
end

let dst_file obj dst m = Fpath.(dst / (m ^ obj))
let cmti_file dst m = dst_file ".cmti" dst m
let cmt_file dst m = dst_file ".cmti" dst m
let cmi_file dst m = dst_file ".cmi" dst m
let cmo_file dst m = dst_file ".cmo" dst m
let cmx_file dst m = dst_file ".cmx" dst m
let cma_file dst m = dst_file ".cma" dst m
let cmxa_file dst m = dst_file ".cmxa" dst m
let cmxs_file dst m = dst_file ".cmxs" dst m
let exe_file c dst m = dst_file (Conf.exe_ext c) dst m
let byte_file c dst m = dst_file (Conf.byte_ext c) dst m
let native_file c dst m = dst_file (Conf.native_ext c) dst m
let cobj_file c dst m = dst_file (Conf.cobj_ext c) dst m
let clib_file c dst m = dst_file (Conf.clib_ext c) dst m
let cdll_file c dst m = dst_file (Conf.cdll_ext c) dst m

let add_dst_deps obj dst idx deps acc =
  let add_dep acc d = match String.Map.mem d idx with
  | false -> acc
  | true -> dst_file obj dst d :: acc
  in
  List.fold_left add_dep acc deps

let add_cmi_deps dst mods deps acc = add_dst_deps ".cmi" dst mods deps acc
let add_cmo_deps dst mods deps acc = add_dst_deps ".cmo" dst mods deps acc
let add_cmx_deps dst mods deps acc = add_dst_deps ".cmx" dst mods deps acc

let add_if b v acc = if b then v :: acc else acc

(* FIXME assert that.

   For compilation the painful -I system implies monitoring
   directories for changes. Since we don't do this we assume changes
   in these directories entail changes in the corresponding
   archives. For cmis we avoid hashing cmas if we are only building
   native. N.B. we should maybe determine them at the lib level to
   support the case of byte or native code only libs. Subtle errors
   could occur.  The notion of bc or nat only might be needed at the
   level of lib. *)

let cmo_libs_inc_stamps libs = Lib.cmas libs
let cmx_libs_inc_stamps libs = Lib.cmxas libs
let cmi_libs_inc_stamps conf libs = match Conf.build_native conf with
| true -> cmx_libs_inc_stamps libs
| false -> cmo_libs_inc_stamps libs

let cmi ?(lib_deps = Lib.empty) b c mods dst m (mli, mod_deps) =
  let compiler = Conf.cmi_compiler c in
  let build_cmtis = Conf.build_cmtis c in
  let cmi = cmi_file dst m in
  let incs = Cmd.(of_values ~slip:"-I" p (dst :: Lib.incs_comp lib_deps)) in
  let incs_stamps = cmi_libs_inc_stamps c lib_deps in
  let bin_annot = Cmd.on build_cmtis (Cmd.v "-bin-annot") in
  let stacktraces = Conf.stacktraces c in
  let reads = mli :: (add_cmi_deps dst mods mod_deps @@ incs_stamps) in
  let writes = add_if build_cmtis (cmti_file dst m) [cmi] in
  Build.spawn b ~reads ~writes @@
  compiler Cmd.(incs %% bin_annot %% stacktraces % "-c" % "-o" % p cmi % p mli)

let cmo ?(lib_deps = Lib.empty) b c ~has_cmi mods dst m (ml, mod_deps) =
  if not (Conf.build_byte c) then () else
  let compiler = Conf.byte_compiler c in
  let cmi = cmi_file dst m in
  let cmo = cmo_file dst m in
  let incs = Cmd.(of_values ~slip:"-I" p (dst :: Lib.incs_comp lib_deps)) in
  let incs_stamps = cmo_libs_inc_stamps lib_deps in
  let stacktraces = Conf.stacktraces c in
  let reads = add_cmi_deps dst mods mod_deps @@ incs_stamps in
  let reads = ml :: add_if has_cmi cmi reads in
  let writes = cmo :: add_if (not has_cmi) cmi [] in
  Build.spawn b ~reads ~writes @@
  compiler Cmd.(incs %% stacktraces % "-c" % "-o" % p cmo % p ml)

let cmx ?(lib_deps = Lib.empty) b c ~has_cmi mods dst m (ml, mod_deps) =
  if not (Conf.build_native c) then () else
  let compiler = Conf.native_compiler c in
  let has_cmi = has_cmi || Conf.build_byte c (* avoid races cf. MPR7472 *) in
  let cmi = cmi_file dst m in
  let cmx = cmx_file dst m in
  let o = cobj_file c dst m in
  let incs = Cmd.(of_values ~slip:"-I" p (dst :: Lib.incs_comp lib_deps)) in
  let incs_stamps = cmx_libs_inc_stamps lib_deps in
  let stacktraces = Conf.stacktraces c in
  let reads = add_cmx_deps dst mods mod_deps @@ incs_stamps in
  let reads = ml :: add_if has_cmi cmi reads in
  let writes = cmx :: o :: add_if (not has_cmi) cmi [] in
  Build.spawn b ~reads ~writes @@
  compiler Cmd.(incs %% stacktraces % "-c" % "-o" % p cmx % p ml)

let cobj b c dst ~headers c_src =
  (* N.B. headers seem to work because ocaml{c,opt} seems to include
     the dir of c_srcs *)
  let compiler =
    (* The logic here would be nice to avoid hashing the byte code compiler
       if we only compile native code but it fails due to MPR7678
    match Conf.build_native c with
    | true -> Conf.native_compiler c
    | false -> Conf.byte_compiler c *)
    Conf.byte_compiler c
  in
  let base = Fpath.(filename @@ rem_ext c_src) in
  let obj = cobj_file c dst base in
  let incs = Cmd.empty (* Cmd.(of_values ~slip:"-I" p [dst]) *) in
  let stacktraces = Conf.stacktraces c in
  (* FIXME .h reads *)
  let reads = c_src :: headers in
  let writes = [obj] in
  let cwd = dst (* Before 4.04.0 the .o is generated in the cwd. Hope
                   was that -o could be used at some point but it doesn't
                   seem so recent versions of the compiler disallow -c and -o
                   on C files, see MPR7677. Since we are using absolute path
                   for the sources we just run the command in dst. *)
  in
  Build.spawn b ~reads ~writes ~cwd @@
  compiler Cmd.(incs %% stacktraces (* % "-o" % p obj *) % "-c" % p c_src);
  obj

let compile ?(lib_deps = Lib.empty) b c mods ~dst =
  let compile m spec = match spec.mli, spec.ml with
  | Some mli, Some ml ->
      let has_cmi = true in
      cmi b c mods ~lib_deps dst m mli;
      cmo b c ~has_cmi mods ~lib_deps dst m ml;
      cmx b c ~has_cmi mods ~lib_deps dst m ml
  | Some mli, None (* mli only *) ->
      cmi b c mods ~lib_deps dst m mli
  | None, Some ml (* ml only *) ->
      let has_cmi = false in
      cmo b c ~has_cmi mods ~lib_deps dst m ml;
      cmx b c ~has_cmi mods ~lib_deps dst m ml
  | None, None -> assert false
  in
  String.Map.iter compile mods

let compile_csrcs b c csrcs ~dst =
  (* C files always depend on all headers which is a bit rough.
     Fundamentally we should maybe use B0_c's support to compile stubs
     rather than go through ocamlc. TODO *)
  let c_files, headers = List.partition (Fpath.has_ext ".c") csrcs in
  let compile objs src = cobj b c dst ~headers src :: objs in
  List.rev (List.fold_left compile [] c_files)

let link_byte ?(lib_deps = Lib.empty) b c rev_mods ~cobjs ~dst name =
  if not (Conf.build_byte c) then () else
  let compiler = Conf.byte_compiler c in
  let cmos = List.rev_map (fun m -> cmo_file dst m.name) rev_mods in
  let cmas = Lib.cmas lib_deps in
  let objs = List.concat [cmas; cobjs; cmos] in
  let custom = match cobjs with
  | [] -> Cmd.v "-custom" (* Cmd.empty FIXME depends if lib_deps has C stubs *)
  | _ -> Cmd.v "-custom"
  in
  let incs = Cmd.(of_values ~slip:"-I" p (dst :: Lib.incs_link lib_deps)) in
  let exe = byte_file c dst name in
  let straces = Conf.stacktraces c in
  Build.spawn b ~reads:objs ~writes:[exe] @@
  compiler Cmd.(incs %% straces %% custom %  "-o" % p exe %% of_values p objs);
  Build.add_path_meta b exe B0_care.exe true;
  ()

(* For linking -I is again needed too lookup the OCaml companion C
   archives we assume that we don't need to hash the C library
   archives. *)

let link_native ?(lib_deps = Lib.empty) b c rev_mods ~cobjs ~dst name =
  if not (Conf.build_native c) then () else
  let compiler = Conf.native_compiler c in
  let cmxs = List.rev_map (fun m -> cmx_file dst m.name) rev_mods in
  let cmxas = Lib.cmxas lib_deps in
  let objs = List.concat [cmxas; cobjs; cmxs] in
  let incs = Cmd.(of_values ~slip:"-I" p (dst :: Lib.incs_link lib_deps)) in
  let exe = native_file c dst name in
  let straces = Conf.stacktraces c in
  Build.spawn b ~reads:objs ~writes:[exe] @@
  compiler Cmd.(incs %% straces % "-o" % p exe %% of_values p objs);
  Build.add_path_meta b exe B0_care.exe true;
  ()

let sort_mods ?(rev = false) mods =
  (* [mods] stable topo sort by depth first exploration of the DAG. *)
  let rec loop seen acc = function
  | ((m, spec) :: rs as l) :: todo ->
      begin match String.Set.mem m seen with
      | true -> loop seen acc (rs :: todo)
      | false ->
          let seen = String.Set.add m seen in
          match spec.ml with
          | None -> loop seen acc (rs :: todo)
          | Some (f, deps) ->
              let add_dep acc d = match String.Set.mem d seen with
              | true -> acc (* early filter *)
              | false ->
                  match String.Map.find d mods with
                  | exception Not_found -> acc
                  | spec -> (d, spec) :: acc
              in
              match List.fold_left add_dep [] deps with
              | [] (* early filter *) -> loop seen (spec :: acc) (rs :: todo)
              | deps -> loop seen acc (deps :: l :: todo)
      end
  | [] :: ((_, spec) :: rs) :: todo -> loop seen (spec :: acc) (rs :: todo)
  | [] :: ([] :: todo) -> loop seen acc todo
  | [] :: [] -> if rev then acc else List.rev acc
  | [] -> assert false
  in
  loop String.Set.empty [] ((String.Map.bindings mods) :: [])

type exe = { byte : Fpath.t option; native : Fpath.t option }

let link ?(lib_deps = Lib.empty) b c mods ~cobjs ~dst name =
  let rev_mods = sort_mods ~rev:true mods in
  link_byte b c rev_mods ~cobjs ~dst ~lib_deps name;
  link_native b c rev_mods ~cobjs ~dst ~lib_deps name;
  ()

let archive_cobjs_base name = strf "%s_stubs" name
let archive_cobjs_clib dst c name = clib_file c dst (strf "lib%s_stubs" name)
let archive_cobjs_dll dst c name = cdll_file c dst (strf "dll%s_stubs" name)
let archive_cobjs b c cobjs ~dst name =
  if cobjs = [] then () else
  let mklib = Conf.mklib c in
  let dll = archive_cobjs_dll dst c name in
  let ar = archive_cobjs_clib dst c name in
  let base = Fpath.(dst / archive_cobjs_base name) in
  Build.spawn b ~reads:cobjs ~writes:[dll;ar] @@
  mklib Cmd.(v "-o" % p base %% of_values p cobjs)

let archive_byte b c rev_mods ~cstubs ~dst name =
  if not (Conf.build_byte c) then () else
  let compiler = Conf.byte_compiler c in
  let cmos = List.rev_map (fun m -> cmo_file dst m.name) rev_mods in
  let cma = cma_file dst name in
  let cstubs = match cstubs with
  | false -> Cmd.empty
  | true ->
      let lib = strf "-l%s" @@ archive_cobjs_base name in
      let cclib = Cmd.(v "-cclib" % lib) in
      match Conf.build_native_dynlink c with
      | true -> Cmd.(cclib % "-dllib" % lib)
      | false -> cclib
  in
  let stacktraces = Conf.stacktraces c in
  Build.spawn b ~reads:cmos ~writes:[cma] @@
  compiler Cmd.(stacktraces % "-a" % "-o" % p cma %% of_values p cmos %% cstubs)

let archive_native b c rev_mods ~cstubs ~dst name =
  if not (Conf.build_native c) then () else
  let compiler = Conf.native_compiler c in
  let cmxs = List.rev_map (fun m -> cmx_file dst m.name) rev_mods in
  let cmxa = cmxa_file dst name in
  let clib = clib_file c dst name in
  let cstubs = match cstubs with
  | false -> Cmd.empty
  | true -> Cmd.(v "-cclib" % (strf "-l%s" @@ archive_cobjs_base name))
  in
  let stacktraces = Conf.stacktraces c in
  Build.spawn b ~reads:cmxs ~writes:[cmxa; clib] @@
  compiler Cmd.(stacktraces % "-a" % "-o" % p cmxa %% of_values p cmxs %%
                cstubs)

let archive_native_dynlink b c rev_mods ~cstubs ~dst name =
  if not (Conf.build_native_dynlink c) then () else
  begin
    if not (Conf.build_native c) then
      archive_native b c rev_mods ~cstubs ~dst name;
    let compiler = Conf.native_compiler c in
    let cmxa = cmxa_file dst name in
    let cmxs = cmxs_file dst name in
    let incs = Cmd.(of_values ~slip:"-I" p [dst]) in
    let c_ar, reads = match cstubs with
    | false -> Cmd.empty, [cmxa]
    | true ->
        let c_ar = archive_cobjs_clib dst c name in
        Cmd.(v @@ p c_ar), [c_ar; cmxa]
    in
    let stacktraces = Conf.stacktraces c in
    Build.spawn b ~reads ~writes:[cmxs] @@
    compiler Cmd.(stacktraces % "-shared" % "-linkall" %% c_ar %% incs %
                  "-o" % p cmxs % p cmxa)
  end

let archive b c mods ~cstubs ~dst name =
  let rev_mods = sort_mods ~rev:true mods in
  archive_byte b c rev_mods ~cstubs ~dst name;
  archive_native b c rev_mods ~cstubs ~dst name;
  archive_native_dynlink b c rev_mods ~cstubs ~dst name;
  ()

let b0_descr = Fpath.v "B0.ml"
let is_b0_descr f = Fpath.filename_equal f b0_descr
let is_ml_src f = Fpath.mem_ext [".mli"; ".ml"] f
let is_c_src f = Fpath.mem_ext [".h"; ".c"] f
let srcs b ~dir =
  begin
    OS.Dir.contents dir >>| fun files ->
    let rec loop ml_srcs c_srcs = function
    | [] -> ml_srcs, c_srcs
    | f :: fs ->
        match is_ml_src f && not (is_b0_descr f) with
        | true -> Build.ready b f; loop (f :: ml_srcs) c_srcs fs
        | false ->
            match is_c_src f with
            | true -> Build.ready b f; loop ml_srcs (f :: c_srcs) fs
            | false -> loop ml_srcs c_srcs fs
    in
    loop [] [] files
  end
  |> Build.fail_on_error_msg

module Build = struct
  let srcs = srcs
  let compile = compile
  let link = link
  let archive = archive
  let exe ?(csrcs = []) ?(lib_deps = Lib.empty) b c ~srcs name =
    let odep = Build.build_file b (strf "%s-bin.odep" name) in
    let dst = Build.build_dir b in
    Deps.write b ~srcs odep;
    Deps.read b odep ~dst begin fun mods ->
      let cobjs = compile_csrcs b c csrcs ~dst in
      compile b c mods ~lib_deps ~dst;
      link b c mods ~cobjs ~lib_deps ~dst name;
    end

  let lib ?(csrcs = []) ?(lib_deps = Lib.empty) b c ~srcs name =
    let odep = Build.build_file b (strf "%s-lib.odep" name) in
    let dst = Build.build_dir b in
    Deps.write b ~srcs odep;
    Deps.read b odep ~dst begin fun mods ->
      let cobjs = compile_csrcs b c csrcs ~dst in
      compile b c mods ~lib_deps ~dst;
      archive_cobjs b c cobjs ~dst name;
      archive b c mods ~cstubs:(cobjs <> []) ~dst name;
    end

  let exe_with_srcs ?resolver ?(lib_deps = []) ~srcs ~csrcs name b =
    let c = conf b in
    let resolver = Lib.get_resolver b resolver in
    let lfile = Build.build_file b (strf "%s-bin.mllibs" name) in
    Lib.resolve b resolver c lib_deps lfile begin fun libs ->
      exe b c ~srcs ~csrcs ~lib_deps:libs name
    end

  let lib_with_srcs ?resolver ?(lib_deps = []) ~srcs ~csrcs name b =
    let c = conf b in
    let resolver = Lib.get_resolver b resolver in
    let lfile = Build.build_file b (strf "%s-lib.mllibs" name) in
    Lib.resolve b resolver c lib_deps lfile begin fun libs ->
      lib b c ~srcs ~csrcs ~lib_deps:libs name
    end

  let collect_src_dirs ~src_dirs b =
    let dirs = match src_dirs with
    | None -> [Build.src_dir b]
    | Some dirs -> List.map (fun d -> Fpath.(Build.src_dir b // d)) dirs
    in
    let rec loop ml_srcs c_srcs = function
    | [] -> ml_srcs, c_srcs
    | dir :: ds ->
        let ml_srcs', c_srcs' = srcs b ~dir in
        loop (List.rev_append ml_srcs' ml_srcs) (List.rev_append c_srcs' c_srcs)
          ds
    in
    loop [] [] dirs

  let exe_with_src_dirs ?resolver ?lib_deps ~src_dirs name b =
    let srcs, csrcs = collect_src_dirs ~src_dirs b in
    exe_with_srcs ?resolver ?lib_deps ~srcs ~csrcs name b

  let lib_with_src_dirs ?resolver ?lib_deps ~src_dirs name b =
    let srcs, csrcs = collect_src_dirs ~src_dirs b in
    lib_with_srcs ?resolver ?lib_deps ~srcs ~csrcs name b

  let collect_srcs b ~srcs =
    let make_src src =
      let src = Fpath.(Build.src_dir b // src) in
      Build.ready b src; src
    in
    let csrcs, other = List.partition is_c_src (List.map make_src srcs) in
    other, csrcs

  let exe_with_srcs ?resolver ?lib_deps ~srcs name b =
    let srcs, csrcs = collect_srcs b ~srcs in
    exe_with_srcs ?resolver ?lib_deps ~srcs ~csrcs name b

  let lib_with_srcs ?resolver ?lib_deps ~srcs name b =
    let srcs, csrcs = collect_srcs b ~srcs in
    lib_with_srcs ?resolver ?lib_deps ~srcs ~csrcs name b

  let tool ?internal ?(env_vars = Tool.env_vars) ?name b u c args =
    let name = match name with
    | None -> Fpath.v (Unit.basename u)
    | Some n -> n
    in
    let ext = match Conf.build_native c with
    | true -> Conf.native_ext c
    | false -> Conf.byte_ext c
    in
    let name = Fpath.(name + ext) in
    let tool = B0.Tool.of_unit_file u ?internal ~env_vars name in
    Build.tool b tool args
end

(* Higher-level artefacts *)

module Unit = struct

  (* OCaml unit metadata keys *)

  let tag = tag
  let lib_deps = lib_deps
  let lib_name = lib_name

  let add_lib_deps libs m = Unit.Meta.add lib_deps libs m

  let base_meta =
    Unit.Meta.(add_tag tag empty)

  type build =
    [ `Src_dirs of Fpath.t list
    | `Srcs of Fpath.t list
    | `Func of
        Lib.resolver option -> Lib.name list -> string -> B0.Build.t -> unit ]

  (* Executables *)

  let exe_meta ~lib_deps name =
    Unit.Meta.(base_meta |> add_tag B0_care.Unit.exe |> add_lib_deps lib_deps)

  let exe_func_of_build ?resolver ~lib_deps name = function
  | `Src_dirs [] -> (* FIXME remove that *)
      Build.exe_with_src_dirs ?resolver ~lib_deps ~src_dirs:None name
  | `Src_dirs ds ->
      Build.exe_with_src_dirs ?resolver ~lib_deps ~src_dirs:(Some ds) name
  | `Srcs srcs ->
      Build.exe_with_srcs ?resolver ~lib_deps ~srcs name
  | `Func f ->
      f resolver lib_deps name

  let exe_doc_outcome = "\xF0\x9F\x90\xAB  exe"
  let exe
      ?loc ?src_root ?doc ?only_aim ?pkg ?meta ?resolver ?(lib_deps = []) ?name
      uname build
    =
    let name = match name with None -> uname | Some name -> name in
    let doc = match doc with None -> strf "The %s binary" name | Some d -> d in
    let doc_outcome = exe_doc_outcome in
    let meta = exe_meta ~lib_deps name in
    let func = exe_func_of_build ?resolver ~lib_deps name build in
    Unit.create ?loc ?src_root ~doc_outcome ~doc ?only_aim ?pkg ~meta uname func

  (* Libraries *)

  let lib_meta ~lib_deps name =
    Unit.Meta.(base_meta |> add_tag B0_care.Unit.lib |> add_lib_deps lib_deps |>
               add lib_name name)

  let lib_doc_outcome = "\xF0\x9F\x90\xAB  lib"

  let lib_func_of_build ?resolver ~lib_deps name = function
  | `Src_dirs [] -> (* FIXME remove that *)
      Build.lib_with_src_dirs ?resolver ~lib_deps ~src_dirs:None name
  | `Src_dirs ds ->
      Build.lib_with_src_dirs ?resolver ~lib_deps ~src_dirs:(Some ds) name
  | `Srcs srcs ->
      Build.lib_with_srcs ?resolver ~lib_deps ~srcs name
  | `Func f ->
      f resolver lib_deps name

  let lib
      ?loc ?src_root ?doc ?only_aim ?pkg ?meta ?resolver ?(lib_deps = [])?name
      uname build
    =
    let name = match name with None -> uname | Some name -> name in
    let doc = match doc with
    | None -> strf "The %s library" name | Some d -> d
    in
    let doc_outcome = lib_doc_outcome in
    let meta = lib_meta ~lib_deps name in
    let ar_name = Lib.archive_of_name name in
    let func = lib_func_of_build ?resolver ~lib_deps ar_name build in
    Unit.create ?loc ?src_root ~doc_outcome ~doc ?only_aim ?pkg ~meta uname func
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
