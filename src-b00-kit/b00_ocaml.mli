(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 [ocaml] support. *)

open B00_std
open B00

(** OCaml tools. *)
module Tool : sig

  (** {1:comp Compilers} *)

  val comp_env_vars : Tool.env_vars
  (** [comp_env_vars] are environment variables that influence the
      OCaml toolchain outputs. *)

  val ocamlc : Tool.t
  (** [ocamlc] is the [ocamlc] tool. *)

  val ocamlopt : Tool.t
  (** [ocamlopt] is the [ocamlopt] tool. *)

  val ocamldep : Tool.t
  (** [ocamldep] is the [ocamldep] tool. *)

  val ocamlmklib : Tool.t
  (** [ocamlmklib] is the [ocamlmklib] tool. *)

  val ocamlobjinfo : Tool.t
  (** [ocamlobjinfo] is the [ocamlobjinfo] tool. *)

  (** {1:top Toplevels} *)

  val ocaml : Tool.t
  (** [ocaml] is the [ocaml] tool. *)

  val ocamlnat : Tool.t
  (** [ocamlnat] is the [ocamlnat] tool. *)

  val top_env_vars : Tool.env_vars
  (** [top_env_vars] are environment variables that influence the
      OCaml toplevel. *)
end

(** OCaml toolchain configuration.

    Thid module provides access to the OCaml toolchain configuration
    as output by [ocaml{c,opt} -config].

    It also provides {{!build}bits} to orchestrate byte and native
    OCaml builds. *)
module Conf : sig

  (** {1:conf Configuration} *)

  type t
  (** The type for the OCaml toolchain configuration. *)

  val find : string -> t -> string option
  (** [find f c] looks up the field [f] in configuration [c].
      See [ocamlc -config] for the list of fields. *)

  val version : t -> int * int * int * string option
  (** [version c] is the compiler version string
      ["major.minor[.patchlevel][+additional-info]"] parsed
      using
      [(major, minor, patch, additional-info)]. If [patch-level]
      is absent it is turned into a [0]. *)

  val where : t -> Fpath.t
  (** [where c] is the location of OCaml's library directory. *)

  val asm_ext : t -> Fpath.ext
  (** [asm_ext] is the file extension for assembly files. *)

  val dll_ext : t -> Fpath.ext
  (** [dll_ext] is the file extension for C dynamic libraries. *)

  val exe_ext : t -> Fpath.ext
  (** [ext_ext] is the file extension for executable binaries. *)

  val lib_ext : t -> Fpath.ext
  (** [ext_lib] is the file extension for C static libraries. *)

  val obj_ext : t -> Fpath.ext
  (** [obj_ext] is the file extension for C object files. *)

  (** {1:convert Converting} *)

  val to_string_map : t -> string String.Map.t
  (** [to_string_map c] are the fields of [c] as a string map. *)

  val of_string_map : string String.Map.t -> (t, string) result
  (** [of_string_map m] is a configuration from string map [m].
      [m] needs at least a ["version"] key otherwise the function
      errors. *)

  (** {1:io IO} *)

  val write : B00.Memo.t -> comp:B00.Tool.t -> o:Fpath.t -> unit
  (** [write m ~o] writes the toolchain configuration to [o] by
      running [comp] with [-config]. *)

  val read : B00.Memo.t -> Fpath.t -> t Memo.fiber
  (** [read m file] reads a toolchain configuration from [file]. *)

  val of_string : ?file:Fpath.t -> string -> (t, string) result
  (** [of_string ~file data] parses toolchain configuration from [data]
      as output by the compiler's [-config] option assuming it was read
      from file [file] (defaults to {!B00_std.Os.File.dash}. *)

  (** {1:build Build orchestration}

      {b XXX} Maybe move that to {!B0_ocaml}.

      In a build it is desirable to know which code is being produced
      because if both are produced the compilers may compete to
      produce some of the shared build artefacts. The following store
      keys allow to express build code {{!build_code}desires} and
      determine the actual {{!built_code}decision}. *)

  type built_code = [ `Byte | `Native | `Both ]
  (** The type indicating which code is being built. *)

  val build_code : [ built_code | `Auto ] Store.key
  (** [build_code] indicates which code should be built, default
      determines to [`Auto]. [`Auto] indicates [`Native] should be
      used if [ocamlopt] can be found in the memo environment and
      [`Byte] otherwise. *)

  val built_code : built_code Store.key
  (** [build_code] is a memo key indicating the built code. By default
      determines by consulting [build_code]. *)

  val key : t Store.key
  (** [key] is a memo key store with the OCaml configuration. *)

  (** {1:todo TODO remove that} *)

  val exists : Memo.t -> bool Memo.fiber
  val if_exists : Memo.t -> (unit -> 'a Memo.fiber) -> 'a option Memo.fiber
  val stdlib_dir : Memo.t -> unit -> Fpath.t Memo.fiber
end

(** Module names. *)
module Mod_name : sig

  (** {1:name Module names} *)

  type t = string
  (** The type for unqualified, capitalized, module names. *)

  val v : string -> t
  (** [v n] is a module name for [n], the result is capitalized. *)

  val of_filename : Fpath.t -> t
  (** [of_filename f] is the basename of [f], without extension, capitalized.
      This assumes the basename of [f] follows the OCaml file naming
      convention mandated by the toolchain. If you know that may not
      be the case use {!of_mangled_filename}. *)

  val equal : t -> t -> bool
  (** [equal n0 n1] is [true] iff [n0] and [n1] are the same module name. *)

  val compare : t -> t -> int
  (** [comare n0 n1] is a total order on module names compatiable with
      {!equal}. *)

  val pp : t Fmt.t
  (** [pp] formats a module name. *)

  (** Module name sets. *)
  module Set = String.Set

  (** Module name maps. *)
  module Map = String.Map

  (** {1:mangled Filename mangling} *)

  val of_mangled_filename : string -> t
  (** [of_mangled_filename s] is module name obtained by mangling
      the filename of [s] as follows:

      {ol
      {- Remove any trailing [.ml] or [.mli].}
      {- Map any dash [-] (0x2D) or dot [.] (0x2E) to an underscore
          [_] (0x5F).}
      {- Map any byte not allowed in OCaml compilation unit names to its two
          digits capital hexadecimal encoding.}
      {- If the result does not start with an US-ASCII letter, prefix
          the unit name with ['M'].}
      {- Capitalize the first letter.}}

      The transformation is consistent with {!of_filename} on files
      that follows the OCaml toolchain convention. However the
      transformation is not injective. Here are a few examples:

{v
           filename   Module name
 ----------------------------------------
    publish-website   Publish_website
    publish_website   Publish_website
     import-data.ml   Import_data
 import-data.xml.ml   Import_data_xml
 import-data.script   Import_data_script
          mix+match   Mix2Bmatch
        _release.ml   M_release
v} *)
end

(** Digested module references.

    {b TODO.} Use that in [B00_odoc]. *)
module Mod_ref : sig

  (** {1:modrefs Module references} *)

  type t
  (** The type for module references as found in compilation objects.
      This is a module name and a digest of its interface. *)

  val v : string -> Digest.t -> t
  (** [v n d] is a module reference with name [n] and digest [d]. *)

  val name : t -> Mod_name.t
  (** [name m] is the capitalized module name of module reference [m]. *)

  val digest : t -> Digest.t
  (** [digest m] is the interface digest of module reference [m]. *)

  val equal : t -> t -> bool
  (** [equal m m'] is [true] iff [m] and [m'] are the same reference. *)

  val compare : t -> t -> int
  (** [compare m m'] is a total order on module references. *)

  val pp : t Fmt.t
  (** [pp] formats a module reference. *)

  (** Module reference sets. *)
  module Set : sig
    include Set.S with type elt = t

    val pp : ?sep:unit Fmt.t -> elt Fmt.t -> t Fmt.t
    (** [pp ~sep pp_elt ppf rs] formats the elements of [rs] on [ppf].
        Each element is formatted with [pp_elt] and elements are
        separated by [~sep] (defaults to {!Fmt.cut}). If the set is
        empty leaves [ppf] untouched. *)

    val dump : t Fmt.t
    (** [dump ppf ss] prints an unspecified representation of [ss] on
        [ppf]. *)
  end

  (** Module reference maps. *)
  module Map : sig
    include Map.S with type key = t

    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)

    val of_list : (key * 'a) list -> 'a t
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    (** {1:add Additional adds} *)

    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    (** [add k v m] is [m] with [k] mapping to [l] such that [l] is
        [v :: find k m] if [k] was bound in [m] and [[v]] otherwise. *)

    val add_to_set :
      (module B00_std.Stdlib_set.S with type elt = 'a and type t = 'set) ->
      key -> 'a -> 'set t -> 'set t
    (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
        [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
        otherwise. *)

    (** {1:fmt Formatting} *)

    val pp : ?sep:unit Fmt.t -> (key * 'a) Fmt.t -> 'a t Fmt.t
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val dump : 'a Fmt.t -> 'a t Fmt.t
    (** [dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)
  end
end

(** Compiled object information. *)
module Cobj : sig

  type code = Byte | Native (** *)
  (** The type for code generated by the OCaml compiler. Either
      bytecode or native-code. *)

  val archive_ext_of_code : code -> Fpath.ext
  (** [archive_ext_of_code c] is [.cma] or [.cmxa] according to [c]. *)

  val object_ext_of_code : code -> Fpath.ext
  (** [object_ext_of_code c] is [.cmo] or [.cmx] according to [c]. *)

  (** {1:cobjs Compilation objects} *)

  type t
  (** The type for compilation objects. This can represent one
      of a [cmi], [cmti], [cmo], [cmx], [cmt], [cma] or [cmxa] file. *)

  val file : t -> Fpath.t
  (** [file c] is the compilation object file path. *)

  val defs : t -> Mod_ref.Set.t
  (** [defs c] are the modules defined by the compilation object. If
      there's more than one you are looking an archive. *)

  val deps : t -> Mod_ref.Set.t
  (** [deps c] is the set of modules needed by [defs c]. More precisely
      these are the module interfaces imported by [c]. See also {!link_deps}. *)

  val link_deps : t -> Mod_ref.Set.t
  (** [link_deps c] is the set of modules needed to link [defs c].

      {b Note.} Unclear whether this is the right data. Basically
      these are the module references that of {!deps} whose name is in the
      {{:https://github.com/ocaml/ocaml/blob/a0fa9aa6e85ca4db9fc19389f89be9ff0d3bd00f/file_formats/cmo_format.mli#L36}required globals}
      (bytecode) or {{:https://github.com/ocaml/ocaml/blob/trunk/file_formats/cmx_format.mli#L43}imported implementations} (native code) as reported
      by ocamlobjinfo. Initially we'd use [deps] for link dependencies
      but it turns out that this may break on
      {{:https://github.com/ocaml/ocaml/issues/8728}certain} install
      structures. It's unclear whether we need both {!deps} and
      {!link_deps} and/or if that's the correct information. *)

  val pp : t Fmt.t
  (** [pp] formats an compilation object. *)

  val sort : ?deps:(t -> Mod_ref.Set.t) -> t list -> t list * Mod_ref.Set.t
  (** [sort ~deps cobjs] is [cobjs] stable sorted in dependency
      order according to [deps] (defaults to {!link_deps}), tupled with
      external dependencies needed by [cobjs]. *)

  val equal : t -> t -> bool
  (** [equal c0 c1] is [Fpath.equal (file c0) (file c1)]. *)

  val compare : t -> t -> int
  (** [compare] is a total order on compilation objects compatible
      with {!equal}. *)

  (** Compilation objects sets. *)
  module Set : Set.S with type elt = t

  (** Compilation objectx maps. *)
  module Map : Map.S with type key = t

  (** {1:io IO} *)

  val write : B00.Memo.t -> cobjs:Fpath.t list -> o:Fpath.t -> unit
  (** [write m ~cobjs o] writes information about the compilation [cobjs]
      to [o]. *)

  val read : B00.Memo.t -> Fpath.t -> t list Memo.fiber
  (** [read m file] has the [cobjs] of a {!write} to [file]. *)

  val of_string : ?file:Fpath.t -> string -> (t list, string) result
  (** [of_string ~file data] parses compilation object information from
      [data] as output by {!Tool.ocamlobjinfo} assuming it was
      read from [file] (defaults to {!B00_std.Os.File.dash}). *)
end

(** Module sources. *)
module Mod_src : sig

  (** {1:mods Modules} *)

  (** Source dependencies. *)
  module Deps : sig
    val write :
      ?src_root:Fpath.t -> Memo.t -> srcs:Fpath.t list -> o:Fpath.t -> unit
    (** [write m ~src_root ~srcs ~o] writes dependencies of [srcs]
        in file [o]. If [src_root] if specified it is used as the [cwd]
        for the operation and assumed to be a prefix of every file in
        [srcs], this allows the output not to the depend on absolute
        paths.

        {b UPSTREAM FIXME.} We don't actually do what is mentioned
        about [src_root]. The problem is that the path of parse errors
        end up being wrongly reported. It would be nice to add an
        option for output prefix trimming to the tool and/or control
        on the whole toolchain for how errors are reported. This means
        that for now we cannot cache these operations across
        machines. *)

    val read :
      ?src_root:Fpath.t -> Memo.t -> Fpath.t ->
      Mod_name.Set.t Fpath.Map.t Memo.fiber
      (** [read ~src_root depsfile] reads dependencies produced by
          {!write} as a map from absolute file paths to their
          dependencies.  Relative file paths are made absolute using
          [src_root] (defaults to {!B00_std.Os.Dir.cwd}). *)
  end

  type t
  (** The type for OCaml module sources, represents a module to compile. *)

  val v :
    mod_name:Mod_name.t -> opaque:bool -> mli:Fpath.t option ->
    mli_deps:Mod_name.Set.t -> ml:Fpath.t option ->
    ml_deps:Mod_name.Set.t -> t
  (** [v ~mod_name ~opaque ~mli ~mli_deps ~ml ~ml_deps] is a module whose name
      is [name], interface file is [mli] (if any), interface file module
      dependencies is [mli_deps], implementation is [ml] (if any) and
      implementation file module dependencies [ml_deps]. For [opaque]
      see {!opaque}. *)

  val mod_name : t -> Mod_name.t
  (** [mod_name m] is [m]'s name. *)

  val opaque : t -> bool
  (** [opaque m] indicates whether the module should be treated as
      opaque for compilation. See the [-opaque] option in the OCaml
      manual. *)

  val mli : t -> Fpath.t option
  (** [mli m] is [m]'s interface file (if any). *)

  val mli_deps : t -> Mod_name.Set.t
  (** [mli_deps m] are [m]'s interface file dependencies. *)

  val ml : t -> Fpath.t option
  (** [ml m] is [m]'s implementation file (if any). *)

  val ml_deps : t -> Mod_name.Set.t
  (** [ml_deps m] are [m]'s implementation file dependencies. *)

  (** {1:files Constructing file paths} *)

  val file : in_dir:Fpath.t -> t -> ext:string -> Fpath.t
  (** [file ~in_dir m ~ext] is a file for module [m] with
      extension [ext] in directory [in_dir]. *)

  val cmi_file : in_dir:Fpath.t -> t -> Fpath.t
  (** [cmi_file ~in_dir m] is [file ~in_dir m ext:".cmi"]. *)

  val cmo_file : in_dir:Fpath.t -> t -> Fpath.t
  (** [cmx_file ~in_dir m] is [fil_ ~in_dir m ext:".cmo"]. *)

  val cmx_file : in_dir:Fpath.t -> t -> Fpath.t
  (** [cmx_file ~in_dir m] is [file ~in_dir m ext:".cmx"]. *)

  val impl_file : code:Cobj.code -> in_dir:Fpath.t -> t -> Fpath.t
  (** [impl_file ~code ~in_dir m] is {!cmx_file} or {!cmo_file}
      according to [code]. *)

  val as_intf_dep_files :
    ?init:Fpath.t list -> in_dir:Fpath.t -> t -> Fpath.t list
  (** [as_intf_dep_files ~init ~in_dir m] adds to [init] (defaults
      to [[]]) the files that are read by the OCaml compiler if module
      source [m] is compiled in [in_dir] and used as an interface
      compilation dependency. *)

  val as_impl_dep_files :
    ?init:Fpath.t list -> code:Cobj.code -> in_dir:Fpath.t -> t ->
    Fpath.t list
  (** [as_impl_dep_files ~init ~code ~in_dir m] adds to [init] (defaults
      to [[]]) the files that are read by the OCaml
      compiler if module source [m] is compiled in [in_dir] and
      used an implementation file dependency for code [code]. *)

  (** {1:map Module name maps} *)

  val of_srcs :
    Memo.t -> src_deps:Mod_name.Set.t Fpath.Map.t -> srcs:Fpath.t list ->
    t Mod_name.Map.t
  (** [of_srcs ~src_deps deps ~srcs] determines source modules (mapped
      by their names) given sources [srcs] and their dependencies
      [src_deps] (e.g. obtainted via {!Deps.read}. If there's more
      than one [mli] or [ml] file for a given module name a warning is
      notified on [m] and a single one is kept. *)

  val find_local_deps :
    t Mod_name.Map.t -> Mod_name.Set.t -> t list * Mod_name.Set.t
  (** [find_local_deps ms deps] is [(mods, remain)] with [mods] the
      modules of [ms] whose name is in [deps] and [remain] the names
      of [deps] which cannot be found in [ms]. *)
end

(** OCaml and C stub compilation. *)
module Compile : sig

  (** {1:step Compilation steps} *)

  val c_to_o :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) ->
    Memo.t -> hs:Fpath.t list -> c:Fpath.t -> o:Fpath.t -> unit
  (** [c_to_obj m ~hs ~c ~o] compiles [c] to the object file
      [o] using assuming [c] includes headers [hs]. *)

  val mli_to_cmi :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    ?with_cmti:bool -> Memo.t -> reads:Fpath.t list -> mli:Fpath.t ->
    o:Fpath.t -> unit
  (** [mli_to_cmi m ~args ~with_cmti m ~reads ~mli ~o] compiles [mli]
      to [o]. [reads] are the cmi files reads by the operation.
      [with_cmti] indicates whether the cmti file should be produced
      (defaults to [true]). [args] are additional arguments that will
      be added on the cli. *)

  val ml_to_cmo :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    ?with_cmt:bool -> Memo.t -> has_cmi:bool -> reads:Fpath.t list ->
    ml:Fpath.t -> o:Fpath.t -> unit
  (** [ml_to_cmo m ~args ~with_cmt ~has_cmi ~reads ~ml ~o]
      compiles [ml] file to a the cmo file [o]. [reads] are
      the cmi files read by the operation. [has_cmi] indicates
      whether the [ml] file has already a corresponding cmi
      file, in which case it should be in [cmi_deps]. [with_cmt]
      indicates whether the cmt file should be produced (defaults to
      [true]). *)

  val ml_to_cmx :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    ?with_cmt:bool -> Memo.t -> has_cmi:bool -> reads:Fpath.t list ->
    ml:Fpath.t -> o:Fpath.t -> unit
  (** [ml_to_cmx m ~args ~with_cmt ~has_cmi ~reads ~ml ~o] compiles
      [ml] file to a the cmx file [o]. [reads] are are the cmx and cmi
      files read by the operation. [has_cmi] indicates whether the
      [ml] file has already a corresponding cmi file, in which case it
      should be in [cmi_deps]. [with_cmt] indicates whether the cmt
      file should be produced (defaults to [true]). *)

  val ml_to_impl :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    ?with_cmt:bool -> Memo.t -> code:Cobj.code -> has_cmi:bool ->
    reads:Fpath.t list -> ml:Fpath.t -> o:Fpath.t -> unit
  (** [ml_to_impl] is {!ml_to_cmo} or {!ml_to_cmx} according to [code]. *)

  val cstubs_archives :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    B00.Memo.t -> c_objs:Fpath.t list -> odir:Fpath.t -> oname:string -> unit
  (** [cstubs_archives m ~args ~c_objs ~odir ~oname] creates in directory
      [odir] C stubs archives for a library named [oname]. *)

  (* FIXME change the odir/oname into files as usual and pass the
     c stubs archive directly. *)

  val byte_archive :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> has_cstubs:bool -> cobjs:Fpath.t list -> odir:Fpath.t ->
    oname:string -> unit
  (** [byte_archive m ~args ~has_cstubs ~cobjs ~obase] creates in directory
      [odir] a bytecode archive named [oname] with the OCaml bytecode
      compilation objects [cobjs]. *)

  val native_archive :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> has_cstubs:bool -> cobjs:Fpath.t list -> odir:Fpath.t ->
    oname:string -> unit
  (** [native_archive m ~args ~has_cstubs ~cobjs ~obase] creates in directory
      [odir] a native code archive named [oname] with the OCaml native
      code compilation objects [cobjs]. *)

  val archive :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> code:Cobj.code -> has_cstubs:bool -> cobjs:Fpath.t list ->
    odir:Fpath.t -> oname:string -> unit
  (** [archive] is {!byte_archive} or {!native_archive} according to
      [code]. *)

  val native_dynlink_archive :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> has_cstubs:bool -> cmxa:Fpath.t -> o:Fpath.t -> unit
end

(** OCaml linking. *)
module Link : sig

  (** Link dependencies. *)
  module Deps : sig
    val write : B00.Memo.t -> cobjs:Fpath.t list -> o:Fpath.t -> unit
    (** [write m cobjs o] writes the external link dependencies and order
        of compilation unit objects of [cobjs] to [o]. *)

    val read :
      B00.Memo.t -> Fpath.t -> (Cobj.t list * Mod_ref.Set.t) Memo.fiber
      (** [read m file] has the [cobjs] of a {!write} to [file] in dependency
          order and external dependencies to resolve. *)
  end

  val byte_exe :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> c_objs:Fpath.t list -> cobjs:Fpath.t list -> o:Fpath.t -> unit
  (** [byte_exe m ~args ~c_objs ~cmos ~o] links the C objects [c_objs]
      and the OCaml compilation object files [cobjs] into a byte code
      executable [o] compiled in [-custom] mode. *)

  val native_exe :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> c_objs:Fpath.t list -> cobjs:Fpath.t list -> o:Fpath.t -> unit
    (** [byte_exe m ~args ~c_objs ~cobjs ~o] links the C objects [c_objs]
        and the OCaml compilation object files [cobjs] into a native code
        executable [o]. An include is added to each element of [cobjs] in
        order to lookup potential C stubs. *)

  val exe :
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> ?args:Cmd.t ->
    Memo.t -> code:Cobj.code -> c_objs:Fpath.t list -> cobjs:Fpath.t list ->
    o:Fpath.t -> unit
  (** [exe] is {!byte_exe} or {!native_exe} according to [code]. *)
end

(** {1:libs Libraries} *)

(** OCAMLPATH search path. *)
module Ocamlpath : sig

  val get : Memo.t -> Fpath.t list option -> Fpath.t list Memo.fiber
  (** [get m o k] is [k ps] if [o] is [Some ps] and otherwise in order:
      {ol
      {- If the [OCAMLPATH] environment variable is defined in [m] and
         non-empty its content is parsed s according to
         {!B00_std.Fpath.list_of_search_path}.}
      {- If the [OPAM_SWITCH_PREFIX] environment variable is defined with
         a path [P] then [[P/lib]] is used.}
      {- The fiber fails.}} *)
end

(** Library names *)
module Lib_name : sig

  (** {1:name Module names} *)

  type t
  (** The type for library names looked up in [OCAMLPATH].

      This is either:
      {ul
      {- The name of an OCaml library that follows the OCaml library
         convention. For example the [b0.std] names looks up
         [b0/std/lib.cm{a,xa,xs}]. These libraries are assumed to have
         been compiled according to the library convention and have
         their library dependencies embedded in their objects.}
      {- For legacy reasons, the name of an OCaml library followed by a
         slash to indicate the name of an archive. That is for example
         [b0.std/b0_std], which looks up [b0/std/b0_std.cm{a,xa,xs}].
         These libraries do not have their library dependencies specified,
         their dependencies are looked by a best-effort data-driven resolution
         procedure.}} *)

  val v : string -> t
  (** [v s] is a library for [n]. Raises [Invalid_argument] if [s] is
      not a valid library name. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is a library name from [n]. *)

  val to_string : ?no_legacy:bool -> t -> string
  (** [to_string n] is [n] as a string. If [no_legacy] is true
      does not show the legacy part of the lib. *)

  val is_legacy : t -> bool
  (** [legacy n] is [true] if [n] is a legacy name. *)

  val equal : t -> t -> bool
  (** [equal n0 n1] is [true] iff [n0] and [n1] are the same library name. *)

  val compare : t -> t -> int
  (** [compare n0 n1] is a total order on library names compatible with
      {!equal}. *)

  val pp : t Fmt.t
  (** [pp] formats a library name. *)

  (** Library name sets. *)
  module Set : Set.S with type elt = t

  (** Library name maps. *)
  module Map : Map.S with type key = t
end

(** Libraries *)
module Lib : sig

  type t
  (** The type for libraries. *)

  val name : t -> Lib_name.t
  (** [name l] is the library name of [l]. *)

  val cmis : t -> Fpath.t list
  (** [cmis l] is the list of cmis for the library. *)

  (*
  val find_cmi : t -> Mod_name.t -> Fpath.t option
  (** [find_cmi l n] is the path to a cmi file for module [n] in [l]
      (if any) *)
*)

  val dir : t -> Fpath.t
  (** [dir l] is the path to the library directory. Incidentally this
      is also the path to the library includes. *)

  val archive : code:Cobj.code -> t -> Fpath.t
  (** [archive ~code l] is the path to the library archive for code [c].
      Not checked for existence. *)
end

(** Library resolvers. *)
module Lib_resolver : sig

  type t
  (** The type for library resolvers. *)

  val create : Memo.t -> memo_dir:Fpath.t -> ocamlpath:Fpath.t list -> t
  (** [create m ocamlpath] is a library resolver looking for
      libraries in the OCAMLPATH [ocamlpath]. *)

  val find : t -> Lib_name.t -> Lib.t Memo.fiber
  (** [find r l] finds library [l] using [r]. The fiber fails
      if the library name cannot be found.

      This should look a list and sort. *)
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
