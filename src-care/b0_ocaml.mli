(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0 support for the OCaml programming language.

    Obviously not ready for the real world but is able to compile b0
    and its library structure (which includes C stubs).

    Showcases using the build unit metadata system to perform library
    lookup inside the source tree with fallback to the environment via
    ocamlfind if the library is absent from the source tree.

    {b Note.} In what follows we use the term module for
    what is really {e compilation units}.

    {b TODO.}
    {ul
    {- Per module flags.}
    {- Exposed fragment do not compose well for now. We certainly don't
       expose the right compilation model yet. The strategy also feels
       a bit wrong. Define data structures for what you want given a [conf]
       configuration and then pass it to operations which should just
       intrerpet the data structure. This should enable static access to what
       is important to use/add metadata. Not everything may be known so a
       meta adder function may still be needed. Desires and environment
       capabilities should be desynched.}
    {- exe, add build native byte flags, all default to [true].
       Should depend on the configuration.}
    {- lib, add build cma, cmxa, cmxs flags, all default to [true].
       Should depend on the configuration. Also {!Lib} lookup should
       be adjusted and error accordingly, for now assumes all these can be
       built.}
    {- lib, for now the full ocamlfind name should be specified. Devise a
      name to ocamlfind name mapping, we might need the notion of package at
      that point though. See also
    {{:https://github.com/dbuenzli/odig/blob/master/SEL.md#module-selectors-wip}
    here}.}
    {- {!Unit.build} constructors need to be more subtle and terse. Certainly
       something to be done with generic source file combinators.}
    {- Explain how clients can make their own library units with their
       own compilation scheme and yet cope with the library lookup.}
    {- {!Lib}, expose constructors so that alternative lookups can
       be implemented.}
    {- Add support for variants, they are supported now but not
       better than [ocamlbuild] did via ocamlfind encoding.}
    {- Do we want to support version checks at this level ?}
    {- {!Unit.lib_deps} full generality would make
       this information not static but configuration dependent,
       how exactly ?}} *)
open B0

(** {1:conf Toolchain and build configuration} *)

(** OCaml tools. *)
module Tool : sig

  val ocamldep : Tool.t Conf.key
  (** [ocamldep] is the OCaml dependency analyzer. *)

  val ocamlc : Tool.t Conf.key
  (** [ocamlc] is the OCaml byte-code compiler. *)

  val ocamlopt : Tool.t Conf.key
  (** [ocamlopt] is the OCaml native-code compiler. *)

  val ocamlmktop : Tool.t Conf.key
  (** [ocamlmktop] is the OCaml custom toplevel builder. *)

  val ocamlmklib : Tool.t Conf.key
  (** [ocamlmklib] is the OCaml mixed C/OCaml library builder. *)

  val env_vars : Tool.env_vars
  (** Environment variables that influence the OCaml toolchain outputs.
      The current list, used for all tools listed above regardless, is:
{[
CAMLLIB CAMLSIGPIPE CAML_DEBUG_FILE CAML_DEBUG_SOCKET
CAML_LD_LIBRARY_PATH OCAMLDEBUG OCAMLLIB OCAMLPROF_DUMP
OCAMLRUNPARAM OCAML_COLOR OCAML_FLEXLINK OCAML_INSTR_FILE
OCAML_INSTR_START OCAML_INSTR_STOP OCAML_SPACETIME_INTERVAL
OCAML_SPACETIME_SNAPSHOT_DIR PATH TERM __AFL_SHM_ID
]} *)
end

(** Configuration keys *)
module Key : sig

  (** {1:ocaml OCaml configuration keys} *)

  val build_byte : bool Conf.key
  (** [build_byte] is [true] if byte code artefacts should be produced. *)

  val build_native : bool Conf.key
  (** [build_native] is [true] if native code artefacts should be produced. *)

  val build_native_dynlink : bool Conf.key
  (** [build_native_dynlink] is [true] if native dynlink artefacts should
      be produced. *)

  val build_cmtis : bool Conf.key
  (** [build_cmtis] is [true] if [cmti] artefacts should be produced. *)

  val stacktraces : bool Conf.key
  (** [stacktraces] is [true] if stacktrace support should be compiled. *)
end

type conf
(** The type for OCaml build configuration. *)

val conf : Build.t -> conf
(** [ctx b] is an OCaml configuration derived from the build configuration. *)

(** OCaml build configuration. *)
module Conf : sig
  val build_byte : conf -> bool
  (** [build_byte c] is [true] if byte code artefacts should be produced. *)

  val build_native : conf -> bool
  (** [build_native c] is [true] if native code artefacts should be produced. *)

  val build_native_dynlink : conf -> bool
  (** [build_native_dynlink c] is [true] if native dynlink artefacts should
      be produced. *)

  val build_cmtis : conf -> bool
  (** [build_cmtis c] is [true] if [cmti] artefacts should be produced. *)

  val byte_compiler : conf -> Cmd.t -> Build.run
  (** [byte_compiler c args] is a tool run for the OCaml byte code compiler. *)

  val native_compiler : conf -> Cmd.t -> Build.run
  (** [anative_compiler c args] is a tool run for the OCaml native code
      compiler. *)

  val cmi_compiler : conf -> Cmd.t -> Build.run
  (** [cmi_compiler c args] is a tool run for the compiler responsible
      for compiling cmi files. *)

  val stacktraces : conf -> Cmd.t
  (** [stacktraces] is [true] if stacktrace support should be compiled. *)

  val exe_ext : conf -> Fpath.ext
  (** [exe_ext c] is the operating system extensions for
      file executables. See {!B0_care.OS.exe_ext}. *)

  val native_ext : conf -> Fpath.ext
  (** [native_ext c] is the file extension for OCaml native code executables. *)

  val byte_ext : conf -> Fpath.ext
  (** [byte_ext c] is the file extension for OCaml byte code executables. *)

  val cobj_ext : conf -> Fpath.ext
  (** [cobj_ext c] is the file extension for C object files.
      See {!B0_c.obj_ext}. *)

  val clib_ext : conf -> Fpath.ext
  (** [clib_ext c] is the file extension for C static libraries.
      See {!B0_c.lib_ext}. *)

  val cdll_ext : conf -> Fpath.ext
  (** [clib_ext c] is the file extension for C dynamic libraries.
      See {!B0_c.dll_ext}. *)
end

(** OCaml library lookup. *)
module Lib : sig

  (** {1:lib Libraries} *)

  type name = string
  (** The type for library names.
      TODO define syntax precisely. *)

  type t
  (** The type for library specification. *)

  (** {1:set Library sets} *)

  type set
  (** The type for a set of libraries and their usage information. *)

  val empty : set
  (** The empty library set. *)

  val is_empty : set -> bool
  (** [is_empty s] is [true] iff [s] is empty. *)

  val root_libs : set -> t list
  (** [root_libs s] is the ordered set of libraries requested in the set. *)

  val libs : set -> t list
  (** [libs s] is the ordered set of libraries requested in the set with
      their recursive dependencies. *)

  val incs_comp : set -> Fpath.t list
  (** [incs_comp s] is the list of includes needed to compile a source
      against the set. *)

  val incs_link : set -> Fpath.t list
  (** [incs_link s] is the list of includes needed to link an object
      against the set. *)

  val cmas : set -> Fpath.t list
  (** [cmas s] is the ordered set of [cma] files needed to byte link
      against the library set. *)

  val cmxas : set -> Fpath.t list
  (** [cmxas s] is the ordered set of [cmxa] files needed to native
      link against the library set. *)

(*
  val cmxss : set -> Fpath.t list
  (** [cmxss s] is the ordered set of [cmxs] files needed to dynlink
      against the library set. *)
*)

  (** {1:res Library set resolution} *)

  type resolver
  (** The type for library set resolvers. *)

  val resolve :
    Build.t -> resolver -> conf -> name list -> Fpath.t ->  set Build.fiber
  (** [resolve b r libs file] lookups [libs] and their recursive
      dependencies in build [b] using resolver [r]. [file] is a unique
      file specifed by the client for storing the result of the lookup
      which might be needed by the resolver.

      All the library archives in the result set are either built or
      marked as {!B0.Build.ready}. *)

  val get_resolver : Build.t -> resolver option -> resolver
  (** [get_resolver b r] is the resolver [r] or a default resolver
      (potententially looked up in [b]'s config). *)

  (** {1:low Low-level interface} *)

  val ocamlfind : B0.Tool.t B0.Conf.key
  (** [ocamlfind] is the [ocamlfind] tool. *)
end

(** {1:build Building OCaml} *)

(** OCaml source module dependencies.

    Computes OCaml source module dependencies via [ocamldep]. *)
module Deps : sig

  (** {1:deps Dependencies} *)

  type t
  (** The type for module dependencies. *)

  val write : ?opts:Cmd.t -> Build.t -> srcs:Fpath.t list -> Fpath.t -> unit
  (** [write b ~srcs file] computes module dependencies of [srcs] to
      [file] in [b]. [opts] are optional arguments that can be passed
      to the {!Tool.ocamldep} invocation (defaults to {!B0.Cmd.empty}). *)

  val read : Build.t -> Fpath.t -> dst:Fpath.t -> t Build.fiber
  (** [read b file] reads a {!write} from [file] and continues with
      source file module dependencies to be compiled in a directory
      [dst]. *)

  (** {1:low Low level interface} *)

  val of_ocamldep_output :
    src:Fpath.t -> string -> ('a -> Fpath.t -> string list -> 'a ) -> 'a ->
    'a result
  (** [of_ocamldep_output ~src data f acc] parses [ocamldep -modules] output
      from [data] and folds [f] starting with [acc] over each file
      dependencies. [src] is the filename used to report parse errors. *)
end

(** OCaml build fragments.

    {b Threads.} FIXME the good strategy here is a bit unclear due to
    ocamlfind and ocaml interaction. In order to use OCaml POSIX or vm
    threads you should simply use the libraries [thread.posix] or
    [thread.vm]. We don't this for now but maybe we could:
    automatically add the needed [-thread] and [-vmthread] on the cli.
    The problem here is that we will compile many things that are not
    needed with these flags since we also use this to signal ocamlfind
    that the mt mt.posix are needed. *)
module Build : sig

  (** {1:steps Build steps} *)

  val srcs : Build.t -> dir:Fpath.t -> Fpath.t list * Fpath.t list
  (** [src b dir] is [(ml_srcs, c_srcs)] where [ml_srcs] are files ending up
      in [.ml] and [.mli] in [dir] and [c_srcs] the files ending up in
      [.c] and [.h]. All the resulting files are made {{!B0.Build.ready}ready}
      in [b].

      A file named [B0.ml] is excluded from the list. *)

  val compile :
    ?lib_deps:Lib.set -> Build.t -> conf -> Deps.t -> dst:Fpath.t -> unit

  val link :
    ?lib_deps:Lib.set -> Build.t -> conf -> Deps.t -> cobjs:Fpath.t list ->
    dst:Fpath.t -> string -> unit

  val archive :
    Build.t -> conf -> Deps.t -> cstubs:bool -> dst:Fpath.t -> string -> unit

  (** {1:combined Combined steps} *)

  val exe :
    ?csrcs:Fpath.t list -> ?lib_deps:Lib.set -> Build.t -> conf ->
    srcs:Fpath.t list -> string -> unit
  (** [exe b c srcs name] produces an executable with base name [name]
      by compiling and linking [srcs] along with [libs]. [srcs]
      are automatically sorted but must be made {{!B0.Build.ready}ready} or
      built. *)

  val lib :
    ?csrcs:Fpath.t list -> ?lib_deps:Lib.set -> Build.t -> conf ->
    srcs:Fpath.t list -> string -> unit
  (** [libs b c srcs name] produces a library with base name [base] by compiling
      and [srcs] against [libs]. [srcs] are automatically sorted but must be
      made {{!B0.Build.ready}ready} or built. *)

  val tool :
    ?internal:B0.Tool.env_vars -> ?env_vars:B0.Tool.env_vars ->
    ?name:Fpath.t -> build -> Unit.t -> conf -> Cmd.t -> Build.run
    (** [tool ~name b u c args] is a run for a tool named [basename] compiled
      via {!exe} in [u] with configuration [c]. This will automatically
      select the native tool if [c] allows and fallback to byte code
      otherwise.

     {b FIXME} Also lookup metadata in [u] for forced byte, also need
      something in [bin]. *)
end

(** OCaml build units and tools. *)
module Unit : sig

  type build =
    [ `Src_dirs of Fpath.t list
    | `Srcs of Fpath.t list
    | `Func of
        Lib.resolver option -> Lib.name list -> string -> B0.Build.t -> unit ]

  val exe :
    ?loc:Def.loc -> ?src_root:Fpath.t -> ?doc:string ->
    ?only_aim:Env.build_aim -> ?pkg:Pkg.t -> ?meta:Unit.Meta.t ->
    ?resolver:Lib.resolver -> ?lib_deps:Lib.name list ->
    ?name:string -> string -> build -> Unit.t
  (** [exe] is an executable named [name]. *)

  val lib :
    ?loc:Def.loc -> ?src_root:Fpath.t -> ?doc:string ->
    ?only_aim:Env.build_aim -> ?pkg:Pkg.t -> ?meta:Unit.Meta.t ->
    ?resolver:Lib.resolver -> ?lib_deps:Lib.name list ->
    ?name:string -> string -> build -> Unit.t
  (** [lib name] is a library named [name]. *)

  (** {1 Low level interface} *)

  val tag : bool Unit.Meta.key
  (** [tag] indicates the unit deals with OCaml related outcomes. *)

  val lib_deps : Lib.name list Unit.Meta.key
  (** [lib_deps] is a list of OCaml libraries {{!Lib.name}names} the
      unit needs to produce its outcomes. *)

  val lib_name : Lib.name Unit.Meta.key
  (** [lib_name] is the name of a library built by the unit. *)

  val exe_meta : lib_deps:Lib.name list -> string -> Unit.Meta.t
  (** [exe_meta] is the default metadata for {!exe}. *)

  val lib_meta : lib_deps:Lib.name list -> string -> Unit.Meta.t
  (** [exe_meta] is the default metadata for {!lib}. *)
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
