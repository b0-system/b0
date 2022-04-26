(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml B0 file support. *)

open B00_std
open B00
open B00_ocaml

(** {1:units Units}

    A few high-level build units. *)

val libname : string -> Lib.Name.t
(** [libname n] is [n] as an OCaml {{!B00_ocaml.Lib.Name}library name}.
    This is shortcut for {!B00_ocaml.Lib.Name.v}. *)

val exe :
  ?wrap:(B0_unit.proc -> B0_unit.proc) -> ?doc:string -> ?meta:B0_meta.t ->
  ?action:B0_unit.action -> ?c_requires:Cmd.t -> ?requires:Lib.Name.t list ->
  ?name:string -> string -> srcs:B0_srcs.sels -> B0_unit.t
(** [exe n] is a build unit for an executable named [n] (without
    the platform specific extension).
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [c_requires] FIXME hack, something more sensitive should be done.
       This each of these options are passed as [-cclib] options.}
    {- [requires] are the OCaml libraries required to compile the executable.}
    {- [name] is the name of the unit (defaults to [n]).}
    {- [srcs] are the executable sources. All files with extension [.ml],
       [.mli], [.c] and [.h] are considered for compiling and linking the
       executable.}
    {- [wrap] allows to extend the build procedure you must call the given
       build procedure. TODO maybe remove once we have good {!frag}.}} *)

val lib :
  ?wrap:(B0_unit.proc -> B0_unit.proc) -> ?doc:string -> ?meta:B0_meta.t ->
  ?action:B0_unit.action -> ?c_requires:Cmd.t ->
  ?requires:Lib.Name.t list -> ?name:string ->
  Lib.Name.t -> srcs:B0_srcs.sels -> B0_unit.t
(** [lib n ~srcs] is a built unit for a library named [l] made of
    sources [src].
    {ul
    {- [doc] is the unit doc string.}
    {- [meta] is the initial metadata.}
    {- [c_requires] FIXME hack, something more sensitive should be done.
       This each of these options are passed as [-cclib] options.}
    {- [requires] are the OCaml libraries required to compile the library.}
    {- [name] is the name of the build unit (default to [n] with [.]
        substituted by [-])}
    {- [srcs] are the library sources. extension [.ml],
       [.mli], [.c] and [.h] are considered for compiling and linking the
       executable.}
    {- [wrap] allows to extend the build procedure you must call the given
       build procedure. TODO maybe remove once we have good {!frag}.}} *)

(** {1:build_conf Build configuration} *)

val conf : B00_ocaml.Conf.t Store.key
(** [conf] is a memo key store with the OCaml configuration. *)

val version : B0_build.t -> (int * int * int * string option) Fut.t
(** [ocaml_version b] gets {!B00_ocaml.Tool.Conf.version} from {!conf}. *)

(** {2:code Generated code}

    In a build it is desirable to know which code is being produced
    because if both are produced the compilers may compete to
    produce some of the shared build artefacts. The following store
    keys allow to express build code {{!val-wanted_code}desires} and
    determine the actual {{!val-built_code}decision}. Note the desires
    may actually be altered units that may build FIXME maybe we
    should stick to must. *)

type built_code = [ `Byte | `Native | `All ]
(** The type indicating which code is being built. *)

val pp_built_code : built_code Fmt.t
(** [pp_built_code] formats {!type-built_code} values. *)

val wanted_code : [ built_code | `Auto ] Store.key
(** [wanted_code] indicates which code should be built, default
    determines to [`Auto]. If [`Auto] is used and no unit that
    may build has specific {!Meta.needs_code} then [`Native] is
    used if [ocamlopt] can be found in the memo environment and
    [`Byte] otherwise. *)

val built_code : built_code Store.key
(** [build_code] is a memo key indicating the built code. By default
    determines by consulting [wanted_code]. *)

(** {1:frag Build fragments}

    See {{!page-TODO.frag}TODO}. *)

(** {1:metadata Metadata} *)

val tag : unit B0_meta.key
(** [tag] indicates the entity is related to OCaml. *)

(** Metadata keys *)
module Meta : sig

  val c_requires : Cmd.t B0_meta.key
  (** [c_requires] hack for now this simply passes these options as
      [-cclib] options. *)

  val requires : Lib.Name.t list B0_meta.key
  (** [requires] on a build unit specifies the OCaml libraries needed to
      compile a unit. *)

  val library : Lib.Name.t B0_meta.key
  (** [library] on a build unit specifies that the unit defines
      the library with the given name. *)

  val mod_srcs : Mod.Src.t Mod.Name.Map.t  Fut.t B0_meta.key
  (** FIXME quick hack this should not be in meta. *)

  val supported_code : built_code B0_meta.key
  (** [supported_code] indicates which backend code the unit supports.
      If the meta is unspecified this assumes [`All]. *)

  val needs_code : built_code  B0_meta.key
  (** [needs_code] indicates the unit unconditionally needs a given code
      build. *)
end

(** {1:lib Library resolution} *)

val lib_resolver : Lib.Resolver.t Store.key
(** [lib_resolver] is the library resolver used by the build units defined by
    this module. See {!default_lib_resolver} for the default. *)

val default_lib_resolver : Store.t -> Memo.t -> Lib.Resolver.t Fut.t
(** [default_lib_resolver] determines the default value of {!lib_resolver}.
    This resolver does the following:
    {ol
    {- It first looks if the library name is {{!Meta.library}defined}
       by a unit that {{!B0_build.may_build}may} be built. If that
       is the case it creates a library out of that build unit via
       {!lib_of_unit}.}
    {- It looks into the build environment via
       {!B00_ocaml.Lib.Resolver.ocamlpath} and
       {!B00_ocaml.Lib.Resolver.ocamlfind}
       using the {!B0_build.shared_build_dir} directory of the build.}} *)

val lib_of_unit :
  B0_build.t -> B00_ocaml.Conf.t -> B0_unit.t -> Lib.t option Fut.t
(** [lib_of_unit b ocaml_conf u] defines a library from unit [u] by
    consulting {!Meta.requires}, {!Meta.library} and {!Meta.mod_srcs}.
    As a side effect this {!B0_build.require} [u]. *)

val lib_resolver_build_scope :
  B0_build.t -> B00_ocaml.Conf.t -> B00_ocaml.Lib.Resolver.scope
(** [lib_resolver_build_scope b conf] is a library resolver scope for
    OCaml libraries that can be built in [b]. For a unit [u] to be recognized
    as such it has to:
    {ul
    {- Be in the {!B0_build.may_build} set of [b].}
    {- Define the {!Meta.library} and {!Meta.requires} in its
       metadata.}} *)

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
