(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml B0 file support. *)

open B00_std
open B00
open B00_ocaml

val lib : string -> Lib.Name.t
(** [lib] is {!B00_ocaml.Lib.Name.v}. *)

(** {1:build_conf Build configuration} *)

val conf : Tool.Conf.t Store.key
(** [conf] is a memo key store with the OCaml configuration. *)

val version : B0_build.t -> (int * int * int * string option) Fut.t
(** [ocaml_version b] gets {!B00_ocaml.Tool.Conf.version} from {!conf}. *)

(** {2:code Generated code}

    In a build it is desirable to know which code is being produced
    because if both are produced the compilers may compete to
    produce some of the shared build artefacts. The following store
    keys allow to express build code {{!code_wanted}desires} and
    determine the actual {{!built_code}decision}. *)

type built_code = [ `Byte | `Native | `Both ]
(** The type indicating which code is being built. *)

val wanted_code : [ built_code | `Auto ] Store.key
(** [wanted_code] indicates which code should be built, default
    determines to [`Auto]. [`Auto] indicates [`Native] should be
    used if [ocamlopt] can be found in the memo environment and
    [`Byte] otherwise. *)

val built_code : built_code Store.key
(** [build_code] is a memo key indicating the built code. By default
    determines by consulting [code_wanted]. *)

(** {1:units Build units} *)

(** Metadata keys *)
module Meta : sig

  val tag : unit B0_meta.key
  (** [tag] indicates the entity deals with OCaml code. *)

  val requires : Lib.Name.t list B0_meta.key
  (** [requires] on a build unit specifies the OCaml libraries needed to
      compile a unit. *)

  val library : Lib.Name.t B0_meta.key
  (** [library] on a build unit specifies that the unit defines
      the library. *)

  val mod_srcs : Mod.Src.t Mod.Name.Map.t  Fut.t B0_meta.key
  (** FIXME quick hack this should not be in meta. *)
end

(** OCaml build units *)
module Unit : sig

  (** {1:library_resolver Library resolver} *)

  type lib_resolver = Lib.Name.t -> Lib.t Fut.t
  (** The type for library resolvers. FIXME maybe we want a data structure
      with given operations. In particular we are interested in getting
      the domain of the resolver for error correction (or we bundle
      this into the function failure). *)

  val lib_resolver : lib_resolver Store.key
  (** [lib_resolver] is the library resolver used by OCaml build units.
      See {!default_lib_resolver} for the default. *)

  val default_lib_resolver : Store.t -> Memo.t -> lib_resolver Fut.t
  (** [default_resolver] determines the default value of {!lib_resolver}.
      This resolver does the following:
      {ol
      {- It first looks if the library name is {{!Meta.library}defined}
         by a unit  that {{!B0_build.Unit.may}may} be built. If that
         is the case it creates a library out of that build unit's.}
      {- It looks into the environment for a library via
         {!B00_ocaml.Lib_resolver} that writes its result in the
         {!B0_build.shared_build_dir} of the build.}} *)

  (** {1:unit Units} *)

  val exe :
    ?doc:string -> ?meta:B0_meta.t -> ?requires:Lib.Name.t list ->
    ?name:string -> string -> srcs:B0_srcs.t -> B0_unit.t
  (** [exe n] is a build unit for an executable named [n] (without
      the platform specific extension).
      {ul
      {- [doc] is the unit doc string.}
      {- [meta] is the initial metadata.}
      {- [requires] are the OCaml libraries required to compile the executable.}
      {- [name] is the name of the unit (defaults to [n]).}
      {- [srcs] are the executable sources. All files with extension [.ml],
         [.mli], [.c] and [.h] are considered for compiling and linking the
         executable.}} *)

  val lib :
    ?doc:string -> ?meta:B0_meta.t -> ?requires:Lib.Name.t list ->
    ?name:string -> Lib.Name.t -> srcs:B0_srcs.t -> B0_unit.t
  (** [lib n ~srcs] is a built unit for a library named [l] made of
      sources [src].
      {ul
      {- [doc] is the unit doc string.}
      {- [meta] is the initial metadata.}
      {- [requires] are the OCaml libraries required to compile the
         library.}
      {- [name] is the name of the build unit (default to [n] with
          [.] substituted by [-])}
      {- [srcs] are the library sources. extension [.ml],
         [.mli], [.c] and [.h] are considered for compiling and linking the
         executable.}} *)
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
