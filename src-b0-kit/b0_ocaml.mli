(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml B0 file support. *)

open B00_std

(** {1 Convenience} *)

val lib : string -> B00_ocaml.Lib_name.t
(** [lib] is {!B00_ocaml.Lib_name}. *)

(** {1 Metadata keys} *)

(** Metadata keys *)
module Meta : sig

  val tag : unit B0_meta.key
  (** [tag] indicates the entity deals with OCaml code. *)

  val requires : B00_ocaml.Lib_name.t list B0_meta.key
  (** [requires] on a build unit specifies the OCaml libraries needed to
      compile a unit. *)

  val library : B00_ocaml.Lib_name.t B0_meta.key
  (** [library] on a build unit specifies that the unit defines
      the library. *)
end

(** OCaml build units *)
module Unit : sig

  (** {1:unit Units} *)

  val exe :
    ?doc:string -> ?meta:B0_meta.t -> ?requires:B00_ocaml.Lib_name.t list ->
    ?name:string -> string -> srcs:B0_srcs.t -> B0_unit.t
  (** [exe n] is a build unit for an executable named [n].
      {ul
      {- [doc] is the unit doc string.}
      {- [meta] is the initial metadata.}
      {- [requires] are the OCaml libraries required to compile the executable.}
      {- [name] is the name of the unit (defaults to [n]).}
      {- [srcs] are the executable sources. All files with extension [.ml],
         [.mli], [.c] and [.h] are considered for compiling and linking the
         executable.}} *)

  val lib :
    ?doc:string -> ?meta:B0_meta.t -> ?requires:B00_ocaml.Lib_name.t list ->
    ?name:string -> B00_ocaml.Lib_name.t -> srcs:B0_srcs.t -> B0_unit.t
  (** [lib n] is a built unit for a library named [l].
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
