(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** OCaml libraries. *)

open B00_std
open B00

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
module Name : sig

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

  val to_string : t -> string
  (** [to_string n] is [n] as a string. *)

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

type t
(** The type for libraries. *)

val name : t -> Name.t
(** [name l] is the library name of [l]. *)

val dir : t -> Fpath.t
(** [dir l] is the path to the library directory. Incidentally this
    is also the path to the library includes. *)

val archive : code:B00_ocaml.Cobj.code -> t -> Fpath.t
(** [archive ~code l] is the path to the library archive for code [c].
    Not checked for existence. *)

(** {1:resolver Resolvers} *)

(** Resolvers. *)
module Resolver : sig

  type lib = t
  (** See {!t} .*)

  type t
  (** The type for library resolvers. *)

  val create : Memo.t -> memo_dir:Fpath.t -> ocamlpath:Fpath.t list -> t
  (** [create m ocamlpath] is a library resolver looking for
      libraries in the OCAMLPATH [ocamlpath]. *)

  val find : t -> Name.t -> lib Memo.fiber
  (** [find r l] finds library [l] using [r]. The fiber fails
      if the library name cannot be found.

      This should look a list and sort. *)

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
