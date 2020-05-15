(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Metadata.

    Typed key-value dictionaries. Values of this type are used with
    various B0 definitions to specify metadata.

    The module defines a few {{!std}standard keys} and a list syntax to
    write metadata literals:
{[
let meta = B0_meta.v @@ B0_meta.[
  authors, ["The project programmers"];
  homepage, "https://example.org"]
]}
    {b XXX.} They used to be serializable, see if we don't want that again. *)

open B00_std

(** {1:keys Keys} *)

type 'a key
(** The type for keys whose lookup value is of type ['a].  *)

(** Metadata keys *)
module Key : sig

  (** {1:typed Typed keys} *)

  val v : ?doc:string -> pp_value:'a Fmt.t -> string -> 'a key
  (** [v name ~doc ~pp_value] is a new metadata key with:
      {ul
      {- [name] the name used for UI interaction and to format the key name
         when its binding is formatted . The [name] {e should} be globally
         unique, the module automatically renames it if that happens not to
         be the case.}
      {- [doc] is a documentation string for the key.}
      {- [pp_value] is used to format the key values.}}  *)

  val tag : ?doc:string -> string -> unit key
  (** [tag ~doc name] is new tag key. Tags denote booleans, presence in
      metadata means [true], absence means [false]. *)

  val name : 'a key -> string
  (** [name k] is [k]'s name. *)

  val doc : 'a key -> string
  (** [doc k] is [k]'s documentation string. *)

  val pp_value : 'a key -> 'a Fmt.t
  (** [pp k] is [k]'s value formatter. *)

  val pp_name : 'a key Fmt.t
  (** [pp_name k] formats [k]'s name with {!pp_name_str}. *)

  (** {1:exist Existential keys} *)

  type t = V : 'a key -> t (** *)
  (** The type for existential keys. *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k] and [k'] are the same key. *)

  val compare : t -> t -> int
  (** [compare k0 k1] is a total order on keys compatible with {!equal}. *)

  val pp_name_str : string Fmt.t
  (** [pp_name_str] formats a key name. *)

  val pp : t Fmt.t
  (** [pp] formats the key name with {!pp_name_str} *)

  (** {1:lookup Lookup keys by name}

      For UI purposes a map from key names to existential keys
      is maintained by the module. *)

  val find : string -> t option
  (** [find n] is the key named [n] (if any). *)

  val get : string -> t
  (** [get n] is the key named [n]. Raises [Invalid_argument] if
      there is no such key. *)

  val get_or_suggest : string -> (t, t list) result
  (** [get_or_suggest n] is the key named [n] or or a (possibly empty)
      list of suggested values whose name could match [n]. *)
end

(** {1:bind Bindings} *)

type binding = B : 'a key * 'a -> binding
(** The type for metadata bindings. *)

val pp_binding : binding Fmt.t
(** [pp_binding] formats a binding using {!B00_std.Fmt.field} and the
    key's value print function. *)

type bindings =
| [] : bindings
| ( :: ) : ('a key * 'a) * bindings -> bindings (** *)
(** The type for sugared lists of bindings. Just because we can. *)

(** {1:meta Metadata} *)

type t
(** The type for metadata. *)

val v : bindings -> t
(** [v bs] is metadata with bindings [vs]. If a key is defined
    more than once in [bs] the last definition in the list takes over. *)

val empty : t
(** [empty] is the empty metadata. *)

val is_empty : t -> bool
(** [is_empty m] is [true] iff [m] is empty. *)

val mem : 'a key -> t -> bool
(** [mem k m] is [true] iff [m] has a binding for [k]. *)

val add : 'a key -> 'a -> t -> t
(** [add k v m] is [m] with [k] bound to [v]. *)

val tag : unit key -> t -> t
(** [tag k m] is [add k () m]. *)

val rem : 'a key -> t -> t
(** [rem k m] is [m] wihtout a binding for [k]. *)

val find : 'a key -> t -> 'a option
(** [find k m] is the binding of [k] in [m] (if any). *)

val find_binding : 'a key -> t -> binding option
(** [find_binding k m] is the binding for [k] in [m] (if any). *)

val find_binding_by_name : string -> t -> binding option
(** [find_binding_by_name n m] is the binding named [n] in [m] (if any). *)

val get : 'a key -> t -> 'a
(** [get k m] is the binding of [k] in [m]. Raises [Invalid_argument] if
    there is no such binding. *)

val get_binding : 'a key -> t -> binding
(** [find_binding k m] is the binding for [k] in [m]. Raises [Invalid_argument]
    if there is no such binding. *)

val get_binding_by_name : string -> t -> binding
(** [get_binding_by_name n m] is the binding named [n] in [m]. Raises
    [Invalid_argument] if there is no such binding. *)

val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f m acc] folds [f] over the bindings of [m] starting with [acc]. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats metadata using {!pp_bindings}. *)

val pp_non_empty : t Fmt.t
(** [pp_non_empty] is {!Fmt.cut} followed by {!pp} if metadata is non
    empty and {!Fmt.nop} otherwise. *)

(** {1:std Standard keys} *)

(** {2:end_user End-user information} *)

val authors : string list key
(** [authors] describes a list of persons with authorship. *)

val doc_tags : string list key
(** [doc_tags] describes a list of documentation classification tags. *)

val homepage : string key
(** [issues] is an URI to an issue tracker. *)

val issues : string key
(** [issues] is an URI to an issue tracker. *)

val licenses : string list key
(** [licenses] describes a list of licenses. Each license {e should}
    be a {{:https://spdx.org/licenses/}SPDX license identifier}. *)

val maintainers : string list key
(** [maintainers] describe a list of persons with maintainership. *)

val online_doc : string key
(** [online_doc] is an URI to online documentation. *)

val repo : string key
(** [repo] is an URI to a VCS system. *)

(** {2:entity Entity tags} *)

val bench : unit key
(** [bench] tags benchmarking entities. *)

val build : unit key
(** [build] tags build system entities. *)

val dev : unit key
(** [dev] tags development entities. *)

val doc : unit key
(** [doc] tags documentation entities. *)

val exe : unit key
(** [exe] tags executable entities. *)

val test : unit key
(** [test] tags testing entities. *)

val lib : unit key
(** [lib] tags library entities. *)

(** {2:entity Entity properties} *)

val exe_name : string key
(** [exe_name] is an executable name without the platform specific
    executable extension. *)

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
