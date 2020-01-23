(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Metadata.

    Typed key-value dictionaries with a few {{!standard_keys}standard keys}.

    {b FIXME.} They used to be serializable, see if we don't want that again. *)

open B00_std

(** {1:meta Metadata} *)

type 'a key
(** The type for keys whose lookup value is of type ['a].  *)

(** Metadata keys *)
module Key : sig

  (** {1:typed Typed keys} *)

  val create : ?doc:string -> pp_value:'a Fmt.t -> string -> 'a key
  (** [create name ~doc ~pp_value] is a new metadata key. [pp] is used to
      format the key values. [name] is used to format the key name
      when its binding is formated; it {e should} be globally unique,
      the module automatically renames it if that happens not to be
      the case since it's not essential. *)

  val tag : ?doc:string -> string -> unit key
  (** [tag ~doc name] is new tag key. Tags can be seen as booleans
      whose presence in metadata means [true]. *)

  val name : 'a key -> string
  (** [name k] is [k]'s name. *)

  val doc : 'a key -> string
  (** [doc k] is [k]'s documentation string. *)

  val pp_value : 'a key -> 'a Fmt.t
  (** [pp k] is [k]'s value formatter. *)

  (** {1:exist Existential keys} *)

  type t = V : 'a key -> t
  (** The type for existential keys. *)

  val find : string -> t option
  (** [find n] is the key named [n] (if any). *)

  val get : string -> t
  (** [get n] is the key named [n]. Raises [Invalid_argument] if
      there is no such key. *)

  val get_or_suggest : string -> (t, t list) result
  (** [get_or_suggest n] is the key named [n] or or a (possibly empty)
      list of suggested values whose name could match [n]. *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k] and [k'] are the same key. *)

  val compare : t -> t -> int
  (** [compare k0 k1] is a total order on keys compatible with {!equal}. *)

  val pp_name_str : string Fmt.t
  (** [pp_name_str] formats a key name. *)

  val pp : t Fmt.t
  (** [pp] formats the key name with {!pp_name_str} *)
end

type t
(** The type for metadata. *)

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

val get : 'a key -> t -> 'a
(** [get k m] is the binding of [k] in [m]. Raises [Invalid_argument] if
    there is no such binding. *)

(** {1:bindings Bindings} *)

type binding = B : 'a key * 'a -> binding
(** The type for metadata bindings. *)

val find_binding : 'a key -> t -> binding option
(** [find_binding k m] is the binding for [k] in [m] (if any). *)

val get_binding : 'a key -> t -> binding
(** [find_binding k m] is the binding for [k] in [m]. Raises [Invalid_argument]
    if there is no such binding. *)

val find_binding_by_name : string -> t -> binding option
(** [find_binding_by_name n m] is the binding named [n] in [m] (if any). *)

val get_binding_by_name : string -> t -> binding
(** [get_binding_by_name n m] is the binding named [n] in [m]. Raises
    [Invalid_argument] if there is no such binding. *)

val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f m acc] folds [f] over the bindings of [m] starting with [acc]. *)

val pp_binding : binding Fmt.t
(** [pp_binding] formats a binding using {!Fmt.field} and the
    key's value print function. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats metadata using {!pp_bindings}. *)

val pp_non_empty : t Fmt.t
(** [pp_non_empty] is {!Fmt.cut} followed by {!pp} if metadata is non
    empty and {!Fmt.nop} otherwise. *)

(** {1:std Standard keys}

    In alphabetical order. *)

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
