(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Metadata.

    Typed key-value dictionaries. These dictionaries are attached to
    various B0 definitions to specify metadata about them.

    The module defines a few {{!std}standard keys}.

    The recommended way of formatting constant dictionaries is:
    {[
open B0_kit.V000 (* Defines the ~~ operator *)
let meta =
  B0_meta.empty
  |> ~~ B0_meta.authors ["The project programmers"]
  |> ~~ B0_meta.homepage "https://example.org"
  |> ~~ B0_meta.tag B0_opam.tag
]} *)

open B0_std

(** {1:keys Keys} *)

type 'a key
(** The type for keys with lookup value of type ['a].  *)

(** Keys. *)
module Key : sig

  (** {1:typed Typed keys} *)

  val make : ?doc:string -> ?default:'a -> string -> pp_value:'a Fmt.t -> 'a key
  (** [make name ~default ~pp_value ~doc] is a new metadata key with:
      {ul
      {- [name] the name used for UI interaction. It {e should} be globally
         unique, the module automatically renames it if that happens not to
         be the case.}
      {- [default] is an optional default value for the key when undefined
         in a dictionary.}
      {- [pp_value] is used to format the key values for end-users.}
      {- [doc] is a documentation string for the key.}} *)

  val make_tag : ?doc:string -> string -> bool key
  (** [make_tag ~doc name] is a new tag key named [name]. Denote booleans that
      are false when absent (that's their default value). In effect this
      is strictly equivalent to:
      {[make ?doc name ~default:false ~pp_value:Fmt.bool]} *)

  (** {2:props Properties} *)

  val name : 'a key -> string
  (** [name k] is [k]'s name. *)

  val default : 'a key -> 'a option
  (** [default k] is a default value for the key (if any). *)

  val get_default : 'a key -> 'a
  (** [get_default k] is the default value of [k]. Raises [Invalid_argument]
      if [k] has no default. *)

  val doc : 'a key -> string
  (** [doc k] is [k]'s documentation string. *)

  (** {2:fmt Formatting} *)

  val pp_value : 'a key -> 'a Fmt.t
  (** [pp k] is [k]'s value formatter. *)

  val pp_name : 'a key Fmt.t
  (** [pp_name k] formats [k]'s name with {!pp_name_str}. *)

  (** {1:exist Existential keys} *)

  type t = V : 'a key -> t (** *)
  (** The type for existential keys. *)

  (** {2:preds Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k] and [k'] are the same key. *)

  val compare : t -> t -> int
  (** [compare k0 k1] is a total order on keys compatible with {!equal}. *)

  (** {2:formatting Formatting} *)

  val pp_name_str : string Fmt.t
  (** [pp_name_str] formats a key name. *)

  val pp : t Fmt.t
  (** [pp] formats the key name with {!pp_name_str} *)

  (** {2:lookup Lookup keys by name}

      For UI purposes a map from key names to existential keys is
      maintained by the module. *)

  val find : string -> t option
  (** [find n] is the key named [n] (if any). *)

  val get : string -> t
  (** [get n] is the key named [n]. Raises [Invalid_argument] if
      there is no such key. *)

  val get_or_suggest : string -> (t, t list) result
  (** [get_or_suggest n] is the key named [n] or or a (possibly empty)
      list of suggested values whose name could match [n]. *)

  val get_or_hint : string -> (t, string) result
  (** [get_or_hint n] is the key named [n] or an error message that
      indicates that [n] could not be found with suggested names. *)

  val list : unit -> t list

  val fold : (t -> 'a -> 'a) -> 'a -> 'a

  val get_list_or_hint :
    all_if_empty:bool -> string list -> (t list, string) result
end

(** {1:meta Metadata} *)

type t
(** The type for metadata. *)

val empty : t
(** [empty] is the empty metadata. *)

(** {1:preds Predicates} *)

val is_empty : t -> bool
(** [is_empty m] is [true] iff [m] is empty. *)

val mem : 'a key -> t -> bool
(** [mem k m] is [true] iff [m] has a binding for [k]. *)

val has_tag : bool key -> t -> bool
(** [has_tag tag m] is the value of [tag] in [m] or its default value
    (that is [false] for those tags created with {!Key.make_tag}).

    Raises [Invalid_argument] if [tag] has no default; regardless of
    whether [tag] is bound in [m]. *)

(** {1:adding_and_removing Adding and removing} *)

val tag : bool key -> t -> t
(** [tag k m] is [add k true m]. *)

val add : 'a key -> 'a -> t -> t
(** [add k v m] is [m] with [k] bound to [v]. *)

val add_some : 'a key -> 'a option -> t -> t
(** [add_some k o m] is [m] if [o] is [None] and [m] with [k]
    bound to [v] if [o] is [Some v]. *)

val add_some_or_default : 'a key -> 'a option -> t -> t
(** [add_some_or_default k o m] is [m] with [k] bound to [v] if
    [o] is [Some v] and {!Key.default}[ k] otherwise. Raises
    [Invalid_argument] if [k] has no default (regardless of [o]'s value). *)

val add_if_undef : 'a key -> 'a -> t -> t
(** [add_if_undef k v m] is [m] with [k] bound to [v] if [k]
    is unbound in [m]. *)

val override : t -> by:t -> t
(** [override m ~by] overrides bindings of [m] by those [by]. The
    result has all the bindings of [by] and those of [m] which are not
    in [by]. *)

val remove : 'a key -> t -> t
(** [remove k m] is [m] without a binding for [k]. *)

(** {1:lookup Lookup} *)

val find : 'a key -> t -> 'a option
(** [find k m] is the binding of [k] in [m] (if any). *)

val find_or_default : 'a key -> t -> 'a
(** [find_or_default k m] is the binding of [k] in [m] or the default
    value of [k]. Raises [Invalid_argument] if [k] has no default; regardless
    of whether [k] is bound in [m]. *)

val get : 'a key -> t -> 'a
(** [get k m] is the binding of [k] in [m]. Raises [Invalid_argument]
    if there is no such binding. This does not lookup the default of
    [k]. *)

(** {1:bindings Bindings} *)

type binding = B : 'a key * 'a -> binding (** *)
(** The type for metadata bindings, a key and its value. *)

val find_binding : 'a key -> t -> binding option
(** [find_binding k m] is the binding for [k] in [m] (if any). *)

val find_binding_by_name : string -> t -> binding option
(** [find_binding_by_name n m] is the binding named [n] in [m] (if any). *)

val get_binding : 'a key -> t -> binding
(** [find_binding k m] is the binding for [k] in [m]. Raises [Invalid_argument]
    if there is no such binding. *)

val get_binding_by_name : string -> t -> binding
(** [get_binding_by_name n m] is the binding named [n] in [m]. Raises
    [Invalid_argument] if there is no such binding. *)

val pp_binding : binding Fmt.t
(** [pp_binding] formats a binding using {!B0_std.Fmt.field} and the
    key's {{!Key.pp_value}value formatter}. *)

(** {1:traverse Traversing} *)

val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f m acc] folds [f] over the bindings of [m] starting with [acc]. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats metadata using {!pp_bindings}. *)

val pp_non_empty : t Fmt.t
(** [pp_non_empty] is {!B0_std.Fmt.cut} followed by {!pp} if metadata is non
    empty and {!B0_std.Fmt.nop} otherwise. *)

(** {1:std Standard keys} *)

(** {2:end_user End-user information} *)

val authors : string list key
(** [authors] describes a list of persons with authorship. *)

val description_tags : string list key
(** [description_tags] describes a list of classification tags used
    for documentation. *)

val description : string key
(** [description] is a long description for the entity. *)

val homepage : string key
(** [homepage] is an URL to a project homepage. *)

val issues : string key
(** [issues] is an URL to an issue tracker. *)

type spdxid = string
(** The type for {{:https://spdx.org/licenses/}SPDX license identifiers}.
    If your license is not in the list you can use anything if
    prefixed by ["LicenseRef-"]. *)

val licenses : spdxid list key
(** [licenses] describes a list of licenses. Some processors like
    {{!B0_init}file generators} consider the first license of this list
    to be the main license of your project and use it accordingly. *)

val maintainers : string list key
(** [maintainers] describe a list of persons with maintainership. *)

val online_doc : string key
(** [online_doc] is an URL to online documentation. *)

val repo : string key
(** [repo] is an URL to a VCS repository. *)

val synopsis : string key
(** [synopsis] is a one line synopsis for an entity. *)

(** {2:tags Entity tags} *)

val bench : bool key
(** [bench] tags benchmarking entities. Defaults to [false]. *)

val build : bool key
(** [build] tags build system entities. Defaults to [false]. *)

val dev : bool key
(** [dev] tags development entities. Defaults to [false]. *)

val doc : bool key
(** [doc] tags documentation entities. Defaults to [false]. *)

val exe : bool key
(** [exe] tags executable entities. Defaults to [false]. *)

val test : bool key
(** [test] tags testing entities. Defaults to [false]. *)

val lib : bool key
(** [lib] tags library entities. Defaults to [false]. *)

val public : bool key
(** [public] indicates if an entity is public. Defaults to
    [false]. The semantics depends on the context but it usually means
    that it exports the entity in an underlying global namespace. *)
