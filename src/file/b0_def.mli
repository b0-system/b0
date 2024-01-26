(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** B0 definitions.

    B0 definitions are uniquely named OCaml values of different types
    used for describing software construction. The value names are
    used for end-user interaction.

    B0 definitions occur in various B0 files and libraries during the
    module initialisation phase of the program consuming the
    definitions. After the module initialisation phase no new
    definition is allowed.

    This module handle the management of these named values in B0
    files and libraries. It provides the infrastructure to track the
    location of their definition, properly {{!page-manual.scope}scope}
    the names, check their unicity in the scope, index them and make
    sure they cannot be defined after the module initialisation phase
    of the program. *)

(** {1:def Definitions } *)

open B0_std

type t
(** The type for definition names and their scoping information. *)

type def = t
(** See {!t}. *)

val scope : t -> B0_scope.t
(** [scope d] is the scope in which [d] is defined. *)

val file : t -> Fpath.t option
(** [file d] is the absolute file path in which [d] is defined, if
    defined in a file. *)

val scope_dir : t -> Fpath.t option
(** [scope_dir] is the parent of [file d]. *)

val name : t -> string
(** [name d] is the qualified name of [d]. *)

val doc : t -> string
(** [doc d] is a one-line documentation string for [d]. *)

val meta : t -> B0_meta.t
(** [meta] is the metadata associated to the definition. *)

(** {1:def_value Defining values} *)

(** The type for values to be named.

    This just indicates an identifier for the kind of names, how to
    retreive the {!VALUE.def} value in a value and how to format the names. *)
module type VALUE = sig

  (** {1:value Values} *)

  type t
  (** The type of defined values. *)

  val def_kind : string
  (** [def_kind] is the kind of defined value, uncapitalized. *)

  val def : t -> def
  (** [def v] is [v]'s definition. *)

  val pp_name_str : string Fmt.t
  (** [pp_name_str] pretty prints value names. *)
end

(** The type of named values.

    The module guarantees name unicity and provides indexing
    and lookup of the named values. *)
module type S = sig

  (** {1:value Named values} *)

  val mangle_basename : string -> string
  (** [mangle_basename s] is a basename that can be used with {!define}. *)

  type t
  (** The type of defined values. *)

  val define : ?doc:string -> ?meta:B0_meta.t -> string -> def
  (** [define ~doc ~meta n] defines name [n] in the current scope with
      documentation string [doc] (defaults to ["undocumented"])e
      and metadata [meta] (defaults to {!B0_meta.empty}).
      Defining a duplicate value in a scope raises an exception. *)

  val def_kind : string
  (** [def_kind] is the kind of defined value. *)

  val def : t -> def
  (** [def v] is the definition of value [v]. *)

  val name : t -> string
  (** [name v] is [v]'s name. Note that this name changes depending
      on how the definition is scoped. *)

  val basename : t -> string
  (** [basename v] is [v]'s name without the scope. *)

  val doc : t -> string
  (** [doc v] is [v]'s documentation string. *)

  val equal : t -> t -> bool
  (** [equal v0 v1] is [true] iff [v0] and [v1] have the same name. *)

  val compare : t -> t -> int
  (** [compare v0 v1] sorts [v0] and [v0] in lexicographical order. *)

  (** {1:metadata Metadata} *)

  val meta : t -> B0_meta.t
  (** [meta v] is [v]'s metadata. *)

  val mem_meta : 'a B0_meta.key -> t -> bool
  (** [mem_meta k v] is [B0_meta.mem k (B0_def.meta v)]. *)

  val has_tag : bool B0_meta.key -> t -> bool
  (** [has_tag k v] is [B0_meta.has_tag k (B0_def.meta v)]. *)

  val find_meta : 'a B0_meta.key -> t -> 'a option
  (** [find_meta k v] is [B0_meta.find k (B0_def.meta v)]. *)

  val find_or_default_meta : 'a B0_meta.key -> t -> 'a
  (** [find_or_default_meta k u] is
      [B0_meta.find_or_default k (B0_unit.meta u)]. *)

  val get_meta : 'a B0_meta.key -> t -> ('a, string) result
  (** [get_meta m k u] is [Ok v] if {!find_meta}[ k u] is [Some v] and
      a final user friendly error message if [None]. *)

  (** {1:add_lookup Add & Lookup} *)

  val add : t -> unit
  (** [add v] adds the value [v] to the list of defined values. *)

  val fold : (t -> 'a -> 'a) -> 'a -> 'a
  (** [fold f acc] folds over the list of defined values. *)

  val list : unit -> t list
  (** [list ()] is the list of defined values. *)

  val find : string -> t option
  (** [find name] is the value named [name] (if any). *)

  val get : string -> t
  (** [get name] looks up the value named [name] and errors the B0 file
      if there no such [name]. *)

  val get_or_suggest : string -> (t, t list) result
  (** [get_or_suggest name] is the value named [name] or a (possibly empty)
      list of suggested values whose name could match [name]. *)

  val get_or_hint : string -> (t, string) result
  (** [get_or_hint name] is the value named [name] or an error message
      that indicates that [name] could not be found with suggested
      names. *)

  val get_list_or_hint :
    all_if_empty:bool -> string list -> (t list, string) result
  (** [get_list_or_hint ~all_if_empty names] are the value named after
      [names] or an error that indicates the names that could not be
      found with suggested names. If [all_if_empty] is [true] an empty
      [ns] returns [list ()] sorted by name. *)

  (** {1:scope Scope} *)

  val scope_path : t -> string list
  (** [scope_path v] are the scopes in which [v] is defined
      starting from the root. If [v] is defined in the root scope this
      is [[]], if [v] is defined in a library scope [lib] this is
      [[[""]; lib]] *)

  val in_root_scope : t -> bool
  (** [in_root_scope v] is [true] iff [v] is in the root scope. *)

  val in_current_scope : t -> bool
  (** [in_current_scope v] is [true] iff [v] is in the current scope. *)

  val scope_dir : t -> Fpath.t option
  (** [scope_dir v] is the scope directory in which [v] is defined. *)

  val scope_dir' : t -> (Fpath.t, string) result
  (** [scope_dir' v] is like {!scope_dir} but errors with an
      end-user message if [None]. *)

  val in_scope_dir : t -> Fpath.t -> Fpath.t option
  (** [in_scope_dir v path] makes the path [path] absolute with respect
      to the scope directory in which [v] is defined. This is [None]
      if [v]'s scope has no directory (e.g. on library scopes). *)

  val in_scope_dir' : t -> Fpath.t -> (Fpath.t, string) result
  (** [in_scope_dir'] is like {!in_scope_dir} but errors with an end-user
      message if [None]. *)

  (** {1:fmt Formatters} *)

  val pp_name_str : string Fmt.t
  (** [pp_name_str v] pretty prints value names. *)

  val pp_name : t Fmt.t
  (** [pp_name v] formats [v]'s name. *)

  val pp_doc : t Fmt.t
  (** [pp_doc] formats [v]'s doc string. *)

  val pp_synopsis : t Fmt.t
  (** [pp_synopsis] formats [v]'s name and its doc string. *)

  val pp : t Fmt.t
  (** [pp] formats [v] 's name, its doc string and its metadata
      dictionary. *)

  (** {1:set_map Value set and maps} *)

  (** Value sets. *)
  module Set : Set.S with type elt = t

  (** Value maps. *)
  module Map : Map.S with type key = t
end

(** [Make (V)] names the values of [V]. *)
module Make (V : VALUE) : S with type t = V.t

type value = V : (module S with type t = 'a) * 'a -> value
