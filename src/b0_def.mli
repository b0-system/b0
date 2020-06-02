(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

open B00_std

(** Scopes are used to track and scope B0 definitions created
    by libraries and B0 files. *)
module Scope : sig

  type t
  (** The type for scopes. *)

  (** {1:library Library scopes} *)

  val lib : string -> unit
  (** [lib l] sets up a scope for library [l]. Must be called before
      making any static definition in a library. FIXME example. *)

  (** {1:b0_file B0 file scopes}

      {b Note.} This is used by the implementation of the driver
      API, if you are fiddling with this you are likely doing
      something wrong. *)

  val root : Fpath.t -> unit
  (** [root file] initializes B0 file scoping for the root B0 file at
      the {e absolute} file path [file].

      This installs a {!Printexc.set_uncaught_exception_handler} to
      handle uncaught and {!Duplicate}. If that happens the error is
      logged and the program {!exit}s with
      {!B0_driver.Exit.b0_file_error}. *)

  val open' : string -> Fpath.t -> unit
  (** [open' name] opens scope [name] to add the definitions of the {e
      absolute} file path [file].

      {b Warning.} Scope unicity is not checked by the module this is
      expected to be handled by the client.*)

  val close : unit -> unit
  (** [close ()] closes the last {!open'}ed scope. *)

  (** {2:sealing Definition sealing} *)

  val seal : unit -> unit
  (** [seal ()] prevents further definitions from being made. This function
      is called at the end of the root B0 file. *)

  exception After_seal of string
  (** Exception thrown if a definition is made after {!seal} was
      invoked. The argument is an error message to print. The
      backtrace should point to the location of the illegal
      definition. *)
end

type t
(** The type for definition names and their scoping information. *)

type def = t
(** See {!t}. *)

val scope : t -> Scope.t
(** [scope d] is the scope in which [d] is defined. *)

val file : t -> Fpath.t option
(** [file d] is the absolute file path in which [d] is defined, if
    defined in a file. *)

val dir : t -> Fpath.t option
(** [dir] is the parent of [file d]. *)

val name : t -> string
(** [name d] is the qualified name of [d]. *)

val doc : t -> string
(** [doc d] is a one-line documentation string for [d]. *)

val meta : t -> B0_meta.t
(** [meta] is the metadata associated to the definition. *)

exception Err of string
(** Exception thrown if a definition error occurs. This can be due to
    {ul
    {- Duplicate name.}
    {- Malformed name.}}
    The argument is an error message to print as is. The
    backtrace should point to the redefinition (it is unfortunately
    difficult to keep track of the previous definition). *)

(** {1:def_value Defining values} *)

(** The type for values to be named.

    This just indicates an identifier for the kind of names, how to
    retreive the {!def} value in a value and how to format the names. *)
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

  type t
  (** The type of defined values. *)

  val define : ?doc:string -> ?meta:B0_meta.t -> string -> def
  (** [define ~doc ~meta n] defines name [n] in the current scope with
      documentation string [doc] (defaults to ["undocumented"])e
      and metadata [meta] (defaults to {!B0_meta.empty}).
      Defining a duplicate value in a scope raises {!Duplicate}. *)

  val def_kind : string
  (** [def_kind] is the kind of defined value. *)

  val def : t -> def
  (** [def v] is the definition of value [v]. *)

  val name : t -> string
  (** [name v] is [v]'s name. *)

  val doc : t -> string
  (** [doc v] is [v]'s documentation string. *)

  val equal : t -> t -> bool
  (** [equal v0 v1] is [true] iff [v0] and [v1] have the same name. *)

  val compare : t -> t -> int
  (** [compare v0 v1] sorts [v0] and [v0] in lexicographical order. *)


  (** {1:metadata Metadata} *)

  val meta : t -> B0_meta.t
  (** [meta v] is [v]'s metadata. *)

  val has_meta : 'a B0_meta.key -> t -> bool
  (** [has_meta k u] is [B0_meta.mem k (B0_unit.meta u)]. *)

  val find_meta : 'a B0_meta.key -> t -> 'a option
  (** [find_meta k u] is [B0_meta.find k (B0_unit.meta u)]. *)

  val get_meta : 'a B0_meta.key -> t -> ('a, string) result
  (** [get_meta m k u] is [Ok v] if {!find_meta}[ k u] is [Some v] and
      a final user friendly error message if [None]. *)

  (** {1:add_lookup Add & Lookup} *)

  val add : t -> unit
  (** [add v] adds the value [v] to the list of defined values. *)

  val list : unit -> t list
  (** [list ()] is the list of units. FIXME scope that. *)

  val find : string -> t option
  (** [find n] is the value named [n] (if any). *)

  val get : string -> t
  (** [get n] looks up the value named [n] and errors the B0 file
      if there no such [n]. *)

  val get_or_suggest : string -> (t, t list) result
  (** [get_or_suggest n] is the value named [n] or a (possibly empty)
      list of suggested values whose name could match [n]. *)

  val get_list : string list -> (t list, string) result
  (** [get_list ns] are the value named [ns] or an error that indicates
      the names that could not be found with suggested names. *)

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
