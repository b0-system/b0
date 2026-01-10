(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A minimal fINI parser. *)

type atom = string
(** The type for atoms. *)

type value = atom list
(** The type for values. *)

type name = atom
(** The type for names. Non-empty atoms. *)

type qname = name list
(** The type for qualified key names. *)

val pp_qname : Format.formatter -> qname -> unit
(** [pp_qname] formats qualified key names for inspection. *)

type doc
(** The type for fINI documents. *)

val empty : doc
(** [empty] is the empty document. *)

val of_string : ?file:string -> string -> (doc, string) result
(** [of_string s] is a fINI document from [s]. [file] is used to
    report errors it defaults to ["-"]. *)

val find : qname -> doc -> value option
(** [find q doc] is the value bound to [q] in [doc] (if any). *)

val find_section : qname -> doc -> doc option
(** [find_section sec doc] is the section [sec] expressed as a
    document. Bindings of section [sec] become toplevel bindings and
    subsections of [sec] become toplevel section. If [sec] also maps
    a key in [d], it is mapped by the empty qname. [None] is returned
    if there is no section [sec] in [doc]. See also {!get_section}. *)

val get_section : qname -> doc -> doc
(** [get_section] is like {!find_section} but returns {!empty}
    if the section name does not exist in the document. *)

val top_sections : doc -> name list
(** [top_sections doc] is the list of top-level section names in [doc] *)

val fold : (qname -> value -> 'acc -> 'acc) -> doc -> 'acc -> 'acc
(** [fold f doc acc] folds [f] over the bindings of [doc] starting
    with [acc]. *)
