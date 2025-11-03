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
(** The type for names. Non empty atoms. *)

type qname = name list
(** The type for qualified names. *)

type doc
(** The type for fINI documents. *)

val of_string : ?file:string -> string -> (doc, string) result
(** [of_string s] is a fINI document from [s]. [file] is used to
      report errors it defaults to ["-"]. *)

val find : qname -> doc -> value option
(** [find q doc] is the value bound to [q] in [doc] (if any). *)

val to_assoc : doc -> (qname * value) list
(** [to_assoc doc] is [doc]'s map as an association list of qualified
      names. *)

type nested_doc =
| Value of value
| Bindings of (string * nested_doc) list (** *)
(** A nested representation of documents. In the binding case the
    string is a {!name} or the empty string, see Appendix B. of the
    specification. *)

val to_nested_doc : doc -> nested_doc
(** [to_nested doc] is the nested representation of [doc]. *)
