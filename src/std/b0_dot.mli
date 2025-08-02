(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Dot graph generation.

    {b Note.} No support for ports. Should be too hard to add though.

    {b References}
    {ul
    {- {{:http://www.graphviz.org/content/dot-language}Dot language
    grammar}}} *)

(** {1:seq Generation sequences} *)

type 'a seq
(** The type for sequences. *)

val empty : 'a seq
(** An empty sequence. *)

val ( ++ ) : 'a seq -> 'a seq -> 'a seq
(** [s ++ s'] is sequence [s'] concatenated to [s]. *)

(** {1:graphs Graphs} *)

type id = string
(** The type for ids, they can be any string and are escaped. *)

type st
(** The type for dot statements. *)

type att
(** The type for dot attributes. *)

type t
(** The type for dot graphs. *)

val edge : ?atts:att seq -> id -> id -> st seq
(** [edge ~atts id id'] is an edge from [id] to [id'] with attribute
    [atts] if specified. *)

val node : ?atts:att seq -> id -> st seq
(** [nod ~atts id] is a node with id [id] and attributes [atts] if
    specified. *)

val atts : [`Graph | `Node | `Edge] -> att seq -> st seq
(** [atts kind atts] are attributes [atts] for [kind]. *)

val att : string -> string -> att seq
(** [att k v] is attribute [k] with value [v]. *)

val att_html : string -> string -> att seq
(** [att_html k v] is an {{:https://graphviz.org/doc/info/shapes.html#html}
    HTML-like} label [v] for [k]. *)

val label : string -> att seq
(** [label l] is label attribute [l]. *)

val color : string -> att seq
(** [color c] is a color attribute [l]. *)

val subgraph : ?id:id -> st seq -> st seq
(** [subgraph ~id sts] is subgraph [id] (default unlabelled) with
    statements [sts]. *)

val graph : ?id:id -> ?strict:bool -> [`Graph | `Digraph] -> st seq -> t
(** [graph ~id ~strict g sts] is according to [g] a graph or digraph [id]
    (default unlabelled) with statements [sts]. If [strict] is [true]
    (defaults to [false]) multi-edges are not created. *)

(** {1:output Output} *)

val buffer_add : Buffer.t -> t -> unit
(** [buffer_add b g] adds the dot graph [g] to [b]. *)

val to_string : t -> string
(** [to_string g] is the dot graph [g] as a string. *)

val output : Out_channel.t -> t -> unit
(** [output oc g] outputs the dot graph [g] on [b]. *)
