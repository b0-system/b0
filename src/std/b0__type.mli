(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Type introspection.

    {b All additions available} since OCaml 5.1 *)

type (_, _) eq = Equal : ('a, 'a) eq (** *)
(** The type for type quality testing. *)

(** Type idenfiers. *)
module Id : sig

  (** {1:typeids Type identifiers} *)

  type 'a t
  (** The type for type identifiers for a type ['a]. *)

  val make : unit -> 'a t
  (** [make ()] is a new type identifier. *)

  val provably_equal : 'a t -> 'b t -> ('a, 'b) eq option
  (** [provably_equal id0 id1] determines if [id0] and [id1] are equal. *)

  val uid : 'a t -> int
  (** [uid id] is a runtime unique identifier for [id]. *)
end
