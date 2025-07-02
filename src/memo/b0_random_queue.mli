(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Random queue *)

type 'a t
(** The type for random queues with elements of type ['a]. *)

val empty : ?rand:Random.State.t -> unit -> 'a t
(** [emtpy ~rand ()] is an empty random queue using [rand] as random
      state (defaults to {!Random.State.make_self_init}). *)

val add : 'a t -> 'a -> unit
(** [add q v] adds [v] to the queue. *)

val take : 'a t -> 'a option
(** [take q] removes and returns a random element in [q] (if any). *)

val length : 'a t -> int
(** [length q] is the number of elements in [q]. *)
