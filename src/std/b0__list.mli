(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Lists. *)

include module type of List (** @closed *)

(** {1:comparing Comparing and sorting} *)

val distinct : ('a -> 'a -> int) -> 'a list -> 'a list
(** [distinct cmp l] are the distinct elements of [l] according to
    [cmp]. The first occurence of an element in [l] is kept and their
    order in [l] is preserved. *)

val classify :
  ?cmp_elts:('a -> 'a -> int) -> ?cmp_classes:('b -> 'b -> int) ->
  classes:('a -> 'b list) -> 'a list -> ('b * 'a list) list
(** [classify ~cmp_elts ~cmp_classes ~classes els] bins elements [els]
    into classes as determined by [classes]. [cmp_elts] is used to
    compare elements and [cmp_classes] to compare classes, both
    default to {!compare}. *)

(** {1:result Interaction with result} *)

val fold_stop_on_error :
  ('a -> 'b -> ('b, 'e) result) -> 'a list -> 'b -> ('b, 'e) result
(** [fold_stop_on_error f l acc] folds [f] on the elements of
    [l] starting with [acc] and stops at the first error. *)

val iter_stop_on_error :
  ('a -> (unit, 'e) result) -> 'a list -> (unit, 'e) result
(** [iter_stop_on_error f acc] applies [f] to the elements of
    [l] and stops at the first error. *)

val iter_iter_on_error :
  error:((unit, 'e) result -> unit) -> ('a -> (unit, 'e) result) -> 'a list ->
  unit
(** [iter_iter_on_error ~error f] applies [f] to the elements of [l]
    starting with [acc] and invoked [error] on those that do. *)
