(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Result values.

    {b All additions available} since OCaml 5.4

     @canonical B0_std.Result *)

include module type of Stdlib.Result (** @closed *)

(** {1:extract Extracting results} *)

val get_ok' : ('a, string) result -> 'a
(** [get_ok' r] is like {!get_ok} but the message of [Invalid_argument]
    is the error meesage. *)

val error_to_failure : ('a, string) result -> 'a
(** [error_to_failure r] is [failwith e] if [r] is [Error e] and [v]
    if [r] is [Ok v]. *)

val retract : ('a, 'a) result -> 'a
(** [retract r] is [v] if [r] is [Ok v] or [Error v]. *)

(** {1:let_ops Let operators} *)

(** let operators. *)
module Syntax : sig
  val ( let* ) :
    ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  (** [( let* )] is {!bind}. *)

  val ( and* ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
  (** [( and* )] is [product]. *)

  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
  (** [( let+ )] is {!map}. *)

  val ( and+ ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
  (** [( and* )] is [product]. *)
end
