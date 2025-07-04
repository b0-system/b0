(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Standard needs for b0 programs.

    Open this module to use it. It redefines a few standard
    modules and introduces a few new ones. *)

module Cmd = B0__cmd
module Fmt = B0__fmt
module Fpath = B0__fpath
module Log = B0__log
module Mtime = B0__mtime
module Os = B0__os

(** {1:stdlib_extensions [Stdlib] extensions} *)

module Char = B0__char
module List = B0__list
module Result = B0__result
module String = B0__string
module Type = B0__type

(** {1:concurrency Concurrency} *)

(** Future values.

    A future is an undetermined value that becomes determined at an an
    arbitrary point in the future. The future acts as a placeholder
    for the value while it is undetermined. *)
module Fut : sig

  (** {1:fut Future values} *)

  type 'a t
  (** The type for futures with values of type ['a]. *)

  val make : unit -> 'a t * ('a -> unit)
  (** [make ()] is [(f, set)] with [f] the future value and [set]
      the function to [set] it. The latter can be called only once,
      [Invalid_argument] is raised otherwise. *)

  val await : 'a t -> ('a -> unit) -> unit
  (** [await f k] waits for [f] to be determined and continues with [k v]
      with [v] the value of the future. If the future never determines
      [k] is not invoked. [k] must not raise. *)

  val value : 'a t -> 'a option
  (** [value f] is [f]'s value, if any. *)

  val sync : 'a t -> 'a
  (** [sync f] waits for [f] to determine. {b Warning.} This is relaxed busy
      waiting. *)

  val return : 'a -> 'a t
  (** [return v] is a future that determines [v]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn f] is [return (fn v)] with [v] the value of [f]. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind f fn] is the future [fn v] with [v] the value of [f]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair f0 f1] determines with the value of [f0] and [f1]. *)

  val of_list : 'a t list -> 'a list t
  (** [of_list fs] determines with the values of all [fs], in the same order. *)

  (** Future syntax. *)
  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [let*] is {!bind}. *)

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    (** [and*] is {!pair}. *)
  end
end

(** Blocking values.

    {b Note.} In direct style the {!Fut.t} values would go away.
    For now be bundled lazy blocking values in the same structure. *)
module Bval : sig

  type 'a setter
  (** The type for setting blocking value. *)

  type 'a t
  (** The type for immutable blocking values. *)

  val make : unit -> 'a t * 'a setter
  (** [make ()] is a blocking value and a setter to set it. *)

  val of_val : 'a -> 'a t
  (** [of_val v] is a (non-)blocking value holding [v]. *)

  val of_lazy_fun : (unit -> 'a) -> 'a t
  (** [of_lazy_fun f] is a blocking value that runs [f]
      iff {!get} or {!poll} is called on the value.

      {b XXX.} Something should be said about the context in
      which f runs.  *)

  val of_setter : 'a setter -> 'a t
  (** [of_setter s] is the blocking value of [s]. *)

  val is_lazy : 'a t -> bool
  (** [is_lazy bv] is [true] iff [bv] is a lazily triggered value. *)

  (** {1:setting Setting} *)

  val set : 'a setter -> 'a -> unit
  (** [set s v] sets the blocking value [of_setter s] to value [v].
      Raises [Invalid_argument] if [set] is already set. *)

  val try_set : 'a setter -> 'a -> bool
  (** [try_set s v] is [true] if [iv] was set to [v] and [false]
      if [iv] was already set. *)

  (** {1:getting Getting} *)

  val get : 'a t -> 'a Fut.t
  (** [get bv] is the value of [bv]. In direct style,
      this should be a blocking call. *)

  val poll : 'a t -> 'a option
  (** [poll bv] is [None] if [get bv] would block
      and [Some _] if it does not block. If [bv] was created
      with {!of_lazy_fun}, this ensure the computation gets triggered. *)

  val stir : 'a t -> unit
  (** [stir bv] is [ignore (poll v)]. Useful if you know [bv] will
      be needed later and may be a {!of_lazy_fun}. *)

  (** {1:formatting Formatting} *)

  val pp : 'a Fmt.t -> 'a t Fmt.t
  (** [pp] formats blocking values. Does not block if the value is not
      set in which case "<pending>" formatted. *)
end
