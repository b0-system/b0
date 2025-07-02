(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hash values and functions.

    The property we want from these functions is speed and collision
    resistance. Build correctness depends on the latter. *)

open B0_std

(** {1:values Hash values} *)

type t
(** The type for hash values. All hash functions use this representation.
    It is not possible to distinguish them, except for their {!length}
    which might vary, or not. *)

val nil : t
(** [nil] is the only hash value of {!length} [0]. *)

val length : t -> int
(** [length h] is the length of [h] in bytes. *)

(** {1:preds Predicate and comparisons} *)

val is_nil : t -> bool
(** [is_nil h] is [true] iff [h] is {!nil}. *)

val equal : t -> t -> bool
(** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

val compare : t -> t -> int
(** [compare h0 h1] is a total order on hashes compatible with {!equal}. *)

(** {1:converting Converting} *)

val to_binary_string : t -> string
(** [to_binary_string h] is the sequence of bytes of [h]. *)

val of_binary_string : string -> t
(** [of_binary_string s] is the sequences of bytes of [s] as a hash value. *)

val to_hex : t -> string
(** [to_hex h] is {!String.Ascii.to_hex}[ (to_bytes h)]. *)

val of_hex : string -> (t, string) result
(** [of_hex s] is
    [Result.map of_binary_string (]{!String.Ascii.of_hex}[ s)]. *)

val of_hex' : string -> (t, int) result
(** [of_hex s] is
    [Result.map of_binary_string (]{!String.Ascii.of_hex'}[ s)]. *)

val pp : t Fmt.t
(** [pp] formats using {!to_hex} or, if the hash is {!nil},
    formats ["nil"]. *)

(** {1:funs Hash functions} *)

(** The type for hash functions. *)
module type T = sig

  (** {1:hash Hash function} *)

  val id : string
  (** [id] is an US-ASCII string identifying the hash function. *)

  val length : int
  (** [length] is the byte length of hashes produced by the function. *)

  val string : string -> t
  (** [string s] is the hash of [s]. *)

  val fd : Unix.file_descr -> t
  (** [fd fd] [mmap(2)]s and hashes the object pointed by [fd].
      @raise Sys_error if [mmap] fails. *)

  val file : Fpath.t -> (t, string) result
  (** [file f] is the hash of file [f]. *)
end

module Xxh3_64 : T
(** [Xxh3_64] is the {{:http://cyan4973.github.io/xxHash/}xxHash3 64-bit}
    hash. *)

module Xxh3_128 : T
(** [Xxh3_128] is the {{:http://cyan4973.github.io/xxHash/}xxHash3 128-bit}
    hash. *)

val funs : unit -> (module T) list
(** [funs ()] is the list of available hash functions. *)

val add_fun : (module T) -> unit
(** [add_fun m] adds [m] to the list returned by [funs]. *)

val get_fun : string -> ((module T), string) result
(** [get_fun id] is the hash function with identifier [id] or an
    error message. *)
