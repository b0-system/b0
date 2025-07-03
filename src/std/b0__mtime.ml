(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type uint64 = int64

module Span = struct

  (* Time spans are by a nanosecond magnitude stored in an unsigned 64-bit
     integer. Allows to represent spans for ~584.5 Julian years. *)

  type t = uint64
  let zero = 0L
  let one = 1L
  let max_span = -1L

  let equal = Int64.equal
  let compare = Int64.unsigned_compare
  let is_shorter s ~than = compare s than < 0
  let is_longer s ~than = compare s than > 0

  let add = Int64.add
  let abs_diff s0 s1 =
    if compare s0 s1 < 0 then Int64.sub s1 s0 else Int64.sub s0 s1

  (* Durations *)

  let ( * ) n span = Int64.mul (Int64.of_int n) span
  let ns   =                      1L
  let us   =                  1_000L
  let ms   =              1_000_000L
  let s    =          1_000_000_000L
  let min  =         60_000_000_000L
  let hour =       3600_000_000_000L
  let day  =      86400_000_000_000L
  let year = 31_557_600_000_000_000L

  (* Conversions *)

  let to_uint64_ns s = s
  let of_uint64_ns ns = ns

  let max_float_int = 9007199254740992. (* 2^53. *)
  let int64_min_int_float = Int64.to_float Int64.min_int
  let int64_max_int_float = Int64.to_float Int64.max_int

  let of_float_ns sf =
    if sf < 0. || sf >= max_float_int || not (Float.is_finite sf)
    then None else Some (Int64.of_float sf)

  let to_float_ns s =
    if Int64.compare 0L s <= 0 then Int64.to_float s else
    int64_max_int_float +. (-. int64_min_int_float +. Int64.to_float s)

  let pp = B0__fmt.uint64_ns_span
  let pp_ns ppf s = B0__fmt.pf ppf "%Luns" s
end

(* Timestamps *)

type t = uint64

let to_uint64_ns t = t
let of_uint64_ns ns = ns
let min_stamp = 0L
let max_stamp = -1L
let pp ppf s = B0__fmt.pf ppf "%Lu" s

(* Predicates *)

let equal = Int64.equal
let compare = Int64.unsigned_compare
let is_earlier t ~than = compare t than < 0
let is_later t ~than = compare t than > 0

(* Arithmetic *)

let span t0 t1 =
  if compare t0 t1 < 0 then Int64.sub t1 t0 else Int64.sub t0 t1

let add_span t span =
  let sum = Int64.add t span in
  if compare t sum <= 0 then Some sum else None

let sub_span t span =
  if compare t span < 0 then None else Some (Int64.sub t span)
