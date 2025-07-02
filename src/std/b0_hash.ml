(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* Hash values *)

type t = string
let nil = ""
let length = String.length

(* Predicates and comparisons *)

let equal = String.equal
let compare = String.compare
let is_nil h = equal nil h

(* Converting *)

let to_binary_string h = h
let of_binary_string h = h
let to_hex = String.Ascii.to_hex
let of_hex' = String.Ascii.of_hex'
let of_hex = String.Ascii.of_hex
let pp ppf h = Fmt.string ppf (if is_nil h then "nil" else to_hex h)

(* Hash functions *)

module type T = sig
  val id : string
  val length : int
  val string : string -> t
  val fd : Unix.file_descr -> t
  val file : Fpath.t -> (t, string) result
end

let rec file_with_hash_fd hash_fd f =
  let err f e = Fmt.error "%a: %s" Fpath.pp f e in
  match Unix.openfile (Fpath.to_string f) Unix.[O_RDONLY] 0 with
  | exception Unix.Unix_error (Unix.EINTR, _, _) ->
      file_with_hash_fd hash_fd f
  | exception Unix.Unix_error (e, _, _) -> err f (Unix.error_message e)
  | fd ->
      match hash_fd fd with
      | exception Sys_error e ->
          (try Unix.close fd with Unix.Unix_error (_, _, _) -> ()); err f e
      | hash ->
          match Unix.close fd with
          | () -> Ok hash
          | exception Unix.Unix_error (e, _, _)  ->
              err f (Unix.error_message e)

external set_64u : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"
external swap_64 : int64 -> int64 = "%bswap_int64"
external noswap : int64 -> int64 = "%identity"
let layout = if Sys.big_endian then noswap else swap_64
let u64_to_bytes t =
  let b = Bytes.create 8 in
  set_64u b 0 (layout t); Bytes.unsafe_to_string b

module Xxh3_64 = struct
  type t = int64
  type seed = int64
  external hash_fd : Unix.file_descr -> seed -> t = "ocaml_b0_xxhash3_64_fd"
  external hash_unsafe : string -> int -> int -> seed -> t =
    "ocaml_b0_xxhash3_64"

  let id = "xxh3-64"
  let seed = 0L
  let length = 8
  let string s = hash_unsafe s 0 (String.length s) seed |> u64_to_bytes
  let fd fd = hash_fd fd seed |> u64_to_bytes
  let file f = file_with_hash_fd fd f
end

module Xxh3_128 = struct
  type t = string
  type seed = int
  let no_seed = 0
  external hash_fd : Unix.file_descr -> seed -> t = "ocaml_b0_xxhash3_128_fd"
  external hash_unsafe : string -> int -> int -> seed -> t =
    "ocaml_b0_xxhash3_128"

  let id = "xxh3-128"
  let length = 16
  let string s = hash_unsafe s 0 (String.length s) no_seed
  let fd fd = hash_fd fd no_seed
  let file f = file_with_hash_fd fd f
end

let funs = ref [(module Xxh3_64 : T); (module Xxh3_128 : T)]
let add_fun m = funs := m :: !funs
let funs () = !funs
let get_fun id =
  let has_id id (module H : T) = String.equal H.id id in
  let funs = funs () in
  match List.find (has_id id) funs with
  | m -> Ok m
  | exception Not_found ->
      let kind = Fmt.any "hash" in
      let pp_id = Fmt.code in
      let ids = List.map (fun (module H : T) -> H.id) funs in
      let dict = fun yield -> List.iter yield ids in
      let hint, ids = match String.spellcheck dict id with
      | [] -> Fmt.must_be, ids
      | ids -> Fmt.did_you_mean, ids
      in
      Fmt.error "@[%a@]" (Fmt.unknown' ~kind pp_id ~hint) (id, ids)
