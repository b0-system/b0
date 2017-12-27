(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Murmur3 = struct
  (* Luckily, MurmurHash3_x64_128 has the same width as Digest.t,
     which means we can be a little dirty. *)

  type t = string
  type seed = int

  external hash_fd : Unix.file_descr -> seed -> t = "ocaml_b0_murmurhash_fd"
  external hash_unsafe : string -> int -> int -> seed -> t =
    "ocaml_b0_murmurhash"

  let name = "murmur3"
  let zero = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
  let no_seed = 0
  let to_bytes t = t
  let to_hex = Digest.to_hex
  let of_hex s = try Some (Digest.from_hex s) with Invalid_argument _ -> None
  let equal = String.equal
  let compare = String.compare
end

module Xxash = struct
  (* XXH64 is fast, but only 64 bits wide. The probability of
     collision in a 10000-file repo is about 2.71e-12, which is
     probably lower than hardware faults. *)

  type t = int64
  type seed = int64

  external hash_fd : Unix.file_descr -> seed -> t = "ocaml_b0_xxhash_fd"
  external hash_unsafe : string -> int -> int -> seed -> t = "ocaml_b0_xxhash"
  external set_64u : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"

  let name = "xxhash"
  let zero = 0L
  let no_seed = 0L
  let to_bytes t =
    let b = Bytes.create 8 in
    set_64u b 0 t; Bytes.unsafe_to_string b

  let to_hex = Printf.sprintf "%016Lx"
  let of_hex s = match String.length s = 16 with
  | false -> None
  | true ->
      try
        let rec loop h i s = match i > 15 with
        | true -> Some h
        | false ->
            let d = Int64.of_int @@ match s.[i] with
            | '0' .. '9' as c -> Char.code c - 0x30
            | 'A' .. 'F' as c -> Char.code c - 0x41 + 10
            | 'a' .. 'f' as c -> Char.code c - 0x61 + 10
            | _ -> raise_notrace Exit
            in
            loop (Int64.(add (shift_left h 4) d)) (i + 1) s
        in
        loop 0L 0 s
      with
      | Exit -> None

  let equal = Int64.equal
  let compare = Int64.compare
end

module type S = sig
  type t
  type seed
  val no_seed : seed
  val name : string
  val zero : t
  val to_bytes : t -> string
  val to_hex : t -> string
  val of_hex : string -> t option
  val hash_fd : Unix.file_descr -> seed -> t
  val hash_unsafe : string -> int -> int -> seed -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

(* Hash values *)

(* NOTE. The following is a bit dirty we could store the impl in a ref and
   expose toggling at the API level. Another alternative would be to
   pass the module to the build as a first class value (though we then
   need to construct maps and sets there. *)

let murmur3 = (module Murmur3 : S)
let xxhash = (module Xxash : S)

let impl = match Unix.getenv "B0_HASH" with
| "murmur3" -> murmur3
| "xxhash" | _ (* Can't log warning at that point *) -> xxhash
| exception Not_found -> xxhash

module H = (val impl)
include H

let pp ppf h = match equal h zero with
| true -> B0_fmt.none_str ppf ()
| false -> B0_fmt.string ppf (to_hex h)

(* Hashing *)

let string s = H.hash_unsafe s 0 (String.length s) H.no_seed
let file p =
  let p = B0_fpath.to_string p in
  let fd = Unix.(openfile p [O_RDONLY] 0) in
  match H.hash_fd fd H.no_seed with
  | exception e -> Unix.close fd; raise e (* FIXME *)
  | res -> Unix.close fd; res

(* Sets and maps *)

module Set = Set.Make (H)
type set = Set.t

module Map = Map.Make (H)
type 'a map = 'a Map.t

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
