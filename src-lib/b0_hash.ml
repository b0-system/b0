(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Murmur3 = struct
  (* Luckily, MurmurHash3_x64_128 has the same width as Digest.t, which means we
   * can be a little dirty. *)

  type t    = string
  type seed = int

  external hash_unsafe : string -> int -> int -> seed -> t =
    "caml_b0_murmurhash"
  external hash_fd : Unix.file_descr -> seed -> t =
    "caml_b0_murmurhash_fd"

  let zero = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
  let no_seed = 0

  let to_byte_string t = t
  let to_hex = Digest.to_hex

  let of_hex s = try Some (Digest.from_hex s) with Invalid_argument _ -> None

  let equal, compare = String.(equal, compare)
end

module XXH = struct
  (* XXH64 is fast, but only 64 bits wide. The probability of collision in a
   * 10000-file repo is about 2.71e-12, which is probably lower than hardware
   * faults. *)

  type t    = int64
  type seed = int64

  external hash_unsafe : string -> int -> int -> seed -> t =
    "caml_b0_xxhash"
  external hash_fd : Unix.file_descr -> seed -> t =
    "caml_b0_xxhash_fd"
  external set_64u : Bytes.t -> int -> int64 -> unit =
    "%caml_string_set64u"

  let zero = 0L
  let no_seed = 0L

  let to_byte_string t =
    let res = Bytes.create 8 in set_64u res 0 t; Bytes.unsafe_to_string res
  let to_hex = Format.sprintf "%016Lx"

  let of_hex s =
    try Scanf.sscanf s "%LX%!" (fun x -> Some x) with Scanf.Scan_failure _ -> None

  let equal, compare = Int64.(equal, compare)
end

include XXH

let string s = hash_unsafe s 0 (String.length s) no_seed

let raw_file ps =
  let fd = Unix.(openfile ps [O_RDONLY] 0) in
  match hash_fd fd no_seed with
  | exception e -> Unix.close fd; raise e
  | res -> Unix.close fd; res

let file p = raw_file (B0_fpath.to_string p)

let pp ppf h = match equal h zero with
| true -> B0_fmt.none_str ppf ()
| false -> B0_fmt.string ppf (to_hex h)

module Set = Set.Make (XXH)
type set = Set.t

module Map = Map.Make (XXH)
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
