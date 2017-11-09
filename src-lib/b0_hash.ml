(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* We could replace this by more recent and efficient hashes like
   blake2, but this hardly seems to be a bottleneck. *)

type t = Digest.t

let string = Digest.string
let file p = Digest.file (B0_fpath.to_string p)
(*
  try Ok (Digest.file (Fpath.to_string p)) with
  | Sys_error e -> R.error_msgf "%a: %s" Fpath.pp p e
*)

let zero = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

let raw_file = Digest.file
let to_byte_string h = h
let to_hex = Digest.to_hex
let of_hex s = try Some (Digest.from_hex s) with Invalid_argument _ -> None
let equal = Digest.equal
let compare = Digest.compare

let pp ppf h = match String.equal h zero with
| true -> B0_fmt.none_str ppf ()
| false -> B0_fmt.string ppf (to_hex h)

module Set = Set.Make (Digest)
type set = Set.t

module Map = Map.Make (Digest)
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
