(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let alpha =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let encode s =
  let rec loop len e ei s i  = match i >= len with
  | true -> Bytes.unsafe_to_string e
  | false ->
      let i0 = i and i1 = i + 1 and i2 = i + 2 in
      let b0 = Char.code s.[i0] in
      let b1 = if i1 >= len then 0 else (Char.code s.[i1]) in
      let b2 = if i2 >= len then 0 else (Char.code s.[i2]) in
      let u = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
      let c0 = alpha.[u lsr 18] in
      let c1 = alpha.[(u lsr 12) land 63] in
      let c2 = if i1 >= len then '=' else alpha.[(u lsr 6) land 63] in
      let c3 = if i2 >= len then '=' else alpha.[u land 63] in
      Bytes.set e (ei    ) c0;
      Bytes.set e (ei + 1) c1;
      Bytes.set e (ei + 2) c2;
      Bytes.set e (ei + 3) c3;
      loop len e (ei + 4) s (i2 + 1)
  in
  let len = String.length s in
  if len = 0 then "" else loop len (Bytes.create (((len + 2) / 3) * 4)) 0 s 0

exception Alpha_error of int (* after 4.04 can be moved as a let below *)
let decode s =
  let decode_alpha len s i = match s.[i] with
  | 'A' .. 'Z' as c -> Char.code c - 0x41
  | 'a' .. 'z' as c -> Char.code c - 0x61 + 26
  | '0' .. '9' as c -> Char.code c - 0x30 + 52
  | '+' -> 62 | '/' -> 63
  | '=' when i = len - 1 || i = len - 2 -> -1
  | _ -> raise_notrace (Alpha_error i)
  in
  let rec loop len d di s i = match i >= len with
  | true -> Bytes.unsafe_to_string d
  | false ->
      let a0 = decode_alpha len s (i    ) in
      let a1 = decode_alpha len s (i + 1) in
      let a2 = decode_alpha len s (i + 2) in
      let a3 = decode_alpha len s (i + 3) in
      let b0 = Char.unsafe_chr ((a0 lsl 2) lor (a1 lsr 4)) in
      Bytes.set d di b0;
      if a2 = -1 then begin
        if a3 = -1 then Bytes.unsafe_to_string d else
        raise_notrace (Alpha_error (i + 2))
      end else begin
        let b1 = Char.unsafe_chr (((a1 land 0xF) lsl 4) lor (a2 lsr 2)) in
        Bytes.set d (di + 1) b1;
        if a3 = -1 then Bytes.unsafe_to_string d else
        let b2 = Char.unsafe_chr (((a2 land 0x3) lsl 6) lor a3) in
        Bytes.set d (di + 2) b2;
        loop len d (di + 3) s (i + 4)
      end
  in
  let len = String.length s in
  if len = 0 then Ok "" else
  if len mod 4 <> 0 then (Error len) else
  let dlen = (len / 4) * 3 in
  let dlen = if s.[len - 1] = '=' then dlen - 1 else dlen in
  let dlen = if s.[len - 2] = '=' then dlen - 1 else dlen in
  try Ok (loop len (Bytes.create dlen) 0 s 0) with Alpha_error i -> Error i

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
