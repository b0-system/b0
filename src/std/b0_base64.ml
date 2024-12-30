(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* See https://www.rfc-editor.org/rfc/rfc4648 *)

type encoding = [ `Base64 | `Base64url ]
type padding = [ `Padded | `Unpadded ]
type error =
| Invalid_length of int
| Invalid_letter of char * int
| Non_canonical_encoding

let error_message enc err =
  let enc = match enc with `Base64url -> "base64url" | `Base64 -> "base64" in
  match err with
  | Invalid_length len ->
      Printf.sprintf "%s: Invalid input length (%d)" enc len
  | Invalid_letter (c, i) ->
      Printf.sprintf "%s: Invalid alphabet character %C at index %d" enc c i
  | Non_canonical_encoding ->
      Printf.sprintf "%s: Non-canonical encoding" enc

let alpha =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let alpha_url =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let enc enc pad s = match String.length s with
| 0 -> ""
| len ->
    let alpha = match enc with `Base64url -> alpha_url | `Base64 -> alpha in
    let pad = match pad with `Padded -> true | `Unpadded -> false in
    let elen = if pad then ((len + 2) / 3) * 4 else (len * 4 + 2) / 3 in
    let e = Bytes.create elen in
    let i = ref 0 and ei = ref 0 in
    try
      while true do
        let i0 = !i and i1 = !i + 1 and i2 = !i + 2 in
        let b0 = String.get_uint8 s i0 in
        let b1 = if i1 >= len then 0 else (String.get_uint8 s i1) in
        let b2 = if i2 >= len then 0 else (String.get_uint8 s i2) in
        let u = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
        Bytes.set e (!ei    ) (alpha.[u lsr 18]);
        Bytes.set e (!ei + 1) (alpha.[(u lsr 12) land 63]);
        if i1 >= len
        then (if pad then Bytes.set e (!ei + 2) '=' else raise_notrace Exit)
        else (Bytes.set e (!ei + 2) alpha.[(u lsr 6) land 63]);
        if i2 >= len
        then (if pad then Bytes.set e (!ei + 3) '=' else raise_notrace Exit)
        else (Bytes.set e (!ei + 3) alpha.[u land 63]);
        i := i2 + 1;
        if !i >= len then raise_notrace Exit;
        ei := !ei + 4;
      done;
      assert false
    with Exit -> Bytes.unsafe_to_string e

let dec enc pad s =
  let exception Error of error in
  let err e = raise_notrace (Error e) in
  let decoded_length ~padded len =
    let dlen = (len / 4) * 3 in
    if padded then begin
      if len mod 4 <> 0 then err (Invalid_length len) else
      let dlen = if s.[len - 1] = '=' then dlen - 1 else dlen in
      let dlen = if s.[len - 2] = '=' then dlen - 1 else dlen in
      dlen
    end else match len mod 4 with
    | 0 -> dlen
    | 1 -> err (Invalid_length len)
    | 2 -> dlen + 1
    | 3 -> dlen + 2
    | _ -> assert false
  in
  let decode_alpha url i = function
  | 'A' .. 'Z' as c -> Char.code c - 0x41
  | 'a' .. 'z' as c -> Char.code c - 0x61 + 26
  | '0' .. '9' as c -> Char.code c - 0x30 + 52
  | '+' as c -> if not url then 62 else err (Invalid_letter (c, i))
  | '/' as c -> if not url then 63 else err (Invalid_letter (c, i))
  | '-' as c -> if url then 62 else err (Invalid_letter (c, i))
  | '_' as c -> if url then 63 else err (Invalid_letter (c, i))
  | c -> err (Invalid_letter (c, i))
  in
  let len = String.length s in
  if len = 0 then Ok "" else
  try
    let url = match enc with `Base64url -> true | `Base64 -> false in
    let padded = match pad with `Padded -> true | `Unpadded -> false in
    let d = Bytes.create (decoded_length ~padded len) in
    try
      let i = ref 0 and di = ref 0 in
      while true do
        let i0 = !i and i1 = !i + 1 and i2 = !i + 2 and i3 = !i + 3 in
        let a0 = String.get s i0 in
        let a1 = String.get s i1 in
        let a2 = if i2 >= len then (* unpadded *) '=' else String.get s i2 in
        let a3 = if i3 >= len then (* unpadded *) '=' else String.get s i3 in
        let d0 = decode_alpha url i0 a0 in
        let d1 = decode_alpha url i1 a1 in
        Bytes.set_uint8 d !di ((d0 lsl 2) lor (d1 lsr 4));
        if a3 = '=' then begin
          if not ((i3 = len - 1 && padded) || i3 >= len)
          then err (Invalid_letter ('=', i3)) else
          if a2 = '=' then
            if not ((i2 = len - 2 && padded) || i2 >= len)
            then err (Invalid_letter ('=', i2)) else
            if (d1 land 0b1111) <> 0 then err Non_canonical_encoding else
            raise_notrace Exit
          else
          let d2 = decode_alpha url i2 a2 in
          if (d2 land 0b11) <> 0 then err Non_canonical_encoding else
          Bytes.set_uint8 d (!di + 1) (((d1 land 0xF) lsl 4) lor (d2 lsr 2));
          raise_notrace Exit
        end;
        let d2 = decode_alpha url i2 a2 in
        let d3 = decode_alpha url i3 a3 in
        Bytes.set_uint8 d (!di + 1) (((d1 land 0xF) lsl 4) lor (d2 lsr 2));
        Bytes.set_uint8 d (!di + 2) (((d2 land 0x3) lsl 6) lor d3);
        i := !i + 4;
        if !i >= len then raise_notrace Exit else
        di := !di + 3;
      done;
      assert false
    with
    | Exit -> Ok (Bytes.unsafe_to_string d)
  with
  | Error e -> Error e

let encode p s = enc `Base64 p s
let decode' p s = dec `Base64 p s
let decode p s = Result.map_error (error_message `Base64) (decode' p s)

let encode_base64url p s = enc `Base64url p s
let decode_base64url' p s = dec `Base64url p s
let decode_base64url p s =
  Result.map_error (error_message `Base64url) (decode_base64url' p s)
