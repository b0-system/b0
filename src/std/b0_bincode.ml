(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* Encoders *)

type 'a enc = Buffer.t -> 'a -> unit

(* Decoders *)

type 'a dec = string -> int -> int * 'a

let err i fmt = Fmt.failwith_notrace ("%d: " ^^ fmt) i
let err_byte ~kind i b =
  err i "corrupted input, unexpected byte 0x%x for %s" b kind

let check_next ~kind s i next =
  if next <= String.length s then () else
  err i  "unexpected end of input, expected %d bytes for %s" (next - i) kind

let get_byte s i = Char.code (String.get s i) [@@ocaml.inline]

let dec_eoi s i =
  if i = String.length s then () else
  err i "expected end of input (len: %d)" (String.length s)

(* Codecs *)

type 'a t = { enc : 'a enc; dec : 'a dec }
let v enc dec = { enc; dec }
let enc c = c.enc
let dec c = c.dec

let to_string ?(buf = Buffer.create 1024) c v =
  c.enc buf v; Buffer.contents buf

let of_string ?(file = Fpath.dash) c s =
  try let i, v = c.dec s 0 in dec_eoi s i; Ok v with
  | Failure e -> Fmt.error "%a: %s" Fpath.pp file e

(* Magic numbers *)

let enc_magic magic b () = Buffer.add_string b magic
let dec_magic magic s i =
  let next = i + String.length magic in
  check_next ~kind:magic s i next;
  let magic' = String.subrange ~first:i ~last:(next - 1) s in
  if String.equal magic magic' then next, () else
  err i "magic mismatch: %S but expected %S" magic' magic

let magic magic = v (enc_magic magic) (dec_magic magic)

(* Bytes *)

let enc_byte b n =
  Buffer.add_char b (Char.chr (n land 0xFF)) [@@ocaml.inline]

let[@inline] dec_byte ~kind s i =
  let next = i + 1 in
  check_next ~kind s i next;
  let b = get_byte s i in
  next, b

let byte ~kind = v enc_byte (dec_byte ~kind)

(* unit *)

let enc_unit b () = enc_byte b 0
let dec_unit s i =
  let kind = "unit" in
  let next, b = dec_byte ~kind s i in
  match b with
  | 0 -> next, ()
  | b -> err_byte ~kind i b

let unit = v enc_unit dec_unit

(* bool *)

let enc_bool b bool = enc_byte b (if bool then 1 else 0)
let dec_bool s i =
  let kind = "bool" in
    let next, b = dec_byte ~kind s i in
  match b with
  | 0 -> next, false
  | 1 -> next, true
  | b -> err_byte ~kind i b

let bool = v enc_bool dec_bool

(* int *)

let enc_int b n =
  let w = enc_byte in
  w b n; w b (n lsr 8); w b (n lsr 16); w b (n lsr 24);
  if Sys.word_size = 32 then (w b 0x00; w b 0x00; w b 0x00; w b 0x00) else
  (w b (n lsr 32); w b (n lsr 40); w b (n lsr 48); w b (n lsr 56))

let dec_int s i =
  let r = get_byte in
  let next = i + 8 in
  check_next ~kind:"int" s i next;
  let b0 = r s (i    ) and b1 = r s (i + 1)
  and b2 = r s (i + 2) and b3 = r s (i + 3) in
  let n = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
  if Sys.word_size = 32 then next, n else
  let b4 = r s (i + 4) and b5 = r s (i + 5)
  and b6 = r s (i + 6) and b7 = r s (i + 7) in
  next, (b7 lsl 56) lor (b6 lsl 48) lor (b5 lsl 40) lor (b4 lsl 32) lor n

let int = v enc_int dec_int

(* int64 *)

let enc_int64 b i =
  (* XXX From 4.08 on use Buffer.add_int64_le *)
  let w = enc_byte in
  let i0 = Int64.to_int i in
  let i1 = Int64.to_int (Int64.shift_right_logical i 16) in
  let i2 = Int64.to_int (Int64.shift_right_logical i 32) in
  let i3 = Int64.to_int (Int64.shift_right_logical i 48) in
  let b0 = i0 and b1 = i0 lsr 8 and b2 = i1 and b3 = i1 lsr 8
  and b4 = i2 and b5 = i2 lsr 8 and b6 = i3 and b7 = i3 lsr 8 in
  w b b0; w b b1; w b b2; w b b3; w b b4; w b b5; w b b6; w b b7

external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_get_int64_ne : string -> int -> int64 = "%caml_string_get64u"

let unsafe_get_int64_le b i = match Sys.big_endian with
| true -> swap64 (unsafe_get_int64_ne b i)
| false -> unsafe_get_int64_ne b i

let dec_int64 s i =
  let next = i + 8 in
  check_next ~kind:"int64" s i next;
  next, unsafe_get_int64_le s i

let int64 = v enc_int64 dec_int64

(* string *)

let enc_string b s = enc_int b (String.length s); Buffer.add_string b s
let dec_string s i =
  let i, len = dec_int s i in
  let next = i + len in
  check_next ~kind:"string" s i next;
  next, String.sub s i len

let string = v enc_string dec_string

(* fpath *)

let enc_fpath b p = enc_string b (Fpath.to_string p)
let dec_fpath s i =
  let next, s = dec_string s i in
  match Fpath.of_string s with
  | Error e -> err i "corrupted file path value: %s" e
  | Ok p -> next, p

let fpath = v enc_fpath dec_fpath

(* list *)

let enc_list el b l =
  let rec loop len acc = function
  | [] -> len, acc | v :: vs -> loop (len + 1) (v :: acc) vs
  in
  let len, rl = loop 0 [] l in
  enc_int b len;
  let rec loop el b = function [] -> () | v :: vs -> el b v; loop el b vs in
  loop el b rl

let dec_list el s i  =
  let i, count = dec_int s i in
  let rec loop el s i count acc = match count = 0 with
  | true -> i, acc (* enc_list writes the reverse list. *)
  | false ->
      let i, v = el s i in
      loop el s i (count - 1) (v :: acc)
  in
  loop el s i count []

let list c = v (enc_list c.enc) (dec_list c.dec)

(* option *)

let enc_option some b = function
| None -> enc_byte b 0
| Some v -> enc_byte b 1; some b v

let dec_option some s i =
  let kind = "option" in
  let next, b = dec_byte ~kind s i in
  match b with
  | 0 -> next, None
  | 1 -> let i, v = some s next in i, Some v
  | b -> err_byte ~kind i b

let option c = v (enc_option c.enc) (dec_option c.dec)

(* result *)

let enc_result ~ok ~error b = function
| Ok v -> enc_byte b 0; ok b v
| Error e -> enc_byte b 1; error b e

let dec_result ~ok ~error s i =
  let kind = "result" in
  let next, b = dec_byte ~kind s i in
  match b with
  | 0 -> let i, v = ok s next in i, Ok v
  | 1 -> let i, e = error s next in i, Error e
  | b -> err_byte ~kind i b

let result ~ok ~error =
  v (enc_result ~ok:ok.enc ~error:error.enc)
    (dec_result ~ok:ok.dec ~error:error.dec)

(* set *)

let enc_set
    (type a) (type t)
    (module S : Set.S with type elt = a and type t = t) enc_elt b v
  =
  let count = S.cardinal v in enc_int b count; S.iter (enc_elt b) v

let dec_set
    (type a) (type t)
    (module S : Set.S with type elt = a and type t = t) dec_elt s i
  =
  let rec loop acc count s i =
    if count <= 0 then i, acc else
    let i, elt = dec_elt s i in
    loop (S.add elt acc) (count - 1) s i
  in
  let i, count = dec_int s i in
  loop S.empty count s i

let set s c = v (enc_set s c.enc) (dec_set s c.dec)

(* Hash.t *)

let enc_hash b h = enc_string b (Hash.to_bytes h)
let dec_hash s i = let i, h = dec_string s i in i, (Hash.of_bytes h)
let hash = v enc_hash dec_hash

(* Time.span *)

let enc_time_span b s = enc_int64 b (Mtime.Span.to_uint64_ns s)
let dec_time_span s i =
  let i, s = dec_int64 s i in i, Mtime.Span.of_uint64_ns s

let time_span = v enc_time_span dec_time_span

(* Time.cpu_span *)

let enc_cpu_time_span b c =
  enc_time_span b (Os.Cpu.Time.utime c);
  enc_time_span b (Os.Cpu.Time.stime c);
  enc_time_span b (Os.Cpu.Time.children_utime c);
  enc_time_span b (Os.Cpu.Time.children_stime c)

let dec_cpu_time_span s i  =
  let i, utime = dec_time_span s i in
  let i, stime = dec_time_span s i in
  let i, children_utime = dec_time_span s i in
  let i, children_stime = dec_time_span s i in
  i, Os.Cpu.Time.span ~utime ~stime ~children_utime ~children_stime

let cpu_time_span = v enc_cpu_time_span dec_cpu_time_span
