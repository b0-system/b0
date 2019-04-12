(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

module Utf_8 = struct
  type case =
  | L1 | L2 | L3_E0 | L3_E1_EC_or_EE_EF | L3_ED | L4_F0 | L4_F1_F3 | L4_F4 | E

  let case =
(*
  (* See https://tools.ietf.org/html/rfc3629#section-4 *)
  Printf.printf "[|";
  for i = 0 to 255 do
    if i mod 16 = 0 then Printf.printf "\n";
    if 0x00 <= i && i <= 0x7F then Printf.printf "L1; " else
    if 0xC2 <= i && i <= 0xDF then Printf.printf "L2; " else
    if 0xE0 = i then Printf.printf "L3_E0; " else
    if 0xE1 <= i && i <= 0xEC || 0xEE <= i && i <= 0xEF
    then Printf.printf "L3_E1_EC_or_EE_EF; " else
    if 0xED = i then Printf.printf "L3_ED;" else
    if 0xF0 = i then Printf.printf "L4_F0; " else
    if 0xF1 <= i && i <= 0xF3 then Printf.printf "L4_F1_F3; " else
    if 0xF4 = i then Printf.printf "L4_F4; " else
    Printf.printf "E; "
  done;
  Printf.printf "\n|]"
*)
  [|
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1; L1;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; E; E; E; E; E; E; E; E; E; E; E; E; E; E;
    E; E; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
    L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2; L2;
    L3_E0; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L3_E1_EC_or_EE_EF; L3_ED;L3_E1_EC_or_EE_EF; L3_E1_EC_or_EE_EF;
    L4_F0; L4_F1_F3; L4_F1_F3; L4_F1_F3; L4_F4; E; E; E; E; E; E; E; E; E; E; E;
  |]
end

module Tloc = struct
  type pos = int
  type line = int

  type t =
    { file : Fpath.t;
      byte_s : pos; byte_e : pos;

      (* For lines we keep their byte locations. It seems editors are still
         waiting for tools to compute visual columns which is stupid. By
         keeping the line byte locations we can approximate columns by
         subtracting the line byte position from the byte location. This
         will only be correct on US-ASCII data though. Best would of course
         would be to give them [byte_s] and [byte_e]. *)
      line_s : pos * line; line_e : pos * line }

  let no_file = Fpath.v "-"
  let v ~file ~byte_s ~byte_e ~line_s ~line_e =
    { file; byte_s; byte_e; line_s; line_e;  } [@@ ocaml.inline]

  let file l = l.file
  let byte_s l = l.byte_s
  let byte_e l = l.byte_e
  let line_s l = l.line_s
  let line_e l = l.line_e
  let nil = v
      ~file:no_file ~byte_s:(-1) ~byte_e:(-1) ~line_s:(-1, -1) ~line_e:(-1, -1)

  let merge l0 l1 =
    let byte_s = if l0.byte_s < l1.byte_s then l0.byte_s else l1.byte_s in
    let byte_e = if l0.byte_e > l1.byte_e then l0.byte_e else l1.byte_e in
    let line_s, line_e =
      (if (fst l0.line_s) < (fst l1.line_s) then l0.line_s else l1.line_s),
      (if (fst l0.line_s) > (fst l1.line_s) then l0.line_s else l1.line_s)
    in
    v ~file:l0.file ~byte_s ~byte_e ~line_s ~line_e

  let to_start l =
    v ~file:l.file ~byte_s:l.byte_s ~byte_e:l.byte_s ~line_s:l.line_s
      ~line_e:l.line_s

  let to_end l =
    v ~file:l.file ~byte_s:l.byte_e ~byte_e:l.byte_e ~line_s:l.line_e
      ~line_e:l.line_e

  let with_start s e =
    v ~file:e.file ~byte_s:s.byte_s ~byte_e:e.byte_e ~line_s:s.line_s
      ~line_e:s.line_e

  let pp_ocaml ppf l = match l.byte_e < 0 with
  | true ->
      Fmt.pf ppf "File \"%a\", line n/a, characters n/a" Fpath.pp l.file
  | false ->
      let pp_lines ppf l = match (snd l.line_s) = (snd l.line_e) with
      | true -> Fmt.pf ppf "line %d" (snd l.line_s)
      | false -> Fmt.pf ppf "lines %d-%d" (snd l.line_s) (snd l.line_e)
      in
      let col_s = l.byte_s - (fst l.line_s) in
      let col_e = l.byte_e - (fst l.line_e) in
      Fmt.pf ppf "File \"%a\", %a, characters %d-%d"
        Fpath.pp l.file pp_lines l col_s col_e

  let pp_gnu ppf l = match l.byte_e < 0 with
  | true -> Fmt.pf ppf "%a" Fpath.pp l.file
  | false ->
      let pp_lines ppf l =
        let col_s = l.byte_s - (fst l.line_s) in
        let col_e = l.byte_e - (fst l.line_e) in
        match (snd l.line_s) = (snd l.line_e) with
        | true ->  Fmt.pf ppf "%d.%d-%d" (snd l.line_s) col_s col_e
        | false ->
            Fmt.pf ppf "%d.%d-%d.%d" (snd l.line_s) col_s (snd l.line_e) col_e
      in
      Fmt.pf ppf "%a:%a" Fpath.pp l.file pp_lines l

  let pp = pp_gnu
end

module Tdec = struct

  (* Decoders *)

  type t =
    { file : Fpath.t; i : string; tok : Buffer.t;
      mutable pos : int; mutable line_pos : int; mutable line : int }

  let create ?(file = Tloc.no_file) i =
    { file; i; tok = Buffer.create 255; pos = 0; line_pos = -1; line = 1 }

  (* Location *)

  let file d = d.file
  let pos d = d.pos
  let line d = d.line_pos, d.line

  let loc d ~byte_s ~byte_e ~line_s ~line_e =
    Tloc.v ~file:d.file ~byte_s ~byte_e ~line_s ~line_e

  let loc_to_here d ~byte_s ~line_s =
    loc d ~byte_s ~byte_e:d.pos ~line_s ~line_e:(d.line_pos, d.line)

  let loc_here d = loc_to_here d ~byte_s:d.pos ~line_s:(d.line_pos, d.line)

  (* Errors *)

  exception Err of Tloc.t * string

  let err loc msg = raise_notrace (Err (loc, msg))
  let err_to_here d ~byte_s ~line_s fmt =
    Fmt.kstr (err (loc_to_here d ~byte_s ~line_s)) fmt

  let err_here d fmt = Fmt.kstr (err (loc_here d)) fmt

  (* Lexing *)

  let incr_line d = match d.i.[d.pos] with (* assert (not (eoi d)) *)
  | '\r' -> (d.line_pos <- d.pos; d.line <- d.line + 1)
  | '\n' when d.pos <> 0 && d.i.[d.pos - 1] <> '\r' ->
      (d.line_pos <- d.pos; d.line <- d.line + 1)
  | _ -> ()
  [@@ ocaml.inline]

  let eoi d = d.pos >= String.length d.i [@@ ocaml.inline]
  let byte d = if eoi d then 0xFFFF else Char.code d.i.[d.pos] [@@ ocaml.inline]
  let accept_byte d = d.pos <- d.pos + 1; if eoi d then () else incr_line d
  [@@ ocaml.inline]

  let accept_utf_8 accept d =
    let err d = match byte d with
    | 0xFFFF -> err_here d "UTF-8 decoding error: unexpected end of input"
    | b -> err_here d "UTF-8 decoding error: byte %02x illegal here" b
    in
    let accept_tail d = if (byte d lsr 6 = 0b10) then accept d else err d in
    match byte d with
    | 0xFFFF -> err d
    | b ->
        (* If a subseqent [byte d] invocation is 0xFFFF we get to [err]. *)
        match Utf_8.case.(b) with
        | L1 -> accept d
        | L2 -> accept d; accept_tail d
        | L3_E0 ->
            accept d;
            if (byte d - 0xA0 < 0xBF - 0xA0) then accept d else err d;
            accept_tail d
        | L3_E1_EC_or_EE_EF -> accept d; accept_tail d; accept_tail d
        | L3_ED ->
            accept d;
            if (byte d - 0x80 < 0x9F - 0x80) then accept d else err d;
            accept_tail d
        | L4_F0 ->
            accept d;
            if (byte d - 0x90 < 0xBF - 0x90) then accept d else err d;
            accept_tail d; accept_tail d
        | L4_F1_F3 ->
            accept d;
            accept_tail d; accept_tail d; accept_tail d;
        | L4_F4 ->
            accept d;
            if (byte d - 0x80 < 0x8F - 0x80) then accept d else err d;
        | E -> err d

  let accept_uchar d = accept_utf_8 accept_byte d

  (* Tokenizer *)

  let tok_reset d = Buffer.reset d.tok [@@ ocaml.inline]
  let tok_pop d = let t = Buffer.contents d.tok in tok_reset d; t
  [@@ ocaml.inline]

  let tok_accept_byte d =
    Buffer.add_char d.tok d.i.[d.pos]; accept_byte d; [@@ ocaml.inline]

  let tok_accept_uchar d = accept_utf_8 tok_accept_byte d [@@ ocaml.inline]
  let tok_add_byte d b = Buffer.add_char d.tok (Char.chr b) [@@ ocaml.inline]
  let tok_add_bytes d s = Buffer.add_string d.tok s [@@ ocaml.inline]
  let tok_add_char d c = Buffer.add_char d.tok c [@@ ocaml.inline]
  let tok_add_uchar d u = match Uchar.to_int u with
  (* XXX From 4.06 use Buffer.add_utf_8_uchar *)
  | u when u < 0 -> assert false
  | u when u <= 0x007F ->
      Buffer.add_char d.tok (Char.unsafe_chr u)
  | u when u <= 0x07FF ->
      Buffer.add_char d.tok (Char.unsafe_chr (0xC0 lor (u lsr 6)));
      Buffer.add_char d.tok (Char.unsafe_chr (0x80 lor (u land 0x3F)));
  | u when u <= 0xFFFF ->
      Buffer.add_char d.tok (Char.unsafe_chr (0xE0 lor (u lsr 12)));
      Buffer.add_char d.tok (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
      Buffer.add_char d.tok (Char.unsafe_chr (0x80 lor (u land 0x3F)));
  | u when u <= 0x10FFFF ->
      Buffer.add_char d.tok (Char.unsafe_chr (0xF0 lor (u lsr 18)));
      Buffer.add_char d.tok (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
      Buffer.add_char d.tok (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
      Buffer.add_char d.tok (Char.unsafe_chr (0x80 lor (u land 0x3F)))
  | _ -> assert false
end

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
