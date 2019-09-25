(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Error message helpers. *)

module Err_msg = struct
  let pp_nop _ () = ()
  let pp_any fmt ppf _ = Format.fprintf ppf fmt
  let pp_one_of ?(empty = pp_nop) pp_v ppf = function
  | [] -> empty ppf ()
  | [v] -> pp_v ppf v
  | [v0; v1] -> Format.fprintf ppf "@[either %a or@ %a@]" pp_v v0 pp_v v1
  | _ :: _ as vs ->
      let rec loop ppf = function
      | [v] -> Format.fprintf ppf "or@ %a" pp_v v
      | v :: vs -> Format.fprintf ppf "%a,@ " pp_v v; loop ppf vs
      | [] -> assert false
      in
      Format.fprintf ppf "@[one@ of@ %a@]" loop vs

  let pp_did_you_mean
      ?(pre = pp_any "Unknown") ?(post = pp_nop) ~kind pp_v ppf (v, hints)
    =
    match hints with
    | [] -> Format.fprintf ppf "@[%a %s %a%a.@]" pre () kind pp_v v post ()
    | hints ->
        Format.fprintf ppf "@[%a %s %a%a.@ Did you mean %a ?@]"
          pre () kind pp_v v post () (pp_one_of pp_v) hints

  let min_by f a b = if f a <= f b then a else b
  let max_by f a b = if f a <= f b then b else a

  let edit_distance s0 s1 =
    let minimum a b c = min a (min b c) in
    let s0 = min_by String.length s0 s1     (* row *)
    and s1 = max_by String.length s0 s1 in  (* column *)
    let m = String.length s0 and n = String.length s1 in
    let rec rows row0 row i =
      if i > n then row0.(m) else begin
        row.(0) <- i;
        for j = 1 to m do
          if s0.[j - 1] = s1.[i - 1] then row.(j) <- row0.(j - 1) else
          row.(j) <-minimum (row0.(j - 1) + 1) (row0.(j) + 1) (row.(j - 1) + 1)
        done;
        rows row row0 (i + 1)
      end in
    rows (Array.init (m + 1) (fun x -> x)) (Array.make (m + 1) 0) 1

  let suggest ?(dist = 2) candidates s =
    let add (min, acc) name =
      let d = edit_distance s name in
      if d = min then min, (name :: acc) else
      if d < min then d, [name] else
      min, acc
    in
    let d, suggs = List.fold_left add (max_int, []) candidates in
    if d <= dist (* suggest only if not too far *) then List.rev suggs else []
end

(* Text locations *)

module Tloc = struct
  type fpath = string
  let pp_path = Format.pp_print_string

  type pos = int
  type line = int
  type line_pos = line * pos
  (* For lines we keep the byte position just after the newlinexs. It
     editors are still expecting tools to compute visual columns which
     is stupid.  By keeping these byte positions we can approximate
     columns by subtracting the line byte position from the byte
     location. This will only be correct on US-ASCII data though. Best
     would be to be able to give them [sbyte] and [ebyte]. *)

  let l v = v
  type t =
    { file : fpath;
      sbyte : pos; ebyte : pos;
      sline : pos * line; eline : pos * line }

  let no_file = "-"
  let v ~file ~sbyte ~ebyte ~sline ~eline = { file; sbyte; ebyte; sline; eline }
  let file l = l.file
  let sbyte l = l.sbyte
  let ebyte l = l.ebyte
  let sline l = l.sline
  let eline l = l.eline
  let nil =
    let pnil = -1 in
    let lnil = (-1, pnil) in
    v ~file:no_file ~sbyte:pnil ~ebyte:pnil ~sline:lnil ~eline:lnil

  let merge l0 l1 =
    let sbyte, sline =
      if l0.sbyte < l1.sbyte then l0.sbyte, l0.sline else l1.sbyte, l1.sline
    in
    let ebyte, eline =
      if l0.ebyte < l1.ebyte then l1.ebyte, l1.eline else l0.ebyte, l0.eline
    in
    v ~file:l0.file ~sbyte ~ebyte ~sline ~eline

  let to_start l =
    v ~file:l.file ~sbyte:l.sbyte ~ebyte:l.sbyte ~sline:l.sline ~eline:l.sline

  let to_end l =
    v ~file:l.file ~sbyte:l.ebyte ~ebyte:l.ebyte ~sline:l.eline ~eline:l.eline

  let restart ~at:s e =
    v ~file:e.file ~sbyte:s.sbyte ~ebyte:e.ebyte ~sline:s.sline ~eline:e.eline

  let pf = Format.fprintf
  let pp_ocaml ppf l = match l.ebyte < 0 with
  | true -> pf ppf "File \"%a\", line n/a, characters n/a" pp_path l.file
  | false ->
      let pp_lines ppf l = match fst l.sline = fst l.eline with
      | true -> pf ppf "line %d" (fst l.sline)
      | false -> pf ppf "lines %d-%d" (fst l.sline) (fst l.eline)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.sbyte - snd l.sline in
      let pos_e = l.ebyte - snd l.eline + 1 in
      pf ppf "File \"%a\", %a, characters %d-%d"
        pp_path l.file pp_lines l pos_s pos_e

  let pp_gnu ppf l = match l.ebyte < 0 with
  | true -> pf ppf "%a:" pp_path l.file
  | false ->
      let pp_lines ppf l =
        let col_s = l.sbyte - snd l.sline + 1 in
        let col_e = l.ebyte - snd l.eline + 1 in
        match fst l.sline = fst l.eline with
        | true ->  pf ppf "%d.%d-%d" (fst l.sline) col_s col_e
        | false ->
            pf ppf "%d.%d-%d.%d" (fst l.sline) col_s (fst l.eline) col_e
      in
      pf ppf "%a:%a" pp_path l.file pp_lines l

  let pp_dump ppf l =
    pf ppf "[bytes %d;%d][lines %d;%d][lbytes %d;%d]"
      l.sbyte l.ebyte (fst l.sline) (fst l.eline) (snd l.sline) (snd l.eline)

  let pp = pp_gnu

  (* Insertions and substitutions *)

  let string_with_index_range ?(first = 0) ?last s =
    let max = String.length s - 1 in
    let last = match last with
    | None -> max
    | Some l when l > max -> max
    | Some l -> l
    in
    let first = if first < 0 then 0 else first in
    if first > last then "" else
    String.sub s first (last - first + 1)

  let string_replace ~start ~stop ~rep s =
    let len = String.length s in
    if stop < start || start < 0 || start > len || stop < 0 || stop > len
    then invalid_arg (Printf.sprintf "invalid start:%d stop:%d" start stop) else
    let b = String.sub s 0 start in
    let a = String.sub s stop (len - stop) in
    String.concat "" [b; rep; a]
end

(* UTF-8 decoding table. *)

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

(* UTF-8 text decoder *)

module Tdec = struct

  (* Decoders *)

  type t =
    { file : Tloc.fpath; i : string; tok : Buffer.t;
      mutable pos : int; mutable line : int; mutable line_pos : int; }

  let create ?(file = Tloc.no_file) i =
    { file; i; tok = Buffer.create 255; pos = 0; line = 1; line_pos = 0 }

  (* Location *)

  let file d = d.file
  let pos d = d.pos
  let line d = d.line, d.line_pos

  let loc d ~sbyte ~ebyte ~sline ~eline =
    Tloc.v ~file:d.file ~sbyte ~ebyte ~sline ~eline

  let loc_to_here d ~sbyte ~sline =
    loc d ~sbyte ~ebyte:d.pos ~sline ~eline:(d.line, d.line_pos)

  let loc_here d = loc_to_here d ~sbyte:d.pos ~sline:(d.line, d.line_pos)

  (* Errors *)

  exception Err of Tloc.t * string

  let err loc msg = raise_notrace (Err (loc, msg))
  let err_to_here d ~sbyte ~sline fmt =
    Format.kasprintf (err (loc_to_here d ~sbyte ~sline)) fmt

  let err_here d fmt = Format.kasprintf (err (loc_here d)) fmt
  let err_suggest = Err_msg.suggest
  let err_did_you_mean = Err_msg.pp_did_you_mean

  (* Lexing *)

  let incr_line d = match d.i.[d.pos] with (* assert (not (eoi d)) *)
  | '\r' -> d.line <- d.line + 1; d.line_pos <- d.pos + 1
  | '\n' ->
      (if d.pos = 0 || d.i.[d.pos - 1] <> '\r' then d.line <- d.line + 1);
      d.line_pos <- d.pos + 1;
  | _ -> ()
  [@@ ocaml.inline]

  let eoi d = d.pos >= String.length d.i [@@ ocaml.inline]
  let byte d = if eoi d then 0xFFFF else Char.code d.i.[d.pos] [@@ ocaml.inline]
  let accept_byte d = incr_line d; d.pos <- d.pos + 1
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
        (* If a subsequent [byte d] invocation is 0xFFFF we get to [err]. *)
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
