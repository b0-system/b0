(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Text locations *)

module Textloc = struct

  (* File paths *)

  type filepath = string
  let file_none = "-"
  let pp_filepath = Format.pp_print_string

  (* Byte positions *)

  type byte_pos = int (* zero-based *)
  let byte_pos_none = -1

  (* Lines *)

  type line_num = int (* one-based *)
  let line_num_none = -1

  (* Line positions

     We keep the byte position of the first element on the line. This
     first element may not exist and be equal to the text length if
     the input ends with a newline. Editors expect tools to compute
     visual columns (not a very good idea). By keeping these byte
     positions we can approximate columns by subtracting the line byte
     position data byte location. This will only be correct on
     US-ASCII data. *)

  type line_pos = line_num * byte_pos
  let line_pos_first = 1, 0
  let line_pos_none = line_num_none, byte_pos_none

  (* Text locations *)

  type t =
    { file : filepath;
      first_byte : byte_pos; last_byte : byte_pos;
      first_line : line_pos; last_line : line_pos }

  let make ~file ~first_byte ~last_byte ~first_line ~last_line =
    { file; first_byte; last_byte; first_line; last_line }

  let file l = l.file
  let set_file l file = { l with file }
  let first_byte l = l.first_byte
  let last_byte l = l.last_byte
  let first_line l = l.first_line
  let last_line l = l.last_line
  let none =
    let first_byte = byte_pos_none and last_byte = byte_pos_none in
    let first_line = line_pos_none and last_line = line_pos_none in
    make ~file:file_none ~first_byte ~last_byte ~first_line ~last_line

  (* Predicates and comparisons *)

  let is_none l = l.first_byte < 0
  let is_empty l = l.first_byte > l.last_byte
  let equal l0 l1 =
    String.equal l0.file l1.file &&
    Int.equal l0.first_byte l1.first_byte &&
    Int.equal l0.last_byte l1.last_byte

  let compare l0 l1 =
    let c = String.compare l0.file l1.file in
    if c <> 0 then c else
    let c = Int.compare l0.first_byte l1.first_byte in
    if c <> 0 then c else
    Int.compare l0.last_byte l1.last_byte

  (* Shrink and stretch *)

  let set_first l ~first_byte ~first_line = { l with first_byte; first_line }
  let set_last l ~last_byte ~last_line = { l with last_byte; last_line }

  [@@@warning "-6"]
  let to_first l =
    make l.file l.first_byte l.first_byte l.first_line l.first_line

  let to_last l =
    make l.file l.last_byte l.last_byte l.last_line l.last_line

  let before l =
    make l.file l.first_byte byte_pos_none l.first_line line_pos_none

  let after l =
    make l.file (l.first_byte + 1) byte_pos_none l.last_line line_pos_none
  [@@@warning "+6"]

  let span l0 l1 =
    let first_byte, first_line =
      if l0.first_byte < l1.first_byte
      then l0.first_byte, l0.first_line
      else l1.first_byte, l1.first_line
    in
    let last_byte, last_line, file =
      if l0.last_byte < l1.last_byte
      then l1.last_byte, l1.last_line, l1.file
      else l0.last_byte, l0.last_line, l0.file
    in
    make ~file ~first_byte ~first_line ~last_byte ~last_line

  [@@@warning "-6"]
  let reloc ~first ~last =
    make last.file first.first_byte last.last_byte first.first_line
      last.last_line
  [@@@warning "+6"]

  (* Formatters *)

  let pf = Format.fprintf
  let pp_ocaml ppf l =
    if is_none l
    then pf ppf "File \"%a\"" pp_filepath l.file else
    let pp_lines ppf l =
      if fst l.first_line = fst l.last_line
      then pf ppf "line %d" (fst l.first_line)
      else pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
    in
    (* "characters" represent positions (insertion points) not columns *)
    let pos_s = l.first_byte - snd l.first_line in
    let pos_e = l.last_byte - snd l.last_line + 1 in
    if pos_s = 0 && pos_e = 0
    then pf ppf "File \"%a\", %a" pp_filepath l.file pp_lines l
    else pf ppf "File \"%a\", %a, characters %d-%d"
        pp_filepath l.file pp_lines l pos_s pos_e

  let pp_gnu ppf l =
    if is_none l then pf ppf "%a:" pp_filepath l.file else
    let pp_lines ppf l =
      let col_s = l.first_byte - snd l.first_line + 1 in
      let col_e = l.last_byte - snd l.last_line + 1 in
      if fst l.first_line = fst l.last_line
      then pf ppf "%d.%d-%d" (fst l.first_line) col_s col_e
      else pf ppf "%d.%d-%d.%d" (fst l.first_line) col_s (fst l.last_line) col_e
    in
    pf ppf "%a:%a" pp_filepath l.file pp_lines l

  let pp = pp_ocaml

  let pp_dump ppf l =
    pf ppf "file:%s bytes:%d-%d lines:(%d,%d)-(%d,%d)"
      l.file l.first_byte l.last_byte (fst l.first_line)
      (snd l.first_line)  (fst l.last_line) (snd l.last_line)
end

module Textdec = struct

  (* Decodes *)

  type decode = int

  let sot = 0x110000  (* start of text U+10FFFF + 1 *)
  let eot = 0x110001  (* end of text   U+10FFFF + 2 *)

  let needs_escape = function
  | u when 0x0000 <= u && u <= 0x001F -> true (* C0 control characters *)
  | u when 0x0080 <= u && u <= 0x009F -> true (* C1 control characters *)
  | 0x2028 (* line separator *) | 0x2029 (* paragraph separator *)
  | 0x200E (* left-to-right mark *) | 0x200F (* right-to-left mark *) -> true
  | _ -> false

  let pp_decode ppf = function
  | 0x110000 -> Format.pp_print_string ppf "start of text"
  | 0x110001 -> Format.pp_print_string ppf "end of text"
  | u when needs_escape u -> Format.fprintf ppf "U+%04X" u
  | u ->
      let uchar = Uchar.of_int u in
      let utf8 =
        let b = Bytes.create (Uchar.utf_8_byte_length uchar) in
        ignore (Bytes.set_utf_8_uchar b 0 uchar); Bytes.unsafe_to_string b
      in
      Format.fprintf ppf "'@<1>%s' (U+%04X)" utf8 u

  (* Decoders *)

  type t =
    { file : string;
      i : string;
      mutable current : decode; (* Current scalar value or sot or eot *)
      mutable is_error : bool;
      mutable next : Textloc.byte_pos; (* Next character byte position. *)
      mutable line_num : Textloc.line_num;
      mutable line_start : Textloc.byte_pos; (* Line first byte *)
      mutable prev_line_start : Textloc.byte_pos; (* Previous line first byte *)
      lexeme : Buffer.t; }

  let make ?(file = "-") i =
    let lexeme = Buffer.create 255 in
    let current = sot and is_error = false in
    { file; i; current; is_error; next = 0; line_num = 1;
      line_start = 0; prev_line_start = 0; lexeme }

  let input d = d.i
  let file d = d.i
  let[@inline] current d = d.current
  let[@inline] is_error d = d.is_error
  let next d =
    if d.next >= String.length d.i
    then (d.current <- eot; d.is_error <- false) else
    begin
      let udec = String.get_utf_8_uchar d.i d.next in
      let u = Uchar.to_int (Uchar.utf_decode_uchar udec) in
      d.is_error <- not (Uchar.utf_decode_is_valid udec);
      d.next <- d.next + Uchar.utf_decode_length udec;
      begin match u with
      | 0x000D (* CR *) ->
          d.line_num <- d.line_num + 1;
          d.prev_line_start <- d.line_start;
          d.line_start <- d.next;
      | 0x000A (* LF *) ->
          if d.current <> 0x000D then begin
            d.line_num <- d.line_num + 1;
            d.prev_line_start <- d.line_start;
          end;
          d.line_start <- d.next;
      | _ -> ()
      end;
      d.current <- u
    end

  (* Byte positions *)

  let first_byte_pos d =
    if d.current = sot then 0 else
    if d.current = eot then String.length d.i else
    d.next - Uchar.utf_8_byte_length (Uchar.unsafe_of_int d.current)

  let last_byte_pos d =
    if d.current = sot then 0 else
    if d.current = eot then String.length d.i else
    d.next - 1

  let prev_decode_last_byte_pos d =
    if d.current = sot then 0 else
    if d.current = eot then String.length d.i - 1 else
    let prev = d.next - Uchar.utf_8_byte_length (Uchar.of_int d.current) - 1 in
    if prev < 0 then 0 else prev

  let[@inline] line_num d = d.line_num
  let[@inline] line_start d = d.line_start
  let[@inline] line_pos d = line_num d, line_start d
  let[@inline] prev_line_num d = if d.line_num = 1 then 1 else d.line_num - 1
  let[@inline] prev_line_start d = d.prev_line_start
  let[@inline] prev_line_pos d = prev_line_num d, prev_line_start d
  let[@inline] pos d = first_byte_pos d, line_pos d
  let textloc d =
    let first_byte = first_byte_pos d and first_line = line_pos d in
    let last_byte = last_byte_pos d and last_line = first_line in
    Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

  let textloc_span d ~start:(first_byte, first_line) =
    let last_byte = last_byte_pos d and last_line = line_pos d in
    Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

  let textloc_span_to_prev_decode d ~start:(first_byte, first_line) =
    let last_byte = prev_decode_last_byte_pos d in
    let last_line =
      if (d.current = 0x000D) ||
         (d.current = 0x000A && last_byte > 0 && d.i.[last_byte] <> '\x0D')
      then prev_line_pos d else line_pos d
    in
    Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line ~last_line

  (* Lexeme buffer *)

  let lexeme_clear d = Buffer.clear d.lexeme
  let lexeme_pop d = let t = Buffer.contents d.lexeme in lexeme_clear d; t
  let lexeme_add d u = Buffer.add_utf_8_uchar d.lexeme u
end
