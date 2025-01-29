(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Type identifiers *)

module Type = struct
  (* See http://alan.petitepomme.net/cwn/2015.03.24.html#1
     In the stdlib since 5.1. *)

  type ('a, 'b) eq = Equal : ('a, 'a) eq

  module Id = struct
    type _ id = ..
    module type ID = sig
      type t
      type _ id += Id : t id
    end

    type 'a t = (module ID with type t = 'a)

    let make (type a) () : a t =
      (module struct type t = a type _ id += Id : t id end)

    let provably_equal
        (type a b) ((module A) : a t) ((module B) : b t) : (a, b) eq option
      =
      match A.Id with B.Id -> Some Equal | _ -> None

    let uid (type a) ((module A) : a t) =
      Obj.Extension_constructor.id (Obj.Extension_constructor.of_val A.Id)
  end
end

module Char = B0__char
module Fmt = B0__fmt
module Result = B0__result

(* Strings *)

module String = struct
  include String

  let empty = ""
  let head s = if s = "" then None else Some s.[0]
  let of_char c = String.make 1 c

  (* Predicates *)

  let is_empty s = equal empty s
  let starts_with ~prefix s =
    let len_a = String.length prefix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let rec loop i =
      if i > max_idx_a then true else
      if unsafe_get prefix i <> unsafe_get s i then false else loop (i + 1)
    in
    loop 0

  let ends_with ~suffix s =
    let len_a = String.length suffix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let max_idx_s = len_s - len_a in
    let rec loop i k =
      if i > max_idx_s then false else
      if k > max_idx_a then true else
      if k > 0 then
        if unsafe_get suffix k = unsafe_get s (i + k)
        then loop i (k + 1) else loop (i + 1) 0
      else
        if unsafe_get suffix 0 = unsafe_get s i
        then loop i 1 else loop (i + 1) 0
    in
    loop 0 0

  let includes ~affix s =
    let len_a = String.length affix in
    let len_s = String.length s in
    if len_a > len_s then false else
    let max_idx_a = len_a - 1 in
    let max_idx_s = len_s - len_a in
    let rec loop i k =
      if i > max_idx_s then false else
      if k > max_idx_a then true else
      if k > 0 then
        if unsafe_get affix k = unsafe_get s (i + k)
        then loop i (k + 1) else loop (i + 1) 0
      else
      if unsafe_get affix 0 = unsafe_get s i
      then loop i 1 else loop (i + 1) 0
    in
    loop 0 0

  let for_all sat s =
    let max_idx = String.length s - 1 in
    let rec loop i =
      if i > max_idx then true else
      if sat (unsafe_get s i) then loop (i + 1) else false
    in
    loop 0

  let exists sat s =
    let max_idx = String.length s - 1 in
    let rec loop i =
      if i > max_idx then false else
      if sat (unsafe_get s i) then true else loop (i + 1)
    in
    loop 0

  (* Finding indices *)

  let find_index ?(start = 0) sat s =
    let max = String.length s - 1 in
    let i = ref start in
    while (!i <= max && not (sat s.[!i])) do incr i done;
    if !i > max then None else Some !i

  let rfind_index ?start sat s =
    let start = match start with None -> String.length s - 1 | Some s -> s in
    let i = ref start in
    while (0 <= !i && not (sat s.[!i])) do decr i done;
    if !i < 0 then None else Some !i

  (* Finding substrings.  *)

  let find_sub ?(start = 0) ~sub s =
    (* naive algorithm, worst case O(length sub * length s) *)
    let len_sub = length sub in
    let len_s = length s in
    let max_idx_sub = len_sub - 1 in
    let max_idx_s = if len_sub <> 0 then len_s - len_sub else len_s - 1 in
    let rec loop i k =
      if i > max_idx_s then None else
      if k > max_idx_sub then Some i else
      if k > 0 then
        if unsafe_get sub k = unsafe_get s (i + k)
        then loop i (k + 1) else loop (i + 1) 0
      else
      if unsafe_get sub 0 = unsafe_get s i
      then loop i 1 else loop (i + 1) 0
    in
    loop start 0

  let rfind_sub ?start ~sub s =
    let start = match start with None -> String.length s - 1 | Some s -> s in
    (* naive algorithm, worst case O(length sub * length s) *)
    let len_sub = length sub in
    if len_sub = 0 then
      if start <= String.length s - 1 then Some start else None
    else
    let max_sub = len_sub - 1 in
    let rec loop i k =
      if i < 0 then None else
      if k > max_sub then Some i else
      if k > 0 then
        if unsafe_get sub k = unsafe_get s (i + k)
        then loop i (k + 1) else loop (i - 1) 0
      else
      if unsafe_get sub 0 = unsafe_get s i
      then loop i 1 else loop (i - 1) 0
    in
    loop (start - max_sub) 0

  let replace_all ~sub ~by s =
    (* Uses the naive search algo. Implement
       https://www-igm.univ-mlv.fr/~lecroq/string/node26.html at some point *)
    let rec loop sub s start chunks = match find_sub ~start ~sub s with
    | None ->
        if start = 0 then s else
        let chunks = String.sub s start (String.length s - start) :: chunks in
        String.concat by (List.rev chunks)
    | Some i ->
        let chunks = String.sub s start (i - start) :: chunks in
        let start = i + String.length sub in
        loop sub s start chunks
    in
    if sub <> "" then loop sub s 0 [] else
    if by = "" then s else
    let slen = String.length s and bylen = String.length by in
    let dst = Bytes.create (slen + (slen + 1) * bylen) in
    for i = 0 to slen - 1 do
      let start = i * (1 + bylen) in
      Bytes.blit_string by 0 dst start bylen;
      Bytes.set dst (start + bylen) s.[i];
    done;
    Bytes.blit_string by 0 dst (slen + slen * bylen) bylen;
    Bytes.unsafe_to_string dst

  (* Extracting substrings *)

  let subrange ?(first = 0) ?last s =
    let max = String.length s - 1 in
    let last = match last with
    | None -> max
    | Some l when l > max -> max
    | Some l -> l
    in
    let first = if first < 0 then 0 else first in
    if first > last then "" else
    String.sub s first (last - first + 1)

  (* Breaking with magnitudes *)

  let take_left n s = subrange ~last:(n - 1) s
  let drop_left n s = subrange ~first:n s
  let break_left n s = (take_left n s, drop_left n s)
  let take_right n s = subrange ~first:(String.length s - n) s
  let drop_right n s = subrange ~last:(String.length s - n - 1) s
  let break_right n s = (drop_right n s, take_right n s)

  (* Breaking with predicates *)

  let keep_left sat s =
    let max = String.length s - 1 in
    let rec loop max s i = match i > max with
    | true -> s
    | false when sat s.[i] -> loop max s (i + 1)
    | false -> subrange ~last:(i - 1) s
    in
    loop max s 0

  let lose_left sat s =
    let max = String.length s - 1 in
    let rec loop max s i = match i > max with
    | true -> ""
    | false when sat s.[i] -> loop max s (i + 1)
    | false -> subrange ~first:i s
    in
    loop max s 0

  let span_left sat s =
    let max = String.length s - 1 in
    let rec loop max s i = match i > max with
    | true -> s, ""
    | false when sat s.[i] -> loop max s (i + 1)
    | false -> subrange ~last:(i - 1) s, subrange ~first:i s
    in
    loop max s 0

  let keep_right sat s =
    let max = String.length s - 1 in
    let rec loop s i = match i < 0 with
    | true -> s
    | false when sat s.[i] -> loop s (i - 1)
    | false -> subrange ~first:(i + 1) s
    in
    loop s max

  let lose_right sat s =
    let max = String.length s - 1 in
    let rec loop s i = match i < 0 with
    | true -> ""
    | false when sat s.[i] -> loop s (i - 1)
    | false -> subrange ~last:i s
    in
    loop s max

  let span_right sat s =
    let max = String.length s - 1 in
    let rec loop s i = match i < 0 with
    | true -> "", s
    | false when sat s.[i] -> loop s (i - 1)
    | false -> subrange ~last:i s, subrange ~first:(i + 1) s
    in
    loop s max

  (* Breaking with separators *)

  let err_empty_sep = "~sep is an empty string"

  let cut_left ~sep s =
    let sep_len = length sep in
    if sep_len = 0 then invalid_arg err_empty_sep else
    let s_len = length s in
    let max_sep_idx = sep_len - 1 in
    let max_s_idx = s_len - sep_len in
    let rec check_sep i k = match k > max_sep_idx with
    | true ->
        let r_start = i + sep_len in
        Some (String.sub s 0 i, String.sub s r_start (s_len - r_start))
    | false ->
        if unsafe_get s (i + k) = unsafe_get sep k
        then check_sep i (k + 1)
        else scan (i + 1)
    and scan i =
      if i > max_s_idx then None else
      if String.get s i = String.get sep 0 then check_sep i 1 else scan (i + 1)
    in
    scan 0

  let cut_right ~sep s =
    let sep_len = length sep in
    if sep_len = 0 then invalid_arg err_empty_sep else
    let s_len = length s in
    let max_sep_idx = sep_len - 1 in
    let max_s_idx = s_len - 1 in
    let rec check_sep i k = match k > max_sep_idx with
    | true ->
        let r_start = i + sep_len in
        Some (String.sub s 0 i, String.sub s r_start (s_len - r_start))
    | false ->
        if unsafe_get s (i + k) = unsafe_get sep k
        then check_sep i (k + 1)
        else rscan (i - 1)
    and rscan i =
      if i < 0 then None else
      if String.get s i = String.get sep 0 then check_sep i 1 else rscan (i - 1)
    in
    rscan (max_s_idx - max_sep_idx)

  let cuts_left ?(drop_empty = false) ~sep s =
    let rec loop acc s = match cut_left ~sep s with
    | Some (v, vs) -> loop (if drop_empty && v = "" then acc else (v :: acc)) vs
    | None -> List.rev (if drop_empty && s = "" then acc else (s :: acc))
    in
    loop [] s

  let cuts_right ?(drop_empty = false) ~sep s =
    let rec loop acc s = match cut_right ~sep s with
    | Some (vs, v) -> loop (if drop_empty && v = "" then acc else (v :: acc)) vs
    | None -> if drop_empty && s = "" then acc else (s :: acc)
    in
    loop [] s

  (* Tokenize *)

  let tokens ?(is_sep = Char.Ascii.is_white) s =
    let rec skip_seps s i =
      if i < 0 || not (is_sep s.[i]) then i else skip_seps s (i - 1)
    in
    let rec find_sep s i =
      if i < 0 || is_sep s.[i] then i else find_sep s (i - 1)
    in
    let rec loop acc s i =
      let last = skip_seps s i in
      if last < 0 then acc else
      let first = find_sep s last + 1 in
      loop (String.sub s first (last - first + 1) :: acc) s (first - 1)
    in
    loop [] s (String.length s - 1)

  (* Breaking lines *)

  let fold_ascii_lines ~strip_newlines:strip f acc s =
    let rec loop ~strip linenum s start i max f acc =
      if i > max then
        f linenum acc (if start = 0 then s else String.sub s start (i - start))
      else
      let next_start =
        let next = i + 1 in
        if unsafe_get s i = '\n' then next else
        if unsafe_get s i = '\r' then
          if next > max then next else
          if unsafe_get s next = '\n' then next + 1 else next
        else start
      in
      if next_start = start
      then loop ~strip linenum s start (i + 1) max f acc else
      let after_line_data = if strip then i else next_start in
      let line = String.sub s start (after_line_data - start) in
      let acc = f linenum acc line in
      loop ~strip (linenum + 1) s next_start next_start max f acc
    in
    if s = "" then acc else loop ~strip 1 s 0 0 (String.length s - 1) f acc

  let detach_ascii_newline s =
    if s = "" then ("", "") else
    let len = String.length s in
    let last = len - 1 in
    let newline_start =
      if unsafe_get s last = '\n' then
        let prev = last - 1 in
        if prev < 0 then last else
        (if unsafe_get s prev = '\r' then prev else last)
      else
      if unsafe_get s last = '\r' then last else len
    in
    if newline_start > last then (s, "") else
    String.sub s 0 newline_start,
    String.sub s newline_start (len - newline_start)

  (* Formatting *)

  let pp = Fmt.string
  let pp_dump ppf s = Fmt.pf ppf "%S" s

  (* Spellchecking *)

  let uchar_utf_8_byte_decode_length = function
  | '\x00' .. '\x7F' -> 1
  | '\x80' .. '\xC1' -> 0
  | '\xC2' .. '\xDF' -> 2
  | '\xE0' .. '\xEF' -> 3
  | '\xF0' .. '\xF4' -> 4
  | _ -> 0

  let utf_8_uchar_length s =
    let slen = length s in
    let i = ref 0 and ulen = ref 0 in
    while (!i < slen) do
      let dec_len = uchar_utf_8_byte_decode_length (unsafe_get s !i) in
      i := (!i + if dec_len = 0 then 1 (* count one Uchar.rep *) else dec_len);
      incr ulen;
    done;
    !ulen

  let uchar_array_of_utf_8_string s =
    let slen = length s in (* is an upper bound on Uchar.t count *)
    let uchars = Array.make slen Uchar.max in
    let k = ref 0 and i = ref 0 in
    while (!i < slen) do
      let dec = get_utf_8_uchar s !i in
      i := !i + Uchar.utf_decode_length dec;
      uchars.(!k) <- Uchar.utf_decode_uchar dec;
      incr k;
    done;
    uchars, !k

  let edit_distance' ?(limit = Int.max_int) s (s0, len0) s1 =
    if limit <= 1 then (if equal s s1 then 0 else limit) else
    let[@inline] minimum a b c = Int.min a (Int.min b c) in
    let s1, len1 = uchar_array_of_utf_8_string s1 in
    let limit = Int.min (Int.max len0 len1) limit in
    if Int.abs (len1 - len0) >= limit then limit else
    let s0, s1 = if len0 > len1 then s0, s1 else s1, s0 in
    let len0, len1 = if len0 > len1 then len0, len1 else len1, len0 in
    let rec loop row_minus2 row_minus1 row i len0 limit s0 s1 =
      if i > len0 then row_minus1.(Array.length row_minus1 - 1) else
      let len1 = Array.length row - 1 in
      let row_min = ref Int.max_int in
      row.(0) <- i;
      let jmax =
        let jmax = Int.min len1 (i + limit - 1) in
        if jmax < 0 then (* overflow *) len1 else jmax
      in
      for j = Int.max 1 (i - limit) to jmax do
        let cost = if Uchar.equal s0.(i-1) s1.(j-1) then 0 else 1 in
        let min = minimum
            (row_minus1.(j-1) + cost) (* substitute *)
            (row_minus1.(j) + 1)      (* delete *)
            (row.(j-1) + 1)           (* insert *)
            (* Note when j = i - limit, the latter [row] read makes a bogus read
               on the value that was in the matrix at d.(i-2).(i - limit - 1).
               Since by induction for all i,j, d.(i).(j) >= abs (i - j),
               (row.(j-1) + 1) is greater or equal to [limit] and thus does
               not affect adversely the minimum computation. *)
        in
        let min =
          if (i > 1 && j > 1 &&
              Uchar.equal s0.(i-1) s1.(j-2) &&
              Uchar.equal s0.(i-2) s1.(j-1))
          then Int.min min (row_minus2.(j-2) + cost) (* transpose *)
          else min
        in
        row.(j) <- min;
        row_min := Int.min !row_min min;
      done;
      if !row_min >= limit then (* can no longer decrease *) limit else
      loop row_minus1 row row_minus2 (i + 1) len0 limit s0 s1
    in
    let ignore =
      (* Value used to make the values around the diagonal stripe ignored
         by the min computations when we have a limit. *)
      limit + 1
    in
    let row_minus2 = Array.make (len1 + 1) ignore in
    let row_minus1 = Array.init (len1 + 1) (fun x -> x) in
    let row = Array.make (len1 + 1) ignore in
    let d = loop row_minus2 row_minus1 row 1 len0 limit s0 s1 in
    if d > limit then limit else d

  let edit_distance ?limit s0 s1 =
    let us0 = uchar_array_of_utf_8_string s0 in
    edit_distance' ?limit s0 us0 s1

  let default_max_dist s = match utf_8_uchar_length s with
  | 0 | 1 | 2 -> 0
  | 3 | 4 -> 1
  | _ -> 2

  let spellcheck ?(max_dist = default_max_dist) iter_dict s =
    let min = ref (max_dist s) in
    let acc = ref [] in
    let select_words s us word =
      let d = edit_distance' ~limit:(!min + 1) s us word in
      if d = !min then (acc := word :: !acc) else
      if d < !min then (min := d; acc := [word]) else ()
    in
    let us = uchar_array_of_utf_8_string s in
    iter_dict (select_words s us);
    List.rev !acc

  (* Escaping and unescaping bytes

     XXX: limitation cannot escape multiple bytes. Multibyte could be achieved
     by tweaking the sigs to return integer pairs but that would allocate
     quite a bit. *)

  let byte_replaced_length char_len s =
    let rec loop s max i l = match i > max with
    | true -> l
    | false -> loop s max (i + 1) (l + char_len s.[i])
    in
    loop s (String.length s - 1) 0 0

  let byte_replace set_char s ~len ~replaced_len =
    let b = Bytes.create replaced_len in
    let rec loop s max i k = match i > max with
    | true -> Bytes.unsafe_to_string b
    | false -> loop s max (i + 1) (set_char b k s.[i])
    in
    loop s (len - 1) 0 0

  let byte_escaper char_len set_char s =
    let len = String.length s in
    let replaced_len = byte_replaced_length char_len s in
    match replaced_len = len with
    | true -> s
    | false -> byte_replace set_char s ~len ~replaced_len

  let byte_replacer char_len set_char s =
    let len = String.length s in
    let replaced_len = byte_replaced_length char_len s in
    byte_replace set_char s ~len ~replaced_len

  exception Illegal_escape of int (* index *)

  let byte_unreplace_length char_len_at s =
    let rec loop max i len = match i > max with
    | true -> len
    | false ->
        let esc_len = char_len_at s i in
        loop max (i + esc_len) (len - esc_len + 1)
    in
    loop (String.length s - 1) 0 (String.length s)

  let byte_unreplace set_char s ~len ~unreplace_len =
    let b = Bytes.create unreplace_len in
    let rec loop s max i k = match i > max with
    | true -> Ok (Bytes.unsafe_to_string b)
    | false -> loop s max (set_char b k s i) (k + 1)
    in
    loop s (String.length s - 1) 0 0

  let byte_unescaper char_len_at set_char s =
    try
      let len = String.length s in
      let unreplace_len = byte_unreplace_length char_len_at s in
      match len = unreplace_len with
      | true -> Ok s
      | false -> byte_unreplace set_char s ~len ~unreplace_len
    with
    | Illegal_escape i -> Error i

  let byte_unreplacer char_len_at set_char s =
    try
      let len = String.length s in
      let unreplace_len = byte_unreplace_length char_len_at s in
      byte_unreplace set_char s ~len ~unreplace_len
    with
    | Illegal_escape i -> Error i

  (* US-ASCII string support *)

  module Ascii = struct

    (* Predicates *)

    let is_valid s =
      let rec loop max i = match i > max with
      | true -> true
      | false when unsafe_get s i > Char.Ascii.max -> false
      | false -> loop max (i + 1)
      in
      loop (String.length s - 1) 0

    (* Casing transforms *)

    let caseify is_not_case to_case s =
      let max_idx = length s - 1 in
      let caseify b i =
        for k = i to max_idx do
          Bytes.unsafe_set b k (to_case (unsafe_get s k))
        done;
        Bytes.unsafe_to_string b
      in
      let rec try_no_alloc i =
        if i > max_idx then s else
        if is_not_case (unsafe_get s i) then caseify (Bytes.of_string s) i else
        try_no_alloc (i + 1)
      in
      try_no_alloc 0

    let uppercase s = caseify Char.Ascii.is_lower Char.Ascii.uppercase s
    let lowercase s = caseify Char.Ascii.is_upper Char.Ascii.lowercase s

    let caseify_first is_not_case to_case s =
      if length s = 0 then s else
      let c = unsafe_get s 0 in
      if not (is_not_case c) then s else
      let b = Bytes.of_string s in
      Bytes.unsafe_set b 0 (to_case c);
      Bytes.unsafe_to_string b

    let capitalize s = caseify_first Char.Ascii.is_lower Char.Ascii.uppercase s
    let uncapitalize s =
      caseify_first Char.Ascii.is_upper Char.Ascii.lowercase s

    (* Converting to US-ASCII hexadecimal characters *)

    let to_hex s =
      let rec loop max s i h k = match i > max with
      | true -> Bytes.unsafe_to_string h
      | false ->
          let c = String.get_uint8 s i in
          Bytes.set h (k    ) (Char.Ascii.lower_hex_digit_of_int (c lsr 4));
          Bytes.set h (k + 1) (Char.Ascii.lower_hex_digit_of_int (c      ));
          loop max s (i + 1) h (k + 2)
      in
      let len = String.length s in
      let h = Bytes.create (2 * len) in
      loop (len - 1) s 0 h 0

    let of_hex' h =
      let hex_value s i = match s.[i] with
      | '0' .. '9' as c -> Char.code c - 0x30
      | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
      | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
      | _ -> raise_notrace (Illegal_escape i)
      in
      match String.length h with
      | len when len mod 2 <> 0 -> Error len
      | len ->
          let rec loop max s i h k = match i > max with
          | true -> Ok (Bytes.unsafe_to_string s)
          | false ->
              let hi = hex_value h k and lo = hex_value h (k + 1) in
              Bytes.set s i (Char.chr @@ (hi lsl 4) lor lo);
              loop max s (i + 1) h (k + 2)
          in
          let s_len = len / 2 in
          let s = Bytes.create s_len in
          try loop (s_len - 1) s 0 h 0 with Illegal_escape i -> Error i

    let of_hex h = match of_hex' h with
    | Ok _ as v -> v
    | Error i ->
        match i = String.length h with
        | true -> Error "Missing final hex digit"
        | false -> Fmt.error "Byte %d: not an ASCII hexadecimal digit" i

    (* Converting to printable US-ASCII characters *)

    let set_ascii_unicode_escape b k c = (* for c <= 0x7F *)
      let byte = Char.code c in
      Bytes.blit_string "\\u{00" 0 b k 5;
      Bytes.set b (k + 5) (Char.Ascii.upper_hex_digit_of_int (byte lsr 4));
      Bytes.set b (k + 6) (Char.Ascii.upper_hex_digit_of_int (byte      ));
      Bytes.set b (k + 7) '}';
      k + 8

    let set_hex_escape b k c =
      let byte = Char.code c in
      Bytes.blit_string "\\x" 0 b k 2;
      Bytes.set b (k + 2) (Char.Ascii.upper_hex_digit_of_int (byte lsr 4));
      Bytes.set b (k + 3) (Char.Ascii.upper_hex_digit_of_int (byte      ));
      k + 4

    let set_symbol_escape b k symbol =
      Bytes.set b k '\\'; Bytes.set b (k + 1) symbol; k + 2

    let escape =
      let char_len = function
      | '\x20' .. '\x5B' | '\x5D' .. '\x7E' -> 1
      | _ (* hex escape *) -> 4
      in
      let set_char b k = function
      | '\x20' .. '\x5B' | '\x5D' .. '\x7E' as c -> Bytes.set b k c; k + 1
      | c -> set_hex_escape b k c
      in
      byte_escaper char_len set_char

    let unescape =
      let char_len_at s i = match s.[i] <> '\\' with
      | true -> 1
      | false ->
          let max = String.length s - 1 in
          let j = i + 1 in
          if j > max then raise_notrace (Illegal_escape i) else
          if s.[j] <> 'x' then raise_notrace (Illegal_escape i) else
          let j = i + 3 in
          if j > max then raise_notrace (Illegal_escape i) else
          if Char.Ascii.is_hex_digit s.[i + 2] &&
             Char.Ascii.is_hex_digit s.[i + 3]
          then 4
          else raise (Illegal_escape i) (* invalid esc *)
      in
      let set_char b k s i = match s.[i] <> '\\' with
      | true -> Bytes.set b k s.[i]; i + 1
      | false ->
          (* assert (s.[i+1] = 'x') *)
          let hi = Char.Ascii.hex_digit_to_int s.[i + 2] in
          let lo = Char.Ascii.hex_digit_to_int s.[i + 3] in
          Bytes.set b k (Char.chr @@ (hi lsl 4) lor lo); i + 4
      in
      byte_unescaper char_len_at set_char

    let ocaml_string_escape =
      let char_len = function
      | '\b' | '\t' | '\n' | '\r' | '"' | '\\' -> 2
      | '\x20' .. '\x7E' -> 1
      | _ (* hex escape *) -> 4
      in
      let set_char b k = function
      | '\b' -> set_symbol_escape b k 'b'
      | '\t' -> set_symbol_escape b k 't'
      | '\n' -> set_symbol_escape b k 'n'
      | '\r' -> set_symbol_escape b k 'r'
      | '"'  -> set_symbol_escape b k '"'
      | '\\' -> set_symbol_escape b k '\\'
      | '\x20' .. '\x7E' as c -> Bytes.set b k c; k + 1
      | c -> set_hex_escape b k c
      in
      byte_escaper char_len set_char

    let ocaml_unescape =
      let char_len_at s i = match s.[i] <> '\\' with
      | true -> 1
      | false ->
          let max = String.length s - 1 in
          let j = i + 1 in
          if j > max then raise_notrace (Illegal_escape i) else
          match s.[j] with
          | 'x' ->
              let j = i + 3 in
              if j > max then raise_notrace (Illegal_escape i) else
              if Char.Ascii.is_hex_digit s.[i + 2] &&
                 Char.Ascii.is_hex_digit s.[i + 3]
              then 4
              else raise_notrace (Illegal_escape i)
          | 'b' | 't' | 'n' | 'r' | ' ' | '"' | '\\' -> 2
          | 'o' ->
              let j = i + 4 in
              if j > max then raise_notrace (Illegal_escape i) else
              let is_octal = function '0' .. '7' -> true | _ -> false in
              if is_octal s.[i + 2] && is_octal s.[i + 3] && is_octal s.[i + 4]
              then 5
              else raise_notrace (Illegal_escape i)
          | c when Char.Ascii.is_digit c ->
              let j = i + 3 in
              if j > max then raise_notrace (Illegal_escape i) else
              if Char.Ascii.is_digit s.[i + 2] && Char.Ascii.is_digit s.[i + 3]
              then 4
              else raise_notrace (Illegal_escape i)
          | _ -> raise_notrace (Illegal_escape i)
      in
      let set_char b k s i =
        if s.[i] <> '\\' then (Bytes.set b k s.[i]; i + 1) else
        match s.[i + 1] with
        | 'x' ->
            let hi = Char.Ascii.hex_digit_to_int s.[i + 2] in
            let lo = Char.Ascii.hex_digit_to_int s.[i + 3] in
            Bytes.set b k (Char.chr @@ (hi lsl 4) lor lo); i + 4
        | '\\' -> Bytes.set b k '\\'; i + 2
        | 'b' -> Bytes.set b k '\b'; i + 2
        | 't' -> Bytes.set b k '\t'; i + 2
        | 'n' -> Bytes.set b k '\n'; i + 2
        | 'r' -> Bytes.set b k '\r'; i + 2
        | ' ' -> Bytes.set b k ' '; i + 2
        | '"' -> Bytes.set b k '"'; i + 2
        | 'o' ->
            let o3 = Char.Ascii.hex_digit_to_int s.[i + 2] in
            let o2 = Char.Ascii.hex_digit_to_int s.[i + 3] in
            let o1 = Char.Ascii.hex_digit_to_int s.[i + 4] in
            let byte = o3 * 64 + o2 * 8 + o1 in
            if byte > 255 then raise_notrace (Illegal_escape i) else
            Bytes.set b k (Char.chr byte); i + 5
        | c when Char.Ascii.is_digit c ->
            let d3 = Char.Ascii.hex_digit_to_int s.[i + 1] in
            let d2 = Char.Ascii.hex_digit_to_int s.[i + 2] in
            let d1 = Char.Ascii.hex_digit_to_int s.[i + 3] in
            let byte = d3 * 100 + d2 * 10 + d1 in
            if byte > 255 then raise_notrace (Illegal_escape i) else
            Bytes.set b k (Char.chr byte); i + 4
        | _ -> assert false
      in
      byte_unescaper char_len_at set_char
  end

  (* Version strings *)

  type version = int * int * int * string option

  let drop_initial_v s =
    if s = "" then s else match s.[0] with
    | 'v' | 'V' -> subrange ~first:1 s
    | _ -> s

  let to_version s =
    if s = "" then None else
    let cut_left_plus_or_tilde s =
      let cut = match String.index_opt s '+', String.index_opt s '~' with
      | None, None -> None
      | (Some _ as i), None | None, (Some _ as i) -> i
      | Some i, Some i' -> Some (if i < i' then i else i')
      in
      match cut with
      | None -> None
      | Some i -> Some (subrange ~last:(i - 1) s, subrange ~first:i s)
    in
    try match cut_left ~sep:"." s with
    | None -> None
    | Some (maj, rest) ->
        let maj = int_of_string (drop_initial_v maj) in
        match cut_left ~sep:"." rest with
        | None ->
            begin match cut_left_plus_or_tilde rest with
            | None -> Some (maj, int_of_string rest, 0, None)
            | Some (min, i) ->  Some (maj, int_of_string min, 0, Some i)
            end
        | Some (min, rest) ->
            let min = int_of_string min in
            begin match cut_left_plus_or_tilde rest with
            | None -> Some (maj, min, int_of_string rest, None)
            | Some (p, i) -> Some (maj, min, int_of_string p, Some i)
            end
    with
    | Failure _ -> None

  let of_version (major, minor, patchlevel, info) =
    Fmt.str "%d.%d.%d%a" major minor patchlevel Fmt.(option string) info

  let version_tty = [`Bold; `Fg `Magenta]
  let pp_version ppf v = Fmt.st version_tty ppf (of_version v)
  let pp_version_str ppf v = Fmt.st version_tty ppf v

  (* CommonMark toy stuff *)

  let commonmark_first_section ~preamble md =
    let atx_heading s (* trimmed *) =
      (* approximate https://spec.commonmark.org/0.29/#atx-headings *)
      let num, title = span_left (Char.equal '#') s in
      let num = String.length num and tlen = String.length title in
      if num = 0 || num > 6 then None else
      if tlen = 0 then Some (num, "") else
      if title.[0] <> ' ' then None else
      Some (num, String.trim title)
    in
    let setex_heading s (* trimmed *) =
      (* approximate https://spec.commonmark.org/0.29/#setext-headings *)
      if String.starts_with ~prefix:"==" s then Some 1 else
      if String.starts_with ~prefix:"--" s then Some 2 else
      None
    in
    let nl = "\n" in
    let prev_lines prev = String.trim (String.concat nl (List.rev prev)) in
    let rec find_content num title prev = function
    | [] -> Some (title, String.concat nl (List.rev prev))
    | l :: ls ->
        let ltrim = String.trim l in
        match atx_heading ltrim with
        | Some (n, _) when n <= num || preamble -> Some (title, prev_lines prev)
        | Some _ -> find_content num title (l :: prev) ls
        | None ->
            match setex_heading ltrim with
            | Some n when n <= num || preamble ->
                let prev = match prev with [] -> [] | _ :: prev -> prev in
                Some (title, prev_lines prev)
            | Some _ | None -> find_content num title (l :: prev) ls
    and find_heading prev = function
    | [] -> None
    | l :: ls ->
        let l = String.trim l in
        if is_empty l then find_heading [] ls else
        match atx_heading l with
        | Some (num, title) -> find_content num title [] ls
        | None ->
            match setex_heading l with
            | None -> find_heading (l :: prev) ls
            | Some num ->
                let title = prev_lines prev in
                find_content num title [] ls
    in
    let add_line _ ls l = l :: ls in
    let rev_lines = fold_ascii_lines ~strip_newlines:true add_line [] md
    in
    find_heading [] (List.rev rev_lines)

  (* ANSI escape stripping *)

  let strip_ansi_escapes s =
    let len = String.length s in
    let b = Buffer.create len in
    let max = len - 1 in
    let flush start stop =
      if start < 0 || start > max then () else
      Buffer.add_substring b s start (stop - start + 1)
    in
    let rec skip_esc i =
      if i > max then loop i i else
      let k = i + 1 in if s.[i] = 'm' then loop k k else skip_esc k
    and loop start i = match i > max with
    | true ->
        if Buffer.length b = len then s else
        (flush start max; Buffer.contents b)
    | false ->
        match s.[i] with
        | '\x1B' -> flush start (i - 1); skip_esc (i + 1)
        | _ -> loop start (i + 1)
    in
    loop 0 0

  (* String map and sets *)

  module Set = struct
    include Set.Make (String)
    let pp ?sep pp_elt = Fmt.iter ?sep iter pp_elt
    let pp_dump ppf ss = Fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:Fmt.sp pp_dump) ss
  end

  module Map = struct
    include Map.Make (String)
    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
    let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

    let add_to_list k v m = match find k m with
    | exception Not_found -> add k [v] m
    | l -> add k (v :: l) m

    let add_to_set
        (type set) (type elt)
        (module S : Stdlib.Set.S with type elt = elt and type t = set)
        k v m = match find k m with
    | exception Not_found -> add k (S.singleton v) m
    | set -> add k (S.add v set) m

    let get_or_suggest k m = match find_opt k m with
    | Some v -> Ok v
    | None ->
        let add_sugg k' v acc =
          if edit_distance k k' <= 2 then k' :: acc else acc
        in
        Error (List.rev (fold add_sugg m []))

    let get_or_hint ?(pp_key = Fmt.code) ~kind k m =
      match get_or_suggest k m with
      | Ok _ as v -> v
      | Error suggs ->
          let kind ppf () = Fmt.string ppf kind in
          let hint = Fmt.did_you_mean in
          let pp = Fmt.unknown' ~kind pp_key ~hint in
          Fmt.error "@[%a@]" pp (k, suggs)

    let pp ?sep pp_binding = Fmt.iter_bindings ?sep iter pp_binding
    let pp_dump_str = pp_dump
    let pp_dump pp_v ppf m =
      let pp_binding ppf (k, v) =
        Fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]" pp_dump_str k pp_v v
      in
      Fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:Fmt.sp pp_binding) m

    let pp_dump_string_map ppf m = pp_dump pp_dump_str ppf m
  end

  (* Uniqueness *)

  let distinct ss =
    let rec loop seen acc = function
    | [] -> List.rev acc
    | s :: ss when Set.mem s seen -> loop seen acc ss
    | s :: ss -> loop (Set.add s seen) (s :: acc) ss
    in
    loop Set.empty [] ss

  let unique ?(limit = 1_000_000_000) ~exists n =
    let rec loop i n = match i > limit with
    | true ->
        Fmt.invalid_arg "Could not make %s unique after %d retries." n limit
    | false ->
        let r = Fmt.str "%s~%d" n i in
        if exists r then loop (i + 1) n else r
    in
    if exists n then loop 1 n else n

  (* Substituting *)

  let subst_pct_vars ?buf vars s =
    let max = String.length s - 1 in
    let buf = match buf with
    | None -> Buffer.create (max + 1)
    | Some buf -> Buffer.clear buf; buf
    in
    let add buf s ~start ~last =
      Buffer.add_substring buf s start (last - start + 1)
    in
    let rec find_var_end s i max = match i > max with
    | true -> None
    | false ->
        if i + 1 > max then None else
        if s.[i] = '%' && s.[i + 1] = '%' then Some (i + 1) else
        find_var_end s (i + 1) max
    in
    let rec loop buf s start i max = match i > max with
    | true ->
        if start = 0 then s else
        if start > max then Buffer.contents buf else
        (add buf s ~start ~last:max; Buffer.contents buf)
    | false ->
        if i + 4 > max then loop buf s start (max + 1) max else
        if s.[i] <> '%' then loop buf s start (i + 1) max else
        if s.[i + 1] <> '%' then loop buf s start (i + 2) max else
        match find_var_end s (i + 3) max with
        | None -> loop buf s start (max + 1) max
        | Some k ->
            let var = subrange ~first:(i + 2) ~last:(k - 2) s in
            match vars var with
            | None -> loop buf s start (k + 1) max
            | Some value ->
                add buf s ~start ~last:(i - 1);
                Buffer.add_string buf value;
                loop buf s (k + 1) (k + 1) max
    in
    loop buf s 0 0 max
end

module List = struct
  include List

  let rec find_map f = function
  | [] -> None
  | v :: vs -> match f v with Some _ as r -> r | None -> find_map f vs

  let concat_map f l =
    let rec loop f acc = function
    | [] -> rev acc
    | v :: vs -> loop f (List.rev_append (f v) acc) vs
    in
    loop f [] l

  let classify
      (type a) (type b)
      ?(cmp_elts : a -> a -> int = Stdlib.compare)
      ?(cmp_classes : b -> b -> int = Stdlib.compare)
      ~classes:(classes : (a -> b list)) els
    =
    let module S = Set.Make (struct type t = a let compare = cmp_elts end) in
    let module M = Map.Make (struct type t = b let compare = cmp_classes end) in
    let add_classes acc p =
      let add_class acc c = try M.add c (S.add p (M.find c acc)) acc with
      | Not_found -> M.add c (S.singleton p) acc
      in
      List.fold_left add_class acc (classes p)
    in
    let classes = List.fold_left add_classes M.empty els in
    List.rev (M.fold (fun c els acc -> (c, S.elements els) :: acc) classes [])

  let distinct (type a) (compare : a -> a -> int) vs =
    let module S = Set.Make (struct type t = a let compare = compare end) in
    let rec loop seen acc = function
    | [] -> List.rev acc
    | v :: vs ->
        if S.mem v seen then loop seen acc vs else
        loop (S.add v seen) (v :: acc) vs
    in
    loop S.empty [] vs

  let rec fold_stop_on_error f l acc = match l with
  | [] -> Ok acc
  | v :: vs ->
      match f v acc with
      | Ok acc -> fold_stop_on_error f vs acc
      | Error _ as e -> e

  let rec iter_stop_on_error f = function
  | [] -> Ok ()
  | v :: vs ->
      match f v with Error _ as e -> e | Ok v -> iter_stop_on_error f vs

  let rec iter_iter_on_error ~error f = function
  | [] -> ()
  | v :: vs ->
      (match f v with Error _ as e -> error e | Ok _ -> ());
      iter_iter_on_error ~error f vs
end

(* File paths *)

module Fpath = struct

  (* Errors *)

  let err_invalid_seg s = Fmt.str "%a: Invalid path segment" String.pp_dump s
  let err_start s = Fmt.error "%a: Not a path" String.pp_dump s
  let err_null s = Fmt.error "%a: Not a path: has null bytes" String.pp_dump s
  let err_empty s = Fmt.error "%a: Not a path: is empty" String.pp_dump s

  (* Pct encoding *)

  let pct_esc_len ~escape_space = function
  | '%' | '#' | '?' -> 3
  | ' ' when escape_space -> 3
  | c when Char.Ascii.is_control c -> 3
  | _ -> 1

  let set_pct_encoded b i c =
    let c = Char.code c in
    let hi = Char.Ascii.upper_hex_digit_of_int (c lsr 4) in
    let lo = Char.Ascii.upper_hex_digit_of_int (c      ) in
    Bytes.set b i '%'; Bytes.set b (i + 1) hi; Bytes.set b (i + 2) lo;
    i + 3

  let pct_esc_set_char ~escape_space b i = function
  | '%' | '#' | '?' as c -> set_pct_encoded b i c
  | ' ' as c when escape_space -> set_pct_encoded b i c
  | c when Char.Ascii.is_control c -> set_pct_encoded b i c
  | c -> Bytes.set b i c; i + 1

  (* Platform specifics. *)

  let undouble_sep sep dbl_sep s =
    let rec loop last_is_sep b k s i max = match i > max with
    | true -> Bytes.unsafe_to_string b
    | false ->
        let c = String.get s i in
        let c_is_sep = Char.equal c sep && i <> 0 (* handle // *) in
        let is_dbl = last_is_sep && c_is_sep in
        match is_dbl with
        | true -> loop c_is_sep b k s (i + 1) max
        | false -> Bytes.set b k c; loop c_is_sep b (k + 1) s (i + 1) max
    in
    let len = String.length s in
    loop false (Bytes.create (len - dbl_sep)) 0 s 0 (len - 1)

  module Windows = struct

    (* XXX the {of_string,path_start} needs reviewing/testing *)

    let dir_sep_char = '\\'
    let char_is_dir_sep c = c = '\\' || c = '/'
    let dir_sep = "\\"
    let has_dir_sep s =
      String.exists (function '/' | '\\' -> true | _ -> false) s

    let is_seg s =
      let valid c = c <> dir_sep_char && c <> '/' && c <> '\x00' in
      String.for_all valid s

    let is_unc_path p = String.starts_with ~prefix:"\\\\" p
    let has_drive p = String.exists (Char.equal ':') p
    let non_unc_path_start p = match String.rindex p ':' with
    | exception Not_found -> 0
    | i -> i + 1 (* exists by construction once injected *)

    let path_start p = (* once [p] is injected this does not raise *)
      if not (is_unc_path p) then non_unc_path_start p else
      let plen = String.length p in
      if plen = 2 then raise Not_found else
      let sep_from p from = String.index_from p from dir_sep_char in
      let i = sep_from p 2 in
      let j = sep_from p (i + 1) in
      match p.[i - 1] with
      | '.' when i = 3 -> j
      | '?' when i = 3 ->
          if p.[j - 1] = ':' then j else
          if i + 3 < plen
          && p.[i + 1] = 'U' && p.[i + 2] = 'N' && p.[i + 3] = 'C'
          then sep_from p (sep_from p (j + 1) + 1)
          else sep_from p (j + 1)
      | _ -> sep_from p j

    let last_non_empty_seg_start p = match String.rindex p dir_sep_char with
    | exception Not_found -> path_start p
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) dir_sep_char with
            | exception Not_found -> path_start p
            | k -> k + 1

    let chop_volume p = String.subrange ~first:(path_start p) p

    let backslashify s =
      let b = Bytes.copy (Bytes.unsafe_of_string s) in
      for i = 0 to Bytes.length b - 1 do
        if Bytes.get b i = '/' then Bytes.set b i '\\'
      done;
      Bytes.unsafe_to_string b

    let of_string s =
      if s = "" then err_empty s else
      try
        let p =
          let rec loop has_slash last_is_sep dbl_sep i max = match i > max with
          | true ->
              let s = if has_slash then backslashify s else s in
              if dbl_sep > 0 then undouble_sep dir_sep_char dbl_sep s else s
          | false ->
              let c = String.unsafe_get s i in
              if Char.equal c '\x00' then raise Exit else
              let is_slash = Char.equal c '/' in
              let has_slash = has_slash || is_slash in
              let c_is_sep = (is_slash || Char.equal c dir_sep_char) && i <> 0
              in
              let is_dbl = last_is_sep && c_is_sep in
              let dbl_sep = if is_dbl then dbl_sep + 1 else dbl_sep in
              loop has_slash c_is_sep dbl_sep (i + 1) max
          in
          loop false false 0 0 (String.length s - 1)
        in
        match path_start p with
        | exception Not_found -> err_start p
        | n ->
            let p = match n = String.length p with
            | true -> (* add root if there's only a UNC volume *) p ^ dir_sep
            | false -> p
            in
            Ok p
      with Exit -> err_null s

    let append p0 p1 =
      if is_unc_path p1 || has_drive p1 || p1.[0] = dir_sep_char
      then (* with volume or absolute *) p1 else
      let p0_last_is_sep = p0.[String.length p0 - 1] = dir_sep_char in
      let sep = if p0_last_is_sep then "" else dir_sep in
      String.concat sep [p0; p1]

    let is_rel p =
      not (is_unc_path p) && p.[non_unc_path_start p] <> dir_sep_char

    let is_root p = p.[path_start p] = dir_sep_char

    let to_url_path ?(escape_space = true) p =
      let set_char b i = function
      | '\\' -> Bytes.set b i '/'; i + 1
      | c -> pct_esc_set_char ~escape_space b i c
      in
      String.byte_escaper (pct_esc_len ~escape_space) set_char p
  end

  module Posix = struct
    let dir_sep_char = '/'
    let char_is_dir_sep c = Char.equal c '/'
    let dir_sep = "/"
    let has_dir_sep s = String.exists (function '/' -> true | _ -> false) s
    let is_seg s = String.for_all (fun c -> c <> dir_sep_char && c <> '\x00') s
    let of_string = function
    | "" as s -> err_empty s
    | s ->
        try
          let rec loop last_is_sep dbl_sep i max = match i > max with
          | true ->
              if dbl_sep > 0 then Ok (undouble_sep dir_sep_char dbl_sep s) else
              Ok s
          | false ->
              let c = String.unsafe_get s i in
              if Char.equal c '\x00' then raise Exit else
              let c_is_sep = Char.equal c dir_sep_char && i <> 0 in
              let is_dbl = last_is_sep && c_is_sep in
              let dbl_sep = if is_dbl then dbl_sep + 1 else dbl_sep in
              loop c_is_sep dbl_sep (i + 1) max
          in
          loop false 0 0 (String.length s - 1)
        with
        | Exit -> err_null s

    let last_non_empty_seg_start p = match String.rindex p dir_sep_char with
    | exception Not_found -> 0
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) dir_sep_char with
            | exception Not_found -> 0
            | k -> k + 1

    let path_start p = if String.equal p "//" then 1 else 0
    let chop_volume p = p
    let append p0 p1 =
      if p1.[0] = dir_sep_char (* absolute *) then p1 else
      let p0_last_is_sep = p0.[String.length p0 - 1] = dir_sep_char in
      let sep = if p0_last_is_sep then "" else dir_sep in
      String.concat sep [p0; p1]

    let is_rel p = p.[0] <> dir_sep_char
    let is_root p = String.equal p dir_sep || String.equal p "//"

    let to_url_path ?(escape_space = true) p =
      let esc_len = pct_esc_len ~escape_space in
      String.byte_escaper esc_len (pct_esc_set_char ~escape_space) p
  end

  let path_start = if Sys.win32 then Windows.path_start else Posix.path_start
  let chop_volume = if Sys.win32 then Windows.chop_volume else Posix.chop_volume

  (* Separators and segments *)

  let dir_sep_char =
    if Sys.win32 then Windows.dir_sep_char else Posix.dir_sep_char

  let dir_sep = if Sys.win32 then Windows.dir_sep else Posix.dir_sep
  let char_is_dir_sep =
    if Sys.win32 then Windows.char_is_dir_sep else Posix.char_is_dir_sep

  let last_is_dir_sep p = Char.equal (p.[String.length p - 1]) dir_sep_char

  let has_dir_sep =
    if Sys.win32 then Windows.has_dir_sep else Posix.has_dir_sep

  let is_seg = if Sys.win32 then Windows.is_seg else Posix.is_seg
  let is_rel_seg = function "." | ".." -> true | _ -> false

  let last_seg_len p = match String.rindex p dir_sep_char with
  | exception Not_found -> String.length p
  | k -> String.length p - (k + 1)

  let last_non_empty_seg_start =
    if Sys.win32 then Windows.last_non_empty_seg_start else
    Posix.last_non_empty_seg_start

  (* Paths *)

  type t = string (* N.B. a path is never "" *)
  let of_string = if Sys.win32 then Windows.of_string else Posix.of_string
  let to_string p = p
  let v s = match of_string s with Ok p -> p | Error m -> invalid_arg m
  let fmt fmt = Fmt.kstr v fmt
  let add_seg' p seg =
    if not (is_seg seg) then invalid_arg (err_invalid_seg seg) else
    let sep = if last_is_dir_sep p then "" else dir_sep in
    String.concat sep [p; seg]

  let add_seg p seg = try Ok (add_seg' p seg) with
  | Invalid_argument e -> Error e

  let append = if Sys.win32 then Windows.append else Posix.append

  (* Famous file paths *)

  let null = v (if Sys.win32 then "NUL" else "/dev/null")
  let dash = v "-"

  (* Directory paths *)

  let is_dir_path p = (* check is . .. or ends with / /. or /.. *)
    let k = String.length p - 1 in
    if k < 0 then (* should not happen *) false else
    match p.[k] with
    | c when Char.equal c dir_sep_char -> true
    | '.' ->
        let k = k - 1 in
        if k < 0 then true else
        begin match p.[k] with
        | c when Char.equal c dir_sep_char -> true
        | '.' ->
            let k = k - 1 in
            k < 0 || Char.equal p.[k] dir_sep_char
        | _ -> false
        end
    | _ -> false

  let add_dir_sep p = add_seg' p ""

  let strip_dir_sep p = match String.length p with
  | 1 -> p
  | 2 ->
      if p.[0] <> dir_sep_char && p.[1] = dir_sep_char
      then String.of_char p.[0]
      else p
  | len ->
      let max = len - 1 in
      if p.[max] <> dir_sep_char then p else
      String.subrange p ~last:(max - 1)

  (* Strict prefixes *)

  let is_prefix pre p =
    String.starts_with ~prefix:pre p &&
    let suff_start = String.length pre in
    let p_len = String.length p in
    (* Check [prefix] and [p] are not equal modulo directoryness. *)
    if suff_start = p_len then false else
    if suff_start = p_len - 1 && p.[suff_start] = dir_sep_char then false else
    (* Check the prefix is segment based *)
    (pre.[suff_start - 1] = dir_sep_char || p.[suff_start] = dir_sep_char)

  let strip_prefix pre p =
    if not (is_prefix pre p) then None else
    let len = String.length pre in
    let first = if p.[len] = dir_sep_char then len + 1 else len in
    Some (String.subrange p ~first)

  let drop_prefixed dirs =
    let is_prefixed d by = is_prefix by d in
    let not_prefixed ~by:dirs d = not (List.exists (is_prefixed d) dirs) in
    List.filter (not_prefixed ~by:dirs) dirs

  let reroot ~src_root ~dst_root src =
    let rel_file = Option.get (strip_prefix src_root src) in
    append dst_root rel_file

  (* Predicates and comparisons *)

  let is_rel = if Sys.win32 then Windows.is_rel else Posix.is_rel
  let is_abs p = not (is_rel p)
  let is_root = if Sys.win32 then Windows.is_root else Posix.is_root

  (* FIXME this is wrong on windows. *)
  let current_dir_dir = "." ^ dir_sep
  let is_current_dir p = String.equal p "." || String.equal p current_dir_dir
  let parent_dir_dir = ".." ^ dir_sep
  let is_parent_dir p = String.equal p ".." || String.equal p parent_dir_dir

  let equal = String.equal
  let compare = String.compare

  (* File extensions *)

  type ext = string
  let ext_sep_char = '.'

  let rec ext_single_range spos epos k p =
    let i = String.rindex_from p k ext_sep_char (* raises if not fnd *) in
    match i <= spos with
    | true -> raise Not_found
    | false ->
        match not (Char.equal p.[i - 1] ext_sep_char) with
        | true -> i, epos
        | false -> ext_single_range spos epos (i - 1) p

  let rec ext_multi_range epos k p =
    let i = String.index_from p k ext_sep_char (* raises if not fnd *) in
    match i > epos with
    | true -> raise Not_found
    | false ->
        match not (Char.equal p.[i - 1] ext_sep_char) with
        | true -> i, epos
        | false -> ext_multi_range epos (i + 1) p

  let ext_range ?(multi = false) p =
    let plen = String.length p in
    let seg_start = last_non_empty_seg_start p in
    let seg_stop = match last_is_dir_sep p with
    | true -> plen - 2
    | false -> plen - 1
    in
    if seg_start >= seg_stop then raise Not_found else
    match multi with
    | true -> ext_multi_range seg_stop (seg_start + 1) p
    | false -> ext_single_range seg_start seg_stop seg_stop p

  let get_ext ?multi p = match ext_range ?multi p with
  | exception Not_found -> ""
  | first, last -> String.subrange ~first ~last p

  let has_ext e p = match ext_range ~multi:true p with
  | exception Not_found -> String.equal e ""
  | first, last ->
      let plen = last - first + 1 in
      let elen = String.length e in
      match plen < elen with
      | true -> false
      | false ->
          let rec loop pi ei = match ei < 0 with
          | true -> true
          | false -> Char.equal p.[pi] e.[ei] && loop (pi - 1) (ei - 1)
          in
          loop last (elen - 1)

  let mem_ext exts p = List.exists (fun ext -> has_ext ext p) exts

  let add_ext e p =
    let plen = String.length p in
    match last_is_dir_sep p with
    | false -> p ^ e
    | true ->
        let elen = String.length e in
        let nlen = plen + elen in
        let n = Bytes.create nlen in
        Bytes.blit_string p 0 n 0 (plen - 1);
        Bytes.blit_string e 0 n (plen - 1) elen;
        Bytes.set n (nlen - 1) dir_sep_char;
        Bytes.unsafe_to_string n

  let _rem_ext efirst elast p =
    let plen = String.length p in
    match elast = plen - 1 with
    | true -> String.subrange ~last:(efirst - 1) p
    | false ->
        let elen = elast - efirst + 1 in
        let nlen = plen - elen in
        let n = Bytes.create nlen in
        Bytes.blit_string p 0 n 0 nlen;
        Bytes.set n (nlen - 1) dir_sep_char;
        Bytes.unsafe_to_string n

  let strip_ext ?multi p = match ext_range ?multi p with
  | exception Not_found -> p
  | efirst, elast -> _rem_ext efirst elast p

  let set_ext ?multi e p = add_ext e (strip_ext ?multi p)

  let cut_ext ?multi p = match ext_range ?multi p with
  | exception Not_found -> p, ""
  | efirst, elast ->
      let ext = String.subrange ~first:efirst ~last:elast p in
      let p = _rem_ext efirst elast p in
      p, ext

  (* Basename and parent directory *)

  let basename ?(strip_ext = false) p =
    let max = String.length p - 1 in
    let first, last = match String.rindex p dir_sep_char with
    | exception Not_found -> (* B *) path_start p, max
    | k when k <> max || k = 0 -> (* /B or .../B *) k + 1, max
    | k -> (* .../ *)
        let j = k - 1 in
        match String.rindex_from p j dir_sep_char with
        | exception Not_found -> (* B/ *) path_start p, j
        | i -> (* .../B/ *) i + 1, j
    in
    match last - first + 1 with
    | 1 when p.[first] = '.' -> ""
    | 2 when p.[first] = '.' && p.[first + 1] = '.' -> ""
    | _ when not strip_ext -> String.subrange ~first ~last p
    | _ -> (* Drop multi ext *)
        let rec loop first last i = match i > last with
        | true -> String.subrange ~first ~last p
        | false ->
            match p.[i] = ext_sep_char with
            | false -> loop first last (i + 1)
            | true ->
                if p.[i - 1] = ext_sep_char then loop first last (i + 1) else
                String.subrange ~first ~last:(i - 1) p
        in
        loop first last (first + 1)

  let rec parent p =
    let plen = String.length p in
    let path_start = path_start p in
    let seg_first = last_non_empty_seg_start p in
    let seg_last = if last_is_dir_sep p then plen - 2 else plen - 1 in
    let seg_len = seg_last - seg_first + 1 in
    match seg_len with
    | 0 -> p
    | 1 when p.[seg_first] = '.' ->
        if seg_first = path_start then ".." else (* Chop '.' and try again *)
        parent (String.subrange ~last:(seg_first - 1) p)
    | 2 when p.[seg_first] = '.' && p.[seg_last] = '.' ->
        let via_dotdot p = add_seg' p ".." in
        via_dotdot p
    | _ when seg_first = path_start -> "."
    | _ -> add_seg' (String.subrange ~last:(seg_first - 1) p) ""

  let equal_basename p0 p1 = (* XXX could avoid alloc *)
    String.equal (basename p0) (basename p1)

  let relative ~to_dir p =
    (* FIXME this function needs to be rewritten *)
    (* XXX dirty, need a normalization function and/or a better parent
       to handle that. Also the results should be normalized again.  *)
    if String.includes ~affix:".." to_dir (* cmon that's obvi..ously wrong *)
    then Fmt.invalid_arg "%s: no dotdot allowed" p;
    let to_dir = add_dir_sep to_dir in
    match strip_prefix to_dir p with
    | Some q -> q
    | None ->
        let rec loop loc dir =
          if is_current_dir dir then p else
          if is_root dir then
            begin match strip_prefix "/" p with
            | None -> p
            | Some rel_root -> append loc rel_root
            end
          else
          match strip_prefix dir p with
          | Some q -> append loc q
          | None -> loop (add_seg' loc "..") (parent dir)
        in
        loop ".." (parent to_dir)

  (* Converting *)

  let to_url_path = if Sys.win32 then Windows.to_url_path else Posix.to_url_path
  let to_segments p = String.split_on_char dir_sep_char (chop_volume p)

  let pp_quoted ppf p = String.pp ppf (Filename.quote p)
  let pp_unquoted = String.pp
  let pp ppf p =
    if String.exists (Char.equal ' ') p
    then pp_quoted ppf p
    else String.pp ppf p

  let pp_dump = String.pp_dump

  let error p fmt = Fmt.error ("%a:" ^^ fmt) pp_unquoted p
  let prefix_msg p msg = Fmt.str "%a: %s" pp_unquoted p msg

  (* Uniqueness *)

  let distinct = String.distinct

  (* Path and sets *)

  type path = t
  module Set = struct
    let pp_set ppf ss =
      Fmt.pf ppf "@[<1>{%a}@]" (String.Set.pp ~sep:Fmt.sp pp) ss

    include String.Set
  end
  module Map = String.Map

  (* Sorts *)

  let sort_by_parent ps =
    let add_path p acc = Map.add_to_set (module Set) (parent p) p acc in
    Set.fold add_path ps Map.empty

  let sort_by_ext ?multi ps =
    let add_path p acc =
      String.Map.add_to_set (module Set) (get_ext ?multi p) p acc
    in
    Set.fold add_path ps String.Map.empty

  (* Search paths *)

  let search_path_sep = if Sys.win32 then ";" else ":"
  let list_of_search_path ?(sep = search_path_sep) path =
    let rec loop acc = function
    | ""  -> Ok (List.rev acc)
    | p ->
        let dir, p = match String.cut_left ~sep p with
        | None -> p, ""
        | Some (dir, p) -> dir, p
        in
        if dir = "" then loop acc p else
        match of_string dir with
        | Error e -> Fmt.error "search path %s: %S: %s" path dir e
        | Ok d -> loop (d :: acc) p
    in
    loop [] path

  (* Operators *)

  let ( / ) = add_seg'
  let ( // ) = append
  let ( + ) p e = add_ext e p
  let ( -+ ) p e = set_ext e p
end

(* Hash values and functions *)

module Hash = struct

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
end

(* Monotonic time stamps and spans *)
module Mtime = struct
  type uint64 = int64

  module Span = struct

    (* Time spans

       Represented by a nanosecond magnitude stored in an unsigned 64-bit
       integer. Allows to represent spans for ~584.5 Julian years. *)

    type t = uint64
    let zero = 0L
    let one = 1L
    let max_span = -1L
    let add = Int64.add
    let abs_diff s0 s1 = match Int64.unsigned_compare s0 s1 < 0 with
    | true ->  Int64.sub s1 s0
    | false -> Int64.sub s0 s1

    (* Predicates and comparisons *)

    let equal = Int64.equal
    let compare = Int64.unsigned_compare
    let is_shorter s ~than = compare s than < 0
    let is_longer s ~than = compare s than > 0

    (* Durations *)

    let ( * ) n s = Int64.mul (Int64.of_int n) s
    let ns   =                      1L
    let us   =                  1_000L
    let ms   =              1_000_000L
    let s    =          1_000_000_000L
    let min  =         60_000_000_000L
    let hour =       3600_000_000_000L
    let day  =      86400_000_000_000L
    let year = 31_557_600_000_000_000L

    (* Conversions *)

    let to_uint64_ns s = s
    let of_uint64_ns ns = ns

    let max_float_int = 9007199254740992. (* 2^53. *)
    let int64_min_int_float = Int64.to_float Int64.min_int
    let int64_max_int_float = Int64.to_float Int64.max_int

    let of_float_ns sf =
      if sf < 0. || sf >= max_float_int || not (Float.is_finite sf)
      then None else Some (Int64.of_float sf)

    let to_float_ns s =
      if Int64.compare 0L s <= 0 then Int64.to_float s else
      int64_max_int_float +. (-. int64_min_int_float +. Int64.to_float s)

    let pp = Fmt.uint64_ns_span
    let pp_ns ppf s = Fmt.pf ppf "%Luns" s
  end

  (* Timestamps *)

  type t = uint64

  let to_uint64_ns s = s
  let of_uint64_ns ns = ns
  let min_stamp = 0L
  let max_stamp = -1L
  let pp ppf s = Fmt.pf ppf "%Lu" s

   (* Predicates *)

  let equal = Int64.equal
  let compare = Int64.unsigned_compare
  let is_earlier t ~than = compare t than < 0
  let is_later t ~than = compare t than > 0

  (* Arithmetic *)

  let span t0 t1 = match compare t0 t1 < 0 with
  | true -> Int64.sub t1 t0
  | false -> Int64.sub t0 t1

  let add_span t s =
    let sum = Int64.add t s in
    if compare t sum <= 0 then Some sum else None

  let sub_span t s =
    if compare t s < 0 then None else Some (Int64.sub t s)
end

(* Command lines *)

module Cmd = struct

  (* Command lines *)

  type t =
  | A of string
  | Unstamp of t
  | Rseq of t list (* Sequence is reversed; only empty at toplevel *)

  let empty = Rseq []
  let rec is_empty = function Rseq [] -> true | _ -> false
  let arg a = A a
  let append l0 l1 = match l0, l1 with
  | Rseq [], l1 -> l1
  | l0, Rseq [] -> l0
  | Rseq ls, l  -> Rseq (l :: ls)
  | l1, l2 -> Rseq ([l2; l1])

  let unstamp = function
  | Rseq [] -> empty
  | l -> Unstamp l

  let ( % ) l a = append l (arg a)
  let ( %% ) = append

  (* Derived combinators *)

  let if' cond l = if cond then l else empty
  let if_some o = match o with Some cmd -> cmd | None -> empty
  let path p = A (Fpath.to_string p)
  let int i = A (string_of_int i)
  let float f = A (string_of_float f)
  let list ?slip l = match slip with
  | None -> Rseq (List.rev_map arg l)
  | Some slip -> Rseq (List.fold_left (fun acc v -> A v :: A slip :: acc) [] l)

  let of_list ?slip conv l = match slip with
  | None -> Rseq (List.rev_map (fun a -> A (conv a)) l)
  | Some slip ->
      let add acc v = A (conv v) :: A slip :: acc in
      Rseq (List.fold_left add [] l)

  let paths ?slip ps = of_list ?slip Fpath.to_string ps

  (* Converting *)

  let to_list l =
    let rec loop acc = function
    | A a -> a :: acc
    | Rseq ls -> List.fold_left loop acc ls
    | Unstamp l -> loop acc l
    in
    loop [] l

  let to_list_and_stamp l =
    let rec loop unstamped acc sg = function
    | A a -> (a :: acc), (if unstamped then sg else a :: sg)
    | Rseq ls ->
        let rec sub unstamped acc sg = function
        | [] -> acc, sg
        | l :: ls ->
            let acc, sg = loop unstamped acc sg l in
            sub unstamped acc sg ls
        in
        sub unstamped acc sg ls
    | Unstamp l -> loop true acc sg l
    in
    loop false [] [] l

  let to_stamp l =
    let rec loop acc = function
    | A a -> (a :: acc)
    | Rseq ls ->  List.fold_left loop acc ls
    | Unstamp l -> acc
    in
    loop [] l

  let of_string s =
  (* Parsing is loosely based on
     http://pubs.opengroup.org/onlinepubs/009695399/utilities/\
     xcu_chap02.html#tag_02_03

     XXX Rewrite, this was quickly ported from bos code based on
     Astring.String.sub *)
    try
      let err_unclosed kind _ =
        Fmt.failwith "unclosed %s quote delimited string" kind
      in
      let skip_white s = String.lose_left Char.Ascii.is_white s in
      let tok_sep c = c = '\'' || c = '\"' || Char.Ascii.is_white c in
      let tok_char c = not (tok_sep c) in
      let not_squote c = c <> '\'' in
      let tail s = (* Yikes *) String.subrange ~first:1 s in
      let parse_squoted s =
        let tok, rem = String.span_left not_squote (tail s) in
        if not (String.equal rem "") then tok, tail rem else
        err_unclosed "single" s
      in
      let parse_dquoted acc s =
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.span_left is_data s in
        match String.head rem with
        | Some '"' -> (data :: acc), (tail rem)
        | Some '\\' ->
            let rem = tail rem in
            begin match String.head rem with
            | Some ('"' | '\\' | '$' | '`' as c) ->
                let acc = String.(of_char c) :: data :: acc in
                loop acc (tail rem)
            | Some ('\n') -> loop (data :: acc) (tail rem)
            | Some c ->
                let acc = (data ^ (Fmt.str "\\%c" c)) :: acc in
                loop acc (tail rem)
            | None ->
                err_unclosed "double" s
            end
        | None -> err_unclosed "double" s
        | Some _ -> assert false
      in
      loop acc (tail s)
      in
      let parse_token s =
        let ret acc s = String.concat "" (List.rev acc), s in
        let rec loop acc s = match String.head s with
        | None -> ret acc s
        | Some c when Char.Ascii.is_white c -> ret acc s
        | Some '\'' ->
            let tok, rem = parse_squoted s in loop (tok :: acc) rem
        | Some '\"' ->
            let acc, rem = parse_dquoted acc s in loop acc rem
        | Some c ->
            let sat = tok_char in
            let tok, rem = String.span_left sat s in loop (tok :: acc) rem
        in
        loop [] s
      in
      let rec loop acc s = match String.equal s "" with
      | false ->
          let token, s = parse_token s in
          loop (A token :: acc) (skip_white s)
      | true ->
          match acc with
          | [a] -> a
          | acc -> Rseq acc
      in
      Ok (loop [] (skip_white s))
    with Failure err -> Fmt.error "command line %a: %s" String.pp_dump s err

  let to_string l = String.concat " " (List.map Filename.quote @@ to_list l)
  let pp ppf l = Fmt.pf ppf "@[%a@]" Fmt.(list ~sep:sp string) (to_list l)
  let pp_dump ppf l =
    let pp_arg ppf a = Fmt.string ppf (Filename.quote a) in
    Fmt.pf ppf "@[<h>%a@]" Fmt.(list ~sep:sp pp_arg) (to_list l)

  let pp_shell =
    let pp_arg ppf a = Fmt.string ppf (Filename.quote a) in
    let pp_cmd ppf l =
      let is_opt s = String.length s > 1 && s.[0] = '-' in
      match (to_list l) with
      | [] -> ()
      | s :: ss ->
          let rec loop ~last_is_opt ppf = function
          | [] -> ()
          | s :: ss ->
              let is_opt = is_opt s in
              (if last_is_opt && not is_opt
               then Fmt.char ppf ' ' else Fmt.sp ppf ());
              pp_arg ppf s; loop ~last_is_opt:is_opt ppf ss
          in
          pp_arg ppf s; loop ~last_is_opt:(is_opt s) ppf ss
    in
    Fmt.suffix_lines ~suffix:" \\" pp_cmd

  let rec fold ~arg ~unstamp ~append ~empty = function
  | A a -> arg a
  | Unstamp c -> unstamp (fold ~arg ~unstamp ~append ~empty c)
  | Rseq l ->
      let append acc v = append (fold ~arg ~unstamp ~append ~empty v) acc in
      List.fold_left append empty l

  let rec iter_enc ~arg ~unstamp ~append ~empty e = function
  | A a -> arg e a
  | Unstamp c -> unstamp e; iter_enc ~arg ~unstamp ~append ~empty e c
  | Rseq l ->
      let append e v = append e; iter_enc ~arg ~unstamp ~append ~empty e v; e in
      ignore (List.fold_left append e l); empty e

  (* Tools *)

  type tool = Fpath.t

  let tool = arg
  let rec find_tool = function
  | A a -> Result.to_option (Fpath.of_string a)
  | Unstamp l -> find_tool l
  | Rseq ls ->
      let rec loop = function
      | [l] -> find_tool l
      | l :: ls -> loop ls
      | [] -> None
      in
      loop ls

  let get_tool l = match find_tool l with
  | Some t -> Ok t
  | None when is_empty l -> Error "The command is empty"
  | None -> Fmt.error "%s: Not a tool" (to_string l)

  let rec set_tool tool = function
  | Rseq [] -> path tool
  | l ->
      let rec loop = function
      | A a -> A (Fpath.to_string tool)
      | Unstamp l -> Unstamp (loop l)
      | Rseq ls ->
          match List.rev ls with
          | arg :: args -> Rseq (List.rev @@ (loop arg) :: args)
          | [] -> assert false
      in
      loop l

  type tool_search = t -> (t, string) result

  (* Predicates *)

  let rec is_singleton = function
  | A a -> true
  | Unstamp l -> is_singleton l
  | Rseq _ -> false
end

(* Futures *)

let relax () = try Unix.sleepf 0.0001 with Unix.Unix_error _ -> ()

module Fut = struct
  type 'a state = Det of 'a | Undet of { mutable awaits : ('a -> unit) list }
  type 'a t = 'a state ref

  let rec kontinue ks v =
    let todo = ref ks in
    while match !todo with [] -> false | _ -> true do
      match !todo with k :: ks -> todo := ks; k v | [] -> ()
    done

  let set f v = match !f with
  | Det _ -> invalid_arg "The future is already set"
  | Undet u -> f := Det v; kontinue u.awaits v

  let _make () = ref (Undet { awaits = [] })
  let make () = let f = _make () in f, set f
  let value f = match !f with Det v -> Some v | _ -> None
  let await f k = match !f with
  | Det v -> k v | Undet u -> u.awaits <- k :: u.awaits

  let rec sync f = match !f with
  | Det v -> v
  | Undet _ -> relax (); sync f

  let return v = ref (Det v)

  let map fn f =
    let r = _make () in
    await f (fun v -> set r (fn v)); r

  let bind f fn =
    let r = _make () in
    await f (fun v -> await (fn v) (set r)); r

  let pair f0 f1 =
    let r = _make () in
    await f0 (fun v0 -> await f1 (fun v1 -> set r (v0, v1))); r

  let of_list fs = match fs with
  | [] -> return []
  | fs ->
      let r = _make () in
      let rec loop acc = function
      | [] -> set r (List.rev acc)
      | f :: fs -> await f (fun v -> loop (v :: acc) fs)
      in
      loop [] fs; r

  module Syntax = struct
    let ( let* ) = bind
    let ( and* ) = pair
  end
end

(* Operating system interactions *)

module Os = struct

  (* A bit of randomness for functions that need unique filenames *)

  let rand_gen = lazy (Random.State.make_self_init ())

  (* Error handling *)

  let doing_exist_test = "Existence test:"
  let err_seg_not_dir = "A segment of the path is not a directory"

  let uerror = Unix.error_message
  let err_doing doing e = Fmt.str "%s: %s" doing e
  let ferr file e = Fmt.error "%a: %s" Fpath.pp file e
  let ffail file e = Fmt.failwith "%a: %s" Fpath.pp file e
  let ffail_notrace file e = Fmt.failwith_notrace "%a: %s" Fpath.pp file e

  module Os_mtime = struct
    (* Mtime is defined at the end of the module otherwise we get problems
       accessing ../Mtime. *)

    external mtime_now_ns : unit -> Mtime.t = "ocaml_b0_monotonic_now_ns"

    (* Monotonic clock *)

    let origin = mtime_now_ns ()
    let elapsed () = Int64.sub (mtime_now_ns ()) origin
    let now = mtime_now_ns

    (* Monotonic time counter *)

    type counter = Mtime.t
    let counter = mtime_now_ns
    let count c = Int64.sub (mtime_now_ns ()) c

    (* Sleep *)

    let sleep dur =
      let measure = counter () in
      let dur = Mtime.Span.to_float_ns dur *. 1e-9 in
      let () = try Unix.sleepf dur with Unix.Unix_error _ -> () in
      count measure
  end

  module Fd = struct
    let unix_buffer_size = 65536 (* UNIX_BUFFER_SIZE 4.0.0 *)

    let rec openfile fn mode perm = try Unix.openfile fn mode perm with
    | Unix.Unix_error (Unix.EINTR, _, _) -> openfile fn mode perm

    let rec close fd = try Unix.close fd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> close fd

    let close_no_unix_exn fd = try close fd with Unix.Unix_error _ -> ()

    let apply ~close fd f =
      let close fd = try close fd with Unix.Unix_error _ -> () in
      match f fd with v -> close fd; v | exception e -> close fd; raise e

    let copy ?buf src ~dst =
      let rec unix_read fd b = try Unix.read fd b 0 (Bytes.length b) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd b
      in
      let rec unix_write fd s i l =
        let rec write fd s i l = try Unix.single_write fd s i l with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s i l
        in
        let bc = write fd s i l in
        if bc < l then unix_write fd s (i + bc) (l - bc) else ()
      in
      let rec loop buf src dst = match unix_read src buf with
      | 0 -> ()
      | l -> unix_write dst buf 0 l; loop buf src dst
      in
      let buf = match buf with
      | Some b -> b
      | None -> Bytes.create unix_buffer_size
      in
      loop buf src dst

    let to_string fd =
      let b = Bytes.create unix_buffer_size in
      let acc = Buffer.create unix_buffer_size in
      let rec loop () = match Unix.read fd b 0 (Bytes.length b) with
      | 0 -> Buffer.contents acc
      | l -> Buffer.add_subbytes acc b 0 l; loop ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
      in
      loop ()

    let rec really_read fd b start len = match len <= 0 with
    | true -> ()
    | false ->
        match Unix.read fd b start len with
        | 0 -> failwith (err_doing "Reading" "Unexpected end of file")
        | r -> really_read fd b (start + r) (len - r)
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            really_read fd b start len

    let read_file file fd =
      try
        match Unix.lseek fd 0 Unix.SEEK_END with
        | exception Unix.Unix_error (Unix.ESPIPE, _, _) -> to_string fd
        | len when len > Sys.max_string_length ->
            Fmt.failwith_notrace
              "File to read too large: %d bytes, max supported: %d"
              len Sys.max_string_length
        | len ->
            let b = Bytes.create len in
            ignore (Unix.lseek fd 0 Unix.SEEK_SET);
            really_read fd b 0 len;
            Bytes.unsafe_to_string b
      with
      | Failure e -> Fmt.failwith_notrace "%a: %s" Fpath.pp file e
      | Unix.Unix_error (e, _, _) ->
          Fmt.failwith_notrace "%s: %s" file (err_doing "Reading" (uerror e))

    module Set = struct (* Maintains a set of fds to close. *)
      module Fd = struct
        type t = Unix.file_descr
        let compare : t -> t -> int = compare
      end
      module S = Set.Make (Fd)
      type t = S.t ref
      let empty () = ref S.empty
      let rem fd s = s := S.remove fd !s
      let add fd s = s := S.add fd !s
      let close_all s = S.iter close_no_unix_exn !s; s := S.empty
      let close fd s =
        if S.mem fd !s then (close_no_unix_exn fd; s := S.remove fd !s)
    end
  end

  module Socket = struct
    let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

    let pp_name_port ppf (n, p) = Fmt.pf ppf "%s:%d" n p
    let pp_sockaddr ppf = function
    | Unix.ADDR_UNIX s -> Fmt.string ppf s
    | Unix.ADDR_INET (a, p) -> pp_name_port ppf (Unix.string_of_inet_addr a, p)

    let rec of_endpoint ep stype = match ep with
    | `Fd fd -> Ok (None, fd, false)
    | `Host (name, port) ->
        begin match Unix.gethostbyname name with
        | exception Not_found -> Fmt.error "%s: host not found" name
        | h ->
            let c = `Sockaddr (Unix.ADDR_INET (h.h_addr_list.(0), port)) in
            of_endpoint c stype
        end
    | `Sockaddr addr ->
        let domain = Unix.domain_of_sockaddr addr in
        match Unix.socket ~cloexec:true domain stype 0 with
        | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
        | fd ->
            match Unix.set_nonblock fd with
            | exception Unix.Unix_error (e, _, _) ->
                close_noerr fd; Error (uerror e)
            | () -> Ok (Some addr, fd, true)

    module Endpoint = struct
      type t =
        [ `Host of string * int
        | `Sockaddr of Unix.sockaddr
        | `Fd of Unix.file_descr ]

      let of_string ~default_port s =
        if String.contains s Filename.dir_sep.[0]
        then Ok (`Sockaddr (Unix.ADDR_UNIX s)) else
        match String.rindex_opt s ':' with
        | None -> Ok (`Host (s, default_port))
        | Some i ->
            match String.index_from_opt s i ']' with (* beware IPv6 *)
            | Some _ -> Ok (`Host (s, default_port))
            | None ->
                let h = String.subrange ~last:(i - 1) s in
                let p = String.subrange ~first:(i + 1) s in
                match int_of_string_opt p with
                | None -> Fmt.error "port %S not an integer" p
                | Some p -> Ok (`Host (h, p))

      let pp ppf = function
      | `Host (n, p) -> pp_name_port ppf (n, p)
      | `Fd _fd -> Fmt.pf ppf "<fd>"
      | `Sockaddr addr -> pp_sockaddr ppf addr

      let err_wait ep e = Fmt.str "Wait on %a: %s" pp ep e

      let wait_connectable ?(socket_type = Unix.SOCK_STREAM) ~timeout ep =
        let open Result.Syntax in
        let relax = Os_mtime.sleep Mtime.Span.(1 * ms) in
        Result.map_error (err_wait ep) @@
        let rec loop ~deadline dur =
          let* addr, fd, close = of_endpoint ep socket_type in
          let () = Unix.clear_nonblock fd in
          let finally () = if close then close_noerr fd in
          let* status = Fun.protect ~finally @@ fun () ->
            match addr with
            | None -> Error "no address to connect to"
            | Some addr ->
                match Unix.connect fd addr with
                | () -> Ok `Ready
                | exception Unix.(Unix_error (ECONNREFUSED, _, _)) -> Ok `Retry
                | exception Unix.Unix_error (e, _, _) ->
                    Error (Unix.error_message e)
          in
          match status with
          | `Ready -> Ok `Ready
          | `Retry ->
              let count = Os_mtime.count dur in
              if Mtime.Span.is_shorter count ~than:deadline
              then (ignore (Os_mtime.sleep relax); loop ~deadline dur)
              else Ok `Timeout
        in
        loop ~deadline:timeout (Os_mtime.counter ())

      let wait_connectable' ?socket_type ~timeout ep =
        match wait_connectable ?socket_type ~timeout ep with
        | Error _ as e -> e
        | Ok `Ready -> Ok ()
        | Ok `Timeout ->
            let err = Fmt.str "timed out after %a" Mtime.Span.pp timeout in
            Error (err_wait ep err)
    end
  end


  module Env = struct
    (* Variables *)

    type var_name = string

    let find ~empty_is_none name = match Unix.getenv name with
    | "" when empty_is_none -> None
    | v -> Some v
    | exception Not_found -> None

    let find' ~empty_is_none parse name = match find ~empty_is_none name with
    | None -> Ok None
    | Some v ->
        match parse v with
        | Error e -> Fmt.error "%s env: %s" name e
        | Ok v -> Ok (Some v)

    (* Process environment *)

    type t = string String.Map.t
    let empty = String.Map.empty
    let add = String.Map.add
    let remove = String.Map.remove
    let mem = String.Map.mem
    let override env ~by =
      if String.Map.is_empty by then env else
      let lean_right _ l r = match r with
      | Some _ as v -> v
      | None -> match l with Some _ as v -> v | None -> assert false
      in
      String.Map.merge lean_right env by

    (* Assignements *)

    let env_err e = Fmt.error "process environment: %s" e

    type assignments = string list
    let current_assignments () =
      try Ok (Array.to_list @@ Unix.environment ()) with
      | Sys_error e -> env_err e
      | Unix.Unix_error (e, _, _) -> env_err (uerror e)

    let parse_assignments ?(init = String.Map.empty) fold v =
      try
        let add acc assign = match String.cut_left ~sep:"=" assign with
        | Some (var, value) -> String.Map.add var value acc
        | None ->
            Fmt.failwith_notrace "%S: cannot parse VAR=VAL assignement" assign
        in
        Ok (fold add init v)
      with
      | Failure e -> Result.error e

    let of_assignments ?init l = parse_assignments ?init List.fold_left l
    let to_assignments env =
      let add var v acc = String.concat "=" [var; v] :: acc in
      String.Map.fold add env []

    let current () =
      match parse_assignments Array.fold_left (Unix.environment ()) with
      | Ok _ as v -> v
      | Error e -> env_err e
      | exception Sys_error e -> env_err e
      | exception Unix.Unix_error (e, _, _) -> env_err (uerror e)

    let pp_assignments = Fmt.(vbox @@ list string)
    let pp ppf env = pp_assignments ppf (to_assignments env)
  end

  module Fs_base = struct
    let rec is_dir p = try (Unix.stat p).Unix.st_kind = Unix.S_DIR with
    | Unix.Unix_error (Unix.EINTR, _, _) -> is_dir p

    let rec is_symlink p = try (Unix.lstat p).Unix.st_kind = Unix.S_LNK with
    | Unix.Unix_error (Unix.EINTR, _, _) -> is_symlink p

    let rec unlink p = try Unix.unlink p with
    | Unix.Unix_error (Unix.EINTR,_, _) -> unlink p

    let rec file_delete p = try Ok (Unix.unlink (Fpath.to_string p); true) with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
    | Unix.Unix_error (Unix.EINTR, _, _) -> file_delete p
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "Deleting" (uerror e))

    (* Directory operations. *)

    let dir_create ?(mode = 0o755) ~make_path dir =
      let create_op = "Creating" in
      let mkdir dir mode = Unix.mkdir (Fpath.to_string dir) mode in
      try
        let pmode = 0o755 in
        try Ok (mkdir dir mode; true) with
        | Unix.Unix_error (Unix.EEXIST, _, _) ->
            if is_dir dir then Ok false else
            ferr dir (err_doing create_op "Path exists but not a directory")
        | Unix.Unix_error (Unix.ENOENT, _, _) when make_path ->
            let rec down = function
            | [] -> assert false
            | [dir] ->
                (try Ok (mkdir dir mode; true) with
                | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok false)
            | dir :: dirs ->
                match mkdir dir pmode with
                | () -> down dirs
                | exception Unix.Unix_error (Unix.EEXIST, _, _) -> down dirs
            in
            let rec up todo p = match Unix.mkdir p pmode with
            | () -> down todo
            | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                up (p :: todo) (Fpath.parent p)
            in
            up [dir] (Fpath.parent dir)
      with
      | Unix.Unix_error (e, _, p) ->
          match String.equal (Fpath.to_string dir) p with
          | true -> ferr dir (err_doing create_op (uerror e))
          | false ->
              let perr = Fmt.str "%s: %s" p (uerror e) in
              ferr dir (err_doing create_op perr)

    let dir_delete ~recurse dir =
      let delete_op = "Deleting" in
      let err e = Fmt.failwith_notrace "%a: %s" Fpath.pp dir e in
      let rec delete_symlink p =
        if is_symlink p then (unlink p; true) else false
      in
      let try_unlink file = match Unix.unlink (Fpath.to_string file) with
      | () -> true
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOENT -> true
          | Unix.EISDIR (* Linux *) | Unix.EPERM (* POSIX *) -> false
          | Unix.EACCES when Sys.win32 ->
              (* This is what Unix.unlink returns on directories on Windows. *)
              false
          | e ->
              let ferr = Fmt.str "%a: %s" Fpath.pp file (uerror e) in
              err (err_doing delete_op ferr)
      in
      let rec delete_contents d dh todo = match Unix.readdir dh with
      | exception End_of_file -> d :: todo
      | ".." | "." -> delete_contents d dh todo
      | file ->
          let file = Fpath.(d / file) in
          if try_unlink file then delete_contents d dh todo else
          file :: d :: todo (* file is a dir we'll come back later for [d] *)
      in
      let rec try_delete d todo = match Unix.opendir (Fpath.to_string d) with
      | dh ->
          let dirs = match delete_contents d dh todo with
          | dirs -> Unix.closedir dh; dirs
          | exception e -> Unix.closedir dh; raise e
          in
          doit dirs
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOENT | Unix.ENOTDIR -> doit todo
          | e ->
              let derr = Fmt.str "%a: %s" Fpath.pp d (uerror e) in
              err (err_doing delete_op derr)
      and doit = function
      | [] -> ()
      | d :: ds ->
          match Unix.rmdir (Fpath.to_string d) with
          | () -> doit ds
          | exception Unix.Unix_error (e, _, _) ->
              match e with
              | Unix.ENOTEMPTY -> try_delete d ds
              | Unix.ENOENT | Unix.ENOTDIR -> doit ds
              | e ->
                  let derr = Fmt.str "%a: %s" Fpath.pp d (uerror e) in
                  err (err_doing delete_op derr)
      in
      try match Unix.rmdir (Fpath.to_string dir) with
      | () -> Ok true
      | exception Unix.Unix_error (e, _, _) ->
          match e with
          | Unix.ENOTEMPTY when recurse -> Ok (try_delete dir []; true)
          | Unix.ENOENT -> Ok false
          | Unix.ENOTDIR ->
              begin try
                if delete_symlink (Fpath.to_string dir) then Ok true else
                err (err_doing delete_op (uerror Unix.ENOTDIR))
              with
              | Unix.Unix_error (e,_,_) -> err (err_doing delete_op (uerror e))
              end
          | e -> err (err_doing delete_op (uerror e))
      with
      | Failure e -> Result.error e

    (* Handling forced file operations *)

    let err_force p = ferr p "Path exists"

    let rec handle_force ~force file =
      if force then Ok () else
      try ignore (Unix.lstat (Fpath.to_string file)); err_force file with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr file err_seg_not_dir
      | Unix.Unix_error (Unix.EINTR, _, _) -> handle_force ~force file
      | Unix.Unix_error (e, _, _) ->
          ferr file (err_doing doing_exist_test (uerror e))

    let rec handle_force_open_fdout
      ?(flags = Unix.[O_WRONLY; O_CREAT; O_SHARE_DELETE; O_CLOEXEC; O_TRUNC])
      ~force ~make_path ~mode file
      =
      let fls = if force then flags else Unix.O_EXCL :: flags in
      match Unix.openfile file fls mode with
      | fd -> Ok fd
      | exception Unix.Unix_error (Unix.EEXIST, _, _) -> err_force file
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          handle_force_open_fdout ~flags ~force ~make_path ~mode file
      | exception Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
          begin match dir_create ~make_path (Fpath.parent file) with
          | Error e -> ferr file e
          | Ok false (* existed *) -> ferr file (uerror e)
          | Ok true (* created *) ->
              handle_force_open_fdout ~flags ~force ~make_path ~mode file
          end
      | exception Unix.Unix_error (e, _, _) -> ferr file (uerror e)

    (* Path operations *)

    let rec path_exists p =
      try (ignore (Unix.stat (Fpath.to_string p)); Ok true) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
      | Unix.Unix_error (Unix.EINTR, _, _) -> path_exists p
      | Unix.Unix_error (e, _, _) ->
          ferr p (err_doing doing_exist_test (uerror e))

    let rec path_get_mode p =
      try Ok ((Unix.stat @@ Fpath.to_string p).Unix.st_perm) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> path_get_mode p
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
      | Unix.Unix_error (e, _, _) ->
          ferr p (err_doing "Getting file mode" (uerror e))

    let rec path_set_mode p m = try Ok (Unix.chmod (Fpath.to_string p) m) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_set_mode p m
    | Unix.Unix_error (e, _, _) ->
        ferr p (err_doing "Setting file mode" (uerror e))

    let rec path_delete ~recurse p =
      try Ok (Unix.unlink (Fpath.to_string p); true) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | Unix.Unix_error (Unix.EINTR, _, _) -> path_delete ~recurse p
      | Unix.Unix_error ((Unix.EPERM | Unix.EISDIR), _, _) ->
          dir_delete ~recurse p
      | Unix.Unix_error (e, _, _) -> ferr p (err_doing "Deleting" (uerror e))

    let rec path_rename ~force ~make_path src ~dst:p =
      let err e = Fmt.error "rename %a to %a: %s" Fpath.pp src Fpath.pp p e in
      match handle_force ~force p with
      | Error e -> err e
      | Ok () ->
          try Ok (Unix.rename (Fpath.to_string src) (Fpath.to_string p)) with
          | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
              begin match dir_create ~make_path (Fpath.parent p) with
              | Error e -> err e
              | Ok false (* existed *) -> err (uerror e)
              | Ok true (* created *) ->
                  path_rename ~force ~make_path src ~dst:p
              end
          | Unix.Unix_error (Unix.EINTR, _, _) ->
              path_rename ~force ~make_path src ~dst:p
          | Unix.Unix_error (e, _, _) -> err (uerror e)

    let rec path_stat p = try Ok (Unix.stat (Fpath.to_string p)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> path_stat p
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "stat" (uerror e))

    (* Links *)

    let rec symlink ~force ~make_path ~src p =
      let err e = Fmt.error "symlink %a to %a: %s" Fpath.pp src Fpath.pp p e in
      try Ok (Unix.symlink (Fpath.to_string src) (Fpath.to_string p)) with
      | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
          begin match file_delete p with
          | Error e -> err e
          | Ok _ -> symlink ~force ~make_path ~src p
          end
      | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR as e), _, _)
        when make_path ->
          begin match dir_create ~make_path (Fpath.parent p) with
          | Error e -> ferr p (err_doing "Creating path" e)
          | Ok false (* existed *) -> err (uerror e)
          | Ok true (* created *) -> symlink ~force ~make_path ~src p
          end
      | Unix.Unix_error (Unix.EINTR, _, _) -> symlink ~force ~make_path ~src p
      | Unix.Unix_error (e, _, _) -> err (uerror e)

    let rec symlink_link p =
      try
        let l = Unix.readlink (Fpath.to_string p) in
        match Fpath.of_string l with
        | Ok _ as v -> v
        | Error e -> ferr p (err_doing "Reading symlink" e)
      with
      | Unix.Unix_error (Unix.EINVAL, _, _) -> ferr p "Not a symbolic link"
      | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_link p
      | Unix.Unix_error (e, _, _) -> ferr p (uerror e)

    let rec symlink_stat p = try Ok (Unix.lstat (Fpath.to_string p)) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> symlink_stat p
    | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
    | Unix.Unix_error (e, _, _) -> ferr p (err_doing "lstat" (uerror e))

    let copy_symlink ~force ~make_path src ~dst =
      Result.bind (symlink_link src) @@ fun src ->
      symlink ~force ~make_path ~src dst
  end

  module Tmp = struct
    let delete_file file = ignore (Fs_base.file_delete file)
    let files = ref Fpath.Set.empty
    let add_file file = files := Fpath.Set.add file !files
    let rem_file file = delete_file file; files := Fpath.Set.remove file !files

    let delete_dir dir = ignore (Fs_base.dir_delete ~recurse:true dir)
    let dirs = ref Fpath.Set.empty
    let add_dir dir = dirs := Fpath.Set.add dir !dirs
    let rem_dir dir = delete_dir dir; dirs := Fpath.Set.remove dir !dirs

    let cleanup () =
      Fpath.Set.iter delete_file !files;
      Fpath.Set.iter delete_dir !dirs

    let () = at_exit cleanup

    let default_dir =
      let tmp_from_env var ~default =
        Option.value ~default (Env.find ~empty_is_none:true var)
      in
      let dir = match Sys.win32 with
      | true -> tmp_from_env "TEMP" ~default:Fpath.(v "./")
      | false -> tmp_from_env "TMPDIR" ~default:(Fpath.v "/tmp/")
      in
      ref (Fpath.add_dir_sep dir)

    type name = (string -> string, unit, string) format
    let default_name = format_of_string "tmp-%s"

    let rand_num () = Random.State.bits (Lazy.force rand_gen) land 0xFFFFFF
    let rand_str () = Printf.sprintf "%06x" (rand_num ())
    let tmp_path dir name rand =
      match dir.[String.length dir - 1] = Fpath.dir_sep_char with
      | true -> Printf.sprintf ("%s" ^^ name) dir rand
      | false -> Printf.sprintf ("%s%c" ^^ name) dir Fpath.dir_sep_char rand

    let err dir name rand e =
      Fmt.error "tmp file %s: %s" (tmp_path dir name rand) e

    let err_too_many dir name =
      err dir name "XXXXXX" "Too many attempts to create"

    let attempts = 10000
    let open'
        ?(flags = Unix.[O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC])
        ?(mode = 0o600) ?(make_path = true) ?dir ?(name = default_name) ()
      =
      let dir = match dir with None -> !default_dir | Some d -> d in
      let dir_str = Fpath.to_string (Fpath.add_dir_sep dir) in
      let rec loop n = match n with
      | 0 -> err_too_many dir name
      | n ->
          let rand = rand_str () in
          try
            let file = tmp_path dir_str name rand in
            let fd = Unix.openfile file flags mode in
            let file = Fpath.v file in
            (add_file file; Ok (file, fd))
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
          | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
              begin match Fs_base.dir_create ~make_path dir with
              | Error e -> err dir name rand e
              | Ok true (* created *) -> loop n
              | Ok false (* existed *) -> err dir name rand (uerror e)
              end
          | Unix.Unix_error (e, _, _) -> err dir name rand (uerror e)
      in
      loop attempts

    let mkdir
        ?(mode = 0o700) ?(make_path = true) ?dir ?(name = default_name) ()
      =
      let dir = match dir with None -> !default_dir | Some d -> d in
      let dir_str = Fpath.to_string dir in
      let rec loop n = match n with
      | 0 -> err_too_many dir name
      | n ->
          let rand = rand_str () in
          try
            let tdir = tmp_path dir_str name rand in
            let () = Unix.mkdir tdir mode in
            let tdir = Fpath.v tdir in
            (add_dir tdir; Ok tdir)
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n - 1)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop n
          | Unix.Unix_error (Unix.ENOENT as e, _, _) when make_path ->
              begin match Fs_base.dir_create ~make_path dir with
              | Error e -> err dir name rand e
              | Ok true (* created *) -> loop n
              | Ok false (* existed *) -> err dir name rand (uerror e)
              end
          | Unix.Unix_error (e, _, _) -> err dir name rand (uerror e)
      in
      loop attempts

    let path ?(make_path = true) ?dir ?(name = format_of_string "tmp-%s") () =
      let dir = match dir with None -> !default_dir | Some d -> d in
      let dir_str = Fpath.to_string dir in
      let rec loop n = match n with
      | 0 -> err_too_many dir name
      | n ->
          let rand = rand_str () in
          let file = tmp_path dir_str name rand in
          match Unix.access file [Unix.F_OK] with
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok file
          | exception Unix.Unix_error (e, _, _) -> err dir name rand (uerror e)
          | _ -> loop (n - 1)
      in
      if not make_path then loop attempts else
      match Fs_base.dir_create ~make_path dir with
      | Error _ as e -> e
      | Ok _ -> loop attempts
  end

  module File = struct
    let channel_apply ~close c f =
      let close c = try close c with Sys_error _ -> () in
      match f c with v -> close c; v | exception e -> close c; raise e

    (* Famous file paths *)

    let is_dash = Fpath.equal Fpath.dash

    (* Existence *)

    let rec exists file =
      match (Unix.stat (Fpath.to_string file)).Unix.st_kind with
      | Unix.S_REG -> Ok true
      | _ -> Ok false
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
          ferr file err_seg_not_dir
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> exists file
      | exception Unix.Unix_error (e, _, _) ->
          ferr file (err_doing doing_exist_test (uerror e))

    let rec must_exist file =
      match (Unix.stat (Fpath.to_string file)).Unix.st_kind with
      | Unix.S_REG -> Ok ()
      | _ -> ferr file "Path exists but not a regular file"
      | exception Unix.Unix_error (Unix.ENOENT, _,_) ->
          ferr file "No such file"
      | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
          ferr file err_seg_not_dir
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> must_exist file
      | exception Unix.Unix_error (e, _, _) ->
          ferr file (err_doing doing_exist_test (uerror e))

    let is_executable file = match Unix.access file [Unix.X_OK] with
    | () -> true
    | exception Unix.Unix_error _ -> false

    (* Deleting and truncating *)

    let delete = Fs_base.file_delete
    let rec truncate file size =
      try Ok (Unix.truncate (Fpath.to_string file) size) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> truncate file size
      | Unix.Unix_error (e, _, _) ->
          ferr file (err_doing "Truncating" (uerror e))

    (* Hard links *)

    let rec link ~force ~make_path ~src file =
      let err e = Fmt.error "link %a to %a: %s" Fpath.pp src Fpath.pp file e in
      try Ok (Unix.link (Fpath.to_string src) (Fpath.to_string file)) with
      | Unix.Unix_error (Unix.EEXIST, _, _) when force ->
          begin match delete file with
          | Error e -> err e
          | Ok _ -> link ~force ~make_path ~src file
          end
      | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR as e), _, _)
        when make_path ->
          begin match Fs_base.dir_create ~make_path (Fpath.parent file) with
          | Error e -> ferr file (err_doing "Creating path" e)
          | Ok false (* existed *) -> err (uerror e)
          | Ok true (* created *) -> link ~force ~make_path ~src file
          end
      | Unix.Unix_error (Unix.EINTR, _, _) -> link ~force ~make_path ~src file
      | Unix.Unix_error (e, _, _) -> err (uerror e)

    (* Reads *)

    let read_with_ic file f =
      try
        let ic, close = match is_dash file with
        | true -> stdin, fun _ -> ()
        | false -> open_in_bin (Fpath.to_string file), close_in
        in
        Ok (channel_apply ~close ic f)
      with
      | Sys_error e -> Result.error e

    let read_with_fd file f =
      try
        let fdin, close = match is_dash file with
        | true -> Unix.stdin, (fun _ -> ())
        | false ->
            Fd.openfile (Fpath.to_string file) Unix.[O_RDONLY] 0, Unix.close
        in
        Ok (Fd.apply ~close fdin f)
      with
      | Unix.Unix_error (e, _, _) -> ferr file (uerror e)

    let in_channel_to_string fd =
      let b = Bytes.create Fd.unix_buffer_size in
      let acc = Buffer.create Fd.unix_buffer_size in
      let rec loop () = match input fd b 0 (Bytes.length b) with
      | 0 -> Buffer.contents acc
      | n -> Buffer.add_subbytes acc b 0 n; loop ()
      in
      loop ()

    let read_stdin () = in_channel_to_string stdin

    let read_file file ic = match in_channel_length ic with
    | exception Sys_error e -> in_channel_to_string ic
    | len when len > Sys.max_string_length ->
        Fmt.failwith_notrace
          "File to read too large: %d bytes, max supported: %d"
          len Sys.max_string_length
    | len ->
        let s = Bytes.create len in
        really_input ic s 0 len;
        Bytes.unsafe_to_string s

    let read file =
      let input c = if c == stdin then read_stdin () else read_file file c in
      try read_with_ic file input with
      | Failure e | Sys_error e -> ferr file e

    (* Writes *)

    let with_tmp_fd ?flags ?mode ?make_path ?dir ?name f =
      Result.bind (Tmp.open' ?flags ?mode ?make_path ?dir ?name ()) @@
      fun (file, fd) ->
      let delete_close fd = Tmp.rem_file file; Unix.close fd in
      Ok (Fd.apply ~close:delete_close fd (f file))

    let open_tmp_fd = Tmp.open'

    let with_tmp_oc ?flags ?mode ?make_path ?dir ?name f =
      Result.bind (Tmp.open' ?flags ?mode ?make_path ?dir ?name ()) @@
      fun (file, fd) ->
      let oc = Unix.out_channel_of_descr fd in
      let delete_close oc = Tmp.rem_file file; close_out oc in
      Ok (channel_apply ~close:delete_close oc (f file))

    let rec rename_tmp src dst =
      try Ok (Unix.rename (Fpath.to_string src) (Fpath.to_string dst)) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> rename_tmp src dst
      | Unix.Unix_error (e, _, _) ->
          let r = Fmt.str "renaming %a to %a" Fpath.pp src Fpath.pp dst in
          Result.error (err_doing r (uerror e))

    let write_op = "Writing"

    let write_with_fd_atomic ~mode ~force ~make_path ~file f =
      Result.bind (Fs_base.handle_force ~force file) @@ fun () ->
      let do_write tmp tmp_oc = match f tmp_oc with
      | Error _ as v -> Ok v
      | Ok _ as v -> Result.map (fun () -> v) (rename_tmp tmp file)
      in
      match with_tmp_fd ~mode ~make_path ~dir:(Fpath.parent file) do_write with
      | Ok v -> v
      | Error e -> ferr file (err_doing write_op e)

    let write_with_fd
        ?(atomic = true) ?(mode = 0o644) ~force ~make_path file f
      =
      match is_dash file with
      | true -> Ok (Fd.apply ~close:(fun _ -> ()) Unix.stdout f)
      | false when atomic ->
          write_with_fd_atomic ~mode ~force ~make_path ~file f
      | false ->
          Result.bind
            (Fs_base.handle_force_open_fdout ~force ~make_path ~mode file) @@
          fun fd -> Ok (Fd.apply ~close:Unix.close fd f)

    let write_with_oc_atomic ~mode ~force ~make_path ~file f =
      Result.bind (Fs_base.handle_force ~force file) @@ fun () ->
      let do_write tmp tmp_oc = match f tmp_oc with
      | Error _ as v -> Ok v
      | Ok _ as v -> Result.map (fun () -> v) (rename_tmp tmp file)
      in
      match with_tmp_oc ~mode ~make_path ~dir:(Fpath.parent file) do_write with
      | Ok v -> v
      | Error e -> ferr file (err_doing write_op e)

    let write_with_oc
        ?(atomic = true) ?(mode = 0o644) ~force ~make_path file f
      =
      match is_dash file with
      | true -> Ok (channel_apply ~close:(fun _ -> ()) stdout f)
      | false when atomic ->
          write_with_oc_atomic ~mode ~force ~make_path ~file f
      | false ->
          Result.bind
            (Fs_base.handle_force_open_fdout ~force ~make_path ~mode file) @@
          fun fd ->
          let oc = Unix.out_channel_of_descr fd in
          Ok (channel_apply ~close:close_out oc f)

    let write ?atomic ?mode ~force ~make_path file data =
      let out data oc = Ok (output_string oc data) in
      try
        Result.join @@
        write_with_oc ?atomic ?mode ~force ~make_path file (out data)
      with
      | Sys_error e -> ferr file e

    let copy ?atomic ?mode ~force ~make_path src ~dst:file =
      let err e = Fmt.str "copy %a to %a: %s" Fpath.pp src Fpath.pp file e in
      Result.map_error err @@ Result.join @@
      read_with_fd src @@ fun fdi ->
      try match is_dash file with
      | true -> Ok (Fd.copy fdi ~dst:Unix.stdout)
      | false ->
          let mode = match mode with
          | None -> Fs_base.path_get_mode src
          | Some m -> Ok m
          in
          Result.join @@ Result.bind mode @@ fun mode ->
          write_with_fd ?atomic ~mode ~force ~make_path file @@ fun fdo ->
          Ok (Fd.copy fdi ~dst:fdo)
      with
      | Unix.Unix_error (e, _, arg) -> Fmt.error "%s: %s" arg (uerror e)

    let copy_to_dir ?atomic ?mode ~force ~make_path ?src_root src ~dir =
        let dst = match src_root with
        | None -> Fpath.(dir / Fpath.basename src)
        | Some src_root -> Fpath.reroot ~src_root ~dst_root:dir src
        in
        copy ?atomic ?mode ~force ~make_path src ~dst
  end

  module Dir = struct

    (* Existence *)

    let rec exists dir =
      match (Unix.stat @@ Fpath.to_string dir).Unix.st_kind with
      | Unix.S_DIR -> Ok true
      | _ -> Ok false
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
      | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
          ferr dir err_seg_not_dir
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> exists dir
      | exception Unix.Unix_error (e, _, _) ->
          ferr dir (err_doing doing_exist_test (uerror e))

    let rec must_exist dir =
      match (Unix.stat @@ Fpath.to_string dir).Unix.st_kind with
      | Unix.S_DIR -> Ok ()
      | _ ->
          ferr dir "Path exists but not a directory"
      | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
          ferr dir "No such directory"
      | exception Unix.Unix_error (Unix.ENOTDIR, _, _) ->
          ferr dir err_seg_not_dir
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> must_exist dir
      | exception Unix.Unix_error (e, _, _) ->
          ferr dir (err_doing doing_exist_test (uerror e))

    (* Creating, deleting and renaming. *)

    let create = Fs_base.dir_create

    (* Contents *)

    let rec readdir ~dotfiles dir =
      let is_dot_file s = String.length s <> 0 && s.[0] = '.' in
      let rec loop ~dotfiles dir dh acc = match Unix.readdir dh with
      | exception End_of_file -> acc
      | ".." | "." -> loop ~dotfiles dir dh acc
      | n when is_dot_file n && not dotfiles -> loop ~dotfiles dir dh acc
      | n when Fpath.is_seg n -> loop ~dotfiles dir dh (n :: acc)
      | n -> ffail dir (Fmt.str "%S: Invalid file name" n)
      in
      let dh = Unix.opendir (Fpath.to_string dir) in
      match loop ~dotfiles dir dh [] with
      | fs -> Unix.closedir dh; fs
      | exception e ->
          (try Unix.closedir dh with Unix.Unix_error (_, _, _) -> ());
          raise e

    let rec stat p = try (Unix.stat @@ Fpath.to_string p) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> stat p

    let rec lstat p = try (Unix.lstat @@ Fpath.to_string p) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> lstat p

    let fold_no_rec ~filter ~rel ~dotfiles ~follow_symlinks dir f acc =
      let rec loop stat f acc adir = function
      | [] -> Ok acc
      | n :: ns ->
          let full = Fpath.(adir / n) in
          match stat full with
          | st ->
              begin match st.Unix.st_kind with
              | Unix.S_DIR ->
                  if filter = `Non_dir then loop stat f acc adir ns else
                  let p = if rel then Fpath.v n else full in
                  loop stat f (f st n p acc) adir ns
              | _ when filter <> `Dir ->
                  let p = if rel then Fpath.v n else full in
                  loop stat f (f st n p acc) adir ns
              | _ ->
                  loop stat f acc adir ns
              end
          | exception Unix.Unix_error ((ENOENT|ENOTDIR|EBADF|EPERM), _, _) ->
              loop stat f acc adir ns
      in
      let stat = if follow_symlinks then stat else lstat in
      loop stat f acc dir (readdir ~dotfiles dir)

    let fold_rec ~prune_dir ~filter ~rel ~dotfiles ~follow_symlinks dir f acc =
      let rec loop stat todo adir rdir f acc = function
      | [] ->
          begin match todo with
          | (dir, rdir, ns) :: todo -> loop stat todo dir rdir f acc ns
          | [] -> Ok acc
          end
      | n :: ns ->
          let full = Fpath.(adir / n) in
          begin match stat full with
          | st ->
              begin match st.Unix.st_kind with
              | Unix.S_DIR ->
                  let rp = match rdir with
                  | None -> Fpath.v n | Some rdir -> Fpath.(rdir / n)
                  in
                  let p = if not rel then full else rp in
                  if prune_dir st n p acc
                  then loop stat todo adir rdir f acc ns else
                  let acc = if filter = `Non_dir then acc else f st n p acc in
                  let todo = (adir, rdir, ns) :: todo in
                  loop stat todo full (Some rp) f acc (readdir ~dotfiles full)
              | _ when filter <> `Dir ->
                  let p = if not rel then full else match rdir with
                  | None -> Fpath.v n | Some rdir -> Fpath.(rdir / n)
                  in
                  loop stat todo adir rdir f (f st n p acc) ns
              | _ ->
                  loop stat todo adir rdir f acc ns
              end
          | exception Unix.Unix_error ((ENOENT|ENOTDIR|EBADF|EPERM), _, _) ->
              loop stat todo adir rdir f acc ns
          end
      in
      let stat = if follow_symlinks then stat else lstat in
      loop stat [] dir None f acc (readdir ~dotfiles dir)

    let _fold
        ~(filter : [`Any | `Non_dir | `Dir]) ?(rel = false) ?(dotfiles = false)
        ?(follow_symlinks = true) ?(prune_dir = fun _ _ _ _ -> false) ~recurse
        f dir acc
      =
      let listing_op = "Listing" in
      try
        let fold = if recurse then fold_rec ~prune_dir else fold_no_rec in
        fold ~filter ~rel ~dotfiles ~follow_symlinks dir f acc
      with
      | Failure e -> ferr dir (err_doing listing_op e)
      | Unix.Unix_error (e, _, ep) ->
          if String.equal (Fpath.to_string dir) ep
          then ferr dir (err_doing listing_op @@ uerror e)
          else ferr dir (err_doing listing_op @@ Fmt.str "%s: %s" ep (uerror e))

    let fold ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse f dir acc =
      _fold ~filter:`Any ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
        f dir acc

    let fold_files
        ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse f dir acc
      =
      _fold ~filter:`Non_dir ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
        f dir acc

    let fold_dirs
        ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse f dir acc =
      _fold ~filter:`Dir ?rel ?dotfiles ?follow_symlinks ?prune_dir ~recurse
        f dir acc

    let path_list stat _ f acc = match stat.Unix.st_kind with
    | Unix.S_DIR -> Fpath.add_dir_sep f :: acc
    | _ -> f :: acc

    let prune_denied _ _ p _ =
      try (Unix.access p Unix.[R_OK; X_OK]; false) with
      | Unix.Unix_error ((EACCES|EPERM(* may happen *)), _, _) -> true

    (* copy *)

    let copy
        ?(rel = true) ?(atomic = true) ?(follow_symlinks = true)
        ?(prune = fun _ _ _ -> false) ~make_path ~recurse src ~dst
      =
      let err e = Fmt.str "copy %a to %a: %s" Fpath.pp src Fpath.pp dst e in
      let prune = match rel with (* we invoke [_fold] with [rel:true] *)
      | true -> fun st name p _ -> prune st name p
      | false -> fun st name p _ -> prune st name (Fpath.(src // p))
      in
      let copy dst st name p (chmods as acc) = match st.Unix.st_kind with
      | Unix.S_DIR (* prune was already called on it *) ->
          let dst = Fpath.(dst // p) in
          let mode = st.Unix.st_perm in
          let writeable = (mode land 0o200 <> 0) in
          let mode, acc = match writeable with
          | true -> mode, acc
          | false ->
              (* We need to be able to write to the directory, we remember
                 the mode and the dir and set it at the end *)
              0o700, ((dst, mode) :: chmods)
          in
          ignore (Fs_base.dir_create ~mode ~make_path:false dst |>
                  Result.error_to_failure);
          acc
      | Unix.S_REG ->
          let cp ~mode src dst =
            Result.join @@
            File.read_with_fd src @@ fun fdi ->
            Result.join @@
            File.write_with_fd
              ~atomic:true ~mode ~force:false ~make_path:false dst @@
            fun fdo -> Ok (Fd.copy fdi ~dst:fdo)
          in
          if prune st name p () then acc else
          let mode = st.Unix.st_perm in
          let src = Fpath.(src // p) in
          let dst = Fpath.(dst // p) in
          (cp ~mode src dst |> Result.error_to_failure); acc
      | Unix.S_LNK ->
          if prune st name p () then acc else
          let dst = Fpath.(dst // p) in
          let src = Fpath.(src // p) in
          let force = false and make_path = false in
          Fs_base.copy_symlink ~force ~make_path src ~dst
          |> Result.error_to_failure;
          acc
      | _ when prune st name p () (* why not *) -> acc
      | _ ->
          Fmt.failwith "%a: Not a regular file, directory or symlink"
            Fpath.pp Fpath.(src // p)
      in
      let rec chmod_dirs = function
      | [] -> ()
      | (d, m) :: ds ->
          (Fs_base.path_set_mode d m) |> Result.error_to_failure; chmod_dirs ds
      in
      Result.map_error err @@
      Result.bind (Fs_base.path_exists dst) @@ function
      | true -> Error "Destination path already exists"
      | false ->
          let tdst = match atomic with
          | true -> Tmp.mkdir ~make_path ~dir:(Fpath.parent dst) ()
          | false ->
              Result.bind (Fs_base.dir_create ~make_path dst) @@
              fun _ -> Ok dst
          in
          Result.bind tdst @@ fun tdst ->
          try
            let src_mode =
              Fs_base.path_get_mode src |> Result.error_to_failure
            in
            let chmods =
              _fold ~filter:`Any ~rel:true ~dotfiles:true ~follow_symlinks
                ~prune_dir:prune ~recurse (copy tdst) src ([tdst, src_mode])
              |> Result.error_to_failure
            in
            chmod_dirs chmods;
            match atomic with
            | false -> Ok ()
            | true ->
                Fs_base.path_rename ~force:false ~make_path:true tdst ~dst
          with Failure e ->
            if atomic then ignore (Fs_base.path_delete ~recurse:true tdst);
            Error e

    (* Default temporary directory *)

    let set_default_tmp p = Tmp.default_dir := Fpath.add_dir_sep p
    let default_tmp () = !Tmp.default_dir

    (* Temporary directories *)

    let with_tmp ?mode ?make_path ?dir ?name f =
      Result.bind (Tmp.mkdir ?mode ?make_path ?dir ?name ()) @@
      fun dir ->
      try let v = f dir in Tmp.rem_dir dir; Ok v with
      | e -> Tmp.rem_dir dir; raise e

    let tmp = Tmp.mkdir

    (* Current working directory *)

    let rec cwd () =
      let err e = Fmt.error "get cwd: %s" e in
      match Fpath.of_string (Unix.getcwd ()) with
      | Ok dir when Fpath.is_abs dir -> Ok dir
      | Ok dir -> err (Fmt.str "%a is relative" Fpath.pp dir)
      | Error e -> err e
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> cwd ()
      | exception Unix.Unix_error (e, _, _) -> err (uerror e)

    let rec set_cwd dir =
      let err e = Fmt.error "set cwd to %a: %s" Fpath.pp dir e in
      try Ok (Unix.chdir (Fpath.to_string dir)) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> set_cwd dir
      | Unix.Unix_error (e, _, _) -> err (uerror e)

    let with_cwd dir f =
      Result.bind (cwd ()) @@ fun old ->
      Result.bind (set_cwd dir) @@ fun () ->
      match f () with
      | v -> Result.map (fun () -> v) (set_cwd old)
      | exception e -> ignore (set_cwd old); raise e

    (* Base directories *)

    let err_dir dir fmt = Fmt.error ("%s directory: " ^^ fmt) dir
    let fpath_of_env_var dir var = match Env.find ~empty_is_none:true var with
    | None -> None
    | Some p ->
        match Fpath.of_string p with
        | Error e -> Some (err_dir dir "%s environment variable: %s" var e)
        | Ok _ as v -> Some v

    let base_dir dir var var_alt fallback = match fpath_of_env_var dir var with
    | Some r -> r
    | None ->
        match Option.bind var_alt (fpath_of_env_var dir) with
        | Some r -> r
        | None -> fallback ()

    let home_dir = "user"
    let home_var = "HOME"
    let user () =
      let home_env home_var = match fpath_of_env_var home_dir home_var with
      | Some r -> r
      | None -> err_dir home_dir "%s environment variable is undefined" home_var
      in
(*      if Sys.win32 then home_env home_win32_var else *)
      match Fpath.of_string (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir with
      | Ok _ as v -> v
      | Error _ -> home_env home_var
      | exception Not_found -> home_env home_var
      | exception Unix.Unix_error (e, _, _) -> home_env home_var

    let home_fallback dir sub = match user () with
    | Error e -> err_dir dir "%s" e
    | Ok home -> Ok Fpath.(home // sub)

    let config_dir = "configuration"
    let config_var = "XDG_CONFIG_HOME"
    let config_var_alt = if Sys.win32 then Some "%APPDATA%" else None
    let config_fallback () = home_fallback config_dir (Fpath.v ".config")
    let config () =
      base_dir config_dir config_var config_var_alt config_fallback

    let data_dir = "data"
    let data_var = "XDG_DATA_HOME"
    let data_var_alt = if Sys.win32 then Some "%APPDATA%" else None
    let data_fallback () = home_fallback data_dir (Fpath.v ".local/share")
    let data () =
      base_dir data_dir data_var data_var_alt data_fallback

    let cache_dir = "cache"
    let cache_var = "XDG_CACHE_HOME"
    let cache_var_alt = if Sys.win32 then Some "%TEMP%" else None
    let cache_fallback () = home_fallback cache_dir (Fpath.v ".cache")
    let cache () =
      base_dir cache_dir cache_var cache_var_alt cache_fallback

    let runtime_dir = "runtime"
    let runtime_var = "XDG_RUNTIME_HOME"
    let runtime_var_alt = None
    let runtime_fallback () = Ok (default_tmp ())
    let runtime () =
      base_dir runtime_dir runtime_var runtime_var_alt runtime_fallback
  end

  module Path = struct

    (* Existence *)

    let exists = Fs_base.path_exists

    let rec must_exist p =
      try (Ok (ignore (Unix.stat (Fpath.to_string p)))) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> ferr p "No such path"
      | Unix.Unix_error (Unix.EINTR, _, _) -> must_exist p
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
      | Unix.Unix_error (e, _, _) ->
          ferr p (err_doing doing_exist_test (uerror e))

    (* Deleting and renaming *)

    let delete = Fs_base.path_delete
    let rename = Fs_base.path_rename

    (* Resolving *)

    external _realpath : string -> string = "ocaml_b0_realpath"
    let rec realpath p =
      try Fpath.of_string (_realpath (Fpath.to_string p)) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> realpath p
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> ferr p err_seg_not_dir
      | Unix.Unix_error (e, _, _) -> ferr p (uerror e)

    (* Copying *)

    let copy
        ?(rel = true) ?(atomic = true) ?(follow_symlinks = true)
        ?(prune = fun _ _ _ -> false) ~make_path ~recurse src ~dst
      =
      let err e = Fmt.str "copy %a to %a: %s" Fpath.pp src Fpath.pp dst e in
      let stat = match follow_symlinks with
      | true -> Fs_base.path_stat
      | false -> Fs_base.symlink_stat
      in
      match stat src with
      | Error e -> Error (err e)
      | Ok stat ->
          match stat.Unix.st_kind with
          | Unix.S_DIR ->
              Dir.copy ~rel ~atomic ~follow_symlinks ~prune ~make_path
                ~recurse src ~dst
          | Unix.S_LNK ->
              Result.map_error err @@
              Fs_base.copy_symlink ~force:false ~make_path src ~dst
          | _  ->
              File.copy ~atomic ~force:false ~make_path src ~dst

    (* File modes and stat *)

    let get_mode = Fs_base.path_get_mode
    let set_mode = Fs_base.path_set_mode
    let stat = Fs_base.path_stat
    let is_mount_point p =
      let err e = Fmt.str "is_mount_point: %s" e in
      match Fs_base.path_stat p with
      | Error e -> Error (err e)
      | Ok stat ->
          match Fs_base.path_stat Fpath.(p / "..") with
          | Error e -> Error (err ("parent: " ^ e))
          | Ok pstat -> Ok (stat.Unix.st_dev <> pstat.Unix.st_dev)

    (* Symlinks *)

    let symlink = Fs_base.symlink
    let symlink_link = Fs_base.symlink_link
    let symlink_stat = Fs_base.symlink_stat

    (* Temporary paths *)

    type tmp_name = Tmp.name
    let tmp = Tmp.path
  end

  (* Processes *)

  module Cmd = struct

    (* Tool search in PATH *)

    let tool_is_path t = String.contains t Fpath.dir_sep_char
    let tool_file ~dir tool = match dir.[String.length dir - 1] with
    | c when Fpath.char_is_dir_sep c -> dir ^ tool
    | _ -> String.concat Fpath.dir_sep [dir; tool]

    let search_in_path_env_var tool = match Unix.getenv "PATH" with
    | exception Not_found ->
        Error (`Msg "The PATH environment variable is undefined")
    | p ->
        let rec loop tool = function
        | "" ->
            Error (`Dirs (String.split_on_char Fpath.search_path_sep.[0] p))
        | p ->
            let dir, p = match String.cut_left ~sep:Fpath.search_path_sep p with
            | None -> p, ""
            | Some (dir, p) -> dir, p
            in
            if dir = "" then loop tool p else
            let tool_file = tool_file ~dir tool in
            match File.is_executable tool_file with
            | false -> loop tool p
            | true -> Ok (Fpath.v tool_file)
        in
        loop tool p

    let search_in_dirs ~dirs tool =
      let rec loop tool = function
      | [] -> Error (`Dirs (List.map Fpath.to_string dirs))
      | d :: dirs ->
          let tool_file = tool_file ~dir:(Fpath.to_string d) tool in
          match File.is_executable tool_file with
          | false -> loop tool dirs
          | true -> Ok (Fpath.v tool_file)
      in
      loop tool dirs

    let path_search ?(win_exe = Sys.win32) ?path () = fun cmd ->
      match Cmd.find_tool cmd with
      | None -> Fmt.error "No tool to lookup: the command is empty"
      | Some tool ->
          let tool =
            let tool = Fpath.to_string tool and suffix = ".exe" in
            if not win_exe || String.ends_with ~suffix tool then tool else
            (tool ^ suffix)
          in
          match tool_is_path tool with
          | true ->
              if File.is_executable tool
              then Ok (Cmd.set_tool (Fpath.v tool) cmd)
              else Fmt.error "%s: No such executable file" tool
          | false ->
              let file = match path with
              | None -> search_in_path_env_var tool
              | Some dirs -> search_in_dirs ~dirs tool
              in
              match file with
              | Ok file -> Ok (Cmd.set_tool file cmd)
              | Error (`Msg e) -> Fmt.error "%s: %s" tool e
              | Error (`Dirs dirs) ->
                  Fmt.error "@[<v1>%s: No such tool found in:@,%a@]"
                    tool (Fmt.(list string)) dirs

    (* Tool search *)

    let default_search = path_search ()
    let find ?(search = default_search) cmd = Result.to_option (search cmd)
    let find_first ?search cmds = List.find_map (find ?search) cmds
    let get ?(search = default_search) cmd = search cmd
    let rec get_first ?(search = default_search) cmds =
      if cmds = [] then Error "No tool to lookup: tool list is empty" else
      match search (List.hd cmds) with
      | Ok _ as v -> v
      | Error e ->
          let rec loop = function
          | [] ->
              Fmt.error "@[<v1>%a@,neither as:@,%a@]"
                Fmt.lines e (Fmt.list Cmd.pp) (List.tl cmds)
          | cmd :: cmds ->
              match search cmd with
              | Ok _ as v -> v
              | Error _ -> loop cmds
          in
          loop (List.tl cmds)

    (* Process completion statuses *)

    type status = [ `Exited of int | `Signaled of int ]

    let status_of_unix_status = function
    | Unix.WEXITED e -> `Exited e
    | Unix.WSIGNALED s -> `Signaled s
    | Unix.WSTOPPED _ -> assert false

    let pp_status ppf = function
    | `Exited n -> Fmt.pf ppf "@[exited [%d]@]" n
    | `Signaled s -> Fmt.pf ppf "@[signaled [%a]@]" Fmt.sys_signal s

    let pp_cmd_status ppf (cmd, st) =
      Fmt.pf ppf "cmd [%s]: %a" (Cmd.to_string cmd) pp_status st

    (* Process standard inputs *)

    type stdi =
    | In_string of string
    | In_file of Fpath.t
    | In_fd of { fd : Unix.file_descr; close : bool }

    let in_string s = In_string s
    let in_file f = In_file f
    let in_fd ~close fd = In_fd { fd; close }
    let in_stdin = In_fd { fd = Unix.stdin; close = false }
    let in_null = In_file Fpath.null
    let stdi_to_fd fds = function
    | In_fd { fd; close } -> if close then Fd.Set.add fd fds; fd
    | In_string s ->
        begin try
          (* We write the input string to a temporary file. *)
          let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE] in
          let f, fd = Result.error_to_failure (Tmp.open' ~flags ()) in
          Fd.Set.add fd fds;
          Tmp.rem_file f; (* We don't need the actual file. *)
          ignore (Unix.write_substring fd s 0 (String.length s));
          ignore (Unix.lseek fd 0 Unix.SEEK_SET);
          fd
        with
        | Unix.Unix_error (e, _, _) ->
            Fmt.failwith_notrace "tmp file for stdin: %s" (uerror e)
        end
    | In_file f ->
        try
          let f = Fpath.to_string f in
          let fd = Fd.openfile f Unix.[O_RDONLY] 0o644 in
          Fd.Set.add fd fds; fd
        with Unix.Unix_error (e, _, _) ->
          Fmt.failwith_notrace "open file %a for stdin: %s"
            Fpath.pp f (uerror e)

    (* Process standard outputs *)

    type stdo =
    | Out_fd of { fd : Unix.file_descr; close : bool }
    | Out_file of
        { mode : int; force : bool; make_path : bool; file : Fpath.t }

    let out_file ?(mode = 0o644) ~force ~make_path file =
      Out_file { mode; force; make_path; file }

    let out_fd ~close fd = Out_fd { fd; close }
    let out_stdout = Out_fd { fd = Unix.stdout; close = false }
    let out_stderr = Out_fd { fd = Unix.stderr; close = false }
    let out_null = out_file ~force:true ~make_path:false Fpath.null

    let stdo_to_fd fds = function
    | Out_fd { fd; close } -> if close then Fd.Set.add fd fds; fd
    | Out_file { mode; force; make_path; file } ->
        let flags = Unix.[O_WRONLY; O_CREAT; O_TRUNC] in
        match
          Fs_base.handle_force_open_fdout ~flags ~force ~make_path ~mode file
        with
        | Error e -> Fmt.failwith_notrace "open for output: %s" e
        | Ok fd -> Fd.Set.add fd fds; fd

    (* Low-level command spawn *)

    type spawn_tracer =
      int option -> Env.assignments option -> cwd:Fpath.t option -> Cmd.t ->
      unit

    let spawn_tracer_nop _ _ ~cwd:_ _ = ()
    let _spawn_tracer = ref spawn_tracer_nop
    let spawn_tracer () = !_spawn_tracer
    let set_spawn_tracer t = _spawn_tracer := t

    let rec getcwd () = try Unix.getcwd () with
    | Unix.Unix_error (Unix.EINTR, _, _) -> getcwd ()
    | Unix.Unix_error (e, _, _) -> Fmt.failwith_notrace "getcwd: %s" (uerror e)

    let rec chdir cwd = try Unix.chdir cwd with
    | Unix.Unix_error (Unix.EINTR, _, _) -> chdir cwd
    | Unix.Unix_error (e, _, _) ->
        Fmt.failwith_notrace "chdir %s: %s" cwd (uerror e)

    let spawn_err cmd e = match Cmd.is_empty cmd with
    | true -> Result.error e
    | false -> Fmt.error "cmd %s: %s" (Cmd.to_string cmd) e

    let spawn_cwd = function None -> getcwd () | Some d -> Fpath.to_string d
    let spawn_env = function
    | None -> Unix.environment ()
    | Some e -> Array.of_list e

    let _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd =
      match Cmd.to_list cmd with
      | [] -> failwith "no command, empty command line"
      | line ->
          try
            let env' = spawn_env env in
            let cwd' = spawn_cwd cwd in
            let line = Array.of_list line in
            let exe = line.(0) in
            let stdin = stdi_to_fd fds stdin in
            let stdout = stdo_to_fd fds stdout in
            let stderr = stdo_to_fd fds stderr in
            let old_cwd = getcwd () in
            let change_cwd = not @@ String.equal old_cwd cwd' in
            if change_cwd then chdir cwd';
            let pid = Unix.create_process_env exe line env' stdin stdout stderr
            in
            if change_cwd then chdir old_cwd; (* XXX pid zombie on fail. *)
            Fd.Set.close_all fds;
            !_spawn_tracer (Some pid) env ~cwd cmd;
            pid
          with
          | e ->
              (* In case one of the std{i,o}_to_fd raises *)
              let add_out_fd fds = function
              | Out_fd { fd ; close = true } -> Fd.Set.add fd fds
              | _ -> ()
              in
              add_out_fd fds stdout; add_out_fd fds stderr;
              raise e

    (* Blocking command execution *)

    let rec run_collect pid = match Unix.waitpid [] pid with
    | _, status -> status_of_unix_status status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> run_collect pid
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.failwith_notrace "waitpid [%d]: %s" pid (uerror e)

    let run_status
        ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
        ?(stderr = out_stderr) cmd
      =
      let fds = Fd.Set.empty () in
      try
        let pid = _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd in
        Ok (run_collect pid)
      with
      | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
      | Unix.Unix_error (e, _, _) ->
          Fd.Set.close_all fds; spawn_err cmd (uerror e)

    let run_status_out
        ?env ?cwd ?(stdin = in_stdin) ?(stderr = `Stdo out_stderr) ~trim cmd
      =
      let fds = Fd.Set.empty () in
      try
        let flags = Unix.[O_RDWR; O_CREAT; O_EXCL; O_SHARE_DELETE; O_CLOEXEC] in
        let tmpf, fd = Result.error_to_failure (Tmp.open' ~flags ()) in
        let stdout = out_fd ~close:false fd in
        let stderr = match stderr with `Out -> stdout | `Stdo o -> o in
        let pid = _spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd in
        let status = run_collect pid in
        let out = Fd.read_file tmpf fd in
        Tmp.rem_file tmpf;
        Ok (status, if trim then String.trim out else out)
      with
      | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
      | Unix.Unix_error (e, _, _) ->
          Fd.Set.close_all fds; spawn_err cmd (uerror e)

    let run ?env ?cwd ?stdin ?stdout ?stderr cmd =
      match run_status ?env ?cwd ?stdin ?stdout ?stderr cmd with
      | Ok (`Exited 0) -> Ok ()
      | Ok st -> Fmt.error "%a" pp_cmd_status (cmd, st)
      | Error _ as e -> e

    let run_out ?env ?cwd ?stdin ?stderr ~trim cmd =
      match run_status_out ?env ?cwd ?stdin ?stderr ~trim cmd with
      | Error _ as e -> e
      | Ok (`Exited 0, v) -> Ok v
      | Ok (st, "") -> Fmt.error "%a" pp_cmd_status (cmd, st)
      | Ok (st, o) ->
          Fmt.error "@[<v>%a after outputing:@, @[%a@]@]"
            pp_cmd_status (cmd, st) Fmt.lines o

    (* Non-blocking command *)

    type pid = int
    let pid_to_int pid = pid

    let spawn
        ?env ?cwd ?(stdin = in_stdin) ?(stdout = out_stdout)
        ?(stderr = out_stderr) cmd
      =
      let fds = Fd.Set.empty () in
      try Ok (_spawn fds ?env ?cwd ~stdin ~stdout ~stderr cmd) with
      | Failure e -> Fd.Set.close_all fds; spawn_err cmd e
      | Unix.Unix_error (e, _, _) ->
          Fd.Set.close_all fds; spawn_err cmd (uerror e)

    let rec spawn_poll_status pid = match Unix.waitpid Unix.[WNOHANG] pid with
    | 0, _ -> Ok None
    | _, status -> Ok (Some (status_of_unix_status status))
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> spawn_poll_status pid
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.error "poll_status: waitpid %d: %s" pid (uerror e)

    let rec spawn_wait_status pid = match Unix.waitpid [] pid with
    | _, status -> Ok (status_of_unix_status status)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> spawn_wait_status pid
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.error "wait_status: waitpid %d: %s" pid (uerror e)

    let kill pid sg = match Unix.kill pid sg with
    | () -> Ok ()
    | exception Unix.Unix_error (e, _, _) ->
        Fmt.error "kill %d with %a: %s" pid Fmt.sys_signal sg (uerror e)

    (* execv

       On Windows when Unix.execv[e] is invoked, control is returned to
       the controlling terminal when the child process starts (vs. child
       process terminates on POSIX). This entails all sort of weird
       behaviour. To workaround this, our execv[e] on Windows simply
       runs the program as a sub-process on which we waitpid(2) and then
       exit with the resulting status. *)

    let _execv_win32 ~env file cmd =
      let exit pid = match Unix.waitpid [] pid with
      | _, (Unix.WEXITED c) -> exit c
      | _, (Unix.WSIGNALED sg) ->
          Unix.(kill (getpid ()) sg);
          (* In case we don't get killed, exit with bash convention. *)
          exit (128 + sg)
      | _ -> assert false
      in
      let env = spawn_env env in
      exit Unix.(create_process_env file cmd env stdin stderr stderr)

    let _execv_posix ~env file cmd =
      Ok (Unix.execve file cmd (spawn_env env))

    let _execv = if Sys.win32 then _execv_win32 else _execv_posix

    let execv ?env ?cwd ?argv0 cmd =
      let err_execv f e = Fmt.error "execv %a: %s" Fpath.pp f e in
      match Cmd.to_list cmd with
      | [] -> Error "execv: empty command line"
      | file :: args as all ->
          let args = match argv0 with None -> all | Some n -> n :: args in
          try
            let file = Path._realpath (Fpath.to_string file) in
            let reset_cwd = match cwd with
            | None -> Fun.id
            | Some cwd ->
              let old_cwd = getcwd () in
              chdir cwd; fun () -> try chdir old_cwd with Failure _ -> ()
            in
            Fun.protect ~finally:reset_cwd @@ fun () ->
            !_spawn_tracer None env ~cwd cmd;
            _execv ~env file (Array.of_list args)
          with
          | Failure e -> err_execv file e
          | Unix.Unix_error (e, _, _) -> err_execv file (uerror e)

    type t = Cmd.t
  end

  module Exit = struct
    (* Very ugly, but Log depends on a bunch of stuff in Os *)
    let log_error : (string -> unit) ref = ref (fun _ -> assert false)

    type code = int
    type execv =
      { env : Env.assignments option;
        cwd : Fpath.t option;
        argv0 : string option;
        cmd : Cmd.t }

    type t = Code : code -> t | Execv : execv -> t
    let code c = Code c
    let execv ?env ?cwd ?argv0 cmd = Execv { env; cwd; argv0; cmd }

    let get_code = function Code c -> c | _ -> invalid_arg "not an Exit.Code"

    let ok = Code 0
    let no_such_name = Code 122
    let some_error = Code 123
    let cli_error = Code 124
    let internal_error = Code 125

    let exit_some_error e = !log_error e; some_error
    let of_result = function Ok () -> ok | Error e -> exit_some_error e
    let of_result' = function Ok e -> e | Error e -> exit_some_error e

    let execv_env e = e.env
    let execv_cwd e = e.cwd
    let execv_argv0 e = e.argv0
    let execv_cmd e = e.cmd

    let rec exit ?(on_error = some_error) = function
    | Code c -> Stdlib.exit c
    | Execv { env; cwd; argv0; cmd } ->
        match Cmd.execv ?env ?cwd ?argv0 cmd with
        | Ok _ -> assert false
        | Error e -> !log_error e; exit on_error

    let on_sigint ~hook f =
      let hook _ = hook (); Stdlib.exit 130 (* as if SIGINT signaled *) in
      let previous = Sys.signal Sys.sigint (Sys.Signal_handle hook) in
      let restore () = Sys.set_signal Sys.sigint previous in
      Fun.protect ~finally:restore f
  end

  (* Sleeping and timing *)

  let sleep = Os_mtime.sleep
  let relax = relax

  module Cpu = struct
    external logical_count : unit -> int = "ocaml_b0_cpu_logical_count"

    (* Measuring CPU time *)

    module Time = struct

      (* CPU time spans *)

      module Span = struct
        type t =
          { utime : Mtime.Span.t; stime : Mtime.Span.t;
            children_utime : Mtime.Span.t; children_stime : Mtime.Span.t; }

        let make ~utime ~stime ~children_utime ~children_stime =
          { utime; stime; children_utime; children_stime }

        let zero =
          make ~utime:0L ~stime:0L ~children_utime:0L ~children_stime:0L

        let utime c = c.utime
        let stime c = c.stime
        let children_utime c = c.children_utime
        let children_stime c = c.children_stime
      end

      (* CPU counters *)

      let sec_to_span sec = Int64.of_float (sec *. 1e9)

      type counter = Span.t
      let counter () : Span.t =
        let now = Unix.times () in
        { utime = sec_to_span now.Unix.tms_utime;
          stime = sec_to_span now.Unix.tms_stime;
          children_utime = sec_to_span now.Unix.tms_cutime;
          children_stime = sec_to_span now.Unix.tms_cstime; }

      let count (c : Span.t) : Span.t =
        let now = Unix.times () in
        { utime = Int64.sub (sec_to_span now.Unix.tms_utime) c.utime;
          stime = Int64.sub (sec_to_span now.Unix.tms_stime) c.stime;
          children_utime =
            Int64.sub (sec_to_span now.Unix.tms_cutime) c.children_utime;
          children_stime =
            Int64.sub (sec_to_span now.Unix.tms_cstime) c.children_stime; }
    end
  end

  module Mtime = struct
    include Os_mtime
  end
end

module Log = struct

  (* Reporting levels *)

  type level = Quiet | Stdout | Stderr | Error | Warning | Info | Debug
  let _level = ref Warning
  let level () = !_level
  let set_level l = _level := l
  let level_to_string = function
  | Quiet -> "quiet" | Stdout -> "stdout" | Stderr -> "stderr"
  | Error -> "error" | Warning -> "warning"
  | Info -> "info" | Debug -> "debug"

  let level_of_string s = match String.trim s with
  | "quiet" -> Ok Quiet
  | "stdout" -> Ok Stdout
  | "stderr" -> Ok Stderr
  | "warning" -> Ok Warning
  | "info" ->  Ok Info
  | "debug" ->  Ok Debug
  | e ->
      let pp_level = Fmt.code in
      let kind = Fmt.any "log level" in
      let dom =
        ["quiet"; "stdout"; "stderr"; "error"; "warning"; "info"; "debug"]
      in
      Fmt.error "%a" Fmt.(unknown' ~kind pp_level ~hint:must_be) (e, dom)

  (* Reporting *)

  let stdout_style = [`Fg `Cyan]
  let stderr_style = [`Fg `Cyan]
  let err_style = [`Fg `Red]
  let warn_style = [`Fg `Yellow]
  let info_style = [`Fg `Blue]
  let debug_style = [`Faint; `Fg `Magenta]

  let pp_level_str level ppf v = match level with
  | Stdout -> Fmt.st stdout_style ppf v
  | Stderr -> Fmt.st stderr_style ppf v
  | Error -> Fmt.st err_style ppf v
  | Warning -> Fmt.st warn_style ppf v
  | Info -> Fmt.st info_style ppf v
  | Debug -> Fmt.st debug_style ppf v
  | Quiet -> assert false

  let pp_level ppf level = match level with
  | Stdout | Stderr -> ()
  | Error -> Fmt.st (`Bold :: err_style) ppf "Error"
  | Warning -> Fmt.st (`Bold :: warn_style) ppf "Warning"
  | Info -> Fmt.st (`Bold :: info_style) ppf "Info"
  | Debug -> Fmt.st (`Bold :: debug_style) ppf "Debug"
  | Quiet -> assert false

  let pp_header =
    let x = match Array.length Sys.argv with
    | 0 -> Filename.basename Sys.executable_name
    | n -> Filename.basename Sys.argv.(0)
    in
    let pp_header ppf (l, h) = match h with
    | None ->
        if l = Stderr || l = Stdout then () else
        Fmt.pf ppf "%s: %a: " x pp_level l
    | Some "" -> ()
    | Some h -> Fmt.pf ppf "%s: [%a] " x (pp_level_str l) h
    in
    pp_header

  (* Log functions *)

  let _err_count = ref 0
  let err_count () = !_err_count

  let _warn_count = ref 0
  let warn_count () = !_warn_count

  type ('a, 'b) msgf =
    (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

  type 'a log = ('a, unit) msgf -> unit
  type 'a func = { log : 'a. 'a log }

  let log func = func.log

  type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }

  let report level k msgf =
    msgf @@ fun ?header fmt ->
    let k _ = k () in
    let ppf =
      if level = Stdout then Format.std_formatter else Format.err_formatter
    in
    Format.kfprintf k ppf ("@[%a" ^^ fmt ^^ "@]@.") pp_header (level, header)

  let kmsg_nop = let kmsg k level msgf = k () in { kmsg }
  let kmsg_default =
    let kmsg k level msgf = match !_level with
    | Quiet -> k ()
    | level' when level > level' || level = Quiet ->
        (if level = Error then incr _err_count else
         if level = Warning then incr _warn_count else ());
        (k ())
    | _ ->
        (if level = Error then incr _err_count else
         if level = Warning then incr _warn_count else ());
        report level k msgf
    in
    { kmsg }

  let _kmsg = ref kmsg_default
  let set_kmsg kmsg = _kmsg := kmsg

  let kunit _ = ()
  let msg level msgf = !_kmsg.kmsg kunit level msgf
  let quiet msgf = !_kmsg.kmsg kunit Quiet msgf
  let stdout msgf = !_kmsg.kmsg kunit Stdout msgf
  let stderr msgf = !_kmsg.kmsg kunit Stderr msgf
  let err msgf = !_kmsg.kmsg kunit Error msgf
  let warn msgf = !_kmsg.kmsg kunit Warning msgf
  let info msgf = !_kmsg.kmsg kunit Info msgf
  let debug msgf = !_kmsg.kmsg kunit Debug msgf
  let kmsg k level msgf = !_kmsg.kmsg k level msgf

  (* Logging result errors *)

  let if_error ?(level = Error) ?header ~use = function
  | Ok v -> v
  | Error msg ->
      !_kmsg.kmsg (fun _ -> use) level @@ fun m ->
      m ?header "@[%a@]" Fmt.lines msg

  let if_error' ?(level = Error) ?header ~use = function
  | Ok _ as v -> v
  | Error msg ->
      !_kmsg.kmsg (fun _ -> Ok use) level @@ fun m ->
      m ?header "@[%a@]" Fmt.lines msg

  let if_error_pp ?(level = Error) ?header pp ~use = function
  | Ok v -> v
  | Error e ->
      !_kmsg.kmsg (fun _ -> use) level @@ fun m -> m ?header "@[%a@]" pp e

  let if_error_pp' ?(level = Error) ?header pp ~use = function
  | Ok _ as v -> v
  | Error e ->
      !_kmsg.kmsg (fun _ -> Ok use) level @@ fun m -> m ?header "@[%a@]" pp e

  (* Timing logging *)

  let time ?(level = Info) m f =
    let time = Os.Mtime.counter () in
    let r = f () in
    let span = Os.Mtime.count time in
    !_kmsg.kmsg (fun () -> r) level
      (fun w ->
         let header = Format.asprintf "%a" Mtime.Span.pp span in
         m r (w ~header))

  (* Spawn logging *)

  let spawn_tracer level =
    if level = Quiet then Os.Cmd.spawn_tracer_nop else
    let header = function
    | None -> "EXECV"
    | Some pid -> "EXEC:" ^ string_of_int (Os.Cmd.pid_to_int pid)
    in
    let pp_env ppf = function
    | None -> () | Some env -> Fmt.pf ppf "%a@," (Fmt.list String.pp_dump) env
    in
    fun pid env ~cwd cmd ->
      msg level (fun m ->
          m ~header:(header pid) "@[<v>%a%a@]" pp_env env Cmd.pp_dump cmd)

  (* Exit logging *)

  let () =
    Os.Exit.log_error := (fun e -> err (fun m -> m "@[%a@]" Fmt.lines e))
end

module Random_queue = struct
  type 'a t =
    { rand : Random.State.t;
      mutable length : int;
      mutable slots : 'a option array }

  let grow q =
    let slots' = Array.make (2 * q.length) None in
    Array.blit q.slots 0 slots' 0 q.length;
    q.slots <- slots'

  let empty ?(rand = Random.State.make_self_init ()) () =
    { rand; length = 0; slots = Array.make 256 None }

  let add q v =
    if q.length = Array.length q.slots then grow q;
    q.slots.(q.length) <- Some v;
    q.length <- q.length + 1;
    ()

  let take q = match q.length with
  | 0 -> None
  | _ ->
      let i = Random.State.int q.rand q.length in
      let v = q.slots.(i) in
      let last = q.length - 1 in
      q.slots.(i) <- q.slots.(last);
      q.slots.(last) <- None;
      q.length <- last;
      v

  let length q = q.length
end

module Bval = struct
  let already_set () = invalid_arg "already set"

  type 'a t =
  | V of 'a
  | Lazy of 'a Fut.t * (unit -> unit)
  | Fut of ('a Fut.t * ('a -> unit))

  type 'a setter = 'a t
  let make () = let bv = Fut (Fut.make ()) in bv, bv
  let of_val v = V v
  let of_lazy_fun f =
    (* XXX stir should spawn a fiber. *)
    let value, set = Fut.make () in
    let run = ref true in
    let stir () = if !run then (run := true; set (f ())) else () in
    Lazy (value, stir)

  let of_setter = Fun.id
  let is_lazy = function Lazy _ -> true | _ -> false

  (* Setting *)

  let set s v = match s with
  | Fut (fut, set) -> set v
  | _ -> assert false

  let try_set s v = match s with
  | Fut (fut, set) ->
      (match Fut.value fut with None -> set v; true | Some _ -> false)
  | _ -> assert false

  (* Getting *)

  let get = function
  | V v -> Fut.return v
  | Lazy (fut, stir) -> stir (); fut
  | Fut (fut, _) -> fut

  let poll = function
  | V v -> Some v
  | Lazy (fut, stir) -> stir (); Fut.value fut
  | Fut (fut, _) -> Fut.value fut

  let stir = function Lazy (_, stir) -> stir () | _ -> ()

  (* Formatting *)

  let pp pp_v ppf = function
  | V v -> pp_v ppf v
  | Lazy (fut, _) | Fut (fut, _) ->
      match Fut.value fut with
      | None -> Fmt.string ppf "<pending>" | Some v -> pp_v ppf v
end
