(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


include String

let invalid_start ~start len =
  let i = Int.to_string in
  invalid_arg @@ concat "" ["start: "; i start; "not in range [0;"; i len; "]"]

(* Strings *)

let empty = ""
let head s = if s = "" then None else Some s.[0]
let of_char c = make 1 c
let is_empty s = equal empty s

(* Finding indices *)

(* FIXME make indices work like find_sub *)

let find_index ?(start = 0) sat s =
  let max = length s - 1 in
  let i = ref start in
  while (!i <= max && not (sat s.[!i])) do incr i done;
  if !i > max then None else Some !i

let rfind_index ?start sat s =
  let start = match start with None -> length s - 1 | Some s -> s in
  let i = ref start in
  while (0 <= !i && not (sat s.[!i])) do decr i done;
  if !i < 0 then None else Some !i

(* Finding substrings.  *)

(* Two way string search, see https://doi.org/10.1145/116825.116845 or
   http://www-igm.univ-mlv.fr/~lecroq/string/node26.html#SECTION00260 *)

let find_maximal_suffix_and_period ~sub =
  let sublen = length sub in
  let i = ref (-1) and j = ref 0 and k = ref 1 and p = ref 1 in
  let[@inline] maximal_suffix ~order =
    while (!j + !k < sublen) do
      let c = order * Char.compare (get sub (!j + !k)) (get sub (!i + !k)) in
      if c < 0 then (j := !j + !k; k := 1; p := !j - !i) else
      if c > 0 then (i := !j; j := !i + 1; k := 1; p := 1) else (* c = 0 *)
      if !k = !p then (j := !j + !p; k := 1) else incr k
    done;
  in
  (maximal_suffix[@inlined]) ~order:1;
  let l0 = !i and p0 = !p in
  i := -1; j := 0; k := 1; p := 1;
  (maximal_suffix[@inlined]) ~order:(-1);
  let l1 = !i and p1 = !p in
  if l0 > l1 then (l0, p0) else (l1, p1)

let periodic_sub ~sub ~sub_lp:(l, p) =
  let i = ref 0 in
  while !i <= l && Char.equal (get sub !i) (get sub (!i + p))
  do incr i done;
  !i > l

let primitive_find_sub ~start ~sub ~sub_lp:(l, p as sub_lp) s =
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
  let sublen = length sub in
  let smax = slen - sublen in
  let j = ref start in
  try
    if periodic_sub ~sub ~sub_lp then begin
      let memory = ref (-1) in
      while (!j <= smax) do
        let i = ref (1 + Int.max l !memory) in
        while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
        do incr i done;
        if !i < sublen then (j := !j + (!i - l); memory := -1) else
        begin
          i := l;
          while (!i > !memory && Char.equal (get sub !i) (get s (!i + !j)))
          do decr i done;
          if !i <= !memory then raise_notrace Exit else
          (j := !j + p; memory := sublen - p - 1)
        end
      done;
      -1
    end else begin
      let p = 1 + Int.max (l + 1) (sublen - l - 1) in
      while (!j <= smax) do
        let i = ref (l + 1) in
        while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
        do incr i done;
        if !i < sublen then (j := !j + (!i - l)) else
        begin
          i := l;
          while (!i >= 0 && Char.equal (get sub !i) (get s (!i + !j)))
          do decr i done;
          if !i < 0 then raise_notrace Exit else (j := !j + p)
        end
      done;
      -1
    end
  with Exit -> !j

let primitive_rfind_sub ~start ~sub ~sub_lp:(l, p as sub_lp) s =
  (* Note this is the same as above except for the assignement
       and test logic on [j] where we move from right to left. *)
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
  let sublen = length sub in
  let smax = slen - sublen in
  let j = ref (if start > smax then smax else start) in
  try
    if periodic_sub ~sub ~sub_lp then begin
      let memory = ref (-1) in
      while (!j >= 0) do
        let i = ref (1 + Int.max l !memory) in
        while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
        do incr i done;
        if !i < sublen then (j := !j - (!i - l); memory := -1) else
        begin
          i := l;
          while (!i > !memory && Char.equal (get sub !i) (get s (!i + !j)))
          do decr i done;
          if !i <= !memory then raise_notrace Exit else
          (j := !j - p; memory := sublen - p - 1)
        end
      done;
      -1
    end else begin
      let p = 1 + Int.max (l + 1) (sublen - l - 1) in
      while (!j >= 0) do
        let i = ref (l + 1) in
        while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
        do incr i done;
        if !i < sublen then (j := !j - (!i - l)) else
        begin
          i := l;
          while (!i >= 0 && Char.equal (get sub !i) (get s (!i + !j)))
          do decr i done;
          if !i < 0 then raise_notrace Exit else (j := !j - p)
        end
      done;
      -1
    end
  with Exit -> !j

let includes ~affix:sub s =
  let sub_lp = find_maximal_suffix_and_period ~sub in
  primitive_find_sub ~start:0 ~sub ~sub_lp s <> -1

let find_sub ?(start = 0) ~sub s =
  let sub_lp = find_maximal_suffix_and_period ~sub in
  match primitive_find_sub ~start ~sub_lp ~sub s with
  | -1 -> None | i -> Some i

let rfind_sub ?start ~sub s =
  let start = match start with None -> length s | Some s -> s in
  let sub_lp = find_maximal_suffix_and_period ~sub in
  match primitive_rfind_sub ~start ~sub_lp ~sub s with
  | -1 -> None | i -> Some i

let find_all_sub ?(start = 0) f ~sub s acc =
  let rec loop f acc sub sub_lp s ~start ~slen =
    if start > slen then acc else
    match primitive_find_sub ~start ~sub ~sub_lp s with
    | -1 -> acc
    | i ->
        let acc = f i acc in
        let start = i + length sub in
        let start = if start = i then start + 1 else start in
        loop f acc sub sub_lp s ~start ~slen
  in
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
  let sub_lp = find_maximal_suffix_and_period ~sub in
  loop f acc sub sub_lp s ~start ~slen

let rfind_all_sub ?start f ~sub s acc =
  let rec loop f acc sub sub_lp s ~start ~slen =
    if start < 0 then acc else
    match primitive_rfind_sub ~start ~sub ~sub_lp s with
    | -1 -> acc
    | i ->
        let start = i - Int.max (length sub) 1 in
        loop f (f i acc) sub sub_lp s ~start ~slen
  in
  let slen = length s in
  let start = match start with None -> length s | Some s -> s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
  let sub_lp = find_maximal_suffix_and_period ~sub in
  loop f acc sub sub_lp s ~start ~slen

let replace_first ?(start = 0) ~sub:needle ~by s =
  let sub_lp = find_maximal_suffix_and_period ~sub:needle in
  match primitive_find_sub ~start ~sub:needle ~sub_lp s with
  | -1 -> s
  | i ->
      let rest_first = i + length needle in
      let rest_len = length s - i - length needle in
      concat by [sub s 0 i; sub s rest_first rest_len]

let replace_all ?start ~sub:needle ~by s =
  let chunk_first = ref 0 in
  let add_chunk i acc =
    let acc = sub s !chunk_first (i - !chunk_first) :: acc in
    chunk_first := i + length needle; acc
  in
  match find_all_sub ?start add_chunk ~sub:needle s [] with
  | [] -> s
  | chunks ->
      let chunks = sub s !chunk_first (length s - !chunk_first) :: chunks in
      concat by (List.rev chunks)

(* Extracting substrings *)

let subrange ?(first = 0) ?last s =
  let max = length s - 1 in
  let first = if first < 0 then 0 else first in
  let last = match last with None -> max | Some last -> last in
  let last = if last > max then max else last in
  if first > last then "" else sub s first (last - first + 1)

(* Breaking with magnitudes *)

let take n s = subrange ~last:(n - 1) s
let drop n s = subrange ~first:n s
let span n s = (take n s, drop n s)
let rtake n s = subrange ~first:(length s - n) s
let rdrop n s = subrange ~last:(length s - n - 1) s
let rspan n s = (rdrop n s, rtake n s)

(* Breaking with predicates *)

let take_while sat s =
  let len = length s and i = ref 0 in
  while !i < len && sat (unsafe_get s !i) do incr i done;
  if !i = len then s else sub s 0 !i

let drop_while sat s =
  let len = length s and i = ref 0 in
  while !i < len && sat (unsafe_get s !i) do incr i done;
  if !i = 0 then s else sub s !i (len - !i)

let span_while sat s =
  let len = length s and i = ref 0 in
  while !i < len && sat (unsafe_get s !i) do incr i done;
  if !i = len then s, "" else
  if !i = 0 then "", s else
  sub s 0 !i, sub s !i (len - !i)

let rtake_while sat s =
  let len = length s in
  let i = ref (len - 1) in
  while !i >= 0 && sat (unsafe_get s !i) do decr i done;
  if !i < 0 then s else sub s (!i + 1) (len - (!i + 1))

let rdrop_while sat s =
  let len = length s in
  let i = ref (len - 1) in
  while !i >= 0 && sat (unsafe_get s !i) do decr i done;
  if !i < 0 then "" else sub s 0 (!i + 1)

let rspan_while sat s =
  let len = length s in
  let i = ref (len - 1) in
  while !i >= 0 && sat s.[!i] do decr i done;
  if !i < 0 then "", s else
  if !i = len - 1 then s, "" else
  let j = !i + 1 in
  sub s 0 j, sub s j (len - j)

(* Breaking with separators *)

(* FIXME use primitive_find_sub *)

let err_empty_sep = "~sep is an empty string"

let cut ~sep s =
  let sep_len = length sep in
  if sep_len = 0 then invalid_arg err_empty_sep else
  let s_len = length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - sep_len in
  let rec check_sep i k = match k > max_sep_idx with
  | true ->
      let r_start = i + sep_len in
      Some (sub s 0 i, sub s r_start (s_len - r_start))
  | false ->
      if unsafe_get s (i + k) = unsafe_get sep k
      then check_sep i (k + 1)
      else scan (i + 1)
  and scan i =
    if i > max_s_idx then None else
    if get s i = get sep 0 then check_sep i 1 else scan (i + 1)
  in
  scan 0

let rcut ~sep s =
  let sep_len = length sep in
  if sep_len = 0 then invalid_arg err_empty_sep else
  let s_len = length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - 1 in
  let rec check_sep i k = match k > max_sep_idx with
  | true ->
      let r_start = i + sep_len in
      Some (sub s 0 i, sub s r_start (s_len - r_start))
  | false ->
      if unsafe_get s (i + k) = unsafe_get sep k
      then check_sep i (k + 1)
      else rscan (i - 1)
  and rscan i =
    if i < 0 then None else
    if get s i = get sep 0 then check_sep i 1 else rscan (i - 1)
  in
  rscan (max_s_idx - max_sep_idx)

let split ?(drop_empty = false) ~sep s =
  let rec loop acc s = match cut ~sep s with
  | Some (v, vs) -> loop (if drop_empty && v = "" then acc else (v :: acc)) vs
  | None -> List.rev (if drop_empty && s = "" then acc else (s :: acc))
  in
  loop [] s

let rsplit ?(drop_empty = false) ~sep s =
  let rec loop acc s = match rcut ~sep s with
  | Some (vs, v) -> loop (if drop_empty && v = "" then acc else (v :: acc)) vs
  | None -> if drop_empty && s = "" then acc else (s :: acc)
  in
  loop [] s

(* Breaking lines *)

let fold_ascii_lines ~strip_newlines:strip f acc s =
  let rec loop ~strip linenum s start i max f acc =
    if i > max then
        f linenum acc (if start = 0 then s else sub s start (i - start))
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
    let line = sub s start (after_line_data - start) in
    let acc = f linenum acc line in
    loop ~strip (linenum + 1) s next_start next_start max f acc
  in
  if s = "" then acc else loop ~strip 1 s 0 0 (length s - 1) f acc

let detach_ascii_newline s =
  if s = "" then ("", "") else
  let len = length s in
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
  sub s 0 newline_start,
  sub s newline_start (len - newline_start)

(* Tokenize *)

let next_token
    ?(is_sep = B0__char.Ascii.is_white)
    ?(is_token = B0__char.Ascii.is_graphic) s
  =
  let len = length s in
  let i = ref 0 in
  while !i < len && is_sep s.[!i] do incr i done;
  if !i >= len then "", s else
  let first = !i in
  while !i < len && is_token s.[!i] do incr i done;
  if !i = first then "", subrange ~first s else
  subrange ~first ~last:(!i - 1) s, subrange ~first:!i s

let tokens ?(is_sep = B0__char.Ascii.is_white) s =
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
    loop (sub s first (last - first + 1) :: acc) s (first - 1)
  in
  loop [] s (length s - 1)

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
  let rec loop s max i l =
    if i > max then l else
    loop s max (i + 1) (l + char_len s.[i])
  in
  loop s (length s - 1) 0 0

let byte_replace set_char s ~len ~replaced_len =
  let b = Bytes.create replaced_len in
  let rec loop s max i k =
    if i > max then Bytes.unsafe_to_string b else
    loop s max (i + 1) (set_char b k s.[i])
  in
  loop s (len - 1) 0 0

let byte_escaper char_len set_char s =
  let len = length s in
  let replaced_len = byte_replaced_length char_len s in
  if replaced_len = len then s else
  byte_replace set_char s ~len ~replaced_len

let byte_replacer char_len set_char s =
  let len = length s in
  let replaced_len = byte_replaced_length char_len s in
  byte_replace set_char s ~len ~replaced_len

exception Illegal_escape of int (* index *)

let byte_unreplace_length char_len_at s =
  let rec loop max i len =
    if i > max then len else
    let esc_len = char_len_at s i in
    loop max (i + esc_len) (len - esc_len + 1)
  in
  loop (length s - 1) 0 (length s)

let byte_unreplace set_char s ~len ~unreplace_len =
  let b = Bytes.create unreplace_len in
  let rec loop s max i k =
    if i > max then Ok (Bytes.unsafe_to_string b) else
    loop s max (set_char b k s i) (k + 1)
  in
  loop s (length s - 1) 0 0

let byte_unescaper char_len_at set_char s =
  try
    let len = length s in
    let unreplace_len = byte_unreplace_length char_len_at s in
    if len = unreplace_len then Ok s else
    byte_unreplace set_char s ~len ~unreplace_len
  with
  | Illegal_escape i -> Error i

let byte_unreplacer char_len_at set_char s =
  try
    let len = length s in
    let unreplace_len = byte_unreplace_length char_len_at s in
    byte_unreplace set_char s ~len ~unreplace_len
  with
  | Illegal_escape i -> Error i

(* ASCII string support *)

module Ascii = struct

  (* Predicates *)

  let is_valid s =
    let rec loop max i = match i > max with
    | true -> true
    | false when unsafe_get s i > B0__char.Ascii.max -> false
    | false -> loop max (i + 1)
    in
    loop (length s - 1) 0

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

  let uppercase s =
    caseify B0__char.Ascii.is_lower B0__char.Ascii.uppercase s

  let lowercase s =
    caseify B0__char.Ascii.is_upper B0__char.Ascii.lowercase s

  let caseify_first is_not_case to_case s =
    if length s = 0 then s else
    let c = unsafe_get s 0 in
    if not (is_not_case c) then s else
    let b = Bytes.of_string s in
    Bytes.unsafe_set b 0 (to_case c);
    Bytes.unsafe_to_string b

  let capitalize s =
    caseify_first B0__char.Ascii.is_lower B0__char.Ascii.uppercase s

  let uncapitalize s =
    caseify_first B0__char.Ascii.is_upper B0__char.Ascii.lowercase s

  (* Converting to ASCII hexadecimal characters *)

  let to_hex s =
    let rec loop max s i h k = match i > max with
    | true -> Bytes.unsafe_to_string h
    | false ->
        let c = get_uint8 s i in
        Bytes.set h (k    ) (B0__char.Ascii.lower_hex_digit_of_int (c lsr 4));
        Bytes.set h (k + 1) (B0__char.Ascii.lower_hex_digit_of_int (c      ));
        loop max s (i + 1) h (k + 2)
    in
    let len = length s in
    let h = Bytes.create (2 * len) in
    loop (len - 1) s 0 h 0

  let of_hex' h =
    let hex_value s i = match s.[i] with
    | '0' .. '9' as c -> Char.code c - 0x30
    | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
    | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
    | _ -> raise_notrace (Illegal_escape i)
    in
    match length h with
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
      match i = length h with
      | true -> Error "Missing final hex digit"
      | false -> B0__fmt.error "Byte %d: not an ASCII hexadecimal digit" i

  (* Converting to printable ASCII characters *)

  let set_ascii_unicode_escape b k c = (* for c <= 0x7F *)
    let byte = Char.code c in
    Bytes.blit_string "\\u{00" 0 b k 5;
    Bytes.set b (k + 5) (B0__char.Ascii.upper_hex_digit_of_int (byte lsr 4));
    Bytes.set b (k + 6) (B0__char.Ascii.upper_hex_digit_of_int (byte      ));
    Bytes.set b (k + 7) '}';
    k + 8

  let set_hex_escape b k c =
    let byte = Char.code c in
    Bytes.blit_string "\\x" 0 b k 2;
    Bytes.set b (k + 2) (B0__char.Ascii.upper_hex_digit_of_int (byte lsr 4));
    Bytes.set b (k + 3) (B0__char.Ascii.upper_hex_digit_of_int (byte      ));
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
          let max = length s - 1 in
          let j = i + 1 in
          if j > max then raise_notrace (Illegal_escape i) else
          if s.[j] <> 'x' then raise_notrace (Illegal_escape i) else
          let j = i + 3 in
          if j > max then raise_notrace (Illegal_escape i) else
          if B0__char.Ascii.is_hex_digit s.[i + 2] &&
             B0__char.Ascii.is_hex_digit s.[i + 3]
          then 4
          else raise (Illegal_escape i) (* invalid esc *)
    in
    let set_char b k s i = match s.[i] <> '\\' with
    | true -> Bytes.set b k s.[i]; i + 1
    | false ->
        (* assert (s.[i+1] = 'x') *)
        let hi = B0__char.Ascii.hex_digit_to_int s.[i + 2] in
        let lo = B0__char.Ascii.hex_digit_to_int s.[i + 3] in
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
    let char_len_at s i =
      if s.[i] <> '\\' then 1 else
      let max = length s - 1 in
      let j = i + 1 in
      if j > max then raise_notrace (Illegal_escape i) else
      match s.[j] with
      | 'x' ->
          let j = i + 3 in
          if j > max then raise_notrace (Illegal_escape i) else
          if B0__char.Ascii.is_hex_digit s.[i + 2] &&
             B0__char.Ascii.is_hex_digit s.[i + 3]
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
      | c when B0__char.Ascii.is_digit c ->
          let j = i + 3 in
          if j > max then raise_notrace (Illegal_escape i) else
          if B0__char.Ascii.is_digit s.[i + 2] &&
             B0__char.Ascii.is_digit s.[i + 3]
          then 4
          else raise_notrace (Illegal_escape i)
      | _ -> raise_notrace (Illegal_escape i)
    in
    let set_char b k s i =
      if s.[i] <> '\\' then (Bytes.set b k s.[i]; i + 1) else
      match s.[i + 1] with
      | 'x' ->
          let hi = B0__char.Ascii.hex_digit_to_int s.[i + 2] in
          let lo = B0__char.Ascii.hex_digit_to_int s.[i + 3] in
          Bytes.set b k (Char.chr @@ (hi lsl 4) lor lo); i + 4
      | '\\' -> Bytes.set b k '\\'; i + 2
      | 'b' -> Bytes.set b k '\b'; i + 2
      | 't' -> Bytes.set b k '\t'; i + 2
      | 'n' -> Bytes.set b k '\n'; i + 2
      | 'r' -> Bytes.set b k '\r'; i + 2
      | ' ' -> Bytes.set b k ' '; i + 2
      | '"' -> Bytes.set b k '"'; i + 2
      | 'o' ->
          let o3 = B0__char.Ascii.hex_digit_to_int s.[i + 2] in
          let o2 = B0__char.Ascii.hex_digit_to_int s.[i + 3] in
          let o1 = B0__char.Ascii.hex_digit_to_int s.[i + 4] in
          let byte = o3 * 64 + o2 * 8 + o1 in
          if byte > 255 then raise_notrace (Illegal_escape i) else
          Bytes.set b k (Char.chr byte); i + 5
      | c when B0__char.Ascii.is_digit c ->
          let d3 = B0__char.Ascii.hex_digit_to_int s.[i + 1] in
          let d2 = B0__char.Ascii.hex_digit_to_int s.[i + 2] in
          let d1 = B0__char.Ascii.hex_digit_to_int s.[i + 3] in
          let byte = d3 * 100 + d2 * 10 + d1 in
          if byte > 255 then raise_notrace (Illegal_escape i) else
          Bytes.set b k (Char.chr byte); i + 4
      | _ -> assert false
    in
    byte_unescaper char_len_at set_char
end

(* Variable substitution *)

let subst_pct_vars ?buf vars s =
  let max = length s - 1 in
  let buf = match buf with
  | None -> Buffer.create (max + 1)
  | Some buf -> Buffer.clear buf; buf
  in
  let add buf s ~start ~last =
    Buffer.add_substring buf s start (last - start + 1)
  in
  let rec find_var_end s i max =
    if i > max || i + 1 > max then None else
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

(* Formatting *)

let pp = B0__fmt.string

(* Sets and maps *)

module Set = struct
  include Set.Make (String)
  let pp ?sep pp_elt = B0__fmt.iter ?sep iter pp_elt
  let pp_dump ppf ss =
    B0__fmt.pf ppf "@[<1>{%a}@]"
      (pp ~sep:B0__fmt.sp B0__fmt.OCaml.string) ss
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

  let get_or_hint ?(pp_key = B0__fmt.code) ~kind k m =
    match get_or_suggest k m with
    | Ok _ as v -> v
    | Error suggs ->
        let kind ppf () = B0__fmt.string ppf kind in
        let hint = B0__fmt.did_you_mean in
        let pp = B0__fmt.unknown' ~kind pp_key ~hint in
        B0__fmt.error "@[%a@]" pp (k, suggs)

  let pp ?sep pp_binding = B0__fmt.iter_bindings ?sep iter pp_binding
  let pp_dump pp_v ppf m =
    let pp_binding ppf (k, v) =
      B0__fmt.pf ppf "@[<1>(@[%a@],@ @[%a@])@]"
        B0__fmt.OCaml.string k pp_v v
    in
    B0__fmt.pf ppf "@[<1>{%a}@]" (pp ~sep:B0__fmt.sp pp_binding) m

  let pp_dump_string_map ppf m =
    pp_dump B0__fmt.OCaml.string ppf m
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
      B0__fmt.invalid_arg "Could not make %s unique after %d retries." n limit
  | false ->
      let r = B0__fmt.str "%s~%d" n i in
      if exists r then loop (i + 1) n else r
  in
  if exists n then loop 1 n else n
