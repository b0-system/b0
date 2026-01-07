(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type atom = string
type value = atom list
type name = atom (* but non empty *)
type qname = name list
module Qname = struct type t = qname let compare = Stdlib.compare end
module Qmap = Map.Make (Qname)
type doc = value Qmap.t
type nested_doc = Value of value | Bindings of (string * nested_doc) list

let empty = Qmap.empty
let find q doc = Qmap.find_opt q doc
let to_assoc doc = Qmap.bindings doc
let to_nested_doc doc =
  let rec find_name rev_left n = function
  | (n', nested) :: bs when n = n' -> Either.Right (rev_left, nested, bs)
  | b :: bs -> find_name (b :: rev_left) n bs
  | [] -> Either.Left rev_left
  in
  let rec add qname value nested = match qname with
  | [] -> Value value
  | n :: ns ->
      match nested with
      | Value _ as v -> add qname value (Bindings ["", v])
      | Bindings bs ->
          match find_name [] n bs with
          | Either.Right (rev_left, nested, right) ->
              let b = n, add ns value nested in
              Bindings (List.rev_append (b :: rev_left) right)
          | Either.Left rev_left ->
              let b = n, add ns value (Bindings []) in
              Bindings (List.rev (b :: rev_left))
  in
  Qmap.fold add doc (Bindings [])

let find_section q doc =
  let rec drop_prefix q0 q1 = match q0, q1 with
  | [], q1 -> Some q1
  | n0 :: q0, n1 :: q1 when String.equal n0 n1 -> drop_prefix q0 q1
  | _ -> None
  in
  let add_sub n v acc = match drop_prefix q n with
  | None -> acc | Some n -> Qmap.add n v acc
  in
  Qmap.fold add_sub doc Qmap.empty

let top_sections doc =
  let add_top_name q _ acc = match q with
  | n :: _ :: _ when not (List.mem n acc) -> n :: acc
  | _ -> acc
  in
  List.rev (Qmap.fold add_top_name doc [])

let pp_qname ppf qname =
  let pp_sep ppf () = Format.pp_print_char ppf '.' in
  Format.pp_open_hovbox ppf 1;
  Format.pp_print_list ~pp_sep Format.pp_print_string ppf qname;
  Format.pp_close_box ppf ()

(* Character classes *)

let sot = 0x1A0000  (* start of text U+10FFFF + 1 *)
let eot = 0x1A0001  (* end of text   U+10FFFF + 2 *)

let[@inline] in_urange (u : int) umin umax = (umin <= u) && (u <= umax)
let is_dec_digit u = in_urange u 0x0030 0x0039
let is_upper_hex_digit u = in_urange u 0x0041 0x0046
let is_lower_hex_digit u = in_urange u 0x0061 0x0066
let is_nl_char = function 0x000A | 0x000D -> true | _ -> false
let is_zschar = function
| 0x0020 | 0x00A0 | 0x1680 | 0x202F | 0x205F | 0x3000 -> true
| u when in_urange u 0x2000 0x200A -> true
| _ -> false

let is_uqchar u =
  u = 0x0021 || in_urange u 0x0024 0x003C || in_urange u 0x003E 0x005A ||
  u = 0x005C || in_urange u 0x005E 0x007E || in_urange u 0x00A1 0x1679 ||
  in_urange u 0x1682 0x1FFF || in_urange u 0x200B 0x2027 ||
  in_urange u 0x202A 0x202E || in_urange u 0x2030 0x205E ||
  in_urange u 0x2060 0x2FFF || in_urange u 0x3001 0xD7FF ||
  in_urange u 0xE000 0x10FFFF

let is_qchar u =
  u = 0x0009 (* TAB *) || u = 0x0023 (* # *) || u = 0x003D (* = *) ||
  u = 0x005B (* [ *)   || u = 0x005D (* ] *) || is_zschar u || is_uqchar u

let is_cchar u = u = 0x0022 (* DQUOTE *) || is_qchar u
let is_atom_start u = u = 0x0022 || is_uqchar u

let uchar_to_utf8 u =
  let b = Bytes.create (Uchar.utf_8_byte_length u) in
  ignore (Bytes.set_utf_8_uchar b 0 u); Bytes.unsafe_to_string b

let pp_uchar_err ppf = function (* Formats uchars in error messages *)
| 0x1A0000 -> Format.pp_print_string ppf "start of text"
| 0x1A0001 -> Format.pp_print_string ppf "end of text"
| u when is_cchar u ->
    let utf8 = uchar_to_utf8 (Uchar.of_int u) in
    Format.fprintf ppf "''@<1>%s' (U+%04X)" utf8 u
| u -> Format.fprintf ppf "U+%04X" u

(* Decoder *)

type decoder =
  { file : string;
    i : string;
    mutable u : int; (* Current character scalar value or sot or eot *)
    mutable line : int; (* Current line number *)
    mutable line_start : int; (* Current line first byte position. *)
    mutable next : int; (* Next character byte position. *)
    token : Buffer.t;
    mutable map : value Qmap.t }

let make_decoder ?(file = "-") i =
  let token = Buffer.create 255 and map = Qmap.empty in
  { file; i; u = sot; line = 1; line_start = 0; next = 0; token; map }

(* Decoder tokenizer *)

let token_clear d = Buffer.clear d.token
let token_pop d = let t = Buffer.contents d.token in token_clear d; t
let token_add d u = Buffer.add_utf_8_uchar d.token (Uchar.unsafe_of_int u)

(* Decoder position and errors *)

let col_next d = d.next - d.line_start
let col_u d =
  if d.u = sot then 0 else
  if d.u = eot then (String.length d.i) - d.line_start else
  (d.next - Uchar.utf_8_byte_length (Uchar.of_int d.u)) - d.line_start

let get_loc d = d.line, col_u d

let err_loc d line col fmt =
  Format.kasprintf failwith ("%s:%d:%d: " ^^ fmt) d.file line col

let err_malformed_utf_8 d =
  err_loc d d.line (col_next d) "UTF-8 decoding error at input byte %d" d.next

(* Next character *)

let nextc d =
  if d.next >= String.length d.i then d.u <- eot else
  let udec = String.get_utf_8_uchar d.i d.next in
  if not (Uchar.utf_decode_is_valid udec) then err_malformed_utf_8 d else
  let u = Uchar.to_int (Uchar.utf_decode_uchar udec) in
  d.next <- d.next + Uchar.utf_decode_length udec;
  begin match u with
  | 0x000D (* CR *) -> d.line_start <- d.next; d.line <- d.line + 1;
  | 0x000A (* LF *) ->
      d.line_start <- d.next; if d.u <> 0x000D then d.line <- d.line + 1;
  | _ -> ()
  end;
  d.u <- u

(* Decode errors *)

let err d fmt = err_loc d d.line (col_u d) fmt
let err_eot d = err d "Unexpected end of text"
let err_illegal_char_in kind d =
  err d "Illegal character %a in %s" pp_uchar_err d.u kind

let err_char_comment d = err_illegal_char_in "comment" d
let err_char_qatom d = err_illegal_char_in "quoted atom" d
let err_char_header d = err_illegal_char_in "section header" d

let err_exp kind d = err d "Expected: %s but found: %a" kind pp_uchar_err d.u
let err_uchar_hex_end d = err_exp "whitespace or '}'" d
let err_char_uescape d = err_exp "an hexdecimal digit" d
let err_atom d = err_exp "an atom" d
let err_equal d = err_exp "'='" d
let err_binding d = err_exp "an atom or a section header" d
let err_doc d = err_exp "a binding or a section header" d
let err_soc d = err_exp "newline" d
let err_name d ~start:(l, c) =
  err_loc d l c "Expected: a name but found: an empty atom"

(* Document decodes *)

let rec decode_wchars d = match d.u with
| 0x0020 (* SP  *) -> nextc d; decode_wchars d
| 0x0009 (* TAB *) -> nextc d; decode_wchars d
| 0x200E (* LRM *) -> nextc d; decode_wchars d
| 0x200F (* RLM *) -> nextc d; decode_wchars d
| _ -> ()

let decode_comment d = (* d.u is '#' *)
  let rec loop d =
    if is_cchar d.u then (nextc d; loop d) else
    if is_nl_char d.u || d.u = eot then () else
    err_char_comment d
  in
  nextc d; loop d

let decode_s_skip d =
  decode_wchars d; if d.u = 0x0023 (* # *) then decode_comment d

let decode_skip d = (* returns [true] if a newline was seen. *)
  let rec loop saw_nl d = match d.u with
  | 0x000A (* LF  *) -> nextc d; loop true d
  | 0x000D (* CR  *) -> nextc d; (if d.u = 0x000A then nextc d); loop true d
  | 0x0020 (* SP  *) -> nextc d; loop saw_nl d
  | 0x0009 (* TAB *) -> nextc d; loop saw_nl d
  | 0x200E (* LRM *) -> nextc d; loop saw_nl d
  | 0x200F (* RLM *) -> nextc d; loop saw_nl d
  | 0x0023 (* #   *) -> decode_comment d; loop saw_nl d
  | _ -> saw_nl
  in
  loop false d

let rec decode_uchar_hex d =
  let rec loop d acc count =  match d.u with
  | c when count > 6 -> err_uchar_hex_end d
  | u when is_dec_digit u ->
      nextc d; loop d (acc * 16 + u - 0x30) (count + 1)
  | u when is_upper_hex_digit u ->
      nextc d; loop d (acc * 16 + u - 0x37) (count + 1)
  | u when is_lower_hex_digit u ->
      nextc d; loop d (acc * 16 + u - 0x57) (count + 1)
  | _ when count > 1 -> token_add d acc
  | _ -> err_char_uescape d
  in
  loop d 0 0

let decode_uescape d = (* \u{ was eaten *)
  let rec loop d =
    decode_uchar_hex d; ignore (decode_skip d);
    if d.u = 0x007D (* } *) then nextc d else loop d
  in
  ignore (decode_skip d); loop d

let decode_abreak d = (* \ was eaten *) match d.u with
| 0x000A (* LF *) -> nextc d; decode_wchars d
| 0x000D (* CR *) ->
    nextc d; (if d.u = 0x000A then nextc d); decode_wchars d
| u -> token_add d 0x005C (* \ *)

let decode_escape_or_break d = match d.u with
| (0x0020 (* SP *) | 0x005C (* \ *) | 0x0022 (* DQUOTE *)) as esc ->
    nextc d; token_add d esc
| 0x0075 (* u *) as uletter ->
      nextc d;
      if d.u = 0x007B (* { *) then (nextc d; decode_uescape d) else
      (token_add d 0x005C (* \ *); token_add d uletter;)
| 0x000A (* LF *) | 0x000D (* CR *) -> decode_abreak d
| u (* not an escape *) -> token_add d 0x005C (* \ *)

let decode_sbreak d = token_add d 0x000A (* LF *); decode_wchars d
let decode_qatom d = (* [ was eaten *)
  let rec loop d = match d.u with
    | 0x005C (* \ *) -> nextc d; decode_escape_or_break d; loop d
    | u when is_qchar u -> nextc d; token_add d u; loop d
    | 0x0022 (* DQUOTE *) -> nextc d; token_pop d
    | 0x000A (* LF *) -> nextc d; decode_sbreak d; loop d
    | 0x000D (* CR *) ->
        nextc d; (if d.u = 0x000A then nextc d); decode_sbreak d; loop d
    | u -> err_char_qatom d
  in
  loop d

let decode_uqatom d =
  let rec loop d = match d.u with
  | 0x005C (* \ *) -> nextc d; decode_abreak d; loop d
  | u when is_uqchar u -> nextc d; token_add d u; loop d
  | _ -> token_pop d
  in
  loop d

let decode_atom d = match d.u with
| 0x0022 (* DQUOTE *) -> nextc d; decode_qatom d
| u when is_uqchar u -> decode_uqatom d
| u -> err_atom d

let atom_to_name d ~start n = if n = "" then err_name d ~start else n
let decode_name d =
  let start = get_loc d in
  atom_to_name d ~start (decode_atom d)

let decode_header d = (* d.u is '[', header name is returned reversed *)
  let rec loop d acc = match (ignore (decode_skip d); d.u) with
  | 0x005D (* ] *) -> nextc d; decode_s_skip d; acc
  | u when is_atom_start u -> loop d (decode_name d :: acc);
  | _ -> err_char_header d
  in
  nextc d; loop d []

let decode_bindings d ~secname =
  let add_binding d sec k v =
    d.map <- Qmap.add (List.rev (k :: sec)) (List.rev v) d.map
  in
  let rec loop d sec nl k v = match d.u with
  | u when is_atom_start u ->
      let start = get_loc d in
      let a = decode_atom d in
      let nl' = decode_skip d in
      if not (nl && d.u = 0x003D (* = *)) then loop d sec nl' k (a :: v) else
      let n = atom_to_name d ~start a in
      let nl = nextc d; decode_skip d in
      add_binding d sec k v; loop d sec nl n []
  | 0x005B (* [ *) when nl -> add_binding d sec k v
  | 0x1A0001 (* eot *) -> add_binding d sec k v
    | _ -> err_binding d
  in
  let start = get_loc d in
  let k = atom_to_name d ~start (decode_atom d) in
  ignore (decode_skip d);
  (if d.u <> 0x003D (* = *) then err_equal d else nextc d);
  let nl = decode_skip d in
  loop d secname nl k []

let decode_soc d =
  begin match d.u with
  | 0x000A (* LF *) -> nextc d
  | 0x000D (* CR *) -> nextc d; if d.u = 0x000A (* LF *) then nextc d
  | 0x1A0000 (* sot *) -> nextc d; if d.u = 0xFEFF (* BOM *) then nextc d
  | _ -> ()
  end;
  decode_wchars d

let decode_doc d =
  let rec loop d ~secname (* in reverse order *) = match d.u with
  | 0x005B (* [ *) ->
      let secname = decode_header d in
      decode_soc d; loop d ~secname
  | u when is_atom_start u -> decode_bindings d ~secname; loop d ~secname
  | 0x000A (* LF *) | 0x000D (* CR *) -> decode_soc d; loop d ~secname
  | 0x0023 (* # *) -> decode_comment d; decode_soc d; loop d ~secname
  | 0x1A0001 (* eot *) -> ()
  | _ -> err_doc d
  in
  decode_soc d; loop d ~secname:[]

let of_string ?file i =
  let d = make_decoder ?file i in
  try decode_doc d; Ok d.map with Failure e -> Error e
