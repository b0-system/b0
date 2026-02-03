(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_text

module Json = struct

  (* JSON text *)

  type meta = Textloc.t
  let meta_none = Textloc.none
  type 'a node = 'a * meta
  type name = string node
  type mem = name * t
  and object' = mem list
  and t =
  | Null of unit node
  | Bool of bool node
  | Number of float node
  | String of string node
  | Array of t list node
  | Object of object' node

  let meta = function
  | Null (_, meta) | Bool (_, meta) | Number (_, meta) | String (_, meta)
  | Array (_, meta) | Object (_, meta) -> meta

  let rec compare j0 j1 = match j0, j1 with
  | Null ((), _), Null ((), _) -> 0
  | Null _, _ -> -1 | _, Null _ -> 1
  | Bool (b0, _), Bool (b1, _) -> Bool.compare b0 b1
  | Bool _, _ -> -1 | _, Bool _ -> 1
  | Number (f0, _), Number (f1, _) -> Float.compare f0 f1
  | Number _, _ -> -1 | _, Number _ -> 1
  | String (s0, _), String (s1, _) -> String.compare s0 s1
  | String _, _ -> -1 | _, String _ -> 1
  | Array (a0, _), (Array (a1, _)) -> List.compare compare a0 a1
  | Array _, _ -> -1 | _, Array _ -> 1
  | Object (o0, _), Object (o1, _) ->
      let order_mem ((n0, _), _) ((n1, _), _) = String.compare n0 n1 in
      let compare_mem ((n0, _), j0) ((n1, _), j1) =
        let c = String.compare n0 n1 in
        if c = 0 then compare j0 j1 else c
      in
      List.compare compare_mem (List.sort order_mem o0) (List.sort order_mem o1)

  let equal j0 j1 = compare j0 j1 = 0

  let rec normalize = function
  | Null _ | Bool _ | Number _ | String _ | Array _ as j -> j
  | Object (mems, loc) ->
      let rev_sort_name ((n0, _), _) ((n1, _), _) = String.compare n1 n0 in
      let mems = List.sort rev_sort_name mems in
      let mems = List.rev_map (fun (m, v) -> (m, normalize v)) mems in
      Object (mems, loc)

  (* Constructors *)

  type 'a cons = ?meta:meta -> 'a -> t

  let null ?(meta = meta_none) () = Null ((), meta)
  let option cons ?meta = function
  | None -> null ?meta () | Some v -> cons ?meta v

  let bool ?(meta = meta_none) b = Bool (b, meta)
  let number ?(meta = meta_none) n = Number (n, meta)
  let any_float ?(meta = meta_none) f =
    if Float.is_finite f then Number (f, meta) else
    String (Float.to_string f, meta)

  let string ?(meta = meta_none) s = String (s, meta)
  let list ?(meta = meta_none) vs = Array (vs, meta)
  let array ?(meta = meta_none) a = Array (Array.to_list a, meta)
  let name ?(meta = meta_none) n = n, meta
  let mem n v = (n, v)
  let object' ?(meta = meta_none) mems = Object (mems, meta)

  (* Formatters *)

  type number_format = (float -> unit, Format.formatter, unit) format
  let default_number_format : number_format = format_of_string "%.17g"

  let pp_substring first len ppf s =
    if first = 0 && len = String.length s
    then Format.pp_print_string ppf s else
    (* OCaml >= 5.3 has Format.pp_print_substring *)
    for i = first to first + len - 1 do Format.pp_print_char ppf s.[i] done

  let pp_null ppf () = Format.pp_print_string ppf "null"
  let pp_bool ppf b = Format.pp_print_string ppf (if b then "true" else "false")
  let pp_number' fmt ppf f = (* cf. ECMAScript's JSON.stringify *)
    if Float.is_finite f then Format.fprintf ppf fmt f else pp_null ppf ()

  let pp_number ppf v = pp_number' default_number_format ppf v
  let pp_string ppf s =
    let string = Format.pp_print_string in
    let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false in
    let len = String.length s in
    let max_idx = len - 1 in
    let flush ppf start i =
      if start < len then pp_substring start (i - start) ppf s
    in
    let rec loop start i =
      if i > max_idx then flush ppf start i else
      let next = i + 1 in
      match String.get s i with
      | '"' -> flush ppf start i; string ppf "\\\""; loop next next
      | '\\' -> flush ppf start i; string ppf "\\\\"; loop next next
      | '\n' -> flush ppf start i; string ppf "\\n"; loop next next
      | '\r' -> flush ppf start i; string ppf "\\r"; loop next next
      | '\t' -> flush ppf start i; string ppf "\\t"; loop next next
      | c when is_control c ->
          flush ppf start i;
          string ppf (Printf.sprintf "\\u%04X" (Char.code c));
          loop next next
      | _c -> loop start next
    in
    Format.pp_print_char ppf '"'; loop 0 0; Format.pp_print_char ppf '"'

  let pp' ?(number_format = default_number_format) () ppf j =
    let pp_indent = 2 in
    let pp_sep ppf () =
      Format.pp_print_char ppf ',';
      Format.pp_print_break ppf 1 pp_indent
    in
    let rec pp_array ppf a =
      Format.pp_open_hovbox ppf 0;
      Format.pp_print_char ppf '[';
      Format.pp_print_break ppf 0 pp_indent;
      (Format.pp_print_list ~pp_sep pp_value) ppf a;
      Format.pp_print_break ppf 0 0;
      Format.pp_print_char ppf ']';
      Format.pp_close_box ppf ()
    and pp_mem ppf ((m, _), v) =
      Format.pp_open_hvbox ppf 0;
      pp_string ppf m; Format.pp_print_string ppf ": "; pp_value ppf v;
      Format.pp_close_box ppf ();
    and pp_obj ppf o =
      Format.pp_open_hvbox ppf 0;
      Format.pp_print_char ppf '{';
      Format.pp_print_break ppf 0 pp_indent;
      (Format.pp_print_list ~pp_sep pp_mem) ppf o;
      Format.pp_print_break ppf 0 0;
      Format.pp_print_char ppf '}';
      Format.pp_close_box ppf ();
    and pp_value ppf = function
    | Null _ -> pp_null ppf ()
    | Bool (b,_ ) -> pp_bool ppf b
    | Number (f, _) -> pp_number' number_format ppf f
    | String (s, _) -> pp_string ppf s
    | Array (a, _) -> pp_array ppf a
    | Object (o, _) -> pp_obj ppf o
    in
    pp_value ppf j

  let pp ppf j = pp' () ppf j

  (* Decode *)

  let error d loc fmt =
    Format.kasprintf (fun s -> raise_notrace (Failure s))
      ("%a: " ^^ fmt) Textloc.pp loc

  let err d fmt = error d (Textdec.textloc d) fmt
  let err_loc = error
  let err_span d ~start fmt = error d (Textdec.textloc_span d ~start) fmt

  let nextc d =
    Textdec.next d;
    if Textdec.is_error d then err d "UTF-8 decoding error"

  let uchar = Uchar.unsafe_of_int

  (* JSON decoding *)

  let decode_ascii d s = (* assert (d.u = s.[0]) *)
    let rec loop d s i max =
      if i > max then () else
      let u = Textdec.current d in
      if Char.code s.[i] = u then (nextc d; loop d s (i + 1) max) else
      err d "Expected %C but found %a while parsing '%s'" s.[i]
        Textdec.pp_decode u s
    in
    nextc d; loop d s 1 (String.length s - 1)

  let rec skip_ws d = match Textdec.current d with
  | 0x20 | 0x09 | 0x0A | 0x0D -> nextc d; skip_ws d | _ -> ()

  let parse_true d =
    let start = Textdec.pos d in
    decode_ascii d "true"; Bool (true, Textdec.textloc_span d ~start)

  let parse_false d =
    let start = Textdec.pos d in
    decode_ascii d "false"; Bool (false, Textdec.textloc_span d ~start)

  let parse_null d =
    let start = Textdec.pos d in
    decode_ascii d "null"; Null ((), Textdec.textloc_span d ~start)

  let parse_number d = (* not fully compliant *)
    let rec loop d ~start = match Textdec.current d with
    | 0x20 | 0x09 | 0x0A | 0x0D | 0x2C | 0x5D | 0x7D | 0x11_0001 ->
        let token = Textdec.lexeme_pop d in
        begin match Float.of_string_opt token with
        | None -> err_span d ~start "could not parse a float from: %S" token
        | Some n -> Number (n, Textdec.textloc_span d ~start)
        end
    | u -> Textdec.lexeme_add d (uchar u); nextc d; loop d ~start
    in
    let start = Textdec.pos d in
    let u = uchar (Textdec.current d) in
    Textdec.lexeme_clear d; Textdec.lexeme_add d u; nextc d; loop d ~start

  let rec parse_uescape d hi u count =
    let pp_ucp ppf d = Format.fprintf ppf "U+%04X" d in
    let err_not_lo d u = err d "not a low surrogate %a" pp_ucp u in
    let err_lo d u = err d "lone low surrogate %a" pp_ucp u in
    let err_hi d u = err d "lone high surrogate %a" pp_ucp u in
    if count > 0 then begin match Textdec.current d with
    | c when 0x30 <= c && c <= 0x39 ->
        nextc d; parse_uescape d hi (u * 16 + c - 0x30) (count - 1)
    | c when 0x41 <= c && c <= 0x46 ->
        nextc d; parse_uescape d hi (u * 16 + c - 0x37) (count - 1)
    | c when 0x61 <= c && c <= 0x66 ->
        nextc d; parse_uescape d hi (u * 16 + c - 0x57) (count - 1)
    | u ->
        err d "Expected hex digit but found %a" Textdec.pp_decode u
    end else match hi with
    | Some hi -> (* combine high and low surrogate into scalar value. *)
        if u < 0xDC00 || u > 0xDFFF then err_not_lo d u else
        let u = ((((hi land 0x3FF) lsl 10) lor (u land 0x3FF)) + 0x10000) in
        Textdec.lexeme_add d (uchar u)
    | None ->
        if u < 0xD800 || u > 0xDFFF
        then Textdec.lexeme_add d (Uchar.unsafe_of_int u)
        else if u > 0xDBFF then err_lo d u
        else match Textdec.current d with
        | 0x5C ->
            nextc d;
            begin match Textdec.current d with
            | 0x75 -> nextc d; parse_uescape d (Some u) 0 4
            | _ -> err_hi d u
            end
        | _ -> err_hi d u

  let parse_string d = (* assert (d.u = '\"') *)
    let uchar u = Uchar.unsafe_of_int u in
    let parse_escape d = match Textdec.current d with
    | (0x22 | 0x5C | 0x2F as b) -> Textdec.lexeme_add d (uchar b); nextc d
    | 0x62 -> Textdec.lexeme_add d (uchar 0x08); nextc d
    | 0x66 -> Textdec.lexeme_add d (uchar 0x0C); nextc d
    | 0x6E -> Textdec.lexeme_add d (uchar 0x0A); nextc d
    | 0x72 -> Textdec.lexeme_add d (uchar 0x0D); nextc d
    | 0x74 -> Textdec.lexeme_add d (uchar 0x09); nextc d
    | 0x75 -> nextc d; parse_uescape d None 0 4
    | u -> err d "Expected escape but found %a" Textdec.pp_decode u
    in
    let rec loop d ~start = match Textdec.current d with
    | 0x5C (* '\' *) -> nextc d; parse_escape d; loop d ~start
    | 0x22 (* '"' *) ->
        let loc = Textdec.textloc_span d ~start in
        nextc d; String (Textdec.lexeme_pop d, loc)
    | 0x11_0001 (* eot *) -> err d "Unclosed string"
    | u -> Textdec.lexeme_add d (uchar u); nextc d; loop d ~start
    in
    let start = Textdec.pos d in
    nextc d; Textdec.lexeme_clear d; loop d ~start

  let rec parse_object d = (* assert (d.u = '{') *)
    let start = Textdec.pos d in
    match (nextc d; skip_ws d; Textdec.current d) with
    | 0x7D (* '}' *) ->
        let loc = Textdec.textloc_span d ~start in
        nextc d; Object ([], loc)
    | _ ->
        let parse_name d =
          let name = match (skip_ws d; Textdec.current d) with
          | 0x22 (* '"' *) ->
              (match parse_string d with String n -> n | _ -> assert false)
          | u -> err d "Expected '\"' but found %a" Textdec.pp_decode u
          in
          skip_ws d; name
        in
        let rec loop acc d ~start =
          let name = parse_name d in
          match Textdec.current d with
          | 0x3A (* ':' *) ->
              let v = (nextc d; parse_value d) in
              begin match Textdec.current d with
              | 0x2C (* ',' *) -> nextc d; loop ((name, v) :: acc) d ~start
              | 0x7D (* '}' *) ->
                  let loc = Textdec.textloc_span d ~start in
                  nextc d;
                  Object (List.rev ((name, v) :: acc), loc)
              | u ->
                  err d "Expected ',' or '}' but found %a" Textdec.pp_decode u
              end
          | u -> err d "Expected ':' but found %a" Textdec.pp_decode u
        in
        loop [] d ~start

  and parse_array d = (* assert (d.u = '[') *)
    let start = Textdec.pos d in
    match (nextc d; skip_ws d; Textdec.current d) with
    | 0x5D (* ']' *) ->
        let loc = Textdec.textloc_span d ~start in
        nextc d; Array ([], loc)
    | _ ->
        let rec loop acc d =
          let v = parse_value d in
          match Textdec.current d with
          | 0x2C (* ',' *) -> nextc d; loop (v :: acc) d
          | 0x5D (* ']' *) ->
              let loc = Textdec.textloc_span d ~start in
              nextc d;
              Array (List.rev (v :: acc), loc)
          | u -> err d "Expected ',' or ']' but found %a" Textdec.pp_decode u
        in
        loop [] d

  and parse_value d : t =
    let v = match (skip_ws d; Textdec.current d) with
    | 0x22 (* '"' *) -> parse_string d
    | 0x74 (* 't' *) -> parse_true d
    | 0x66 (* 'f' *) -> parse_false d
    | 0x6E (* 'n' *) -> parse_null d
    | 0x7B (* '{' *) -> parse_object d
    | 0x5B (* '[' *) -> parse_array d
    | 0x2D (* '-' *) -> parse_number d
    | b when 0x30 (* '0' *) <= b && b <= 0x39 (* '9' *) -> parse_number d
    | u -> err d "Expected a JSON value but found %a" Textdec.pp_decode u
    in
    skip_ws d;
    v

  let of_string ?file s =
    try
      let d = Textdec.make ?file s in
      let v = nextc d; parse_value d in
      match Textdec.current d with
      | 0x11_0001 (* eot *) -> Ok v
      | u -> err d "Expected end of input but found %a" Textdec.pp_decode u
    with
    | Failure e -> Error e

  (* JSON generation *)

  module G = struct
    (* Not T.R. we could CPS. *)

    type enc = { mutable sep : bool; b : Buffer.t }
    type t = enc -> unit

    let addc c enc = Buffer.add_char enc.b c
    let adds s enc = Buffer.add_string enc.b s
    let adds_esc s enc =
      let is_control =
        function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false
      in
      let len = String.length s in
      let max_idx = len - 1 in
      let flush b start i =
        if start < len then Buffer.add_substring b s start (i - start);
      in
      let rec loop start i = match i > max_idx with
      | true -> flush enc.b start i
      | false ->
          let next = i + 1 in
          match String.get s i with
          | '"' -> flush enc.b start i; adds "\\\"" enc; loop next next
          | '\\' -> flush enc.b start i; adds "\\\\" enc; loop next next
          | c when is_control c ->
              flush enc.b start i;
              adds (Format.asprintf "\\u%04X" (Char.code c)) enc;
              loop next next
          | c -> loop start next
      in
      loop 0 0

    let null enc = adds "null" enc
    let bool b enc = adds (if b then "true" else "false") enc
    let int i enc = adds (string_of_int i) enc
    let float f enc = adds (Format.asprintf "%.16g" f) enc
    let string s enc = addc '"' enc; adds_esc s enc; addc '"' enc

    let nosep enc = enc.sep <- false
    let sep enc = enc.sep
    let set_sep sep enc = enc.sep <- sep
    let if_sep enc = if not enc.sep then enc.sep <- true else addc ',' enc

    type array = t
    let array enc = ()
    let array_end els enc =
      let sep = sep enc in
      addc '[' enc; nosep enc; els enc; addc ']' enc; set_sep sep enc

    let el e arr enc = arr enc; if_sep enc; e enc
    let el_if c e arr enc = if c then el (e ()) arr enc else arr enc

    type obj = t
    let obj enc = ()
    let obj_end mems enc =
      let sep = sep enc in
      addc '{' enc; nosep enc; mems enc; addc '}' enc; set_sep sep enc

    let mem m v obj enc = obj enc; if_sep enc; string m enc; addc ':' enc; v enc
    let mem_if c m v obj enc = if c then mem m (v ()) obj enc else obj enc

    (* Derived generators *)

    let strf fmt = Format.kasprintf string fmt
    let list elv l =
      array_end (List.fold_left (fun a v -> el (elv v) a) array l)

    let option some o = match o with None -> null | Some v -> some v
    let rec json = function
    | Null _ -> null
    | Bool (b, _) -> bool b
    | Number (f, _) -> float f
    | String (s, _) -> string s
    | Array (a, _) ->
        array_end @@ List.fold_left (fun a e -> el (json e) a) array a
    | Object (o, _) ->
        obj_end @@ List.fold_left (fun o ((m, _), v) -> mem m (json v) o) obj o

    (* Output generated values *)

    let buffer_add b g = g { sep = true; b }
    let to_string g =
      let b = Buffer.create 65535 in
      (buffer_add b g; Buffer.contents b)
  end

  let to_string v = G.to_string (G.json v)
end

module Jsong = Json.G
module Jsonq = struct
  module Sset = Set.Make (String)
  module Smap = Map.Make (String)

  let pp_quote ppf s = Format.fprintf ppf "'%s'" s
  let pp_mem = pp_quote
  let kind = function
  | Json.Null _ -> "null" | Bool _ -> "bool" | Number _ -> "number"
  | String _ -> "string" | Array _ -> "array" | Object _ -> "object"

  type path = (* Paths in JSON values, array and object member traversals. *)
    ([`A | `O of string] * Json.meta) list (* in reverse order *)

  let path_to_string p =
    let seg = function `A -> "[]" | `O n -> "." ^ n in
    String.concat "" (List.rev_map seg p)

  let path_to_trace ?(pp_mem = pp_mem) p =
    let seg = function
    | `A, l -> Format.asprintf "%a: in array" Textloc.pp l
    | `O m, l -> Format.asprintf "%a: in key %a" Textloc.pp l pp_mem m
    in
    String.concat "\n" (List.map seg p)

  (* Errors *)

  exception Error of path * Textloc.t * string

  let err p l msg = raise_notrace (Error (p, l, msg))
  let errf p l fmt = Format.kasprintf (err p l) fmt
  let err_exp exp p fnd =
    errf p (Json.meta fnd) "Found %s but expected %s" (kind fnd) exp

  let err_exp_null = err_exp "null"
  let err_exp_bool = err_exp "bool"
  let err_exp_number = err_exp "number"
  let err_exp_string = err_exp "string"
  let err_exp_array = err_exp "array"
  let err_exp_obj = err_exp "object"
  let err_empty_array p l = errf p l "unexpected empty array"
  let err_miss_mem p l n = errf p l "member %a unbound in object" pp_mem n
  let err_to_string ?pp_mem p loc msg =
    let pp_lines ppf s =
      Format.fprintf ppf "@[<v>%a@]"
        (Format.pp_print_list Format.pp_print_string)
        (String.split_on_char '\n' s)
    in
    match p with
    | [] -> Format.asprintf "%a:@\n%a" Textloc.pp loc pp_lines msg
    | p ->
        Format.asprintf "%a:@\n%a@\n  @[%a@]"
          Textloc.pp loc pp_lines msg pp_lines (path_to_trace p)

  (* Queries *)

  type 'a t = path -> Json.t -> 'a

  let query q s = try Ok (q [] s) with
  | Error (p, l, m) -> Result.Error (err_to_string p l m)

  (* Succeeding and failing queries *)

  let succeed v p j = v
  let fail msg p j = err p (Json.meta j) msg
  let failf fmt = Format.kasprintf fail fmt

  (* Query combinators *)

  let app fq q p j = fq p j (q p j)
  let ( $ ) = app
  let pair q0 q1 p j = let v0 = q0 p j in v0, q1 p j
  let bind q f p j = f (q p j) p j
  let map f q p j = f (q p j)
  let some q p j = Some (q p j)

  (* JSON queries *)

  let fold ~null ~bool ~float ~string ~array ~obj p = function
  | Json.Null _ as j -> null p j
  | Bool _ as j -> bool p j
  | Number _ as j -> float p j
  | String _ as j -> string p j
  | Array _ as j -> array p j
  | Object _ as j -> obj p j

  let partial_fold ?null ?bool ?float ?string ?array ?obj () p j =
    let with_q q p j = match q with
    | None ->
        let kind k = function None -> "" | Some _ -> k  in
        let kinds = [ kind "null" null; kind "bool" bool;
                      kind "number" float; kind "string" string;
                      kind "array" array; kind "obj" obj ]
        in
        let kinds = List.filter (fun s -> s <> "") kinds in
        let kinds = String.concat ", " kinds in
        (* FIXME use error messages from Err_msg *)
        let kinds = if kinds = "" then "nothing" else "one of " ^ kinds in
        err_exp kinds p j
    | Some q -> q p j
    in
    match (j : Json.t) with
    | Null _ as j -> with_q null p j
    | Bool _ as j -> with_q bool p j
    | Number _ as j -> with_q float p j
    | String _ as j -> with_q string p j
    | Array _ as j -> with_q array p j
    | Object _ as j -> with_q obj p j

  let json p s = s
  let meta p s = Json.meta s
  let with_meta q p s = (q p s), Json.meta s

  (* Nulls *)

  let is_null p = function Json.Null _ -> true | j -> false
  let null p = function Json.Null _ -> () | j -> err_exp_null p j
  let nullable q p = function Json.Null _ -> None | j -> Some (q p j)

  (* Atomic values *)

  let bool p = function Json.Bool (b, _) -> b | j -> err_exp_bool p j
  let number p = function Json.Number (f, _) -> f | j -> err_exp_number p j
  let int = map truncate number
  let string p = function Json.String (s, _) -> s | j -> err_exp_string p j

  let string_to ~kind parse p = function
  | Json.String (s, _) as j ->
      (match parse s with Ok v -> v | Error m -> fail m p j)
  | j -> err_exp kind p j

  let enum ~kind ss p = function
  | Json.String (s, _) when Sset.mem s ss -> s
  | Json.String (s, l) ->
      let ss = Sset.elements ss in
      let dict yield = List.iter yield ss in
      let hint, ss = match B0_std.String.spellcheck dict s with
      | [] -> B0_std.Fmt.must_be, ss
      | ss -> B0_std.Fmt.did_you_mean, ss
      in
      let kind ppf () = Format.pp_print_string ppf kind in
      let pp_v = Format.pp_print_string in
      errf p l "%a" (B0_std.Fmt.unknown' ~kind pp_v ~hint) (s, ss)
  | j -> err_exp kind p j

  let enum_map ~kind sm p = function
  | Json.String (s, l) ->
      begin match Smap.find s sm with
      | v -> v
      | exception Not_found ->
          let ss = Smap.fold (fun k _ acc -> k :: acc) sm [] in
          let dict yield = List.iter yield ss in
          let hint, ss = match B0_std.String.spellcheck dict s with
          | [] -> B0_std.Fmt.must_be, ss
          | ss -> B0_std.Fmt.did_you_mean, ss
          in
          let kind ppf () = Format.pp_print_string ppf kind in
          let pp_v = Format.pp_print_string in
          errf p l "%a" (B0_std.Fmt.unknown' ~kind pp_v ~hint) (s, ss)
      end
  | j -> err_exp kind p j

  (* Array *)

  let is_empty_array p = function
  | Json.Array (a, _) -> a = [] | j -> err_exp_array p j

  let hd q p = function
  | Json.Array ([], l) -> err_empty_array p l
  | Json.Array (v :: _, l) -> q ((`A, l) :: p) v
  | j -> err_exp_array p j

  let tl q p = function
  | Json.Array ([], l) -> err_empty_array p l
  | Json.Array (_ :: [], l) -> q p (Json.Array ([], Textloc.to_last l))
  | Json.Array (_ :: (v :: _ as a), l) ->
      let l = Textloc.reloc ~first:(Json.meta v) ~last:l in
      q p (Json.Array (a, l))
  | j -> err_exp_array p j

  let nth ?absent n q p = function
  | Json.Array (vs, l) ->
      let p = (`A, l) :: p in
      let k, vs = if n < 0 then - n - 1, List.rev vs else n, vs in
      let rec loop k = function
      | v :: vs when k = 0 -> q p v
      | _ :: vs -> loop (k - 1) vs
      | [] ->
          match absent with
          | None -> errf p l "%d: no such index in array" n
          | Some absent -> absent
      in
      loop k vs
  | j -> err_exp_array p j

  let fold_array f q acc p = function
  | Json.Array (vs, l) ->
      let p = (`A, l) :: p in
      let add p acc v = f (q p v) acc in
      List.fold_left (add p) acc vs
  | j -> err_exp_array p j

  let array qv = map List.rev (fold_array (fun v acc -> v :: acc) qv [])

  (* Objects *)

  let rec mem_find n = function
  | ((n', _), j) :: ms when String.equal n' n -> Some j
  | _  :: ms -> mem_find n ms
  | [] -> None

  let mem : string -> 'a t -> 'a t = fun n q p -> function
  | Json.Object (ms, l) ->
      begin match mem_find n ms with
      | None -> err_miss_mem p l n
      | Some j -> q  ((`O n, l) :: p) j
      end
  | j -> err_exp_obj p j

  let opt_mem n q ~absent p = function
  | Json.Object (ms, l) ->
      begin match mem_find n ms with
      | None -> absent
      | Some j -> q ((`O n, l) :: p) j
      end
  | j -> err_exp_obj p j

  let mem_dom ~validate p = function
  | Json.Object (ms, l) ->
      let add_mem = match validate with
      | None -> fun acc ((n, _), _) -> Sset.add n acc
      | Some dom ->
          fun acc ((n, _), _) -> match Sset.mem n dom with
          | true -> Sset.add n acc
          | false ->
              let ns = Sset.elements dom in
              let dict yield = List.iter yield ns in
              let hint, ss = match B0_std.String.spellcheck dict n with
              | [] -> B0_std.Fmt.must_be, ns
              | ss -> B0_std.Fmt.did_you_mean, ss
              in
              let kind ppf () = Format.pp_print_string ppf "member" in
              let pp_v = Format.pp_print_string in
              errf p l "%a" (B0_std.Fmt.unknown' ~kind pp_v ~hint) (n, ss)
      in
      List.fold_left add_mem Sset.empty ms
  | j -> err_exp_obj p j
end
