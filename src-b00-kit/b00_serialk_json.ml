(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_serialk_text

module Json = struct

  (* JSON text *)

  type loc = Tloc.t
  type mem = (string * loc) * t
  and t =
  [ `Null of loc
  | `Bool of bool * loc
  | `Float of float * loc
  | `String of string * loc
  | `A of t list * loc
  | `O of mem list * loc ]

  let loc_nil = Tloc.nil
  let loc = function
  | `Null l | `Bool (_, l) | `Float (_, l) | `String (_, l) | `A (_, l)
  | `O (_, l) -> l

  (* Constructors *)

  let null = `Null loc_nil
  let bool b = `Bool (b, loc_nil)
  let float f = `Float (f, loc_nil)
  let string s = `String (s, loc_nil)
  let array vs = `A (vs, loc_nil)
  let mem n v = ((n, loc_nil), v)
  let obj mems = `O (mems, loc_nil)

  (* Accessors *)

  let kind = function
  | `Null _ -> "null" | `Bool _ -> "bool" | `Float _ -> "float"
  | `String _ -> "string" | `A _ -> "array" | `O _ -> "object"

  let err_exp exp fnd =
    Format.asprintf "%a: %s but expected %s" Tloc.pp (loc fnd) (kind fnd) exp

  let err_exp_null = err_exp "null"
  let err_exp_bool = err_exp "bool"
  let err_exp_float = err_exp "number"
  let err_exp_string = err_exp "string"
  let err_exp_array = err_exp "array"
  let err_exp_obj = err_exp "object"

  let err e = Error e
  let to_null = function `Null _ -> Ok () | j -> err (err_exp_null j)
  let to_bool = function `Bool (b, _) -> Ok b | j -> err (err_exp_bool j)
  let to_float = function `Float (f, _) -> Ok f | j -> err (err_exp_float j)
  let to_string = function `String (s,_) -> Ok s | j -> err (err_exp_string j)
  let to_array = function `A (vs, _) -> Ok vs | j -> err (err_exp_array j)
  let to_obj = function `O (mems, _) -> Ok mems | j -> err (err_exp_obj j)

  let err = invalid_arg
  let get_null = function `Null _ -> () | j -> err (err_exp_null j)
  let get_bool = function `Bool (b, _) -> b | j -> err (err_exp_bool j)
  let get_float = function `Float (f, _) -> f | j -> err (err_exp_float j)
  let get_string = function `String (s,_) -> s | j -> err (err_exp_string j)
  let get_array = function `A (vs, _) -> vs | j -> err (err_exp_array j)
  let get_obj = function `O (mems, _) -> mems | j -> err (err_exp_obj j)

  (* Decode *)

  (* FIXME add positions and reuse Tlex. *)

  type decoder = { t : Buffer.t; i : string; mutable pos : int; }
  let decoder s = { t = Buffer.create 255; i = s; pos = 0 }
  let accept d = d.pos <- d.pos + 1 [@@ ocaml.inline]
  let treset d = Buffer.reset d.t [@@ ocaml.inline]
  let taccept d = Buffer.add_char d.t d.i.[d.pos]; accept d; [@@ ocaml.inline]
  let taddc d c = Buffer.add_char d.t c [@@ ocaml.inline]
  let token d = Buffer.contents d.t [@@ ocaml.inline]
  let eoi d = d.pos = String.length d.i [@@ ocaml.inline]
  let byte d = match eoi d with
  | true -> 0xFFF
  | false -> Char.code d.i.[d.pos]
  [@@ ocaml.inline]

  let err d fmt =
    Format.kasprintf (fun s -> raise_notrace (Failure s)) ("%d: " ^^ fmt) d.pos

  let pp_byte ppf d = match byte d with
  | 0xFFF -> Format.fprintf ppf "end of input"
  | b -> Format.fprintf ppf "%C" (Char.chr b)

  type utf_8_case =
  | L1 | L2 | L3_E0 | L3_E1_EC_or_EE_EF | L3_ED | L4_F0 | L4_F1_F3 | L4_F4 | E

  let utf_8_case =
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

  let taccept_utf_8 d =
    let err d = err d "expected UTF-8 byte found: %a" pp_byte d in
    let b = byte d in
    let accept_tail d =
      if (byte d lsr 6 = 0b10) then taccept d else err d [@@ocaml.inline]
  in
  match utf_8_case.(b) with
  | L1 -> taccept d
  | L2 -> taccept d; accept_tail d
  | L3_E0 ->
      taccept d;
      if (byte d - 0xA0 < 0xBF - 0xA0) then taccept d else err d;
      accept_tail d
  | L3_E1_EC_or_EE_EF -> taccept d; accept_tail d; accept_tail d
  | L3_ED ->
      taccept d;
      if (byte d - 0x80 < 0x9F - 0x80) then taccept d else err d;
      accept_tail d
  | L4_F0 ->
      taccept d;
      if (byte d - 0x90 < 0xBF - 0x90) then taccept d else err d;
      accept_tail d; accept_tail d
  | L4_F1_F3 -> taccept d; accept_tail d; accept_tail d; accept_tail d;
  | L4_F4 ->
      taccept d;
      if (byte d - 0x80 < 0x8F - 0x80) then taccept d else err d;
  | E -> err d

  let accept_bytes d bytes = (* first byte already checked *)
    let max = String.length bytes - 1 in
    let rec loop i = match i > max with
    | true -> ()
    | false ->
        match Char.code bytes.[i] = byte d with
        | true -> accept d; loop (i + 1)
        | false ->
            err d "expected %C found: %a while parsing '%s'"
              bytes.[i] pp_byte d bytes
    in
    accept d; loop 1

  let rec skip_ws d = match byte d with
  | 0x20 | 0x09 | 0x0A | 0x0D -> accept d; skip_ws d
  | _ -> ()

  let parse_true d = accept_bytes d "true"; `Bool (true, loc_nil)
  let parse_false d = accept_bytes d "false"; `Bool (false, loc_nil)
  let parse_null d = accept_bytes d "null"; `Null loc_nil
  let parse_number d = (* not fully compliant *)
    let conv d = try `Float (float_of_string (token d), loc_nil) with
    | Failure e -> err d "could not parse a float from: %S" (token d)
    in
    let rec taccept_non_sep d = match byte d with
    | 0x20 | 0x09 | 0x0A | 0x0D | 0x2C | 0x5D | 0x7D | 0xFFF -> conv d
    | _ -> taccept d; taccept_non_sep d
    in
    treset d; taccept d; taccept_non_sep d

  let parse_string d =
    let parse_escape d = match byte d with
    | (0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 as b) ->
        taddc d (Char.chr b); accept d;
    | 0x75 -> (* Unicode escapes are not treated. *)
        taddc d '\\'; taccept d;
    | _ -> err d "expected escape found: %a" pp_byte d
    in
    let rec loop d = match byte d with
    | 0x5C (* '\' *) -> accept d; parse_escape d; loop d
    | 0x22 (* '"' *) -> accept d; `String ((token d), loc_nil)
    | 0xFFF -> err d "unclosed string"
    | _ -> taccept_utf_8 d; loop d
    in
    accept d; treset d; loop d

  let rec parse_object d = match (accept d; skip_ws d; byte d) with
  | 0x7D (* '}' *) -> accept d; `O ([], loc_nil)
  | _ ->
      let parse_name d =
        let `String name = match (skip_ws d; byte d) with
        | 0x22 (* '"' *) -> parse_string d
        | _ -> err d "expected '\"' found: %a" pp_byte d
        in
        skip_ws d; name
      in
      let rec loop acc d =
        let name = parse_name d in
        match byte d with
        | 0x3A (* ':' *) ->
            let v = (accept d; parse_value d) in
            begin match byte d with
            | 0x2C (* ',' *) -> accept d; loop ((name, v) :: acc) d
            | 0x7D (* '}' *) -> accept d; `O (List.rev ((name, v) :: acc),
                                              loc_nil)
            | _ -> err d "expected ',' or '}' found: %a" pp_byte d
            end
        | _ -> err d "expected ':' found: %a" pp_byte d
      in
      loop [] d

  and parse_array d = match (accept d; skip_ws d; byte d) with
  | 0x5D (* ']' *) -> accept d; `A ([], loc_nil)
  | _ ->
      let rec loop acc d =
        let v = parse_value d in
        match byte d with
        | 0x2C (* ',' *) -> accept d; loop (v :: acc) d
        | 0x5D (* ']' *) -> accept d; `A (List.rev (v :: acc), loc_nil)
        | _ -> err d "expected ',' or ']' found: %a" pp_byte d
      in
      loop [] d

  and parse_value d : t =
    let v = match (skip_ws d; byte d) with
    | 0x22 (* '"' *) -> parse_string d
    | 0x74 (* 't' *) -> parse_true d
    | 0x66 (* 'f' *) -> parse_false d
    | 0x6E (* 'n' *) -> parse_null d
    | 0x7B (* '{' *) -> parse_object d
    | 0x5B (* '[' *) -> parse_array d
    | 0x2D (* '-' *) -> parse_number d
    | b when 0x30 (* '0' *) <= b && b <= 0x39 (* '9' *) -> parse_number d
    | _ -> err d "expected a JSON value found: %a" pp_byte d
    in
    skip_ws d;
    v

  let of_string ?(file = Tloc.no_file) s =
    try
      let d = decoder s in
      let v = parse_value d in
      match byte d with
      | 0xFFF (* eoi *) -> Ok v
      | _ -> err d "expected end of input found: %a" pp_byte d
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
    | `Null _ -> null
    | `Bool (b, _) -> bool b
    | `Float (f, _) -> float f
    | `String (s, _) -> string s
    | `A (a, _) ->
        array_end @@ List.fold_left (fun a e -> el (json e) a) array a
    | `O (o, _) ->
        obj_end @@ List.fold_left (fun o ((m, _), v) -> mem m (json v) o) obj o

    (* Output generated values *)

    let buffer_add b g = g { sep = true; b }
    let to_string g =
      let b = Buffer.create 65535 in
      (buffer_add b g; Buffer.contents b)
  end

  let to_string v = G.to_string (G.json v)

  let pp ppf (v : t) = (* FIXME not T.R. *)
    let pp_string ppf s = (* FIXME quick & dirty escaping *)
      Format.pp_print_string ppf (G.to_string (G.json ((`String (s, loc_nil)))))
    in
    let pp_comma ppf () =
      Format.(pp_print_char ppf ','; pp_print_space ppf ())
    in
    let rec loop ppf = function
    | `Null _ -> Format.pp_print_string ppf "null"
    | `Bool (b,_ ) -> Format.pp_print_string ppf (if b then "true" else "false")
    | `Float (f, _) -> Format.fprintf ppf "%.16g" f
    | `String (s, _) -> pp_string ppf s
    | `A (a, _) ->
        Format.pp_open_box ppf 1;
        Format.pp_print_char ppf '[';
        Format.pp_print_list ~pp_sep:pp_comma loop ppf a;
        Format.pp_print_char ppf ']';
        Format.pp_close_box ppf ();
    | `O (o, _) ->
        let pp_mem ppf ((m, _), v) =
          Format.pp_open_box ppf 1;
          pp_string ppf m;
          Format.pp_print_char ppf ':'; Format.pp_print_space ppf ();
          loop ppf v;
          Format.pp_close_box ppf ();
        in
        Format.pp_open_vbox ppf 1;
        Format.pp_print_char ppf '{';
        Format.pp_print_list ~pp_sep:pp_comma pp_mem ppf o;
        Format.pp_print_char ppf '}';
        Format.pp_close_box ppf ();
    in
    loop ppf v
end

module Jsong = Json.G
module Jsonq = struct

  module Sset = Set.Make (String)
  module Smap = Map.Make (String)

  let pp_quote ppf s = Format.fprintf ppf "'%s'" s
  let pp_mem = pp_quote

  type path = (* Paths in JSON values, array and object member traversals. *)
    ([`A | `O of string] * Json.loc) list (* in reverse order *)

  let path_to_string p =
    let seg = function `A -> "[]" | `O n -> "." ^ n in
    String.concat "" (List.rev_map seg p)

  let path_to_trace ?(pp_mem = pp_mem) p =
    let seg = function
    | `A, l -> Format.asprintf "%a: in array" Tloc.pp l
    | `O m, l -> Format.asprintf "%a: in key %a" Tloc.pp l pp_mem m
    in
    String.concat "\n" (List.map seg p)

  (* Errors *)

  exception Err of path * Tloc.t * string

  let err p l msg = raise_notrace (Err (p, l, msg))
  let errf p l fmt = Format.kasprintf (err p l) fmt
  let err_exp exp p fnd =
    errf p (Json.loc fnd) "found %s but expected %s" (Json.kind fnd) exp

  let err_exp_null = err_exp "null"
  let err_exp_bool = err_exp "bool"
  let err_exp_float = err_exp "number"
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
    | [] -> Format.asprintf "%a:@\n%a" Tloc.pp loc pp_lines msg
    | p ->
        Format.asprintf "%a:@\n%a@\n  @[%a@]"
          Tloc.pp loc pp_lines msg pp_lines (path_to_trace p)

  (* Queries *)

  type 'a t = path -> Json.t -> 'a

  let query q s = try Ok (q [] s) with
  | Err (p, l, m) -> Error (err_to_string p l m)

  (* Succeeding and failing queries *)

  let succeed v p j = v
  let fail msg p j = err p (Json.loc j) msg
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
  | `Null _ as j -> null p j
  | `Bool _ as j -> bool p j
  | `Float _ as j -> float p j
  | `String _ as j -> string p j
  | `A _ as j -> array p j
  | `O _ as j -> obj p j

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
    match j with
    | `Null _ as j -> with_q null p j
    | `Bool _ as j -> with_q bool p j
    | `Float _ as j -> with_q float p j
    | `String _ as j -> with_q string p j
    | `A _ as j -> with_q array p j
    | `O _ as j -> with_q obj p j

  let json p s = s
  let loc p s = Json.loc s
  let with_loc q p s = (q p s), Json.loc s

  (* Nulls *)

  let is_null p = function `Null _ -> true | j -> false
  let null p = function `Null _ -> () | j -> err_exp_null p j
  let nullable q p = function `Null _ -> None | j -> Some (q p j)

  (* Atomic values *)

  let bool p = function `Bool (b, _) -> b | j -> err_exp_bool p j
  let float p = function `Float (f, _) -> f | j -> err_exp_float p j
  let int = map truncate float
  let string p = function `String (s, _) -> s | j -> err_exp_string p j

  let string_to ~kind parse p = function
  | `String (s, _) as j ->
      (match parse s with Ok v -> v | Error m -> fail m p j)
  | j -> err_exp kind p j

  let enum ~kind ss p = function
  | `String (s, _) when Sset.mem s ss -> s
  | `String (s, l) ->
      let ss = Sset.elements ss in
      let hint, ss = match Tdec.err_suggest ss s with
      | [] -> Tdec.pp_must_be, ss
      | ss -> Tdec.pp_did_you_mean, ss
      in
      let kind ppf () = Format.pp_print_string ppf kind in
      let pp_v = Format.pp_print_string in
      errf p l "%a" (Tdec.pp_unknown' ~kind pp_v ~hint) (s, ss)
  | j -> err_exp kind p j

  let enum_map ~kind sm p = function
  | `String (s, l) ->
      begin match Smap.find s sm with
      | v -> v
      | exception Not_found ->
          let ss = Smap.fold (fun k _ acc -> k :: acc) sm [] in
          let hint, ss = match Tdec.err_suggest ss s with
          | [] -> Tdec.pp_must_be, ss
          | ss -> Tdec.pp_did_you_mean, ss
          in
          let kind ppf () = Format.pp_print_string ppf kind in
          let pp_v = Format.pp_print_string in
          errf p l "%a" (Tdec.pp_unknown' ~kind pp_v ~hint) (s, ss)
      end
  | j -> err_exp kind p j

  (* Array *)

  let is_empty_array p = function `A (a, _) -> a = [] | j -> err_exp_array p j
  let hd q p = function
  | `A ([], l) -> err_empty_array p l
  | `A (v :: _, l) -> q ((`A, l) :: p) v
  | j -> err_exp_array p j

  let tl q p = function
  | `A ([], l) -> err_empty_array p l
  | `A (_ :: [], l) -> q p (`A ([], Tloc.to_end l))
  | `A (_ :: (v :: _ as a), l) ->
      let l = Tloc.restart ~at:(Tloc.to_start (Json.loc v)) l in
      q p (`A (a, l))
  | j -> err_exp_array p j

  let nth ?absent n q p = function
  | `A (vs, l) ->
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
  | `A (vs, l) ->
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
  | `O (ms, l) ->
      begin match mem_find n ms with
      | None -> err_miss_mem p l n
      | Some j -> q  ((`O n, l) :: p) j
      end
  | j -> err_exp_obj p j

  let opt_mem n q ~absent p = function
  | `O (ms, l) ->
      begin match mem_find n ms with
      | None -> absent
      | Some j -> q ((`O n, l) :: p) j
      end
  | j -> err_exp_obj p j

  let mem_dom ~validate p = function
  | `O (ms, l) ->
      let add_mem = match validate with
      | None -> fun acc ((n, _), _) -> Sset.add n acc
      | Some dom ->
          fun acc ((n, _), _) -> match Sset.mem n dom with
          | true -> Sset.add n acc
          | false ->
              let ns = Sset.elements dom in
              let hint, ss = match Tdec.err_suggest ns n with
              | [] -> Tdec.pp_must_be, ns
              | ss -> Tdec.pp_did_you_mean, ss
              in
              let kind ppf () = Format.pp_print_string ppf "member" in
              let pp_v = Format.pp_print_string in
              errf p l "%a" (Tdec.pp_unknown' ~kind pp_v ~hint) (n, ss)
      in
      List.fold_left add_mem Sset.empty ms
  | j -> err_exp_obj p j
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers

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
