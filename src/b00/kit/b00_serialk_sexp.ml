(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_serialk_text

(* FIXME quickly rehacked after a_meta and l_meta introduction.
   This needs a good cleanup and simplifications.

   {ul
   {- Preserve quoted tokens.}
   {- Add functions to cleanup layout.}
   {- Devise a way to [pp] absent layout according to context}
   {- Revise empty paths.} *)

module Sexp = struct

  (* Meta information *)

  type loc = Tloc.t
  let pp_loc = Tloc.pp

  type a_meta =
    { a_loc : loc; a_quoted : string option; a_before : string;
      a_after : string }

  type l_meta =
    { l_loc : loc; l_before : string; l_start : string; l_end : string;
      l_after : string }

  let a_meta_loc a_loc =
    { a_loc; a_quoted = None; a_before = ""; a_after = "" }

  let l_meta_loc l_loc =
    { l_loc; l_before = ""; l_start = ""; l_end = ""; l_after = "" }

  let loc_nil = Tloc.nil
  let a_meta_nil = a_meta_loc loc_nil
  let l_meta_nil = l_meta_loc loc_nil

  (* S-expressions *)

  (* FIXME this should maybe become private to sync the a_quoted with
     the atom. *)
  type t = [ `A of string * a_meta | `L of t list * l_meta ]
  let atom a = `A (a, a_meta_nil)
  let list l = `L (l, l_meta_nil)

  (* Accessors *)

  let loc = function `A (_, p) -> p.a_loc | `L (_, p) -> p.l_loc
  let kind = function `A (_, _) -> "atom" | `L (_, _) -> "list"
  let err_exp exp fnd =
    Format.asprintf "%a: %s but expected %s" Tloc.pp (loc fnd) (kind fnd) exp

  let err_exp_atom s = err_exp "atom" s
  let err_exp_list s = err_exp "list" s
  let err_get = invalid_arg

  let to_atom = function `A (a, _) -> Ok a | s -> Error (err_exp_atom s)
  let get_atom = function `A (a, _) -> a | s -> err_get (err_exp_atom s)
  let to_list = function `L (l, _) -> Ok l | s -> Error (err_exp_list s)
  let get_list = function `L (l, _) -> l | s -> err_get (err_exp_list s)
  let to_splice = function
  | `A _ as s -> [s]
  | `L (l, _) -> l (* FIXME merge the white from the list meta in
                      the first and last element of the list (?) *)

  (* Decode *)

  type error_kind = string (* FIXME eventually move to a variant *)
  let pp_error_kind () = Format.pp_print_string

  type error = error_kind * loc
  let pp_prefix ppf () = Format.pp_print_string ppf "Error: "
  let pp_error
      ?(pp_loc = Tloc.pp) ?(pp_error_kind = pp_error_kind ())
      ?(pp_prefix = pp_prefix) () ppf (k, l)
    =
    Format.fprintf ppf "@[<v>%a:@,%a%a@]" pp_loc l pp_prefix () pp_error_kind k

  let error_to_string ?(pp_error = pp_error ()) = function
  | Ok _ as v -> v | Error e -> Error (Format.asprintf "%a" pp_error e)

  let curr_char d = (* TODO better escaping (this is for error reports) *)
    Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

  let err_eoi msg d ~sbyte ~sline =
    Tdec.err_to_here d ~sbyte ~sline "end of input: %s" msg

  let err_eoi_qtoken = err_eoi "unclosed quoted atom"
  let err_eoi_list = err_eoi "unclosed list"
  let err_eoi_esc = err_eoi "truncated escape"
  let err_illegal_uchar d b = Tdec.err_here d "illegal character U+%04X" b
  let err_rpar d = Tdec.err_here d "mismatched right parenthesis ')'"

  let err_esc_exp_hex d ~sbyte ~sline =
    Tdec.err_to_here d ~sbyte ~sline
      "%s: illegal Unicode escape: expected an hexadecimal digit" (curr_char d)

  let err_esc_uchar d ~sbyte ~sline code =
    Tdec.err_to_here d ~sbyte ~sline
      "illegal Unicode escape: %04X is not a Unicode character" code

  let err_esc_illegal d ~sbyte ~sline pre =
    Tdec.err_to_here d ~sbyte ~sline "%s%s: illegal escape" pre (curr_char d)

  let err_esc_uchar_end d ~sbyte ~sline =
    Tdec.err_to_here d ~sbyte ~sline
      "%s: illegal Unicode escape: expected end of escape '}'"
      (curr_char d)

  let err_esc_char d =
    Tdec.err_here d "escape character '^' illegal outside quoted atoms"

  let dec_byte d = match Tdec.byte d with
  | c when 0x00 <= c && c <= 0x08 || 0x0E <= c && c <= 0x1F || c = 0x7F ->
      err_illegal_uchar d c
  | c -> c
  [@@ ocaml.inline]

  let rec skip_white d = match dec_byte d with
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip_white d
  | _ -> ()

  let dec_skip_as_tok d = (* skip white and comment, but tokenize it *)
    let rec skip d = match dec_byte d with
    | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.tok_accept_byte d; skip d
    | 0x3B (* ; *) -> Tdec.tok_accept_byte d; skip_comment d
    | _ -> ()
    and skip_comment d = match dec_byte d with
    | 0x0A | 0x0D -> Tdec.tok_accept_byte d; skip d
    | 0xFFFF -> ()
    | _ -> Tdec.tok_accept_uchar d; skip_comment d
    in
    skip d

  let rec dec_uchar_esc d ~sbyte ~sline =
    match (Tdec.accept_byte d; dec_byte d) with
    | 0x7B (* { *)  ->
        let rec loop d acc count = match (Tdec.accept_byte d; dec_byte d) with
        | c when count > 6 -> err_esc_uchar_end d ~sbyte ~sline
        | c when 0x30 <= c && c <= 0x39 ->
            loop d (acc * 16 + c - 0x30) (count + 1)
        | c when 0x41 <= c && c <= 0x46 ->
            loop d (acc * 16 + c - 0x37) (count + 1)
        | c when 0x61 <= c && c <= 0x66 ->
            loop d (acc * 16 + c - 0x57) (count + 1)
        | 0x7D when count = 0 -> err_esc_exp_hex d ~sbyte ~sline
        | 0x7D when not (Uchar.is_valid acc) ->
            err_esc_uchar d ~sbyte ~sline acc
        | 0x7D ->
            Tdec.accept_byte d; Tdec.tok_add_uchar d (Uchar.unsafe_of_int acc);
        | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
        | _ -> err_esc_exp_hex d ~sbyte ~sline
        in
        loop d 0 0
    | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
    | c -> err_esc_illegal d ~sbyte ~sline "^u"

  let rec dec_esc d =
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    match (Tdec.accept_byte d; dec_byte d) with
    | 0x22 -> Tdec.accept_byte d; Tdec.tok_add_char d '"'
    | 0x5E -> Tdec.accept_byte d; Tdec.tok_add_char d '^'
    | 0x6E -> Tdec.accept_byte d; Tdec.tok_add_char d '\n'
    | 0x72 -> Tdec.accept_byte d; Tdec.tok_add_char d '\r'
    | 0x20 -> Tdec.accept_byte d; Tdec.tok_add_char d ' '
    | 0x75 -> dec_uchar_esc d ~sbyte ~sline
    | 0x0A | 0x0D -> (* continuation line *) skip_white d
    | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
    | _ -> err_esc_illegal d ~sbyte ~sline "^"

  let rec dec_qtoken d ws =
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    let rec loop d = match dec_byte d with
    | 0x22 ->
        let a = Tdec.tok_pop d in
        let a_quoted =
          (* TODO this should preserve escapes. It seems we are better
             off to simply tokenize without escaping and then parse the
             tok. But problem for err report add an alternate raw token to
             decoder ? *)
          Some a
        in
        let a_loc = Tdec.loc_to_here d ~sbyte ~sline in
        let m = { a_loc; a_quoted; a_before = ws; a_after = "" } in
        Tdec.accept_byte d; `A (a, m)
    | 0x5E -> dec_esc d; loop d
    | 0xFFFF -> err_eoi_qtoken d ~sbyte ~sline
    | _ -> Tdec.tok_accept_uchar d; loop d
    in
    Tdec.accept_byte d; loop d

  and dec_token d ws =
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    let rec loop d = match dec_byte d with
    | 0x28 | 0x29 | 0x3B | 0x22
    | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D
    | 0xFFFF ->
        let ebyte = Tdec.pos d - 1 in
        let eline = Tdec.line d in
        let a_loc = Tdec.loc d ~sbyte ~ebyte ~sline ~eline in
        let m = { a_loc; a_quoted = None; a_before = ws; a_after = "" } in
        `A (Tdec.tok_pop d, m)
    | 0x5E -> err_esc_char d
    | _ -> Tdec.tok_accept_uchar d; loop d
    in
    loop d

  and dec_eoi d stack ews acc = match stack with
  | (sbyte, sline, sws, _) :: [] ->
      begin match acc with
      | [] ->
          (* There's a tricky bit here. If sws is a comment followed
             by end of file then an s-expression update followed by
             a pp layout might comment the update. There are various
             ways of going around this. But we'd like to avoid a
             pp layout without update changing anything so we simply
             put the comment in l_after. This means that edits
             will occur before the comment. *)
          let l_loc = Tdec.loc d ~sbyte:0 ~ebyte:0 ~sline:(1,0) ~eline:(1,0) in
          let l_after = String.concat "" [sws; ews] in
          let m = { l_loc; l_before = ""; l_start = ""; l_end = ""; l_after } in
          `L ([], m)
      | acc ->
          let eloc = loc (List.hd acc) in
          let acc = List.rev acc in
          let sloc = loc (List.hd acc) in
          let l_loc = Tloc.merge sloc eloc  in
          let m =
            { l_loc; l_before = sws; l_start = ""; l_end = ""; l_after = ews }
          in
          `L (acc, m)
      end
  | (sbyte, sline, _, _) :: locs -> err_eoi_list d ~sbyte ~sline
  | [] -> assert false

  and dec_sexp_seq d stack acc =
    let ws = (dec_skip_as_tok d; Tdec.tok_pop d) in
    match dec_byte d with
    | 0x28 ->
        let stack = (Tdec.pos d, Tdec.line d, ws, acc) :: stack in
        Tdec.accept_byte d; dec_sexp_seq d stack []
    | 0x29 ->
        begin match stack with
        | (sbyte, sline, _, _) :: [] -> err_rpar d
        | (sbyte, sline, l_before, prev_acc) :: stack ->
            let ebyte = Tdec.pos d and eline = Tdec.line d in
            let l_loc = Tdec.loc d ~sbyte ~ebyte ~sline ~eline in
            let m = { l_loc; l_before; l_start = ""; l_end = ws; l_after = "" }
            in
            let acc = `L (List.rev acc, m) :: prev_acc in
            Tdec.accept_byte d; dec_sexp_seq d stack acc
        | [] -> assert false
        end
    | 0xFFFF -> dec_eoi d stack ws acc
    | 0x22 -> dec_sexp_seq d stack (dec_qtoken d ws :: acc)
    | _ -> dec_sexp_seq d stack (dec_token d ws :: acc)

  let seq_of_string ?(file = Tloc.no_file) s =
    try
      let d = Tdec.create ~file s in
      let before = (dec_skip_as_tok d; Tdec.tok_pop d) in
      Ok (dec_sexp_seq d [(0, (1, 0), before, [])] [])
    with Tdec.Err (loc, msg) -> Error (msg, loc)

  let seq_of_string' ?pp_error ?file s =
    error_to_string ?pp_error (seq_of_string ?file s)

  (* s-expression generation. *)

  let must_quote a =
    let rec loop max i s = match i < max with
    | false -> if max < 0 then true (* empty string *) else false
    | true ->
        match a.[i] with
        | '\x00' .. '\x1F' | '\x7F' | ' ' (* ctrl + white *)
        | '(' | ')' | ';' | '"' (* atom separators *)
        | '^'  (* escape char *) -> true
        | c -> loop max (i + 1) s
    in
    loop (String.length a - 1) 0 a

  let buffer_add_qatom b a = (* Adds a quoted atom to [b] *)
    let len = String.length a in
    let flush start i =
      if start < len then Buffer.add_substring b a start (i - start)
    in
    let rec loop start i = match i < len with
    | false -> flush start i
    | true ->
        let next = i + 1 in
        match a.[i] with
        | '"' -> flush start i; Buffer.add_string b "^\""; loop next next
        | '^' -> flush start i; Buffer.add_string b "^^"; loop next next
        | '\n' -> flush start i; Buffer.add_string b "^n"; loop next next
        | '\r' -> flush start i; Buffer.add_string b "^r"; loop next next
        | '\x00' .. '\x1F' | '\x7F' as c (* ctrl + white except ' ' *) ->
            flush start i;
            Buffer.add_string b (Format.asprintf "^u{%04X}" (Char.code c));
            loop next next
        | c -> loop start next
    in
    Buffer.add_char b '\"'; loop 0 0; Buffer.add_char b '\"'

  let buffer_add_atom b a = match must_quote a with
  | false -> Buffer.add_string b a
  | true -> buffer_add_qatom b a

  module G = struct
    (* Not T.R. we could CPS. *)

    type enc = { mutable sep : bool; b : Buffer.t }
    type t = enc -> unit
    let addc c enc = Buffer.add_char enc.b c [@@ ocaml.inline]
    let adds s enc = Buffer.add_string enc.b s [@@ ocaml.inline]
    let adds_atom a enc = buffer_add_atom enc.b a

    let nosep enc = enc.sep <- false
    let sep enc = enc.sep
    let set_sep sep enc = enc.sep <- sep
    let if_sep enc = if not enc.sep then enc.sep <- true else addc ' ' enc

    (* Generation *)

    type lyst = t

    let atom = adds_atom
    let ls enc = ()
    let le els enc =
      let sep = sep enc in
      addc '(' enc; nosep enc; els enc; addc ')' enc; set_sep sep enc

    let el e l enc = l enc; if_sep enc; e enc
    let el_if c e l enc = if c then el (e ()) l enc else l enc

    (* Derived generators. *)

    let atomf fmt = Format.kasprintf atom fmt
    let list elv data = le (List.fold_left (fun l v -> el (elv v) l) ls data)

    let bool b = adds (string_of_bool b)
    let int i  = adds (string_of_int i)
    let float f = adds (Format.sprintf "%g" f)
    let float_hex f = adds (Format.sprintf "%h" f)
    let string = atom

    let option some o enc = match o with
    | None -> adds "none" enc
    | Some v -> le (el (some v) (el (adds "some") ls)) enc

    let rec sexp = function (* not T.R. *)
    | `A (a, _) -> atom a
    | `L (l, _) -> le @@ List.fold_left (fun l v -> el (sexp v) l) ls l

    (* Output *)

    let enc b = { sep = true; b }
    let buffer_add b g = g (enc b)
    let to_string g =
      let b = Buffer.create 65535 in
      (buffer_add b g; Buffer.contents b)
  end

  let seq_to_string s =
    let g s enc = match s with
    | `A (a, _) -> G.adds_atom a enc
    | `L (data, _) ->
        G.nosep enc;
        (List.fold_left (fun l v -> G.el (G.sexp v) l) G.ls data) enc
    in
    G.to_string (g s)

  let quote b a = match must_quote a with
  | false -> a
  | true ->
      buffer_add_qatom b a;
      let a = Buffer.contents b in
      Buffer.reset b; a

  (* FIXME cleanup

     Also maybe do layout entirely by hand to avoid Format suprises.
     Also it would be nice to pp what has no ws. *)

  type 'a fmt = Format.formatter -> 'a -> unit

  let pp_atom b ~sp ppf a =
    if sp then Format.pp_print_space ppf ();
    Format.pp_print_string ppf (quote b a)

  let pp_slist ~sp ppf () =
    if sp then Format.pp_print_space ppf ();
    Format.fprintf ppf "@[<1>("

  let pp_elist ppf () = Format.fprintf ppf ")@]"

  let rec pp_seq b ~sp ppf = function
  | (`A (a, _) :: ss) :: todo ->
      pp_atom b ~sp ppf a; pp_seq b ~sp:true ppf (ss :: todo)
  | (`L (l, _) :: ss) :: todo ->
      pp_slist ~sp ppf (); pp_seq b ~sp:false ppf (l :: ss :: todo)
  | [] :: [] -> ()
  | [] :: todo -> pp_elist ppf (); pp_seq b ~sp:true ppf todo
  | [] -> assert false

  let pp ppf s = pp_seq (Buffer.create 255) ~sp:false ppf [[s]]
  let pp_seq ppf = function   (* FIXME call that pp_splice ? *)
  | `A _ as s -> pp ppf s
  | `L (l, _) ->
      Format.fprintf ppf "@[<v>%a@]" (pp_seq (Buffer.create 255) ~sp:false) [l]

  let pp_layout_atom b ~sp ppf a m =
    (* FIXME this should use m.a_qtoken *)
    (if m.a_before = ""
     then (if sp then Format.pp_print_char ppf ' ')
     else Format.pp_print_string ppf m.a_before);
    Format.pp_print_string ppf (quote b a);
    Format.pp_print_string ppf m.a_after

  let pp_layout_slist ~sp ppf m =
    (if m.l_before = ""
     then (if sp then Format.pp_print_char ppf ' ')
     else Format.pp_print_string ppf m.l_before);
    Format.pp_print_char ppf '(';
    Format.pp_print_string ppf m.l_start

  let pp_layout_elist ppf m =
    Format.pp_print_string ppf m.l_end;
    Format.pp_print_char ppf ')';
    Format.pp_print_string ppf m.l_after

  let rec pp_layout_seq b ~sp ppf = function
  | ((`A (a, m) :: ss), me) :: todo ->
      pp_layout_atom b ~sp ppf a m;
      pp_layout_seq b ~sp:(m.a_after = "") ppf ((ss, me) :: todo)
  | ((`L (l, m) :: ss), me) :: todo ->
      pp_layout_slist ~sp ppf m;
      pp_layout_seq b ~sp:false ppf ((l, m) :: (ss, me) :: todo)
  | ([], _) :: [] -> ()
  | ([], me) :: todo ->
      pp_layout_elist ppf me;
      pp_layout_seq b ~sp:(me.l_after = "") ppf todo
  | [] -> assert false

  let pp_layout ppf s =
    pp_layout_seq (Buffer.create 255) ~sp:false ppf [([s], l_meta_nil)]

  let pp_seq_layout ppf = function
  | `A (a, m) -> pp_layout_atom (Buffer.create 255) ~sp:false ppf a m
  | `L (l, m) ->
      let pp_seq = pp_layout_seq (Buffer.create 255) ~sp:false in
      Format.fprintf ppf "@[<h>%s%s%a%s%s@]"
        m.l_before m.l_start pp_seq [(l, l_meta_nil)] m.l_end m.l_after

  (* Indices *)

  type index = Nth of int | Key of string

  let pp_key = Format.pp_print_string
  let pp_index ?(pp_key = pp_key) () ppf = function
  | Nth n -> Format.fprintf ppf "[%d]" n
  | Key k -> pp_key ppf k

  let pp_bracketed_index ?(pp_key = pp_key) () ppf = function
  | Nth n -> Format.fprintf ppf "[%d]" n
  | Key k -> Format.fprintf ppf "[%a]" pp_key k

  (* Paths *)

  type path = index list (* reversed *)

  let path_err i fmt = Format.kasprintf failwith ("%d: " ^^ fmt) i
  let path_err_unexp_eoi i = path_err i "unexpected end of input"
  let path_err_unexp_char i s = path_err i "unexpected character: %C" s.[i]
  let path_err_illegal_char i s = path_err i "illegal character here: %C" s.[i]
  let err_unexp i s =
    path_err i "unexpected input: %S" (Tloc.string_subrange ~first:i s)

  let path_parse_eoi s i max = if i > max then () else err_unexp i s
  let path_parse_index p s i max =
    let first, stop = match s.[i] with '[' -> i + 1, ']' | _ -> i, '.' in
    let last, next =
      let rec loop stop s i max = match i > max with
      | true -> if stop = ']' then path_err_unexp_eoi i else (i - 1), i
      | false ->
          let illegal = s.[i] = '[' || (s.[i] = ']' && stop = '.') in
          if illegal then path_err_illegal_char i s else
          if s.[i] <> stop then loop stop s (i + 1) max else
          (i - 1), if stop = ']' then i + 1 else i
      in
      loop stop s first max
    in
    let idx = Tloc.string_subrange ~first ~last s in
    if idx = "" then path_err first "illegal empty index" else
    match int_of_string idx with
    | exception Failure _ -> next, (Key idx) :: p
    | idx -> next, (Nth idx) :: p

  let path_of_string s =
    let rec loop p s i max =
      if i > max then p else
      let next, p = path_parse_index p s i max in
      if next > max then p else
      if s.[next] <> '.' then path_err_unexp_char next s else
      if next + 1 <= max then loop p s (next + 1) max else
      path_err_unexp_eoi next
    in
    try
      if s = "" then Ok [] else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let pp_path ?pp_key () ppf is =
    let pp_sep ppf () = Format.pp_print_char ppf '.' in
    Format.pp_print_list ~pp_sep (pp_index ?pp_key ()) ppf (List.rev is)

  (* Carets *)

  type caret_loc = Before | Over | After
  type caret = caret_loc * path

  let caret_path (_, p) = p
  let caret_of_string s =
    let rec loop p s i max =
      if i > max then Over, p else
      let next = i + 1 in
      match s.[i] with
      | 'v' when next <= max && s.[next] = '[' ->
          let next, p = path_parse_index p s next max in
          path_parse_eoi s next max; Before, p
      | c ->
          let next, p = path_parse_index p s i max in
          if next > max then Over, p else
          if s.[next] = 'v'
          then (path_parse_eoi s (next + 1) max; After, p) else
          if s.[next] <> '.' then path_err_unexp_char next s else
          if next + 1 <= max then loop p s (next + 1) max else
          path_err_unexp_eoi next
    in
    try
      if s = "" then Ok (Over, []) else
      let start = if s.[0] = '.' then 1 else 0 in
      Ok (loop [] s start (String.length s - 1))
    with Failure e -> Error e

  let pp_caret ?pp_key () ppf = function
  | Over, p -> (pp_path ?pp_key ()) ppf p
  | Before, (c :: p) ->
      (pp_path ?pp_key ()) ppf p;
      (if p <> [] then Format.pp_print_char ppf '.');
      Format.pp_print_char ppf 'v'; (pp_bracketed_index ?pp_key ()) ppf c
  | After, (c :: p) ->
      (pp_path ?pp_key ()) ppf p;
      (if p <> [] then Format.pp_print_char ppf '.');
      (pp_bracketed_index ?pp_key ()) ppf c; Format.pp_print_char ppf 'v'
  | _ -> ()
end

module Sexpg = Sexp.G
module Sexpq = struct
  module Sset = Set.Make (String)
  module Smap = Map.Make (String)

  let pf = Format.fprintf
  let pp_lines ppf s =
    let lines = String.split_on_char '\n' s in
    pf ppf "@[<v>%a@]" Format.(pp_print_list pp_print_string) lines

  (* Result paths *)

  type path = (Sexp.index * Sexp.loc) list (* reversed *)
  let push_nth n v p = (Sexp.Nth n, Sexp.loc v) :: p
  let push_key k b p = (Sexp.Key k, Sexp.loc b) :: p

  let pp_path ?(pp_loc = Sexp.pp_loc) ?(pp_key = Sexp.pp_key) () ppf p =
    let pp_index pp_key ppf = function
    | Sexp.Key k, l -> pf ppf "%a: in key %a" pp_loc l pp_key k
    | Sexp.Nth i, l -> pf ppf "%a: at index %d" pp_loc l i
    in
    pf ppf "@[<v>%a@]" (Format.pp_print_list (pp_index pp_key)) p

  (* Query errors *)

  type error_kind =
  [ `Key_unbound of string * string list
  | `Msg of string
  | `Nth_unbound of int * int
  | `Out_of_dom of string * string * string list ]

  let pp_error_kind
      ?(pp_em = Format.pp_print_string) ?(pp_key = Sexp.pp_key) () ppf
    = function
    | `Key_unbound (k, ks) ->
        let binds pp_v ppf l =
          pf ppf "This@ dictionary@ only@ binds@ %a." (Tdec.pp_and_enum pp_v) l
        in
        let hint, ks = match Tdec.err_suggest ks k with
        | [] -> binds, ks
        | ks -> Tdec.pp_did_you_mean, ks
        in
        pf ppf "@[Key %a unbound@].@ %a" pp_key k (hint pp_key) ks
    | `Msg m -> pp_lines ppf m
    | `Nth_unbound (n, len) ->
        let pp_idx ppf i = pp_em ppf (string_of_int i) in
        begin match len with
        | 0 -> pf ppf "No@ index@ %a.@ This@ list@ is@ empty." pp_idx n
        | n ->
            pf ppf "No@ index@ %a.@ This@ list@ only@ has@ indices@ \
                    in@ range@ [%a;%a]."
              pp_idx n pp_idx (-len) pp_idx (len - 1)
        end
    | `Out_of_dom (kind, s, ss) ->
        let kind ppf () = Format.pp_print_string ppf kind in
        let hint, ss = match Tdec.err_suggest ss s with
        | [] -> Tdec.pp_must_be, ss
        | ss -> Tdec.pp_did_you_mean, ss
        in
        pf ppf "@[%a@]" (Tdec.pp_unknown' ~kind pp_em ~hint) (s, ss)

  type error = error_kind * (path * Sexp.loc)
  let pp_prefix ppf () = Format.pp_print_string ppf "Error: "
  let pp_error
      ?(pp_loc = Sexp.pp_loc) ?(pp_path = pp_path ~pp_loc ())
      ?(pp_error_kind = pp_error_kind ()) ?(pp_prefix = pp_prefix) ()
      ppf (k, (p, loc))
    =
    match p with
    | [] ->
        pf ppf "@[<v>@[%a@]@[%a@]@,%a:@]"
          pp_prefix () pp_error_kind k pp_loc loc
    | p ->
        pf ppf "@[<v>@[%a@]@[%a@]@,%a:@,%a@]"
          pp_prefix () pp_error_kind k pp_loc loc pp_path p

  let error_to_string ?(pp_error = pp_error ()) = function
  | Ok _ as v -> v
  | Error error -> Error (Format.asprintf "%a" pp_error error)

  exception Err of error
  let err p l k = raise_notrace (Err (k, (p, l)))
  let errf p l fmt = Format.kasprintf (fun m -> (err p l (`Msg m))) fmt
  let err_key_unbound p l k ks = err p l (`Key_unbound (k, ks))
  let err_nth_unbound p l n len = err p l (`Nth_unbound (n, len))
  let err_out_of_dom p l kind v dom = err p l (`Out_of_dom (kind, v, dom))
  let err_empty_list p l = err p l (`Msg "unexpected empty list")
  let err_exp_fnd_raw exp p l fnd = errf p l "found %s but expected %s" fnd exp
  let err_exp exp p fnd =
    errf p (Sexp.loc fnd) "found %s but expected %s" (Sexp.kind fnd) exp

  let err_exp_atom = err_exp "atom"
  let err_exp_list = err_exp "list"
  let err_exp_dict = err_exp "dictionary"
  let esc_atom a = a (* TODO (for error report) *)

  (* Queries *)

  type 'a t = path -> Sexp.t -> 'a
  let query_at_path q (s, p) = try Ok (q p s) with Err (k, pl) -> Error (k, pl)
  let query q s = query_at_path q (s, [])
  let query' ?pp_error q s = error_to_string ?pp_error (query q s)

  (* Success and failure *)

  let succeed v p s = v
  let fail kind p s = err p (Sexp.loc s) kind
  let failf fmt = Format.kasprintf (fun m -> fail (`Msg m)) fmt

  (* Query combinators *)

  let app fq q p s = fq p s (q p s)
  let ( $ ) = app
  let pair q0 q1 p s = let r0 = q0 p s in r0, q1 p s
  let bind q f p s = f (q p s) p s
  let map f q p s = f (q p s)
  let some q p s = Some (q p s)
  let loc q p s = (q p s), (p, Sexp.loc s)

  (* S-expression queries *)

  let fold ~atom ~list p = function
  | `A _ as s -> atom p s
  | `L _ as s -> list p s

  let sexp p s = s
  let sexp_with_path p s = s, p

  (* Atom queries *)

  let atom p = function `A (a, _) -> a | `L (_, _) as s -> err_exp_atom p s
  let atom_to ~kind parse p = function
  | `L (_, _) as s -> err_exp kind p s
  | `A (a, _) as s ->
      (match parse a with Ok v -> v | Error m -> fail (`Msg m) p s)

  let enum ~kind dom p = function
  | `A (a, _) when Sset.mem a dom -> a
  | `A (a, m) -> err_out_of_dom p m.Sexp.a_loc kind a (Sset.elements dom)
  | `L (_, _) as s -> err_exp kind p s

  let enum_map ~kind sm p = function
  | `L (_, _) as s -> err_exp kind p s
  | `A (a, m) ->
      match Smap.find a sm with
      | v -> v
      | exception Not_found ->
          let dom = Smap.fold (fun k _ acc -> k :: acc) sm [] in
          err_out_of_dom p m.Sexp.a_loc kind a dom

  let exn_atom_to ~kind parse p = function
  | `L (_, _) as s -> err_exp kind p s
  | `A (a, m) ->
      try parse a with
      | Failure _ ->
          errf p m.Sexp.a_loc "%s: could not parse %s" (esc_atom a) kind

  let _tf = "true or false"
  let bool p = function
  | `A ("true", _) -> true
  | `A ("false", _) -> false
  | `A (a, m) -> err_exp_fnd_raw _tf p m.Sexp.a_loc (esc_atom a)
  | `L (_, _) as s -> err_exp _tf p s

  let int = exn_atom_to ~kind:"int" int_of_string
  let int32 = exn_atom_to ~kind:"int32" Int32.of_string
  let int64 = exn_atom_to ~kind:"int64" Int64.of_string
  let float = exn_atom_to ~kind:"float" float_of_string

  (* List queries *)

  let is_empty p = function
  | `L (vs, _) -> vs = []
  | `A (_, _) as s -> err_exp_list p s

  let hd q p = function
  | `L (v :: _, m) -> q (push_nth 0 v p) v
  | `L ([], m) -> err_empty_list p m.Sexp.l_loc
  | `A (_, _) as s -> err_exp_list p s

  let tl q p = function
  | `L (_ :: [], m) -> q p (`L ([], Sexp.l_meta_loc m.Sexp.l_loc))
  | `L (_ :: (v :: _ as s), m) ->
      let l_loc = Tloc.restart ~at:(Tloc.to_start (Sexp.loc v)) m.Sexp.l_loc in
      q p (`L (s, Sexp.l_meta_loc l_loc))
  | `L ([], m) -> err_empty_list p m.Sexp.l_loc
  | `A (_, _) as s -> err_exp_list p s

  let fold_list f q acc p = function
  | `A (_, _) as s -> err_exp_list p s
  | `L (vs, m) ->
      let rec loop f q p acc i = function
      | [] -> acc
      | v :: vs -> loop f q p (f (q (push_nth i v p) v) acc) (i + 1) vs
      in
      loop f q p acc 0 vs

  let list q = map List.rev (fold_list List.cons q [])

  (* List index queries *)

  let list_find n p = function
  | `A (_, _) as s -> err_exp_list p s
  | `L (vs, lmeta) ->
      let k, vs' = if n < 0 then -n - 1, List.rev vs else n, vs in
      match List.nth vs' k with
      | v -> Ok v | exception Failure _ -> Error (vs, lmeta)

  let list_find_split n p = function
  | `A (_, _) as s -> err_exp_list p s
  | `L (vs, m) ->
      let k, vs' = if n < 0 then -n - 1, List.rev vs else n, vs in
      let rec loop left k = function
      | [] -> Error (left, k, m)
      | v :: vs ->
          if k > 0 then loop (v :: left) (k - 1) vs else
          let left, right = if n < 0 then vs, left else left, vs in
          Ok (left, v, right, m)
      in
      loop [] k vs'

  let nth ?absent n q p s = match list_find n p s with
  | Ok v -> q (push_nth n v p) v
  | Error (vs, m) ->
      match absent with
      | None -> err_nth_unbound p m.Sexp.l_loc n (List.length vs)
      | Some absent -> absent

  let delete_nth ~must_exist n p s = match list_find_split n p s with
  | Ok (left, _, right, lmeta) -> `L (List.rev_append left right, lmeta)
  | Error (rvs, _, m) when must_exist ->
      err_nth_unbound p m.Sexp.l_loc n (List.length rvs)
  | Error _ -> s

  (* Dictionary queries *)

  let dict_dom bs =
    let rec loop dom = function
    | `L (`A (a, _) :: _, _) :: vs -> loop (Sset.add a dom) vs
    | _ :: bs -> loop dom bs
    | [] -> dom
    in
    loop Sset.empty bs

  let key_value_fake_list vs bmeta =
    let fake_list_loc bmeta = function
    | [] -> (* XXX problem span emptyness... *) Tloc.to_end bmeta.Sexp.l_loc
    | vs -> Tloc.merge (Sexp.loc (List.hd vs)) (Sexp.loc List.(hd (rev vs)))
    in
    `L (vs, Sexp.l_meta_loc (fake_list_loc bmeta vs))

  let dict_find k p = function
  | `A (_, _) as s -> err_exp_dict p s
  | `L (bs, dmeta) ->
      let rec loop res = function
      | `L (`A (a, _) as key :: vs, bmeta) as b :: bs when String.equal a k ->
          (* last one takes over so we continue *)
          loop (Ok (b, key, vs, bmeta)) bs
      | [] -> res
      | _ :: bs -> loop res bs
      in
      loop (Error (bs, dmeta)) bs

  let dict_find_split k p = function (* like dict_find but with context *)
  | `A (_, _) as s -> err_exp_dict p s
  | `L (bs, dmeta) ->
      let rec loop res left = function
      | `L (`A (a, _) as key :: vs, bmeta) as b :: bs when String.equal a k ->
          (* last one takes over so we continue *)
          loop (Ok (left, (b, key, vs, bmeta), bs, dmeta)) (b :: left) bs
      | b :: bs -> loop res (b :: left) bs
      | [] ->
          match res with
          | Ok _ as v -> v
          | Error (_, dmeta) -> Error (left, dmeta)
      in
      loop (Error ([], dmeta)) [] bs

  let err_key_unbound p m k bs =
    let dom = Sset.elements (dict_dom bs) in
    err_key_unbound p m.Sexp.l_loc k dom

  let key ?absent k q p s = match dict_find k p s with
  | Ok (b, _, vs, bmeta) -> q (push_key k b p) (key_value_fake_list vs bmeta)
  | Error (bs, dmeta) ->
      match absent with
      | None -> err_key_unbound p dmeta k bs
      | Some absent -> absent

  let delete_key ~must_exist k p s = match dict_find_split k p s with
  | Ok (left, _, right, dmeta) -> `L (List.rev_append left right, dmeta)
  | Error (bs, m) when must_exist -> err_key_unbound p m k bs
  | Error _ -> s

  let key_dom ~validate p = function
  | `A (_, _) as s -> err_exp_dict p s
  | `L (bs, _) ->
      let add_key = match validate with
      | None -> fun p m k acc -> Sset.add k acc
      | Some dom ->
          fun p m k acc -> match Sset.mem k dom with
          | true -> Sset.add k acc
          | false ->
              (* FIXME Out_of_key_dom ? *)
              err_out_of_dom p m.Sexp.a_loc "key" k (Sset.elements dom)
      in
      let add_key validate acc = function
      | `L (`A (k, m) :: v, _) -> add_key p m k acc
      | `L ([], m) ->
          err_exp_fnd_raw "(atom ...) list" p m.Sexp.l_loc "empty list"
      | `L (_, m) ->
          err_exp_fnd_raw "(atom ...) list" p m.Sexp.l_loc "malformed list"
      | `A (_, _) as s -> err_exp_list p s
      in
      List.fold_left (add_key validate) Sset.empty bs

  let atomic q p = function
  | `A (_, _)  as a -> q p a
  | `L ([`A _ as a], _) -> q p a
  | `L ([], m) -> err_exp_fnd_raw "atom" p m.Sexp.l_loc "nothing"
  | `L (_, m) -> err_exp_fnd_raw "an atom" p m.Sexp.l_loc "list"

  (* Index queries *)

  let index ?absent i q = match i with
  | Sexp.Nth n -> nth ?absent n q
  | Sexp.Key k -> key ?absent k q

  let delete_index ~must_exist i = match i with
  | Sexp.Nth n -> delete_nth ~must_exist n
  | Sexp.Key k -> delete_key ~must_exist k

  (* Path and caret queries *)

  let path ?absent p q = List.fold_left (fun acc i -> index ?absent i acc) q p
  let probe_path is p s =
    let rec loop p s = function
    | [] -> p, s, []
    | Sexp.Nth n :: is as missing ->
        begin match list_find n p s with
        | Ok v -> loop (push_nth n v p) v is
        | Error (_, _) -> p, s, List.rev missing
        end
    | Sexp.Key k :: is as missing ->
        begin match dict_find k p s with
        | Ok (b, _, vs, bmeta) ->
            loop (push_key k b p) (key_value_fake_list vs bmeta) is
        | Error (bs, _) -> p, s, List.rev missing
        end
    in
    loop p s (List.rev is)

  let delete_at_path ~must_exist is p s =
    let rec loop p s = function
    | [] -> raise Exit
    | i :: [] -> delete_index ~must_exist i p s
    | Sexp.Nth n :: is ->
        begin match list_find_split n p s with
        | Ok (left, v, right, lmeta) ->
            let v' = loop (push_nth n v p) v is in
            `L (List.rev_append left (v' :: right), lmeta)
        | Error (rvs, _, m) when must_exist ->
            err_nth_unbound p m.Sexp.l_loc n (List.length rvs)
        | Error _ -> raise Exit
        end
    | Sexp.Key k :: is ->
        begin match dict_find_split k p s with
        | Ok (left, (b, key, vs, bmeta), right, dmeta) ->
            let p = push_key k b p in
            let v = key_value_fake_list vs bmeta in
            let b = `L (key :: Sexp.get_list (loop p v is), bmeta) in
            `L (List.rev_append left (b :: right), dmeta)
        | Error (bs, m) when must_exist -> err_key_unbound p m k bs
        | Error _ -> raise Exit
        end
    in
    try loop p s (List.rev is) with Exit -> s

  let rec push_stubs ?(stub = Sexp.atom "") n l =
    if n <= 0 then l else push_stubs (n - 1) (stub :: l)

  let pave_path ?stub is s =
    let pave acc = function
    | Sexp.Key k -> Sexp.list [Sexp.list (Sexp.atom k :: Sexp.to_splice acc)]
    | Sexp.Nth n when n >= 0 -> Sexp.list (push_stubs ?stub n [acc])
    | Sexp.Nth n -> Sexp.list (List.rev (push_stubs ?stub (-n - 1) [acc]))
    in
    List.fold_left pave s is

  let pave_splice_caret_nth ?stub caret_loc n ~rep lrev =
    let rep = Sexp.to_splice rep in
    match n >= 0 with
    | true ->
        let nstub = match caret_loc with
        | Sexp.Over | Sexp.Before -> n | Sexp.After -> n + 1
        in
        List.rev (List.rev_append rep (push_stubs ?stub nstub lrev))
    | false ->
        let l = List.rev_append rep lrev in
        let n = (-n - 1) - (List.length lrev - 1) in
        let nstub = match caret_loc with
        | Sexp.Over | Sexp.Before -> n | Sexp.After -> n - 1
        in
        List.rev (push_stubs ?stub nstub l)

  let pave_splice_caret_key caret_loc k ~rep lrev =
    let rev_rep = match caret_loc with
    | Sexp.Before | Sexp.After -> List.rev (Sexp.to_splice rep)
    | Sexp.Over -> [ Sexp.list (Sexp.atom k :: Sexp.to_splice rep) ]
    in
    List.rev (List.rev_append rev_rep lrev)

  let pave_splice_caret_index ?stub caret_loc i ~rep lrev = match i with
  | Sexp.Nth n -> pave_splice_caret_nth ?stub caret_loc n ~rep lrev
  | Sexp.Key k -> pave_splice_caret_key caret_loc k ~rep lrev

  let pave_splice_caret_path ?stub caret_loc is ~rep = match is with
  | [] -> assert false
  | i :: is ->
      let e = Sexp.list (pave_splice_caret_index ?stub caret_loc i ~rep []) in
      pave_path ?stub is e

  let splice_caret_nth ?stub ~must_exist caret_loc n ~rep p s =
    match list_find_split n p s with
    | Ok (left, v, right, lmeta) ->
        let rev_rep = match caret_loc with
        | Sexp.Over -> List.rev (Sexp.to_splice rep)
        | Sexp.Before -> v :: List.rev (Sexp.to_splice rep)
        | Sexp.After -> List.rev (v :: Sexp.to_splice rep)
        in
        `L (List.rev_append left (List.rev_append rev_rep right), lmeta)
    | Error (rvs, _, m) when must_exist ->
        err_nth_unbound p m.Sexp.l_loc n (List.length rvs)
    | Error (left, k, lmeta) ->
        `L (pave_splice_caret_nth ?stub caret_loc k ~rep left, lmeta)

  let splice_caret_key ~must_exist caret_loc k ~rep p s =
    match dict_find_split k p s with
    | Ok (left, (b, key, _, bmeta), right, dmeta) ->
        let rev_rep = match caret_loc with
        | Sexp.Over -> [ `L (key :: Sexp.to_splice rep, bmeta) ]
        | Sexp.Before -> b :: List.rev (Sexp.to_splice rep)
        | Sexp.After -> List.rev (b :: (Sexp.to_splice rep))
        in
        `L (List.rev_append left (List.rev_append rev_rep right), dmeta)
    | Error (bs, m) when must_exist -> err_key_unbound p m k bs
    | Error (left, dmeta) ->
        (* Just put it at the end regardless. *)
        `L (pave_splice_caret_key caret_loc k ~rep left, dmeta)

  let splice_caret_index ?stub ~must_exist caret_loc i ~rep p s = match i with
  | Sexp.Nth n -> splice_caret_nth ?stub ~must_exist caret_loc n ~rep p s
  | Sexp.Key k -> splice_caret_key ~must_exist caret_loc k ~rep p s

  let splice_at_caret ?stub ~must_exist (caret_loc, is) ~rep p s =
    let rec loop caret_loc p s = function
    | [] -> rep
    | i :: [] -> splice_caret_index ?stub ~must_exist caret_loc i ~rep p s
    | Sexp.Nth n :: is ->
        begin match list_find_split n p s with
        | Ok (left, v, right, lmeta) ->
            let v' = loop caret_loc (push_nth n v p) v is in
            `L (List.rev_append left (v' :: right), lmeta)
        | Error (rvs, _, m) when must_exist ->
            err_nth_unbound p m.Sexp.l_loc n (List.length rvs)
        | Error (left, k, lmeta) ->
            let rem = List.rev is in
            let v = pave_splice_caret_path ?stub caret_loc rem ~rep in
            let left = push_stubs ?stub k left in
            `L (List.rev (v :: left), lmeta)
        end
    | Sexp.Key k :: is as missing ->
        begin match dict_find_split k p s with
        | Ok (left, (b, key, vs, bmeta), right, dmeta) ->
            let p = push_key k b p in
            let v = key_value_fake_list vs bmeta in
            let b = `L (key :: Sexp.get_list (loop caret_loc p v is), bmeta) in
            `L (List.rev_append left (b :: right), dmeta)
        | Error (bs, m) when must_exist -> err_key_unbound p m k bs
        | Error (left, dmeta) ->
            let rem = List.rev missing in
            let b = match pave_splice_caret_path ?stub caret_loc rem ~rep with
            | `L ([b], _) (* remove outer dict *) -> b | _ -> assert false
            in
            `L (List.rev (b :: left), dmeta)
        end
    in
    loop caret_loc p s (List.rev is)

  let splice_at_path ?stub ~must_exist is ~rep p s =
    splice_at_caret ?stub ~must_exist (Sexp.Over, is) ~rep p s

  (* OCaml encoding queries *)

  let option q p = function (* TODO improve or remove *)
  | `A ("none", m) -> None
  | `L ((`A ("some", _) :: v), m) ->
      Some (q ((Sexp.Key "some", m.Sexp.l_loc) (* ? *) :: p) (`L (v, m)))
  | `A (a, m) ->
      err_exp_fnd_raw "none or (some ...)" p m.Sexp.a_loc (esc_atom a)
  | `L ((`A (a, _) :: v), m) ->
      err_exp_fnd_raw "none or (some ...)" p m.Sexp.l_loc ("(" ^ (esc_atom a))
  | `L (_, m) ->
      err_exp_fnd_raw "none or (some ...)" p m.Sexp.l_loc "an arbitrary list"
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
