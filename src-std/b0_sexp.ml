(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* Source positions. *)

type pos = int
type range = pos * pos
type src = File of B0_fpath.t
type loc = src * range

let no_loc = File (B0_fpath.v "?"), (-1, -1)
let loc_pos src pos = src, (pos + 1, pos + 2)
let loc src start stop = src, (start + 1, stop + 1)

let pp_loc ppf (src, (s, e)) = match src with
| File f -> B0_fmt.pf ppf "File \"%a\", characters %d-%d" B0_fpath.pp f s e

type error =
[ `Unclosed_quote
| `Unclosed_list
| `Unexpected_list_end
| `Illegal_escape_char of char ] * loc

let pp_error ppf (e, loc) = match e with
| `Unclosed_quote ->
    B0_fmt.pf ppf "%a: unclosed quote" pp_loc loc
| `Unclosed_list ->
    B0_fmt.pf ppf "%a: unclosed list" pp_loc loc
| `Unexpected_list_end ->
    B0_fmt.pf ppf "%a: unexpected list end" pp_loc loc
| `Illegal_escape_char c ->
    B0_fmt.pf ppf "%a: illegal escape character (%c)" pp_loc loc c

exception Error of error
let error e loc = raise (Error (e, loc))

type t = [ `Atom of string | `List of t list ] * loc

let get_atom = function
| `Atom a, _ -> a
| `List _, _ -> invalid_arg "expected atom found a list"

let get_list = function
| `List l, _ -> l
| `Atom a, _ -> invalid_arg "expected list found an atom"

let atom a loc = (`Atom a), loc
let list a loc = (`List a), loc

let is_comment_char c = not (Char.equal c '\n' || Char.equal c '\r')
let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false
let is_atom_char = function
| '(' | ')' | ';' -> false
| c when is_white c -> false
| _ -> true

(* The following code could be clarified it was a quick port from
   a parser using Astring.String.Sub. *)

let skip_white (pos, s) =
  let s' = B0_string.drop ~sat:is_white s in
  pos + (B0_string.length s - B0_string.length s'), s'

let skip_comment (pos, s) =
  let s' = B0_string.drop ~sat:is_comment_char s in
  pos + (B0_string.length s - B0_string.length s'), s'

let rec skip (pos, s as ps) =
  let _, s as ps  = skip_white ps in
  match B0_string.head s with
  | Some ';' -> skip (skip_comment ps)
  | Some _ | None -> ps

let p_atom src (pos, s) = match B0_string.span ~sat:is_atom_char s with
| ("", _) -> assert false
| (a, rem) ->
    let end_pos = pos + B0_string.length a - 1 in
    let loc = loc src pos end_pos in
    atom a loc, (end_pos + 1, rem)

let p_qatom src (pos, s) =
  let is_data = function '\\' | '"' -> false | _ -> true in
  let start_pos = pos in
  let rec loop acc pos s =
    let data, rem = B0_string.span ~sat:is_data s in
    let rem_start_pos = pos + B0_string.length data in
    match B0_string.head rem with
    | Some '"' ->
        let acc = List.rev (data :: acc) in
        let loc = loc src start_pos rem_start_pos in
        let a = B0_string.concat "" acc in
        atom a loc,
        ((rem_start_pos + 1),
         (B0_string.with_index_range ~first:1 rem) (* waste *))
    | Some '\\' ->
        begin match B0_string.length rem < 2 with
        | true -> error `Unclosed_quote (loc_pos src start_pos)
        | false ->
            begin match rem.[1] with
            | '"' | '\\' | 'n' | 'r' | 't' as c ->
                let esc = match c with
                | '"' -> "\"" | '\\' -> "\\" | 'n' ->  "\n"
                | 'r' -> "\r" | 't' -> "\t" | _ -> assert false
                in
                loop
                  (esc :: data :: acc)
                  rem_start_pos
                  (B0_string.with_index_range ~first:2 rem)
            | c ->
                error (`Illegal_escape_char c) (loc_pos src rem_start_pos)
            end
        end
    | None -> error `Unclosed_quote (loc_pos src start_pos)
    | Some _ -> assert false
  in
  loop [] (pos + 1) (B0_string.with_index_range ~first:1 s) (* waste *)

let rec p_list src (pos, s) = (* TODO not t.r. *)
  let start_pos = pos in
  let rec loop acc ps =
    let (sspos, s as ps) = skip ps in
    match B0_string.head s with
    | Some '"' ->
        let a, rem = p_qatom src ps in
        loop (a :: acc) rem
    | Some '(' ->
        let l, rem = p_list src ps in
        loop (l :: acc) rem
    | Some ')' ->
        let loc = loc src start_pos (sspos + 1) in
        list (List.rev acc) loc,
        (sspos + 1,
         (B0_string.with_index_range ~first:1 s)) (* waste *)
    | Some _ ->
        let a, rem = p_atom src ps in
        loop (a :: acc) rem
    | None ->
        error `Unclosed_list (loc_pos src start_pos)
  in
  loop [] ((pos + 1), (B0_string.with_index_range ~first:1 s)) (* waste *)

let of_string ~src s =
  let rec loop acc ps =
    let (pos, s as ps) = skip ps in
    match B0_string.head s with
    | Some '(' ->
        let l, rem = p_list src ps in
        loop (l :: acc) rem
    | Some '"' ->
        let a, rem = p_qatom src ps in
        loop (a :: acc) rem
    | Some ')' ->
        error `Unexpected_list_end (loc_pos src pos)
    | Some _ ->
        let a, rem = p_atom src ps in
        loop (a :: acc) rem
    | None ->
        List.rev acc
  in
  let loc = (src, (0, String.length s - 1)) in
  try Ok (`List (loop [] (0, s)), loc) with
  | Error e -> R.error_msgf "%a" pp_error e

let of_file f = B0_os.File.read f >>= fun s -> of_string ~src:(File f) s

let dump_locs ppf v = (* Not T.R. *)
  let rec loop ppf = function
  | `Atom at, loc -> B0_fmt.pf ppf "@[%a: Atom %S@]@," pp_loc loc at
  | `List l, loc ->
      B0_fmt.pf ppf "@[%a: List@]@," pp_loc loc;
      List.iter (loop ppf) l
  in
  B0_fmt.pf ppf "@[<v>%a@]" loop v

(* Parsing key-value maps *)

type map = (t * loc) B0_string.Map.t * loc
type 'a key = map -> 'a

let to_string_map ?known:(is_known = fun _ -> true) s =
  let err_atom loc =
    R.error_msgf "%a: expected a list, found an atom" pp_loc loc
  in
  let rec loop known unknown = function
  | [] -> Ok (known, unknown)
  | (`List ((`Atom k, _) :: v), loc) :: l ->
      begin match v with
      | [`Atom _, _ as v] | [`List _, _ as v] ->
          begin match is_known k with
          | true -> loop (B0_string.Map.add k (v, loc) known) unknown l
          | false -> loop known (B0_string.Map.add k (v, loc) unknown) l
          end
      | [] ->
          R.error_msgf "%a: key unbound to a list or atom" pp_loc loc
      | v ->
          R.error_msgf "%a: key bound to more than one list or atom" pp_loc loc
      end
  | (`List ((`List _, _) :: _), loc) :: l ->
      R.error_msgf "%a: expected a key, found a list" pp_loc loc
  | (`List [], loc) :: l ->
      R.error_msgf "%a: illegal empty list" pp_loc loc
  | (`Atom _, loc) :: l ->
      err_atom loc
  in
  match s with
  | `Atom _, loc -> err_atom loc
  | `List l, loc ->
      loop B0_string.Map.empty B0_string.Map.empty l
      >>= fun (k, u) -> Ok ((k, loc), (u, loc))

(* Extractors *)

let failwithf fmt =
  Format.kasprintf (fun s -> raise_notrace (Failure s)) fmt

let parse_atom k = function
| `Atom a, _ -> a
| `List _, loc ->
    failwithf "%a: key %s: expected an atom found a list" pp_loc loc k

let parse_list ?(empty = true) k = function
| `List [], loc when not empty ->
    failwithf "%a: key %s: list cannot be empty" pp_loc loc k
| `List l, _ -> l
| `Atom _, loc ->
    failwithf "%a: key %s: expected a list found an atom" pp_loc loc k

let parse_atom_kind parse k se = match parse (parse_atom k se) with
| exception Failure m -> failwithf "%a: key %s: %s" pp_loc (snd se) k m
| v -> v

let parse_list_kind ?empty parse_el k se =
  List.map (parse_el k) (parse_list ?empty k se)

let key ?absent parse k (m, loc) = match B0_string.Map.find k m with
| (se, _) -> parse k se
| exception Not_found ->
    match absent with
    | Some v -> v
    | None -> failwithf "%a: mandatory key %s is missing" pp_loc loc k

let atom_key ?absent parse k m =
  key ?absent (parse_atom_kind parse) k m

let list_key ?empty ?absent parse_el k m =
  key ?absent (parse_list_kind ?empty parse_el) k m

let atom_list_key ?empty ?absent parse k m =
  list_key ?empty ?absent (parse_atom_kind parse) k m

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
