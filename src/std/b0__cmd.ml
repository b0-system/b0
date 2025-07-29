(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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
let path p = A (B0__fpath.to_string p)
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

let paths ?slip ps = of_list ?slip B0__fpath.to_string ps

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
      B0__fmt.failwith "unclosed %s quote delimited string" kind
    in
    let skip_white s =
      B0__string.drop_first_while B0__char.Ascii.is_white s
    in
    let tok_sep c = c = '\'' || c = '\"' || B0__char.Ascii.is_white c in
    let tok_char c = not (tok_sep c) in
    let not_squote c = c <> '\'' in
    let tail s = (* Yikes *) B0__string.subrange ~first:1 s in
    let parse_squoted s =
      let tok, rem = B0__string.cut_first_while not_squote (tail s) in
      if not (String.equal rem "") then tok, tail rem else
      err_unclosed "single" s
    in
    let parse_dquoted acc s =
    let is_data = function '\\' | '"' -> false | _ -> true in
    let rec loop acc s =
      let data, rem = B0__string.cut_first_while is_data s in
      match B0__string.head rem with
      | Some '"' -> (data :: acc), (tail rem)
      | Some '\\' ->
          let rem = tail rem in
          begin match B0__string.head rem with
          | Some ('"' | '\\' | '$' | '`' as c) ->
              let acc = (B0__string.of_char c) :: data :: acc in
              loop acc (tail rem)
          | Some ('\n') -> loop (data :: acc) (tail rem)
          | Some c ->
              let acc = (data ^ (B0__fmt.str "\\%c" c)) :: acc in
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
      let rec loop acc s = match B0__string.head s with
      | None -> ret acc s
      | Some c when B0__char.Ascii.is_white c -> ret acc s
      | Some '\'' ->
          let tok, rem = parse_squoted s in loop (tok :: acc) rem
      | Some '\"' ->
          let acc, rem = parse_dquoted acc s in loop acc rem
      | Some c ->
          let sat = tok_char in
          let tok, rem = B0__string.cut_first_while sat s in
          loop (tok :: acc) rem
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
  with Failure err ->
    B0__fmt.error "command line %a: %s" B0__fmt.OCaml.string s err

let to_string l = String.concat " " (List.map Filename.quote @@ to_list l)
let pp ppf l =
  B0__fmt.pf ppf "@[%a@]" B0__fmt.(list ~sep:sp string) (to_list l)

let pp_dump ppf l =
  let pp_arg ppf a = B0__fmt.string ppf (Filename.quote a) in
  B0__fmt.pf ppf "@[<h>%a@]" B0__fmt.(list ~sep:sp pp_arg) (to_list l)

let pp_shell =
  let pp_arg ppf a = B0__fmt.string ppf (Filename.quote a) in
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
             then B0__fmt.char ppf ' ' else B0__fmt.sp ppf ());
            pp_arg ppf s; loop ~last_is_opt:is_opt ppf ss
        in
        pp_arg ppf s; loop ~last_is_opt:(is_opt s) ppf ss
  in
  B0__fmt.suffix_lines ~suffix:" \\" pp_cmd

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

type tool = B0__fpath.t

let tool = arg
let rec find_tool = function
| A a -> Result.to_option (B0__fpath.of_string a)
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
| None -> B0__fmt.error "%s: Not a tool" (to_string l)

let rec set_tool tool = function
| Rseq [] -> path tool
| l ->
    let rec loop = function
    | A a -> A (B0__fpath.to_string tool)
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
