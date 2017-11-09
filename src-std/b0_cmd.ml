(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* Command line fragments *)

type t = string list

let empty = []
let is_empty = function [] -> true | _ -> false
let v a = [a]
let ( % ) l a = a :: l
let ( %% ) l0 l1 = List.rev_append (List.rev l1) l0
let add_arg l a = l % a
let add_args l a = l %% a
let on bool l = if bool then l else []
let p = B0_fpath.to_string

(* Command lines *)

let line_exec l = try Some List.(hd @@ rev l) with Failure _ -> None
let get_line_exec l =
  try List.(hd @@ rev l) with Failure _ -> invalid_arg "the command is empty"

let line_args l = try List.(tl @@ rev l) with Failure _ -> []

(* Predicates and comparison *)

let equal l l' = l = l'
let compare l l' = Pervasives.compare l l'

(* Conversions and pretty printing *)

(* Parsing is loosely based on
   http://pubs.opengroup.org/onlinepubs/009695399/utilities/\
   xcu_chap02.html#tag_02_03 *)

(* FIXME quickly ported from bos code based on Astring.String.sub
   Rewrite. *)

let parse_cmdline s =
  try
    let err_unclosed kind s =
      failwith @@
      B0_string.strf "unclosed %s quote delimited string"
        (* (String.Sub.start_pos s) *) kind
    in
    let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false in
    let skip_white s = B0_string.drop ~sat:is_white s in
    let tok_sep c = c = '\'' || c = '\"' || is_white c in
    let tok_char c = not (tok_sep c) in
    let not_squote c = c <> '\'' in
    let tail s = (* Yikes *) B0_string.with_index_range ~first:1 s in
    let parse_squoted s =
      let tok, rem = B0_string.span ~sat:not_squote (tail s) in
      if not (B0_string.equal rem "") then tok, tail rem else
      err_unclosed "single" s
    in
    let parse_dquoted acc s =
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = B0_string.span ~sat:is_data s in
        match B0_string.head rem with
        | Some '"' -> (data :: acc), (tail rem)
        | Some '\\' ->
            let rem = tail rem in
            begin match B0_string.head rem with
            | Some ('"' | '\\' | '$' | '`' as c) ->
                let acc = B0_string.(of_char c) :: data :: acc in
                loop acc (tail rem)
            | Some ('\n') -> loop (data :: acc) (tail rem)
            | Some c ->
                let acc = (data ^ B0_string.strf "\\%c" c) :: acc in
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
      let ret acc s = B0_string.concat "" (List.rev acc), s in
      let rec loop acc s = match B0_string.head s with
      | None -> ret acc s
      | Some c when is_white c -> ret acc s
      | Some '\'' ->
          let tok, rem = parse_squoted s in loop (tok :: acc) rem
      | Some '\"' ->
          let acc, rem = parse_dquoted acc s in loop acc rem
      | Some c ->
          let sat = tok_char in
          let tok, rem = B0_string.span ~sat s in loop (tok :: acc) rem
      in
      loop [] s
    in
    let rec loop acc s =
      if B0_string.equal s "" then acc else
      let token, s = parse_token s in
      loop (token :: acc) (skip_white s)
    in
    Ok (loop [] (skip_white s))
  with Failure err -> R.error_msgf "command line %a:%s" B0_string.dump s err

let of_string s = parse_cmdline s
let to_string l = String.concat " " (List.rev_map Filename.quote l)

let to_list line = List.rev line
let to_rev_list line = line

let of_list ?slip line = match slip with
| None -> List.rev line
| Some slip -> List.fold_left (fun acc v -> v :: slip :: acc) [] line

let of_rev_list l = l

let of_values ?slip conv vs = match slip with
| None -> List.rev_map conv vs
| Some slip -> List.fold_left (fun acc v -> conv v :: slip :: acc) [] vs

let pp ppf cmd = match List.rev cmd with
| [] -> ()
| cmd :: [] -> Format.fprintf ppf "%s" cmd
| cmd :: args ->
    Format.fprintf ppf "@[<2>%s@ %a@]" cmd
      Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string) args

let dump ppf cmd =
  let pp_arg ppf a = Format.fprintf ppf "%s" (Filename.quote a) in
  Format.fprintf ppf "@[<h>[%a]@]"
    Format.(pp_print_list ~pp_sep:pp_print_space pp_arg) (List.rev cmd)

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
