(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Terminal kind and capabilities *)

type kind = No_tty | Dumb | Term of string

let kind ~out =
  let rec isatty fd = try Unix.isatty fd with
  | Unix.Unix_error (Unix.EINTR, _, _) -> isatty fd
  | Unix.Unix_error (e, _, _) -> false
  in
  if not (isatty out) then No_tty else
  match Unix.getenv "TERM" with
  | "" -> No_tty
  | "dumb" -> Dumb
  | v -> Term v
  | exception Not_found -> No_tty

type cap = Ansi | None
let cap kind = match kind with No_tty | Dumb -> None | Term _ -> Ansi

let strip_escapes s =
  let len = String.length s in
  let b = Buffer.create len in
  let max = len - 1 in
  let flush start stop = match start < 0 || start > max with
  | true -> ()
  | false -> Buffer.add_substring b s start (stop - start + 1)
  in
  let rec skip_esc i = match i > max with
  | true -> loop i i
  | false -> let k = i + 1 in if s.[i] = 'm' then loop k k else skip_esc k
  and loop start i = match i > max with
  | true ->
      if Buffer.length b = len then s else
      (flush start max; Buffer.contents b)
  | false ->
      match s.[i] with
      | '\027' -> flush start (i - 1); skip_esc (i + 1)
      | _ -> loop start (i + 1)
  in
  loop 0 0

(* ANSI styling *)

type color =
[ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
| `White ]

let sgr_base_int_of_color = function
| `Black -> 0 | `Red -> 1 | `Green -> 2 | `Yellow -> 3  | `Blue -> 4
| `Magenta -> 5 | `Cyan -> 6 | `White -> 7 | `Default -> 9

let sgr_of_fg_color c = Printf.sprintf "%d" (30 + sgr_base_int_of_color c)
let sgr_of_bg_color c = Printf.sprintf "%d" (40 + sgr_base_int_of_color c)

type style =
[ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Rapid | `Slow ]
| `Reverse | `Fg of color | `Bg of color ]

let sgr_of_style = function
| `Bold -> "01"
| `Faint -> "02"
| `Italic -> "03"
| `Underline -> "04"
| `Blink `Slow -> "05"
| `Blink `Rapid -> "06"
| `Reverse -> "07"
| `Fg c -> sgr_of_fg_color c
| `Bg c -> sgr_of_bg_color c

let sgrs_of_styles styles = String.concat ";" (List.map sgr_of_style styles)

let str_cap cap styles s = match cap with
| None -> s
| Ansi -> Printf.sprintf "\027[%sm%s\027[m" (sgrs_of_styles styles) s

(* N.B. what we are doing here is a bit less subtle than what we did
   in Fmt where capability was associated to formatters (and hence
   could distinguish between stdout/stderr. For now that seems
   sufficient. *)

let _styling_cap = ref None
let set_styling_cap cap = _styling_cap := cap
let styling_cap () = !_styling_cap

let str styles s = str_cap !_styling_cap styles s

let pp_str styles ppf s = match !_styling_cap with
| None -> Format.pp_print_string ppf s
| Ansi ->
    Format.fprintf ppf "@<0>%s%s@<0>%s"
      (Printf.sprintf "\027[%sm" @@ sgrs_of_styles styles) s "\027[m"

let pp styles pp_v ppf v = match !_styling_cap with
| None -> pp_v ppf v
| Ansi ->
    let reset ppf = Format.fprintf ppf "@<0>%s" "\027[m" in
    Format.kfprintf reset ppf "@<0>%s%a"
      (Printf.sprintf "\027[%sm" @@ sgrs_of_styles styles) pp_v v

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
