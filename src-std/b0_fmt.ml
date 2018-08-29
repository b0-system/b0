(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Formatting *)

let pf = Format.fprintf

(* Formatters *)

type 'a t = Format.formatter -> 'a -> unit

let nop ppf _ = ()
let sp = Format.pp_print_space
let cut = Format.pp_print_cut
let comma ppf () = pf ppf ",@ "
let unit fmt ppf () = pf ppf fmt

(* Base type formatters *)

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let float ppf f = pf ppf "%g" f
let char = Format.pp_print_char
let string = Format.pp_print_string

let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
  pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

let list ?sep:pp_sep = Format.pp_print_list ?pp_sep

let option ?none:(pp_none = nop) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let none_stub ppf () = string ppf "<none>"

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let array ?sep pp_elt = iter ?sep Array.iter pp_elt

let iter_bindings ?sep:(pp_sep = cut) iter pp_binding ppf v =
  let is_first = ref true in
  let pp_binding k v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_binding ppf (k, v)
  in
  iter pp_binding v

let text = Format.pp_print_text
let lines ppf s =
  let rec stop_at sat ~start ~max s =
    if start > max then start else
    if sat s.[start] then start else
    stop_at sat ~start:(start + 1) ~max s
  in
  let sub s start stop ~max =
    if start = stop then "" else
    if start = 0 && stop > max then s else
    String.sub s start (stop - start)
  in
  let is_nl c = c = '\n' in
  let max = String.length s - 1 in
  let rec loop start s = match stop_at is_nl ~start ~max s with
  | stop when stop > max -> Format.pp_print_string ppf (sub s start stop ~max)
  | stop ->
      Format.pp_print_string ppf (sub s start stop ~max);
      Format.pp_force_newline ppf ();
      loop (stop + 1) s
  in
  loop 0 s

let exn ppf e = string ppf (Printexc.to_string e)
let exn_backtrace ppf (e, bt) =
  let pp_backtrace_str ppf s =
    let stop = String.length s - 1 (* there's a newline at the end *) in
    let rec loop left right =
      if right = stop then string ppf (String.sub s left (right - left)) else
      if s.[right] <> '\n' then loop left (right + 1) else
      begin
        string ppf (String.sub s left (right - left));
        cut ppf ();
        loop (right + 1) (right + 1)
      end
    in
    if s = "" then (string ppf "No backtrace available.") else
    loop 0 0
  in
  pf ppf "@[<v>Exception: %a@,%a@]"
    exn e pp_backtrace_str (Printexc.raw_backtrace_to_string bt)

(* Boxes *)

let box ?(indent = 0) pp ppf =
  Format.pp_open_hovbox ppf indent; pf ppf "%a@]" pp

let hbox pp ppf =
  Format.pp_open_hbox ppf (); pf ppf "%a@]" pp

let vbox ?(indent = 0) pp ppf =
  Format.pp_open_vbox ppf indent; pf ppf "%a@]" pp

let hvbox ?(indent = 0) pp ppf =
  Format.pp_open_hvbox ppf indent; pf ppf "%a@]" pp

(* Brackets *)

let parens pp_v ppf v = pf ppf "@[<1>(%a)@]" pp_v v
let brackets pp_v ppf v = pf ppf "@[<1>[%a]@]" pp_v v
let braces pp_v ppf v = pf ppf "@[<1>{%a}@]" pp_v v

(* Tty

   N.B. what we are doing here is a bit less subtle than what we did
   in Fmt where capability was associated to formatters (and hence
   could distinguish between stdout/stderr. For now that seems
   sufficient. *)

let _tty_styling_cap = ref B0_tty.None
let set_tty_styling_cap cap = _tty_styling_cap := cap
let tty_styling_cap () = !_tty_styling_cap

let tty_str styles ppf s = match !_tty_styling_cap with
| B0_tty.None -> Format.pp_print_string ppf s
| B0_tty.Ansi ->
    Format.fprintf ppf "@<0>%s%s@<0>%s"
      (Printf.sprintf "\027[%sm" @@ B0_tty.sgrs_of_styles styles) s "\027[m"

let tty styles pp_v ppf v = match !_tty_styling_cap with
| B0_tty.None -> pp_v ppf v
| B0_tty.Ansi ->
    (* This doesn't compose well, we should get the current state
       and restore it afterwards rather than resetting. *)
    let reset ppf = Format.fprintf ppf "@<0>%s" "\027[m" in
    Format.kfprintf reset ppf "@<0>%s%a"
      (Printf.sprintf "\027[%sm" @@ B0_tty.sgrs_of_styles styles) pp_v v

(* Fields *)

let field ?(style = [`Fg `Yellow]) f pp_v ppf v =
  pf ppf "@[%a: @[%a@]@]" (tty_str style) f pp_v v


(* Magnitudes *)

let div_round_up x y = (x + y - 1) / y
let ilog10 x =
  let rec loop p = function 0 -> p | x -> loop (p + 1) (x / 10) in loop (-1) x

let ipow10 n =
  let rec loop r = function 0 -> r | y -> loop (r * 10) (y - 1) in loop 1 n

let si_max = 8
let si_symb =
  let prefix = [|""; "k"; "M"; "G"; "T"; "P"; "E"; "Z"; "Y"|] in
  fun p -> if p > si_max then prefix.(si_max) else prefix.(p)

let rec si_prefix u ppf s =
  let pow = if s = 0 then 0 else ilog10 s in
  let prefix = pow / 3 in
  match prefix with
  | 0 -> pf ppf "%d%s" s u
  | prefix ->
      let prefix = if prefix > si_max then si_max else prefix in
      let div = ipow10 (prefix * 3) in
      let size = s / div in
      match size with
      | size when size >= 100 ->
          let size = div_round_up s div in
          let s' = size * div in
          let pow' = ilog10 s' in
          if pow' > pow then si_prefix u ppf s' else
          pf ppf "%d%s%s" size (si_symb prefix) u
      | _ ->
          let frac = div_round_up (s mod div) (div / 10) in
          match frac with
          | f when f = 0 || s >= 10 ->
              pf ppf "%d%s%s" (div_round_up s div) (si_symb prefix) u
          | _ ->
              pf ppf "%d.%d%s%s" size frac (si_symb prefix) u

let byte_size ppf s = si_prefix "B" ppf s

(* Synopses

   TODO this should go somewhere else.  *)

let info ~name ?doc info ppf v =
  let bold ppf v = tty [`Bold] ppf v in
  match doc with
  | None ->  pf ppf "@[<v 1>%a:@,%a@]" (bold name) v info v
  | Some doc ->
      pf ppf "@[<v 1>%a:@,%a%a@]" (bold name) v (field "doc" doc) v info v

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
