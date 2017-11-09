(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type 'a t = Format.formatter -> 'a -> unit

let pf = Format.fprintf

let nop ppf _ = ()
let sp = Format.pp_print_space
let cut = Format.pp_print_cut
let comma ppf () = pf ppf ",@ "
let unit fmt ppf () = pf ppf fmt

let bool = Format.pp_print_bool
let int = Format.pp_print_int
let char = Format.pp_print_char
let string = Format.pp_print_string

let pair ?sep:(pp_sep = cut) pp_fst pp_snd ppf (fst, snd) =
  pp_fst ppf fst; pp_sep ppf (); pp_snd ppf snd

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

let list ?sep:pp_sep = Format.pp_print_list ?pp_sep
let array ?sep pp_elt = iter ?sep Array.iter pp_elt

let option ?none:(pp_none = nop) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let none_str ppf () = string ppf "<none>"

let iter ?sep:(pp_sep = cut) iter pp_elt ppf v =
  let is_first = ref true in
  let pp_elt v =
    if !is_first then (is_first := false) else pp_sep ppf ();
    pp_elt ppf v
  in
  iter pp_elt v

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

(* Fields *)

let field_label ppf l = B0_tty.pp_str [`Fg `Yellow] ppf l
let field f pp_v ppf v = pf ppf "@[%a: @[%a@]@]" field_label f pp_v v

(* Synopses & info *)

let bold ppf v = B0_tty.pp [`Bold] ppf v
let synopsis ~name ~doc ppf v =
  pf ppf "@[<h>%a: %a@]" (bold name) v doc v

let info ~name ?doc info ppf v = match doc with
| None ->  pf ppf "@[<v 1>%a:@,%a@]" (bold name) v info v
| Some doc ->
    pf ppf "@[<v 1>%a:@,%a%a@]" (bold name) v (field "doc" doc) v info v

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
