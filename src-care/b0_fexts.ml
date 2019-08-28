(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

type t = String.Set.t
type map = Fpath.t list String.Map.t
let v = String.Set.of_list
let ext = String.Set.singleton

let find_files exts m =
  let add_ext ext acc = match String.Map.find ext m with
  | exception Not_found -> acc
  | fs -> List.rev_append fs acc
  in
  String.Set.fold add_ext exts []

let exists_file exts fm =
  let check_ext ext = match String.Map.find ext fm with
  | exception Not_found -> ()
  | [] -> ()
  | _-> raise Exit
  in
  try String.Set.iter check_ext exts; false with Exit -> true

let ( + )  = String.Set.union
let ( - ) = String.Set.diff

(* Constants *)

let c_lang = v [".c"; ".h"]
let cmark = v [".md"]
let css = v [".css"]
let data = v [".json"; ".xml"]
let font = v [".otf"; ".ttf"; ".woff"; ".woff2" ]
let html_lang = v [".html"; ".css"; ".js"; ]
let image =
  v [".eps"; ".gif"; ".ico"; ".jpeg"; ".jpg"; ".pdf"; ".png"; ".ps";
     ".svg"; ".tiff"]

let javascript = v [".js"]
let latex_lang = v [".tex"; ".sty"; ".bib"; ".bibdoi"]
let ocaml_lang = v [".ml"; ".mld"; ".mli"; ".mll"; ".mly"]
let sound = v [".aiff"; ".flac"; ".mp3"; ".wav"]
let tex = v [".tex"]
let video = v [".flv"; ".mov"; ".mp4"]
let www = data + font + html_lang + image + sound + video
let all =
  c_lang + cmark + css + data + font + html_lang + image + javascript +
  latex_lang + video + ocaml_lang + sound

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
