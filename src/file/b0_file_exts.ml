(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type t = String.Set.t
type map = Fpath.t list String.Map.t

let make = String.Set.of_list
let ext = String.Set.singleton

let find_files exts m =
  let add_ext ext acc = match String.Map.find ext m with
  | exception Not_found -> acc
  | fs -> List.rev_append fs acc
  in
  String.Set.fold add_ext exts []

let all_files m =
  let add _ files acc = List.rev_append files acc in
  String.Map.fold add m []

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

let c_lang = make [".c"; ".h"]
let cmark = make [".md"]
let css = make [".css"]
let data = make [".json"; ".xml"]
let font = make [".otf"; ".ttf"; ".woff"; ".woff2" ]
let html = make [".html"]
let html_lang = make [".html"; ".css"; ".js"; ]
let image =
  make [".eps"; ".gif"; ".ico"; ".jpeg"; ".jpg"; ".pdf"; ".png"; ".ps"; ".svg";
        ".tiff"]

let js = make [".js"]
let latex_lang = make [".tex"; ".sty"; ".bib"; ".bibdoi"]
let ocaml_lang = make [".ml"; ".mld"; ".mli"; ".mll"; ".mly"]
let sound = make [".aiff"; ".flac"; ".mp3"; ".wav"]
let tex = make [".tex"]
let video = make [".flv"; ".mov"; ".mp4"]
let www = data + font + html_lang + image + sound + video
let all =
  c_lang + cmark + css + data + font + html_lang + image + js +
  latex_lang + video + ocaml_lang + sound
