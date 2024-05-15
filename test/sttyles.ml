(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type color = [ Tty.color | `Hi of Tty.color ]

let colors =
  [ `Default; `Black; `Red; `Green; `Yellow; `Blue; `Magenta; `Cyan; `White; ]

let color_to_string ~hi c =
  let c = match c with
  | `Default -> "dft"
  | `Black   -> "blk"
  | `Red     -> "red"
  | `Green   -> "grn"
  | `Yellow  -> "yel"
  | `Blue    -> "blu"
  | `Magenta -> "mag"
  | `Cyan    -> "cya"
  | `White   -> "whi"
  | _ -> assert false
  in
  if hi then c ^ "H" else c ^ "L"

let txt = " TEXT "
let sp = "  "

let row ~base =
  let cells f =
    [Fmt.str "%a" (Fmt.tty ((`Fg (f :> color)) :: base)) txt;
     Fmt.str "%a" (Fmt.tty ((`Fg (`Hi f)) :: base)) txt; ]
  in
  String.concat sp (List.concat_map cells colors)

let col_labels () =
  let cols c = [color_to_string ~hi:false c; color_to_string ~hi:true c] in
  let labels = List.concat_map cols colors in
  String.concat sp (List.map (Fmt.str " %s ") labels)

let matrix ~base () =
  Fmt.pr "    %s%s@.@." sp (col_labels ());
  let bg ~hi_bg b =
    let base = (if hi_bg then `Bg (`Hi b) else `Bg (b :> color)) :: base in
    Fmt.pr "%s%s%s@.@." (color_to_string ~hi:hi_bg b) sp (row ~base)
  in
  let bg b = bg ~hi_bg:false b; bg ~hi_bg:true b; in
  List.iter bg colors

let make_base ~bold ~faint ~italic ~underline ~reverse =
  let add_if c v acc = if c then v :: acc else acc in
  add_if bold `Bold @@ add_if faint `Faint @@
  add_if italic `Italic @@ add_if underline `Underline @@
  add_if reverse `Reverse @@ []

let main () =
  let usage = "sttyles [--bold]" in
  let bold = ref false in
  let faint = ref false in
  let italic = ref false in
  let underline = ref false in
  let reverse = ref false in
  let spec = ["--bold", (Arg.Set bold), "Use bold";
              "--faint", (Arg.Set faint), "Use faint";
              "--italic", (Arg.Set italic), "Use italic";
              "--underline", (Arg.Set underline), "Use underline";
              "--reverse", (Arg.Set reverse), "Use reverse"]
  in
  let () =
    Arg.parse spec (fun _ -> raise (Arg.Bad "No positional argument")) usage
  in
  let base =
    make_base ~bold:!bold ~faint:!faint ~italic:!italic ~underline:!underline
      ~reverse:!reverse
  in
  Fmt.set_tty_cap (); matrix ~base (); 0

let () = if !Sys.interactive then () else exit (main ())
