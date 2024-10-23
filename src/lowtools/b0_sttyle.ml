(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type color = [ Fmt.color | `Hi of Fmt.color ]

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
    [Fmt.str "%a" (Fmt.st ((`Fg (f :> color)) :: base)) txt;
     Fmt.str "%a" (Fmt.st ((`Fg (`Hi f)) :: base)) txt; ]
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

let sttyle bold faint italic underline reverse =
  let base = make_base ~bold ~faint ~italic ~underline ~reverse in
  matrix ~base ()

open Cmdliner

let cmd =
  let doc = "Show ANSI styles" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(iname) tool outputs the various ANSI styles that \
          can be devised using $(b,B0_std.Tty.style) specifications. \
          It shows a basic color matrix (which does not fit on 80 columns) \
          and options can add more styling.";
      `Pre "$(iname)"; `Noblank;
      `Pre "$(iname) $(b,--bold)"; `Noblank;
      `Pre "$(iname) $(b,--italic --bold)";
      `P "See more options below.";
      `S Manpage.s_bugs;
      `P "This program is distributed with the b0 system. \
          See https://erratique.ch/software/b0 for contact information."; ]
  in
  let bold = Arg.(value & flag & info ["b"; "bold"] ~doc:"Use bold.") in
  let faint = Arg.(value & flag & info ["f"; "faint"] ~doc:"Use faint.") in
  let italic = Arg.(value & flag & info ["i"; "italic"] ~doc:"Use italic.") in
  let underline =
    Arg.(value & flag & info ["u"; "underline"] ~doc:"Use underline.")
  in
  let reverse = Arg.(value & flag & info ["r"; "reverse"] ~doc:"Use reverse.")in
  Cmd.v (Cmd.info "b0-sttyle" ~version:"%%VERSION%%" ~doc ~man) @@
  Term.(const sttyle $ bold $ faint $ italic $ underline $ reverse)

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
