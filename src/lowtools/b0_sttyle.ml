(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type bright =
  [ `Black_bright | `Red_bright | `Green_bright | `Yellow_bright
  | `Blue_bright | `Magenta_bright | `Cyan_bright | `White_bright ]

let colors =
  [ `Default;
    `Black;   `Black_bright;
    `Red;     `Red_bright;
    `Green;   `Green_bright;
    `Yellow;  `Yellow_bright;
    `Blue;    `Blue_bright;
    `Magenta; `Magenta_bright;
    `Cyan;    `Cyan_bright;
    `White;   `White_bright ]

let color_to_string = function
| `Default -> "dft "
| `Black -> "blk "   | `Black_bright  -> "blkB"
| `Red -> "red "     | `Red_bright  -> "redB"
| `Green -> "grn "   | `Green_bright  -> "grnB"
| `Yellow -> "yel "  | `Yellow_bright -> "yelB"
| `Blue -> "blu "    | `Blue_bright -> "bluB"
| `Magenta -> "mag " | `Magenta_bright -> "magB"
| `Cyan    -> "cya " | `Cyan_bright    -> "cyaB"
| `White   -> "whi " | `White_bright   -> "whiB"

let txt = " TEXT "
let sp = "  "

let row ~base =
  let cell c = Fmt.str "%a" (Fmt.st ((`Fg c) :: base)) txt in
  String.concat sp (List.map cell colors)

let col_labels () =
  let labels = List.map color_to_string colors in
  String.concat sp (List.map (Fmt.str " %s ") labels)

let matrix ~base () =
  Fmt.pr "    %s%s@.@." sp (col_labels ());
  let bg b =
    let base = `Bg b :: base in
    Fmt.pr "%s%s%s@.@." (color_to_string b) sp (row ~base)
  in
  List.iter bg colors

let make_base ~bold ~faint ~italic ~underline ~reverse =
  let add_if c v acc = if c then v :: acc else acc in
  add_if bold `Bold @@ add_if faint `Faint @@
  add_if italic `Italic @@ add_if underline `Underline @@
  add_if reverse `Reverse @@ []

let sttyle ~bold ~faint ~italic ~underline ~reverse =
  let base = make_base ~bold ~faint ~italic ~underline ~reverse in
  matrix ~base ()

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Show ANSI style matrix" in
  let man =
    [ `S Manpage.s_description;
      `P "The $(cmd) tool outputs the various ANSI styles that \
          can be devised using $(b,B0_std.Fmt.style) specifications. \
          It shows a basic color matrix (which does not fit on 80 columns) \
          and options can add more styling.";
      `Pre "$(cmd)"; `Noblank;
      `Pre "$(cmd) $(b,--bold)"; `Noblank;
      `Pre "$(cmd) $(b,--italic --bold)";
      `P "See more options below.";
      `S Manpage.s_bugs;
      `P "This program is distributed with the $(b,b0) system. \
          See $(i,https://erratique.ch/software/b0) for contact information." ]
  in
  Cmd.make (Cmd.info "b0-sttyle" ~version:"%%VERSION%%" ~doc ~man) @@
  let+ bold = Arg.(value & flag & info ["b"; "bold"] ~doc:"Use bold.")
  and+ faint = Arg.(value & flag & info ["f"; "faint"] ~doc:"Use faint.")
  and+ italic = Arg.(value & flag & info ["i"; "italic"] ~doc:"Use italic.")
  and+ underline =
    Arg.(value & flag & info ["u"; "underline"] ~doc:"Use underline.")
  and+ reverse = Arg.(value & flag & info ["r"; "reverse"] ~doc:"Use reverse.")
  in
  sttyle ~bold ~faint ~italic ~underline ~reverse

let () = if !Sys.interactive then () else exit (Cmd.eval cmd)
