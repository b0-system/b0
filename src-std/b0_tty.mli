(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** ANSI terminal interaction. See {!B0.Tty}. *)

(* Terminal kind and capabilities *)

type kind = No_tty | Dumb | Term of string
val kind : out:Unix.file_descr -> kind

type cap = Ansi | None
val cap : kind -> cap

(* ANSI escapes and styling *)

type color =
[ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
| `White ]

type style =
[ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Slow | `Rapid ]
| `Reverse | `Fg of color | `Bg of color ]

val sgrs_of_styles : style list -> string
val styled_str : cap -> style list -> string -> string
val strip_escapes : string -> string

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
