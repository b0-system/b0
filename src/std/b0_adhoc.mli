(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Ad-hoc data extraction. *)

(** {1:commmonmark CommonMark} *)

val commonmark_first_section :
  preamble:bool -> string -> (string * string) option
(** [first_section src] is [Some (title, content)] where [title] is
    the content of the first CommonMark header found in CommonMark source
    [src] and [content] everything that follows until the next header
    ([preamble] is [true]) or next header of the same of smaller level
    ([preamble] is [false]). Trailing blank lines are discarded.

    {b Warning.} This function may break on valid CommonMark inputs in
    all sorts of fashion. *)
