(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Command lines see {!B0.Cmd} *)

type t

val v : string -> t

val empty : t
val is_empty : t -> bool

val ( % ) : t -> string -> t
val ( %% ) : t -> t -> t
val add_arg : t -> string -> t
val add_args : t -> t -> t

val on : bool -> t -> t
val p : B0_fpath.t -> string

val line_tool : t -> string option
val get_line_tool : t -> string
val line_args : t -> string list

val equal : t -> t -> bool
val compare : t -> t -> int

val of_string : string -> t B0_result.result
val to_string : t -> string

val to_list : t -> string list
val to_rev_list : t -> string list
val of_list : ?slip:string -> string list -> t
val of_rev_list : string list -> t
val of_values : ?slip:string -> ('a -> string) -> 'a list -> t

val pp : t B0_fmt.t
val dump : t B0_fmt.t

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
