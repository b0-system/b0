(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** S-expression parser.

    See {!B0.Sexp}. *)

open B0_result

(** {1 Source positions} *)

type pos = int
type range = pos * pos
type src = File of B0_fpath.t
type loc = src * range

val pp_loc : Format.formatter -> loc -> unit

(** {1 S-expressions} *)

type t = [ `Atom of string | `List of t list ] * loc
val get_atom : t -> string
val get_list : t -> t list

(** {1 Parsers} *)

val of_string : src:src -> string -> t result
val of_file : B0_fpath.t -> t result
val dump_locs : Format.formatter -> t -> unit

(** {1 Parsing key-value maps} *)

type map = (t * loc) B0_string.Map.t * loc
type 'a key = map -> 'a

val to_string_map : ?known:(string -> bool) -> t -> (map * map) result

val key : ?absent:'a -> (string -> t -> 'a) -> string -> 'a key
val atom_key : ?absent:'a -> (string -> 'a) -> string -> 'a key

val list_key :
  ?empty:bool -> ?absent:'a list -> (string -> t -> 'a) -> string ->
  'a list key

val atom_list_key :
  ?empty:bool -> ?absent:'a list -> (string -> 'a) -> string -> 'a list key


val parse_atom : string -> t -> string
val parse_list :?empty:bool ->  string -> t -> t list
val parse_atom_kind : (string -> 'a) -> string -> t -> 'a
val parse_list_kind :
  ?empty:bool -> (string -> t -> 'a) -> string -> t -> 'a list


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
