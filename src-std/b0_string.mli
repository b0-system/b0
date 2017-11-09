(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Strings. See {!B0.String}. *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a

include module type of String

val head : string -> char option
val of_char : char -> string

(** {1 Predicates} *)

val is_prefix : affix:string -> string -> bool
val is_suffix : affix:string -> string -> bool
val for_all : (char -> bool) -> string -> bool
val exists : (char -> bool) -> string -> bool

(** {1 Extracting substrings} *)

val with_index_range : ?first:int -> ?last:int -> string -> string
val span : sat:(char -> bool) -> string -> string * string
val take : sat:(char -> bool) -> string -> string
val drop : sat:(char -> bool) -> string -> string
val cut : ?rev:bool -> sep:string -> string -> (string * string) option
val cuts : ?rev:bool -> ?empty:bool -> sep:string -> string -> string list

(** {1 Traversing} *)

val map : (char -> char) -> string -> string

(** {1 Uniqueness} *)

val uniquify : string list -> string list
val unique :
  exists:(string -> bool) -> string -> (string, [`Msg of string]) result

(** {1 Suggesting} *)

val edit_distance : string -> string -> int
val suggest : ?dist:int -> string list -> string -> string list

(** {1 Parsing version strings} *)

val parse_version : string -> (int * int * int * string option) option
val drop_initial_v : string -> string

(** {1 Pretty-printing} *)

val pp : Format.formatter -> string -> unit
val dump : Format.formatter -> string -> unit

(** {1 String map and sets} *)

type set

module Set : sig
  include Set.S with type elt := string
                 and type t := set
  type t = set
  val pp : ?sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> string -> unit) ->
    Format.formatter -> set -> unit
  val dump : Format.formatter -> set -> unit
end

type +'a map

module Map : sig
  include Map.S with type key := string
                 and type 'a t := 'a map

  type 'a t = 'a map
  val dom : 'a map -> set
  val of_list : (string * 'a) list -> 'a map
  val pp :
    ?sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> string * 'a -> unit) -> Format.formatter ->
    'a map -> unit

  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter ->
    'a map -> unit

  val dump_string_map : Format.formatter -> string map -> unit
end

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
