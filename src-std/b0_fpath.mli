(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** File paths.

    See {!B0.Fpath}. *)

open B0_result

(** {1 Separators and segments} *)

val dir_sep_char : char
val dir_sep : string
val char_is_dir_sep : char -> bool
val is_seg : string -> bool
val is_rel_seg : string -> bool

(** {1 File paths} *)

type t

val v : string -> t
val to_string : t -> string
val of_string : string -> t result
val add_seg : t -> string -> t
val append : t -> t -> t
val ( / ) : t -> string -> t
val ( // ) : t -> t -> t

(** {1 File and directory paths} *)

val is_dir_path : t -> bool
val is_file_path : t -> bool
val to_dir_path : t -> t
val filename : t -> string
val filename_equal : t -> t -> bool
val basename : t -> string
val basename_equal : t -> t -> bool
val parent : t -> t

(** {1 Predicates} *)

val is_root : t -> bool
val is_abs : t -> bool
val is_rel : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int

(** {1 Extensions} *)

type ext = string

val get_ext : ?multi:bool -> t -> ext
val has_ext : ext -> t -> bool
val mem_ext : ext list -> t -> bool
val add_ext : ext -> t -> t
val rem_ext : ?multi:bool -> t -> t
val set_ext : ?multi:bool -> ext -> t -> t
val split_ext : ?multi:bool -> t -> t * ext
val ( + ) : t -> ext -> t
val ( -+ ) : t -> ext -> t

(** {1 Pretty printing} *)

val pp : Format.formatter -> t -> unit
val dump : Format.formatter -> t -> unit

(** {1 Sets and maps} *)

type set

module Set : sig
  include Set.S with type elt := t
                 and type t := set
  val pp : ?sep:unit B0_fmt.t -> t B0_fmt.t -> set B0_fmt.t
  val dump : set B0_fmt.t
  type t = set
end

type +'a map

module Map : sig
  include Map.S with type key := t
                 and type 'a t := 'a map

  val dom : 'a map -> set
  val of_list : (t * 'a) list -> 'a map
  val pp : ?sep:unit B0_fmt.t -> (t * 'a) B0_fmt.t -> 'a map B0_fmt.t
  val dump : 'a B0_fmt.t -> 'a map B0_fmt.t
  type 'a t = 'a map
end

val uniquify : t list -> t list

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
