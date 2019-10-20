(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** UTF-8 text lexing tools.

    Open this module to use it defines only module in your scope. *)

(** Text locations. *)
module Tloc : sig

  (** {1:tloc Text locations} *)

  type fpath = string
  (** The type for file paths. *)

  val no_file : fpath
  (** [no_file] is [Fpath.t "-"], a path used when no file is specified. *)

  type pos = int
  (** The type for zero-based, absolute, byte positions in text. *)

  type line = int
  (** The type for one-based, line numbers in the text. Lines
      increment after a line feed ['\n'] (U+000A), a carriage return
      ['\r'] (U+000D) or a carriage return and a line feed ["\r\n"]
      (<U+000D,U+000A>). *)

  type line_pos = line * pos
  (** The type for line positions. The line number and the byte
      position of the first element on the line. The later is the
      byte position after the newline which may not exist (at the end
      of file). *)

  type t
  (** The type for text locations. A text location is a range of byte
      positions and the lines on which they occur in the UTF-8 encoded
      text of a particular file. *)

  val v :
    file:fpath -> sbyte:pos -> ebyte:pos -> sline:line_pos -> eline:line_pos ->
    t
  (** [v ~file ~sbyte ~ebyte ~sline ~eline] is a contructor for
      text locations. See corresponding accessors for the semantics.
      If you don't have a file use {!no_file}. *)

  val file : t -> fpath
  (** [file l] is [l]'s file. *)

  val sbyte : t -> pos
  (** [sbyte l] is [l]'s start position. *)

  val ebyte : t -> pos
  (** [ebyte l] is [l]'s end position. *)

  val sline : t -> line_pos
  (** [sline l] is the line position on which [sbyte l] lies. *)

  val eline : t -> line_pos
  (** [elin l] is the line position on which [ebyte l] lies. *)

  val nil : t
  (** [loc_nil] is an invalid location. *)

  val merge : t -> t -> t
  (** [merge l0 l1] merges the location [l0] and [l1] to the smallest
      location that spans both location. The file path taken from [l0]. *)

  val to_start : t -> t
  (** [to_start l] has both start and end positions at [l]'s start. *)

  val to_end : t -> t
  (** [to_end l] has both start and end positions at [l]'s end. *)

  val restart : at:t -> t -> t
  (** [restart ~at l] is [l] with the start position of [at]. *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats location like the OCaml compiler. *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats location according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is {!pp_gnu}. *)

  val pp_dump : Format.formatter -> t -> unit
  (** [pp_dump] formats raw data for debugging. *)

  (** {1:text Substitutions and insertions}

      Strictly speaking this doesn't belong here but here you go. *)

  val string_with_index_range : ?first:int -> ?last:int -> string -> string
  (** [string with_index_range ~first ~last s] are the consecutive bytes of [s]
      whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  val string_replace : start:int -> stop:int -> rep:string -> string -> string
  (** [string_replace ~start ~stop ~rep s] replaces the index range
      \[[start];stop-1\] of [s] with [rep] as follows. If [start = stop]
      the [rep] is inserted before [start]. [start] and [stop] must be
      in range \[[0];[String.length s]\] and [start <= stop] or
      [Invalid_argument] is raised. *)
end

(** Text decoder.

    A text decoder inputs UTF-8 data and checks its validity.  It
    updates locations according to advances in the input and has a
    token buffer used for lexing. *)
module Tdec : sig

  (** {1:dec Decoder} *)

  type t
  (** The type for UTF-8 text decoders. *)

  val create : ?file:Tloc.fpath -> string -> t
  (** [create ~file input] decodes [input] using [file] (defaults to
      {!Tloc.no_file}) for text location. *)

  (** {1:loc Locations} *)

  val file : t -> Tloc.fpath
  (** [file d] is the input file. *)

  val pos : t -> Tloc.pos
  (** [pos d] is the current decoding byte position. *)

  val line : t -> Tloc.line_pos
  (** [line d] is the current line position. Lines increment as
      described {{!Tloc.line}here}. *)

  val loc :
    t -> sbyte:Tloc.pos -> ebyte:Tloc.pos -> sline:Tloc.line_pos ->
    eline:Tloc.line_pos -> Tloc.t
  (** [loc d ~sbyte ~ebyte ~sline ~eline] is a location with the
      correponding position ranges and file according to {!file}. *)

  val loc_to_here :
    t -> sbyte:Tloc.pos -> sline:Tloc.line_pos -> Tloc.t
  (** [loc_to_here d ~sbyte ~sline] is a location that starts at
      [~sbyte] and [~sline] and ends at the current decoding
      position. *)

  val loc_here : t -> Tloc.t
  (** [loc_here d] is like {!loc_to_here} with the start position
      at the current decoding position. *)

  (** {1:err Errors} *)

  exception Err of Tloc.t * string
  (** The exception for errors. A location and an error message *)

  val err : Tloc.t -> string -> 'b
  (** [err loc msg] raises [Err (loc, msg)] with no trace. *)

  val err_to_here :
    t -> sbyte:Tloc.pos -> sline:Tloc.line_pos ->
    ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [err_to_here d ~sbyte ~sline fmt ...] is
      [err d (loc_to_here d ~sbyte ~sline) fmt ...] *)

  val err_here : t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [err_here d] is [err d (loc_here d) fmt ...]. *)

  (** {2:err_msg Error message helpers} *)

  val err_suggest : ?dist:int -> string list -> string -> string list
  (** [err_suggest ~dist candidates s] are the elements of [candidates]
      whose {{!edit_distance}edit distance} is the smallest to [s] and
      at most at a distance of [dist] of [s] (defaults to [2]). If
      multiple results are returned the order of [candidates] is
      preserved. *)

  val err_did_you_mean :
    ?pre:(Format.formatter -> unit -> unit) ->
    ?post:(Format.formatter -> unit -> unit) ->
    kind:string -> (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a * 'a list -> unit
  (** [did_you_mean ~pre kind ~post pp_v] formats a faulty value [v] of
      kind [kind] and a list of [hints] that [v] could have been
      mistaken for.

      [pre] defaults to [unit "Unknown"], [post] to {!nop} they surround
      the faulty value before the "did you mean" part as follows ["%a %s
      %a%a." pre () kind pp_v v post ()]. If [hints] is empty no "did
      you mean" part is printed. *)

  (** {1:dec Decoding} *)

  val eoi : t -> bool
  (** [eoi d] is [true] iff the decoder is at the end of input. *)

  val byte : t -> int
  (** [byte d] is the byte at current position or [0xFFFF] if
      [eoi d] is [true]. *)

  val accept_uchar : t -> unit
  (** [accept_uchar d] accepts an UTF-8 encoded character starting at
      the current position and moves to the byte after it. Raises
      {!Err} in case of UTF-8 decoding error. *)

  val accept_byte : t -> unit
  (** [accept_byte d] accepts the byte at the current position and
      moves to the next byte. {b Warning.} Faster than {!accept_uchar}
      but the client needs to make sure it's not accepting invalid
      UTF-8 data, i.e. that [byte d] is an US-ASCII encoded character
      (i.e. [<= 0x7F]). *)

  (** {1:tok Token buffer} *)

  val tok_reset : t -> unit
  (** [tok_reset d] resets the token. *)

  val tok_pop : t -> string
  (** [tok_pop d] returns the token and {!tok_reset}s it. *)

  val tok_accept_uchar : t -> unit
  (** [tok_accept_uchar d] is like {!accept_uchar} but also
      adds the UTF-8 byte sequence to the token. *)

  val tok_accept_byte : t -> unit
  (** [tok_accept_byte d] is like {!accept_byte} but also
      adds the byte to the token. {b Warning.} {!accept_byte}'s
      warning applies. *)

  val tok_add_byte : t -> int -> unit
  (** [tok_add_byte d b] adds byte [b] to the token. *)

  val tok_add_bytes : t -> string -> unit
  (** [tok_add_byte d s] adds bytes [s] to the token. *)

  val tok_add_char : t -> char -> unit
  (** [tok_add_char d c] adds character [c] to the token. *)

  val tok_add_uchar : t -> Uchar.t -> unit
  (** [tok_add_uchar t u] adds the UTF-8 encoding of character [u]
      to the token. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
