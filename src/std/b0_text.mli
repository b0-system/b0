(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** UTF-8 text lexing tools.

    Open this module to use it defines only module in your scope. *)

(** Text locations.

    A text location identifies a text span in a given UTF-8 encoded file
    by an inclusive range of absolute {{!Textloc.type-byte_pos}byte} positions
    and the {{!Textloc.type-line_pos}line positions} on which those occur. *)
module Textloc : sig

  (** {1:filepath File paths} *)

  type filepath = string
  (** The type for file paths. *)

  val file_none : filepath
  (** [file_none] is ["-"]. A file path to use when there is none. *)

  (** {1:pos Positions} *)

  (** {2:byte_pos Byte positions} *)

  type byte_pos = int
  (** The type for zero-based, absolute, byte positions in text. If
      the text has [n] bytes, [0] is the first position and [n-1] is
      the last position. *)

  val byte_pos_none : byte_pos
  (** [byte_pos_none] is [-1]. A position to use when there is none. *)

  (** {2:lines Lines} *)

  type line_num = int
  (** The type for one-based, line numbers in the text. Lines
      increment after a {e newline} which is either a line feed ['\n']
      (U+000A), a carriage return ['\r'] (U+000D) or a carriage return
      and a line feed ["\r\n"] (<U+000D,U+000A>). *)

  val line_num_none : line_num
  (** [line_num_none] is [-1]. A line number to use when there is none. *)

  (** {2:line_pos Line positions} *)

  type line_pos = line_num * byte_pos
  (** The type for line positions. This identifies a line by its line
      number and the absolute byte position following its newline
      (or the start of text for the first line). That byte position:
      {ul
      {- Indexes the first byte of text of the line if the line is non-empty.}
      {- Indexes the first byte of the next {e newline} sequence if the line
         is empty.}
      {- Is out of bounds and equal to the text's length for a last empty
         line. This is also the case on empty text.}} *)

  val line_pos_first : line_pos
  (** [line_pos_first] is [1, 0]. Note that this is the only line position
      of the empty text. *)

  val line_pos_none : line_pos
  (** [line_pos_none] is [(line_pos_none, pos_pos_none)]. *)

  (** {1:tloc Text locations} *)

  type t
  (** The type for text locations. A text location identifies a text
      span in an UTF-8 encoded file by an inclusive range of absolute
      {{!type-byte_pos}byte positions} and the {{!type-line_pos}line
      positions} on which they occur.

      If the first byte equals the last byte the range contains
      exactly that byte. If the first byte is greater than the last
      byte this represents an insertion point before the first
      byte. In this case information about the last position should
      be ignored: it can contain anything. *)

  val none : t
  (** [none] is a position to use when there is none. *)

  val make :
    file:filepath -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t
  (** [v ~file ~first_byte ~last_byte ~first_line ~last_line] is a text
      location with the given arguments, see corresponding accessors for
      the semantics. If you don't have a file use {!file_none}. *)

  val file : t -> filepath
  (** [file l] is [l]'s file. *)

  val set_file : t -> filepath -> t
  (** [set_file l file] is [l] with {!file} set to [file]. *)

  val first_byte : t -> byte_pos
  (** [first_byte l] is [l]'s first byte. Irrelevant if {!is_none} is
      [true]. *)

  val last_byte : t -> byte_pos
  (** [last_byte l] is [l]'s last byte. Irrelevant if {!is_none} or
      {!is_empty} is [true]. *)

  val first_line : t -> line_pos
  (** [first_line l] is the line position on which [first_byte l] lies.
      Irrelevant if {!is_none} is [true].*)

  val last_line : t -> line_pos
  (** [last_line l] is the line position on which [last_byte l] lies.
      Irrelevant if {!is_none} or {!is_empty} is [true].*)

  (** {2:preds Predicates and comparisons} *)

  val is_none : t -> bool
  (** [is_none t] is [true] iff [first_byte < 0]. *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [first_byte t > last_byte t]. *)

  val equal : t -> t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] are equal. This checks
      that {!file}, {!first_byte} and {!last_byte} are equal. Line information
      is ignored. *)

  val compare : t -> t -> int
  (** [compare t0 t1] orders [t0] and [t1]. The order is compatible
      with {!equal}. Comparison starts with {!file}, follows with
      {!first_byte} and ends, if needed, with {!last_byte}. Line
      information is ignored. *)

  (** {2:shrink_and_stretch Shrink and stretch} *)

  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  (** [set_first l ~first_byte ~first_line] sets the the first position of
      [l] to given values. *)

  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  (** [set_last l ~last_byte ~last_line] sets the last position of [l]
      to given values. *)

  val to_first : t -> t
  (** [to_first l] has both first and last positions set to [l]'s first
      position. The range spans {!first_byte}. See also {!before}. *)

  val to_last : t -> t
  (** [to_last l] has both first and last positions set to [l]'s last
        position. The range spans {!last_byte}. See also {!after}. *)

  val before : t -> t
  (** [before t] is the {{!is_empty}empty} text location starting at
      {!first_byte}. *)

  val after : t -> t
  (** [after t] is the empty {{!is_empty}empty} location starting at
      [last_byte t + 1]; note that at the end of input this may be an
      invalid byte {e index}. The {!first_line} and {!last_line} of the
      result is [last_line t]. *)

  val span : t -> t -> t
  (** [span l0 l1] is the span from the smallest byte position of [l0] and
      [l1] to the largest byte position of [l0] and [l1]. The file path is
      taken from the greatest byte position. *)

  val reloc : first:t -> last:t -> t
  (** [reloc ~first ~last] uses the first position of [first], the
      last position of [last] and the file of [last]. *)

  (** {2:fmt Formatting} *)

  val pp_ocaml : Format.formatter -> t -> unit
  (** [pp_ocaml] formats text locations like the OCaml compiler. *)

  val pp_gnu : Format.formatter -> t -> unit
  (** [pp_gnu] formats text locations according to the
      {{:https://www.gnu.org/prep/standards/standards.html#Errors}GNU
      convention}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is {!pp_ocaml}. *)

  val pp_dump : Format.formatter -> t -> unit
  (** [pp_dump] formats raw data for debugging. *)
end

(** Text decoder.

    A text decoder inputs UTF-8 encoded characters from a string. It checks
    its validity and maintains information the absolute byte positions and line
    position (incrementing on LF, CR or CRLF) of the last decoded character.
    It also has a token buffer that can be used for lexing. *)
module Textdec : sig

  (** {1:decodes Decodes} *)

  type decode = int
  (** The type for decodes. This is either an arbitrary Unicode scalar
      value, {!sot} or {!eot}, if not either of those can be safely
      converted to an [Uchar.t] value with {!Uchar.unsafe_of_int}. *)

  val sot : decode
  (** [sot] is [x11_0000] ({!Uchar.max} + 1) an integer to represent the start
      of text. *)

  val eot : decode
  (** [eot] is [x11_0001] ({!Uchar.max} + 2) an integer to represent the end
      of text. *)

  val pp_decode : Format.formatter -> decode -> unit
  (** [pp_decode] formats decodes for inspection. This can be used
      in error messages, it escapes control characters and uses
      the strings ["start of text"] and ["end of text"] for {!sot}
      and {!eot}. *)

  (** {1:decoders Decoders} *)

  type t
  (** The type for text decoders. *)

  val make : ?file:Textloc.filepath -> string -> t
  (** [make ~file s] decodes UTF-8 text from [s] assuming
      to have been read from a file [file] (defaults to
      {!Textloc.file_none}). *)

  val input : t -> string
  (** [input d] is the input string of [d] *)

  val file : t -> Textloc.filepath
  (** [file d] is the file associated to [d]. *)

  (** {1:decoding Decoding} *)

  val current : t -> decode
  (** [current d] is the current decode. This is either:
      {ul
      {- {!sot}, if {!next} was never called on [d].}
      {- {!eot}, if all {!input} characters have been decoded via {!next}.}
      {- A Unicode scalar value.}} *)

  val is_error : t -> bool
  (** [is_error d] is [true] if [current d] is {!Uchar.rep} and not the result
      of a valid UTF-8 decode. *)

  val next : t -> unit
  (** [next d] decodes the next UTF-8 character into {!current} and
      updates the text locations. Repeated calls to {!next} after
      {!eot} has been returned have no effect.

      If an UTF-8 decoding error occurs {!current} becomes
      {!Uchar.rep} and {!is_error} returns [true]. {!next} can
      still be called afterwards for best-effort decoding. *)

  (** {1:textlocs Text locations} *)

  (** {2:byte_pos Byte positions} *)

  val first_byte_pos : t -> Textloc.byte_pos
  (** [first_byte_pos d] is the first byte position of the
      {!current} decode. If {!current} is:
      {ul
      {- {!sot}, this is [0].}
      {- {!eot}, this is [String.length (input d)]}
      {- A Unicode Scalar value, this is the first index in [input d]
         of its UTF-8 encoding.}} *)

  val last_byte_pos : t -> Textloc.byte_pos
  (** [last_byte_pos d] is the last position of the current {{!decode}decode}.
      If {!current} is:
      {ul
      {- {!sot}, this is [0].}
      {- {!eot}, this is [String.length (input d)]}
      {- A Unicode Scalar value, this is the last index in [input d]
         of its UTF-8 encoding.}} *)

  (** {2:line_pos Line positions} *)

  val line_num : t -> Textloc.line_num
  (** [line_num d] is the current line number. *)

  val line_start : t -> Textloc.byte_pos
  (** [line_num d] is the first byte position on the current line.
      See {!Textloc.val-line_pos}. *)

  val line_pos : t -> Textloc.line_pos
  (** [line_pos d] is the line position of the current decode. *)

  val prev_line_num : t -> Textloc.line_num
  (** [prev_line_num d] is the previous line number. This is {!line_num}
      minus one or 1 on the first line. *)

  val prev_line_start : t -> Textloc.byte_pos
  (** [prev_line_start] is the line start of the previous line. *)

  val prev_line_pos : t -> Textloc.line_pos
  (** [previous_line_pos d] is the line position of the previous line. *)

  (** {2:text_loc Text locations} *)

  val pos : t -> Textloc.byte_pos * Textloc.line_pos
  (** [pos d] is [first_byte_pos d, line_pos d]. This is the first
      position of the current decode. *)

  val textloc : t -> Textloc.t
  (** [textloc d] is the text position of the current decode. The text location
      spans the UTF-8 bytes of the decode it is on {!val-line_pos}[ d]. *)

  val textloc_span :
    t -> start:(Textloc.byte_pos * Textloc.line_pos) -> Textloc.t
  (** [textloc_span d ~start] is a text location that spans from [start] to
      the last byte of the current decode. *)

  val textloc_span_to_prev_decode :
    t -> start:(Textloc.byte_pos * Textloc.line_pos) -> Textloc.t
  (** [textloc_span_to_prev_decode d ~start] is a text location that spans
      from [start] to the last byte of the previous decode. *)

  (** {1:lexemebuffer Lexeme buffer} *)

  val lexeme_clear : t -> unit
  (** [lexeme_clear d] clears the lexeme buffer. *)

  val lexeme_pop : t -> string
  (** [lexeme_pop d] gets the lexeme buffer contents and clears is. *)

  val lexeme_add : t -> Uchar.t -> unit
  (** [lexeme_add d u] adds the UTF-8 encoding of [u] to the lexeme
      buffer. *)
end
