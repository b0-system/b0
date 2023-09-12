(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Binary coding of values. *)

open B0_std

(** {1:enc Encoders} *)

type 'a enc = Buffer.t -> 'a -> unit
(** The type for encoders of values of type ['a].
    The call [enc b v] must encode [v] in buffer [b]. *)

(** {1:dec Decoders} *)

type 'a dec = string -> int -> int * 'a
(** The type for decoders of values of type ['a]. The call [dec s i]
    must decode a value in [s] starting at [i] (which may be
    [String.length s]) and return the index of the byte in [s] after
    the decoded value (this can be [String.length s]). The function must
    raise {!Failure} in case of error, use {!err} for this. *)

val err : int -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [err i fmt] reports a decoding error for position [i] formatted according
    to [fmt]. *)

val err_byte : kind:string -> int -> int -> 'a
(** [err_byte ~kind i byte] report an error for the unexpected byte
    [byte] at position [i] for decoding a value of type [kind]. *)

val get_byte : string -> int -> int
(** [get_byte s i] is the byte [s.[i]]. Does not check that [i] is
    in bounds. *)

val dec_eoi : string -> int -> unit
(** [dec_eoi s i] asserts that [i] is exactly at the end of input,
    i.e. [String.length s]. *)

(** {1:codec Codecs} *)

type 'a t
(** The type for encoding and decoding values of type ['a]. *)

val make : 'a enc -> 'a dec -> 'a t
(** [make enc dec] is a decoder using [enc] to encode and [dec] to decode. *)

val enc : 'a t -> 'a enc
(** [enc c] is [c]'s encoder. *)

val dec : 'a t -> 'a dec
(** [dec c] is [c]'s decoder. *)

val to_string : ?buf:Buffer.t -> 'a t -> 'a -> string
(** [encode c v] encodes [v] using [c] (and [buf] if provided, it is
    the client's duty to reset it before an encoding). *)

val of_string : ?file:Fpath.t -> 'a t -> string -> ('a, string) result
(** [of_string ~file c s] decodes a value from [s] using [c] and
    {!dec_eoi}. In case of error [file] is mentioned in the error
    message (defaults to {!B0_std.Fpath.dash}). *)

(** {1:base Base codecs} *)

(** {2:magics Magic numbers} *)

val enc_magic : string -> unit enc
(** [enc_magic m] encodes string [m] as a magic number. *)

val dec_magic : string -> unit dec
(** [dec_magic m] decodes magic number [m] and returns
    the next index to read from. *)

val magic : string -> unit t
(** [magic m] is a codec for magic number [m]. *)

(** {2:bytes Bytes} *)

val enc_byte : int enc
(** [enc_byte] is a byte encoder. Values larger than [0xFF] are
    truncated. *)

val dec_byte : kind:string -> int dec
(** [dec_byte] decodes a byte for a value of type [kind] (used with
    {!err_byte}) *)

val byte : kind:string -> int t
(** [byte] codecs a byte for a value of type [kind] (used with
    {!err_byte}). *)

(** {2:units [unit]} *)

val enc_unit : unit enc
(** [enc_unit] encodes unit. *)

val dec_unit : unit dec
(** [dec_unit] decodes unit. *)

val unit : unit t
(** [unti] is a codec for unit. *)

(** {2:bools [bool]} *)

val enc_bool : bool enc
(** [enc_bool] encodes a boolean. *)

val dec_bool : bool dec
(** [dec_bool] decodes a boolean. *)

val bool : bool t
(** [bool] is a codec for [bool]. *)

(** {2:ints [int]} *)

val enc_int : int enc
(** [enc_int] encodes an integer. The encoding does
    not depend on {!Sys.word_size}. *)

val dec_int : int dec
(** [dec_int] dedodes an integer. {b Warning.} An [int]
    encoded on a 64-bit platform may end up being truncated
    if read back on 32-bit platform. *)

val int : int t
(** [int] is a codec for integers. *)

(** {2:int64 [int64]} *)

val enc_int64 : int64 enc
(** [enc_int64] encodes an [int64]. *)

val dec_int64 : int64 dec
(** [dec_int64] decodes an [int64]. *)

val int64 : int64 t
(** [int64] is a coded for [int64]. *)

(** {2:string [string]} *)

val enc_string : string enc
(** [enc_string] encodes a string. *)

val dec_string : string dec
(** [dec_string] decodes a string. *)

val string : string t
(** [string] is a codec for [string]. *)

(** {2:fpath [Fpath.t]} *)

val enc_fpath : Fpath.t enc
(** [enc_fpath] encodes an {!B0_std.Fpath.t}. *)

val dec_fpath : Fpath.t dec
(** [dec_fpath] decodes an {!B0_std.Fpath.t}. *)

val fpath : Fpath.t t
(** [fpath] is a coded for [!B0_std.Fpath.t]. *)

(** {2:list [list]} *)

val enc_list : 'a enc -> 'a list enc
(** [enc_list enc] encodes the elements of a list using [enc]. *)

val dec_list : 'a dec -> 'a list dec
(** [dec_list dec] decodes the lements of a list using [dec]. *)

val list : 'a t -> 'a list t
(** [list c] is a codec for lists of elements coded with [c]. *)

(** {2:option [option]} *)

val enc_option : 'a enc -> 'a option enc
(** [enc_option enc] encodes an option using [enc] for the [Some] case
    value. *)

val dec_option : 'a dec -> 'a option dec
(** [dec_option dec] decodes an option using [dec] for the [Some] case
    value. *)

val option : 'a t -> 'a option t
(** [option c] is a codec for options with [Some] elements
    coded with [c]. *)

(** {2:result [result]} *)

val enc_result : ok:'a enc -> error:'b enc -> ('a, 'b) result enc
(** [enc_result ~ok ~error] encodes a result value with the corresponding
      case encoders. *)

val dec_result : ok:'a dec -> error:'b dec -> ('a, 'b) result dec
(** [dec_result ~ok ~error] decodes a result value with the corresponding
    case decoders. *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result] is a codec for results with [Ok] elements coded with [ok]
    and [Error] elements coded with [error]. *)

(** {2:set [Set.t]} *)

val enc_set :
  (module Set.S with type elt = 'a and type t = 'set) -> 'a enc -> 'set enc
(** [enc_set (module S) enc] encodes [S.t] sets using [enc] for its
    elements. *)

val dec_set :
  (module Set.S with type elt = 'a and type t = 'set) -> 'a dec -> 'set dec
(** [dec_set (module S) dec] decodes [S.t] sets with [dec] for its
    elements. *)

val set :
  (module Set.S with type elt = 'a and type t = 'set) -> 'a t -> 'set t
(** [set (module S) c] is a codec for [S.t] sets using [c] for its
    elements. *)

(** {2:hash [Hash.t]} *)

val enc_hash : Hash.t enc
(** [enc_hash] encodes a {!B0_std.Hash.t}. *)

val dec_hash : Hash.t dec
(** [dec_hash] decodes a {!B0_std.Hash.t}. *)

val hash : Hash.t t
(** [hash] is a codec for {!B0_std.Hash.t} *)

(** {2:mtime_span [Mtime.Span.t]} *)

val enc_mtime_span : Mtime.Span.t enc
(** [enc_mtime_span] encodes a {!B0_std.Mtime.Span.t}. *)

val dec_mtime_span : Mtime.Span.t dec
(** [dec_mtime_span] decodes a {!B0_std.Mtime.Span.t}. *)

val mtime_span : Mtime.Span.t t
(** [mtime_span] is a codec for {!B0_std.Mtime.Span.t}. *)

(** {2:os_cpu_time_span [Os.Cpu.Time.Span.t]} *)

val enc_cpu_time_span : Os.Cpu.Time.Span.t enc
(** [enc_cpu_time_span] encodes a {!B0_std.Os.Cpu.Time.Span.t}. *)

val dec_cpu_time_span : Os.Cpu.Time.Span.t dec
(** [dec_cpu_time_span] decodes a {!B0_std.Os.Cpu.Time.Span.t}. *)

val cpu_time_span : Os.Cpu.Time.Span.t t
(** [cpu_time_span] is a codec for {!B0_std.Os.Cpu.Time.Span.t}. *)
