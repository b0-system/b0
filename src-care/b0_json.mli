(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** JSON text support.

    Open this module to use it, this only introduces modules in your scope.

    {b FIXME.} Add locations the way {!B0_sexp} does. *)

open B0_std

(** JSON text definitions and codec.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)
module Json : sig

  (** {1:json JSON text} *)

  type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]
  (** The type for generic JSON text representations. *)

  (** {1:codec Codec} *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses JSON text from [s] according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} with the following
      limitations:
      {ul
      {- Numbers are parsed with [string_of_float] which is not
       compliant.}.
      {- Unicode escapes are left unparsed (this will not round trip
       with {!to_string}).}} *)

  val to_string : t -> string
  (** [to_string v] is [v] as JSON text, encoded according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} *)
end

(** JSON value generation. *)
module Jsong : sig

  (** {1:gen Generation} *)

  type t
  (** The type for generated JSON values. *)

  val null : t
  (** [null] is the generated JSON null value. *)

  val bool : bool -> t
  (** [bool b] is [b] as a generated JSON boolean value. *)

  val int : int -> t
  (** [int i] is [i] as a generated JSON number. *)

  val float : float -> t
  (** [float f] is [f] as a generated JSON number. *)

  val string : string -> t
  (** [str s] is [s] as a generated JSON string value. *)

  type arr
  (** The type for generated JSON arrays. *)

  val arr : arr
  (** [arr] is an empty array. *)

  val arr_end : arr -> t
  (** [arr_end els] is arr a a generated JSON value. *)

  val el : t -> arr -> arr
  (** [el e arr] is array [arr] wit [e] added at the end. *)

  val el_if : bool -> (unit -> t) -> arr -> arr
  (** [el cond v arr] is [el (v ()) arr] if [cond] is [true] and
      [arr] otherwise. *)

  type obj
  (** The type for generated JSON objects. *)

  val obj : obj
  (** [obj] is an empty object. *)

  val obj_end : obj -> t
  (** [obj_end o] is [o] as a generated JSON value. *)

  val mem : string -> t -> obj -> obj
  (** [mem name v o] is [o] with member [name] bound to value [v]
      added. *)

  val mem_if : bool -> string -> (unit -> t) -> obj -> obj
  (** [mem_if cond name v o] is [mem name (v ()) o] if [cond] is [true]
      and [o] otherwise. *)

  (** {1:derived Derived generators} *)

  val strf : ('a, Format.formatter, unit, t) format4 -> 'a
  (** [strf fmt ...] is a JSON string generated value formatted according
      to [fmt]. *)

  val fpath : Fpath.t -> t
  (** [fpath p] is [p] as a generated JSON string value. *)

  val cmd : Cmd.t -> t
  (** [cmd c] is [c] as a generated JSON string array value. *)

  val option : ('a -> t) -> 'a option -> t
  (** [option some o] is [o] as a generated JSON value which is
      {!null} if [o] is [None] and [some v] if [o] is [some v]. *)

  val list : ('a -> t) -> 'a list -> t
  (** [list el l] is [l] as a generated JSON array whose elements
      are generated using [el]. *)

  val json : Json.t -> t
  (** [of_json v] is the JSON value [v] as a generated value. *)

  (** {1:output Output} *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b g] adds the generated JSON value [g] to [b]. *)

  val to_string : t -> string
  (** [to_string g] is the generated JSON value [g] as a string. *)
end

(** JSON value queries. *)
module Jsonq : sig

  (** {1:query Queries} *)

  type 'a t
  (** The type for a query on a JSON value returning values of type ['a]. *)

  val null : unit t
  (** [null] queries a null JSON value. *)

  val nullable : 'a t -> 'a option t
  (** [nullable q] queries either a null JSON value or with [q]. *)

  val bool : bool t
  (** [bool] queries a boolean JSON value. *)

  val int : int t
  (** [int] queries a float JSON value and {!truncate}s it. *)

  val float : float t
  (** [float] queries a float JSON value. *)

  val string : string t
  (** [string] queries a string JSON value. *)

  val array : 'a t -> 'a list t
  (** [array q] queries the elements of a JSON array with [q]. *)

  val mem : string -> 'a t -> ('a -> 'b) t -> 'b t
  (** [mem name q o] queries a JSON object [o]'s member named
      [name] with [q]. *)

  val mem_opt : string -> 'a t -> ('a option -> 'b) t -> 'b t
  (** [mem_opt name q] queries a JSON object [o]'s optional member named
      [name] with [q]. *)

  val obj : 'a -> 'a t
  (** [obj v] queries an object and returns [v]. *)

  val json : Json.t t
  (** [json] queries any JSON value. *)

  val get : 'a -> 'a
  (** [get] is the identity function *)

  val sel : string -> 'a t -> 'a t
  (** [sel name q] is [obj get |> mem name q] *)

  val query : 'a t -> Json.t -> ('a, string) result
  (** [query q j] queries a JSON value [j] with [q]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers

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
