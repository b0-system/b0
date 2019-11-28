(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** JSON text support.

    As specified in {{:https://tools.ietf.org/html/rfc8259}RFC8259}.

    Open this module to use it, this only introduces modules in your scope. *)

(** JSON text definitions and codec. *)

open B0_serialk_text

module Json : sig

  (** {1:json JSON text} *)

  type loc = Tloc.t
  (** The type for text locations. *)

  val loc_nil : loc
  (** [loc_nil] is an invalid input location. *)

  type mem = (string * loc) * t
  (** The type for JSON object members. *)

  and t =
  [ `Null of loc
  | `Bool of bool * loc
  | `Float of float * loc
  | `String of string * loc
  | `A of t list * loc
  | `O of mem list * loc ]
  (** The type for generic JSON text representations. *)

  val loc : t -> loc
  (** [loc j] is [j]'s input location. *)

  (** {1:cons Constructors} *)

  val null : t
  (** [null] is [`Null loc_nil]. *)

  val bool : bool -> t
  (** [bool b] is [`Bool (b, loc_nil)]. *)

  val float : float -> t
  (** [float b] is [`Float (f, loc_nil)]. *)

  val string : string -> t
  (** [string s] is [`String (s, loc_nil)]. *)

  val array : t list -> t
  (** [a vs] is [`A (vs, loc_nil)]. *)

  val mem : string -> t -> mem
  (** [mem n v] is [((n, loc_nil), v)]. *)

  val obj : mem list -> t
  (** [obj mems] is [`O (mems, loc_nil)]. *)

  (** {1:access Accessors} *)

  val to_null : t -> (unit, string) result
  (** [to_null j] extracts a null from [j]. If [j] is not a null an
      error with the location formatted according to {!Tloc.pp}
      is returned. *)

  val to_bool : t -> (bool, string) result
  (** [to_bool j] extracts a bool from [j]. If [j] is not a bool an
      error with the location formatted according to {!Tloc.pp}
      is returned. *)

  val to_float : t -> (float, string) result
  (** [to_float j] extracts a float from [j]. If [j] is not a float an
      error with the location formatted according to {!Tloc.pp}
      is returned. *)

  val to_string : t -> (string, string) result
  (** [to_string j] extracts a string from [j]. If [j] is not a string an
      error with the location formatted according to {!Tloc.pp}
      is returned. *)

  val to_array : t -> (t list, string) result
  (** [to_array j] extracts a array from [j]. If [j] is not a array an
      error with the location formatted according to {!Tloc.pp}
      is returned. *)

  val to_obj : t -> (mem list, string) result
  (** [to_obj j] extracts a array from [j]. If [j] is not a array an
      error with the location formatted according to {!Tloc.pp}
      is returned. *)

  val get_null : t -> unit
  (** [get_null j] is like {!to_null} but raises {!Invalid_argument}
      if [j] is not a null. *)

  val get_bool : t -> bool
  (** [get_bool j] is like {!to_bool} but raises {!Invalid_argument}
      if [j] is not a bool. *)

  val get_float : t -> float
  (** [get_float j] is like {!to_float} but raises {!Invalid_argument}
      if [j] is not a float. *)

  val get_string : t -> string
  (** [get_string j] is like {!to_string} but raises {!Invalid_argument}
      if [j] is not a string. *)

  val get_array : t -> t list
  (** [get_array j] is like {!to_array} but raises {!Invalid_argument}
      if [j] is not a array. *)

  val get_obj : t -> mem list
  (** [get_obj j] is like {!to_obj} but raises {!Invalid_argument}
      if [j] is not a array. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats JSON text.

      {b Warning.} Assumes all OCaml strings in the formatted value
      are UTF-8 encoded. *)

  (** {1:codec Codec} *)

  val of_string : ?file:Tloc.fpath -> string -> (t, string) result
  (** [of_string s] parses JSON text from [s] according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} with the following
      limitations:
      {ul
      {- Numbers are parsed with [string_of_float] which is not
         compliant.}
      {- TODO Unicode escapes are left unparsed (this will not round trip
         with {!to_string}).}}

      {b Note.} All OCaml strings returned by this function are UTF-8
      encoded. *)

  val to_string : t -> string
  (** [to_string j] is [j] as JSON text, encoded according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259}.

      {b Warning.} Assumes all OCaml strings in [j] are UTF-8 encoded. *)
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

  type array
  (** The type for generated JSON arrays. *)

  val array : array
  (** [array] is an empty array. *)

  val array_end : array -> t
  (** [array_end els] is arr a a generated JSON value. *)

  val el : t -> array -> array
  (** [el e arr] is array [arr] wit [e] added at the end. *)

  val el_if : bool -> (unit -> t) -> array -> array
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

(** JSON value queries.

    {b TODO} maybe we could expose a bit more options for error
    reporting. In particular the internal [path] type and a combinator
    in the vein of {!loc} to report back the path trace. Basically
    see {!Serialk_sexp}. *)
module Jsonq : sig

  (** {1:query Queries} *)

  type 'a t
  (** The type JSON value queries. A query either fails or succeeds against
      a JSON value returning a value of type ['a]. *)

  val query : 'a t -> Json.t -> ('a, string) result
  (** [query q j] is [Ok v] if que query [q] succeeds on [s] and
      a (multiline) [Error e] otherwise. *)

  (** {1:success Success and failure} *)

  val succeed : 'a -> 'a t
  (** [succeed v] is a query that succeeds with value [v] on any
      JSON value. *)

  val fail : string -> 'a t
  (** [fail msg] is a query that fails on any JSON value with message
      [msg]. Do not include position information in [msg], this
      is automatically handled by the module. *)

  val failf : ('a, Format.formatter, unit, 'b t) format4 -> 'a
  (** [failf fmt ...] is like {!fail} but formats the message
      according to [fmt]. *)

  (** {1:qcomb Query combinators} *)

  val app : ('a -> 'b) t -> 'a t -> 'b t
  (** [app fq q] queries a s-expression first with [fq] and then with [q]
      and applies the result of latter to the former. *)

  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
  (** [f $ v] is [app f v]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair q0 q1] queries first with [q0] and then with [q1] and returns
      the pair of their result. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind q f] queries a s-expression with [q], applies the result to
      [f] and re-queries the s-expression with the result. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f q] is [app (succeed f) q]. *)

  val some : 'a t -> 'a option t
  (** [some q] is [map Option.some q]. *)

  (** {1:json JSON queries} *)

  val fold :
    null:'a t -> bool:'a t -> float:'a t -> string:'a t ->  array:'a t ->
    obj:'a t -> 'a t
  (** [fold] queries JSON values according to their kind using the
      provided queries. *)

  val partial_fold :
    ?null:'a t -> ?bool:'a t -> ?float:'a t -> ?string:'a t -> ?array:'a t ->
    ?obj:'a t -> unit -> 'a t
  (** [partial_fold] is like {!fold} but only queries the kinds that
      are explicitely specified. It errors on other kinds. *)

  val json : Json.t t
  (** [json] queries any JSON value and returns it. *)

  val loc : Json.loc t
  (** [loc]is [map Sexp.loc sexp]. *)

  val with_loc : 'a t -> ('a * Json.loc) t
  (** [with_loc q] queries with [q] and returns the result with the
      location of the queried JSON value. *)

  (** {1:nulls Nulls} *)

  val is_null : bool t
  (** [is_null] tests for a JSON null value. *)

  val null : unit t
  (** [null] queries JSON null as unit and fails otherwise. *)

  val nullable : 'a t -> 'a option t
  (** [nullable q] is None on JSON null and otherwise queries the value
      with [q]. *)

  (** {1:atoms Atomic values} *)

  val bool : bool t
  (** [bool] queries JSON bool values as a [bool] value and fails otherwise. *)

  val float : float t
  (** [float] queries JSON number values as a [float] value and fails
      otherwise. *)

  val int : int t
  (** [int] is [map truncate float]. *)

  val string : string t
  (** [string] queries JSON string values as a [string] value and
      fails otherwise. *)

  val string_to : kind:string -> (string -> ('a, string) result) -> 'a t
  (** [string_to ~kind parse] queries a JSON string and parses it
      with [p]. In case of [Error m] error {!fail}s with [m]. [kind]
      is the kind of value parsed, it is used for the error in case no
      JSON string is found. *)

  val enum : kind:string -> Set.Make(String).t -> string t
  (** [enum ~kind ss] queries a JSON string for one of the elements of [ss]
      and fails otherwise. [kind] is for the kind of elements in [ss],
      it used for error reporting. *)

  val enum_map : kind:string -> 'a Map.Make(String).t -> 'a t
  (** [enum_map ~kind sm] queries a string for it's map in [sm] and fails
      if the string is not bound in [sm]. [kind] is for the kind elements
      in [sm], it is used for error reporting. *)

  (** {1:arrays Arrays}

      These queries only succeed on JSON array values. *)

  val is_empty_array : bool t
  (** [is_empty_array] queries an array for emptyness. *)

  val hd : 'a t -> 'a t
  (** [hd q] queries the first element of an array with [q]. Fails on empty
      arrays. *)

  val tl : 'a t -> 'a t
  (** [tail q] queries the tail of an array with [q]. Fails on empty
      arrays. *)

  val fold_array : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b t
  (** [fold_array f q acc] queries the elements of an array from left to
      right with [q] and folds the result with [f] starting with [acc]. *)

  val array : 'a t -> 'a list t
  (** [array q] queries the elements of an array with [q]. *)

  (** {2:array_index Array index queries} *)

  val nth : ?absent:'a -> int -> 'a t -> 'a t
  (** [nth ?absent n q] queries the [n]th element of an array with [q]. If
      [n] is negative counts from the end of the array, so [-1] is the
      last array element. If the element does not exist this fails if
      [absent] is [None] and succeeds with [v] if [absent] is [Some v]. *)

  (** {1:objects Objects}

      These queries only succeed on JSON object values. *)

  val mem : string -> 'a t -> 'a t
  (** [mem n q] queries the member [n] of a JSON object with [q]. The
      query fails if [n] is unbound in the object. *)

  val opt_mem : string -> 'a t -> absent:'a -> 'a t
  (** [opt_mem n q ~absent] queries the member [n] of a JSON object with [q].
      absent is returned if [n] is unbound in the object. *)

  val mem_dom : validate:Set.Make(String).t option -> Set.Make(String).t t
  (** [mem_dom ~validate] queries the member domain of a JSON object.
      If [validate] is [Some dom], the query fails if a member name is not in
      [dom]. *)
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
