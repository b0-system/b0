(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON text support.

    As specified in {{:https://tools.ietf.org/html/rfc8259}RFC8259}.

    Open this module to use it, this only introduces modules in your scope. *)

(** JSON text definitions and codec. *)

open B0_text

(** Generic JSON values. *)
module Json : sig

  (** {1:json JSON text} *)

  type meta = Textloc.t
  (** The type for node metadata. *)

  val meta_none : meta
  (** [meta_none] is an invalid input location. *)

  type 'a node = 'a * meta
  (** Abstract syntax tree nodes. *)

  type name = string node
  (** The type for generic JSON object names. *)

  type mem = name * t
  (** The type for generic JSON object members. *)

  and object' = mem list
  (** The type for generic JSON objects. *)

  and t =
  | Null of unit node
  | Bool of bool node
  | Number of float node (** Encoders must use [Null] if float is not finite *)
  | String of string node
  | Array of t list node
  | Object of object' node
  (** The type for generic JSON text representations. *)

  val meta : t -> meta
  (** [meta j] is [j]'s node meta data. *)

  val equal : t -> t -> bool
  (** [equal j0 j1] is {!compare}[ j0 j1 = 0]. *)

  val compare : t -> t -> int
  (** [compare j0 j1] is a total order on JSON values:
      {ul
      {- Floating point values are compared with {!Float.compare},
       this means NaN values are equal.}
      {- Strings are compared byte wise.}
      {- Objects members are sorted before being compared.}
      {- meta values are ignored.}} *)

  val normalize : t -> t
  (** [normalize j] normalizes JSON [j] by sorting object's
      members by name using {!String.compare}. *)

  (** {1:cons Constructors} *)

  type 'a cons = ?meta:meta -> 'a -> t
  (** The type for constructing JSON values from an OCaml value of type ['a].
      [meta] default to {!meta_none}. *)

  (** {2:nulls Nulls and options} *)

  val null : unit cons
  (** [null] is [Null (unit, meta)]. *)

  val option : 'a cons -> 'a option cons
  (** [null] is [Null (unit, meta)]. *)

  (** {2:bools Booleans} *)

  val bool : bool cons
  (** [bool b] is [Bool (b, meta)]. *)

  (** {2:numbers Numbers} *)

  val number : float cons
  (** [number n] is [Number (b, meta)]. *)

  val any_float : float cons
  (** [any_float v] is [number v] if {!Float.is_finit}[ v] is [true]
      and string [Float.to_string v] otherwise. *)

  (** {1:strings Strings} *)

  val string : string cons
  (** [string s] is [`String (s, meta)]. *)

  (** {1:arrays Arrays} *)

  val list : t list cons
  (** [list vs] is [Array (vs, meta)]. *)

  val array : t array cons
  (** [array a] is [Array (Array.to_list a, meta)]. *)

  (** {1:objects Objects} *)

  val name : ?meta:meta -> string -> name
  (** [name ?meta n] is [n, meta]. [meta] defaults to {!meta_none}. *)

  val mem : name -> t -> mem
  (** [mem n v] is [(name, v)]. *)

  val object' : mem list cons
  (** [object' mems] is [Object (mems, meta)]. *)

  (** {1:codec Codec} *)

  val of_string : ?file:Textloc.filepath -> string -> (t, string) result
  (** [of_string s] parses JSON text from [s] according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} with the following
      limitations:
      {ul
      {- Numbers are parsed with [string_of_float] which is not
         compliant.}}

      {b Note.} All OCaml strings returned by this function are UTF-8
      encoded. *)

  val to_string : t -> string
  (** [to_string j] is [j] as JSON text, encoded according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259}.

      {b Warning.} Assumes all OCaml strings in [j] are UTF-8 encoded. *)

  (** {1:fmt Formatters} *)

  type number_format = (float -> unit, Format.formatter, unit) Stdlib.format
  (** The type for JSON number formatters. *)

  val default_number_format : number_format
  (** [default_number_format] is ["%.17g"]. This number formats ensures
      that finite floating point values can be interchanged without loss
      of precision. *)

  val pp' :
    ?number_format:number_format -> unit -> Format.formatter -> t -> unit
  (** [pp' ~format ~number_format () ppf j] formats [j] on [ppf]. The output
      is indented but may be more compact than an [Indent] JSON encoder may do.
      For example arrays may be output on one line if they fit etc.
      {ul
      {- [number_format] is used to format JSON numbers. Defaults to
       {!default_number_format}}
      {- Non-finite numbers are output as JSON nulls
       ({{!page-cookbook.non_finite_numbers}explanation}).}
      {- Strings are assumed to be valid UTF-8.}} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats JSON, see {!pp'}. *)
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

  val meta : Json.meta t
  (** [loc] is [map Json.meta sexp]. *)

  val with_meta : 'a t -> ('a * Json.meta) t
  (** [with_meta q] queries with [q] and returns the result with the
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

  val number : float t
  (** [number] queries JSON number values as a [float] value and fails
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
