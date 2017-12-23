(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Software construction care.

    [B0] is a set of OCaml libraries and command line tools to
    configure, build and deploy generic software projects using
    modular and extensible descriptions written in OCaml. It provides
    a fully integrated and customizable software construction
    experience from development to deployment.

    See the {{!manual}manual} or the {{!Build}build API}.

    Open the module to use it, it defines only types, modules and
    result combinators in your scope.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:prelims Preliminaries} *)

type 'a result = ('a, [`Msg of string]) Pervasives.result
(** The type for [B0] results of type ['a]. *)

val ( >>= ) :
  ('a, 'b) Pervasives.result -> ('a -> ('c, 'b) Pervasives.result) ->
  ('c, 'b) Pervasives.result
(** [r >>= f] is [f v] if [r = Ok v] and [r] otherwise. *)

val ( >>| ) :
  ('a, 'b) Pervasives.result -> ('a -> 'c) -> ('c, 'b) Pervasives.result
(** [r >>| f] is [f >>= fun v -> Ok (f v)]. *)

(** Result value combinators. *)
module R : sig

  (** {1:composing Composing results} *)

  val reword_error :
    ('b -> 'c) -> ('a, 'b) Pervasives.result -> ('a, 'c) Pervasives.result
  (** [reword_error reword r] is:
      {ul
      {- [r] if [r = Ok v]}
      {- [Error (reword e)] if [r = Error e]}} *)

  val join :
    (('a, 'b) Pervasives.result, 'b) Pervasives.result ->
    ('a, 'b) Pervasives.result
  (** [join r] is [v] if [r = Ok v] and [r] otherwise. *)

  (** {1:errmsg Error messages} *)

  type msg = [ `Msg of string ]
  (** The type for (error) messages. *)

  val msgf : ('a, Format.formatter, unit, [> msg]) format4 -> 'a
  (** [msgf fmt ...] is a {!msg} formatted according to [fmt]. *)

  val error_msg : string -> ('b, [> msg]) Pervasives.result
  (** [error_msg s] is [Error (`Msg s)]. *)

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> msg]) Pervasives.result) format4 -> 'a
  (** [error_msgf fmt ...] is an error formatted according to [fmt]. *)

  val reword_error_msg :
    ?replace:bool -> (string -> msg) -> ('a, msg) Pervasives.result ->
    ('a, [> msg]) Pervasives.result
  (** [reword_error_msg ~replace reword r] is like {!reword_error} except
      if [replace] is [false] (default), the result of [reword old_msg] is
      concatened, on a new line to the old message. *)

  val open_error_msg :
    ('a, msg) Pervasives.result -> ('a, [> msg]) Pervasives.result
  (** [open_error_msg r] allows to combine a closed error message
      variant with other variants. *)

  val failwith_error_msg : ('a , msg) Pervasives.result -> 'a
  (** [failwith_error_msg r] raises [Failure m] if [r] is [Error (`Msg m)] *)
end

(** {!Format} combinators. *)
module Fmt : sig

  (** {1:formatting Formatting} *)

  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.fprintf}. *)

  (** {1:formatters Formatters} *)

  type 'a t = Format.formatter -> 'a -> unit
  (** The type for formatter of values of type ['a]. *)

  val nop : 'a t
  (** [nop] formats nothing. *)

  val cut : unit t
  (** [cut] is {!Format.pp_print_cut}. *)

  val sp : unit t
  (** [sp] is {!Format.pp_print_space}. *)

  val comma : unit t
  (** [comma] is [unit ",@ "]. *)

  val unit : (unit, Format.formatter, unit) Pervasives.format -> unit t
  (** [unit fmt] formats a unit value with the format [fmt]. *)

  (** {1:basetypes Base type formatters} *)

  val bool : bool t
  (** [bool] is {!Format.pp_print_bool}. *)

  val int : int t
  (** [int] is [pf ppf "%d"]. *)

  val char : char t
  (** [char] is {!Format.pp_print_char}. *)

  val string : string t
  (** [string] is {!Format.pp_print_string}. *)

  val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
  (** [pair ~sep pp_fst pp_snd] formats a pair. The first and second
      projection are formatted using [pp_fst] and [pp_snd] and are
      separated by [sep] (defaults to {!cut}). *)

  val list : ?sep:unit t -> 'a t -> 'a list t
  (** [list sep pp_v] formats list elements. Each element of the list is
      formatted in order with [pp_v]. Elements are separated by [sep]
      (defaults to {!cut}). If the list is empty, this is {!nop}. *)

  val option : ?none:unit t -> 'a t -> 'a option t
  (** [option ~none pp_v] formats an optional value. The [Some] case
      uses [pp_v] and [None] uses [none] (defaults to {!nop}). *)

  val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t
  (** [iter ~sep iter pp_elt] formats the iterations of [iter] over a
      value using [pp_elt]. Iterations are separated by [sep] (defaults to
      {!cut}). *)

  val iter_bindings : ?sep:unit t -> (('a -> 'b -> unit) -> 'c -> unit) ->
    ('a * 'b) t -> 'c t
  (** [iter_bindings ~sep iter pp_binding] formats the iterations of
      [iter] over a value using [pp_binding]. Iterations are separated
      by [sep] (defaults to {!cut}). *)

  val text : string t
  (** [text] is {Format.pp_print_text}. *)

  val lines : string t
  (** [lines] formats lines by replacing newlines ('\n') in the string
      with calls to {!Format.pp_force_newline}. *)

  val exn : exn t
  (** [exn] formats an exception. *)

  val exn_backtrace : (exn * Printexc.raw_backtrace) t
  (** [exn_backtrace] formats an exception backtrace. *)

  (** {1:boxes Boxes} *)

  val box : ?indent:int -> 'a t -> 'a t
  (** [box ~indent pp ppf] wraps [pp] in a horizontal or vertical box. Break
      hints that lead to a new line add [indent] to the current indentation
      (defaults to [0]). *)

  val hbox : 'a t -> 'a t
  (** [hbox] is like {!box} but is a horizontal box: the line is not split
      in this box (but may be in sub-boxes). *)

  val vbox : ?indent:int -> 'a t -> 'a t
  (** [vbox] is like {!box} but is a vertical box: every break hint leads
      to a new line which adds [indent] to the current indentation
      (default to [0]). *)

  val hvbox : ?indent:int -> 'a t -> 'a t
  (** [hvbox] is like {!box} but is either {!hbox} if its fits on
      a single line or {!vbox} otherwise. *)

  (** {1:bracks Brackets} *)

  val parens : 'a t -> 'a t
  (** [parens pp_v ppf] is [pf "@[<1>(%a)@]" pp_v]. *)

  val brackets : 'a t -> 'a t
  (** [brackets pp_v ppf] is [pf "@[<1>[%a]@]" pp_v]. *)

  val braces : 'a t -> 'a t
  (** [braces pp_v ppf] is [pf "@[<1>{%a}@]" pp_v]. *)

  (** {1:fields Fields} *)

  val field_label : string t
  (** [field_label l] pretty prints a field label [l]. *)

  val field : string -> 'a t -> 'a t
  (** [field l pp_v] pretty prints a field with label [l] using
      [pp_v] to print the value. *)
end

val strf : ('a, Format.formatter, unit, string) Pervasives.format4 -> 'a
(** [strf] is {!Format.asprintf}. *)

(** Strings. *)
module String : sig

  (** {1:string String} *)

  include module type of String

  val head : string -> char option
  (** [head s] if [Some s.[0]] if [s <> ""] and [None] otherwise. *)

  val of_char : char -> string
  (** [of_char c] is [c] as a string. *)

  (** {1:preds Predicates} *)

  val is_prefix : affix:string -> string -> bool
  (** [is_prefix ~affix s] is [true] iff [affix.[i] = s.[i]] for
      all indices [i] of [affix]. *)

  val is_suffix : affix:string -> string -> bool
  (** [is_suffix ~affix s] is true iff [affix.[n - i] = s.[m - i]] for all
      indices [i] of [affix] with [n = String.length affix - 1] and [m =
      String.length s - 1]. *)

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] is [true] iff for all indices [i] of [s], [p s.[i]
      = true]. *)

  val exists : (char -> bool) -> string -> bool
  (** [exists p s] is [true] iff there exists an index [i] of [s] with
      [p s.[i] = true]. *)

  (** {1:subs Extracting substrings} *)

  val with_index_range : ?first:int -> ?last:int -> string -> string
  (** [with_index_range ~first ~last s] are the consecutive bytes of [s]
      whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  val span : sat:(char -> bool) -> string -> string * string
  (** [span ~sat s] is [(l,r)] where [l] are the consecutive
      initial [sat] satisfying bytes of [s] or the empty string
      if there are no such bytes and [r] the remaining bytes. *)

  val take : sat:(char -> bool) -> string -> string
  (** [take ~sat s] is [fst (span ~sat s)]. *)

  val drop : sat:(char -> bool) -> string -> string
  (** [drop ~sat s] is [snd (span ~sat s)]. *)

  val cut : ?rev:bool -> sep:string -> string -> (string * string) option
  (** [cut ~sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the
      first match of the separator character [sep] or [None] if
      [sep] can't be matched in [s]. Matching starts from the
      beginning of [s] ([rev] is [false], default) or the end ([rev]
      is [true]).

      The invariant [l ^ sep ^ r = s] holds.

      @raise Invalid_argument if [sep] is the empty string. *)

  val cuts : ?rev:bool -> ?empty:bool ->  sep:string -> string -> string list
  (** [cuts sep s] is the list of all substrings of [s] that are
      delimited by matches of the non empty separator string
      [sep]. Empty substrings are omitted in the list if [empty] is
      [false] (defaults to [true]).

      Matching separators in [s] starts from the beginning of [s]
      ([rev] is [false], default) or the end ([rev] is [true]). Once
      one is found, the separator is skipped and matching starts
      again, that is separator matches can't overlap. If there is no
      separator match in [s], the list [[s]] is returned.

      The following invariants hold:
      {ul
      {- [concat ~sep (cuts ~empty:true ~sep s) = s]}
      {- [cuts ~empty:true ~sep s <> []]}}

      @raise Invalid_argument if [sep] is the empty string. *)

  (** {1:traversing Traversing} *)

  val map : (char -> char) -> string -> string
  (** [map f s] is [s'] with [s'.[i] = f s.[i]] for all indices [i] of
      [s]. [f] is invoked in increasing index order. *)

  val mapi : (int -> char -> char) -> string -> string
  (** [mapi f s] is [s'] with [s'.[i] = f i s.[i]] for all indices [i]
      of [s]. [f] is invoked in increasing index order. *)

  (** {1:pp Pretty printing} *)

  val pp : string Fmt.t
  (** [pp ppf s] prints [s]'s bytes on [ppf]. *)

  val dump : string Fmt.t
  (** [dump ppf s] prints [s] as a syntactically valid OCaml string
      on [ppf]. *)

  (** {1:unique Uniqueness} *)

  val uniquify : string list -> string list
  (** [uniquify ss] is [ss] without duplicates, the list order is
      preserved. *)

  val unique :
    exists:(string -> bool) -> string -> string result
  (** [unique ~exist n] is [n] if [exists n] is [false] or
      [r = strf "%s~%d" n d] with [d] the smallest integer in \[[1];[1e9]\]
      such that [exists r] is [false] or an error if there is no such
      string. *)

  (** {1:suggesting Suggesting} *)

  val edit_distance : string -> string -> int
  (** [edit_distance s0 s1] is the number of single character edits (insertion,
      deletion, substitution) that are needed to change [s0] into [s1]. *)

  val suggest : ?dist:int -> string list -> string -> string list
  (** [suggest ~dist candidates s] are the elements of [candidates]
      whose {{!edit_distance}edit distance} is the smallest to [s] and
      at most at a distance of [dist] of [s] (defaults to [2]). *)

  (** {1:vers Parsing version strings} *)

  val parse_version : string -> (int * int * int * string option) option
  (** [parse_version] parses version strings of the form:
{[
"[v]major.minor[.patchlevel][+additional-info]"
]}
      into [(major, minor, patch, additional_info)] tuples. *)

  val drop_initial_v : string -> string
  (** [drop_initial_v s] drops a leading ['v'] or ['V'] from [s]. *)

  (** {1:setmap String map and sets} *)

  type set
  (** The type for sets of strings. *)

  (** String sets. *)
  module Set : sig
    include Set.S with type elt := string
                   and type t := set

    type t = set
    (** The type for string sets. *)

    val pp : ?sep:unit Fmt.t -> string Fmt.t -> set Fmt.t
    (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}). If the set is empty leaves [ppf]
        untouched. *)

    val dump : set Fmt.t
    (** [dump ppf ss] prints an unspecified representation of [ss] on
        [ppf]. *)
  end

  type +'a map
  (** The type for maps from strings to values of type ['a]. *)

  (** String maps. *)
  module Map : sig

    (** {1 String maps} *)

    include Map.S with type key := string
                   and type 'a t := 'a map

    type 'a t = 'a map
    (** The type for string maps. *)

    val dom : 'a map -> set
    (** [dom m] is the domain of [m]. *)

    val of_list : (string * 'a) list -> 'a map
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    val pp : ?sep:unit Fmt.t -> (string * 'a) Fmt.t -> 'a map Fmt.t
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val dump : 'a Fmt.t -> 'a map Fmt.t
    (** [dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)

    val dump_string_map : string map Fmt.t
    (** [dump_string_map ppf m] prints an unspecified representation of the
        string map [m] on [ppf]. *)
  end
end

(**/**)
type fpath = B0_fpath.t (* needed to cut the def -> hmap -> fpath cycle. *)

(* FIXME remove this *)
module Codec : sig
  type 'a t
  val v : id:string -> 'a t
  val write : 'a t -> fpath -> 'a -> unit result
  val read : 'a t -> fpath -> 'a result
end
(**/**)

(** Textual and type-safe binary value conversions. *)
module Conv : sig

  (** {1:conv Converters} *)

  type 'a codec
  (** The type for type-safe binary codecs for values of type ['a]. *)

  val codec : (string -> 'a result) * ('a -> string result) -> 'a codec
  (** [codec (decode, encode)] is a codec using [decode] to decode values from
      byte strings and [encode] to encode values to byte strings. *)

  type 'a text
  (** The type for textual converters for values of type ['a]. *)

  val text : (string -> 'a result) * 'a Fmt.t -> 'a text
  (** [text (parse, print)] is a textual converter using [parse] to
      parse text into values and [print] to pretty-print values. Note
      that [print] need not necessarily be the inverse of [parse]. See
      {!v} for details. *)

  type 'a t
  (** The type for value converter for values of type ['a]. *)

  val v : ?docv:string -> ?codec:'a codec -> 'a text -> 'a t
  (** [v ~docv ~codec text] is a value converter using [text] for
      textual conversions and [codec] for binary conversions. If
      [codec] is [None] the printer of [text] {b must be} the inverse
      of its parser as it is used to derive a {!codec}. [docv] is a
      documentation meta-variable used in documentation to stand for
      the configuration value, it defaults to ["VALUE"]. *)

  val with_docv : 'a t -> string -> 'a t
  (** [with_docv c docv] is [c] with [docv] as a documentation
      meta-variable. *)

  val parse : 'a t -> (string -> 'a result)
  (** [parse c] is [c]'s textual parser. *)

  val print : 'a t -> 'a Fmt.t
  (** [print c] is [c]'s textual printer. *)

  val decode : 'a t -> (string -> 'a result)
  (** [decode c] is [c]'s binary decoder. *)

  val encode : 'a t -> ('a -> string result)
  (** [encode c] is [c]'s binary encoder. *)

  val docv : 'a t -> string
  (** [docv c] is [c] 's documentation meta-variable. *)

  (** {1:predef Predefined converters} *)

  val bool : bool t
  (** [bool] is a converter for booleans. *)

  val char : char t
  (** [char] is a convert for a single character. *)

  val int : int t
  (** [int] is a converter for ints. *)

  val int32 : int32 t
  (** [int32] is a converter for int32. *)

  val int64 : int64 t
  (** [int64] is a converter for int64. *)

  val float : float t
  (** [float] is a converter for floats. *)

  val string : string t
  (** [string] is a converter for strings. *)

  val string_non_empty : string t
  (** [string_non_empty] is a converter for non-empty strings. *)

  val fpath : fpath t
  (** [fpath] is a converter for file paths. *)

  val file : fpath t
  (** [file] is a converter for files. The file path is not checked for
      existence or non-directoryness. *)

  val dir : fpath t
  (** [dir] is a converter for directories. The directory path is not checked
      for existence or directoryness. *)

  val bin : fpath t
  (** [bin] is a converter for a binary. The file path is not checked
      for existence or executableness. *)

  val enum : ?docv:string -> (string * 'a) list -> 'a t
  (** [enum ~docv alts] derives a converter from the association list
      [alts]. [alts] should be a complete enumeration of the type ['a] or
      [Invalid_argument] may be raised by {!print} or {!encode}. *)

  val list : ?sep:string -> 'a t -> 'a list t
  (** [list ?sep c] is a converter for lists of [c].

      {b Warning.} Be aware of the following current limitations:
      {ul
      {- The textual parser uses [sep] (default to [","]) as a delimiter
         which is not allowed to occur in the parsed representation of [c]}
      {- The binary encoder uses ["\x00"] as a delimiter which is not
         allowed to occur in the output of [c]'s encoder.}}  *)

  val pair : ?sep:string -> 'a t -> 'b t -> ('a * 'b) t
  (** [pair ?sep fst snd] is a converter for pairs of [fst] and [snd].

      {b Warning}. The warning of {!list} applies. *)

  val option : ?none:string -> 'a t -> 'a option t
  (** [option c] parses a [c] option using [none] (defaults to [""]) to
      denote [None] in textual converters. In a textual parse [None] is
      unconditionally returned on [none], it takes over a potential
      decode of that string by [c]. *)
end

(** ANSI terminal interaction. *)
module Tty : sig

  (** {1:kind Terminal kind and capabilities} *)

  type kind = No_tty | Dumb | Term of string (** *)
  (** The type for terminals. *)

  val kind : out:Unix.file_descr -> kind
  (** [kind out] determines the kind of terminal by consulting the the
      [TERM] environment variable and using [Unix.isatty] on [out]. *)

  type cap = Ansi | None (** *)
  (** The type for terminal capabilities. Either
      {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
      ANSI} terminal or none. *)

  val cap : kind -> cap
  (** [cap kind] determines capabilities according to [kind]. *)

  val strip_escapes : string -> string
  (** [strip_escapes s] removes ANSI escapes from [s]. *)

  (** {1:style ANSI styling} *)

  type color =
  [ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
  | `White ]
  (** The type for ANSI colors. *)

  type style =
  [ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Slow | `Rapid ]
  | `Reverse | `Fg of color | `Bg of color ]
  (** The type for ANSI styles. *)

  val str_cap : cap -> style list -> string -> string
  (** [str_cap cap styles s] styles [s] according to [styles] and [cap]. *)

  (** {2:glob Global styling specification} *)

  val set_styling_cap : cap -> unit
  (** [set_styling_cap c] sets the global styling capabilities to
      [c]. Affects the output of {!str}, {!pp} and {!pp_str}. *)

  val styling_cap : unit -> cap
  (** [styling_cap ()] is the global styling capability. *)

  val str : style list -> string -> string
  (** [str styles s] styles [s] according to [styles] and the value of
      {!styling_cap}. *)

  val pp_str : style list -> string Fmt.t
  (** [pp_str styles ppf s] prints [s] on [ppf] according to [styles] and
      the value of {!styling_cap}. *)

  val pp : style list -> 'a Fmt.t -> 'a Fmt.t
  (** [pp styles pp_v ppf v] prints [v] with [pp_v] on [ppf] according
      to [styles] and the value of {!styling_cap}. *)
end

(** The [B0] program  log.

    This log should be used to log general program activity not for
    logging build operations.

    This module is modelled after {!Logs} logging, see the
    {{!Logs.basics}quick introduction} there. It can be made
    to log on a {!Logs} source, see {{!logger}here}. *)
module Log : sig

  (** {1:levels Reporting levels} *)

  type level = App | Error | Warning | Info | Debug (** *)
  (** The type for reporting levels. *)

  val level : unit -> level option
  (** [level ()] is the current reporting level. *)

  val set_level : level option -> unit
  (** [set_level l] sets the current reporting level to [l]. *)

  val pp_level : level Fmt.t
  (** [pp_level ppf l] prints and unspecified representation of [l]
      on [ppf]. *)

  val level_to_string : level option -> string
  (** [level_to_string l] converts [l] to a string representation. *)

  val level_of_string : string ->
    (level option, [`Msg of string]) Pervasives.result
  (** [level_of_string s] parses a level from [s] according to the
      representation of {!level_to_string}. *)

  (** {1:func Log functions} *)

  type ('a, 'b) msgf =
    (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  (** The type for client specified message formatting functions. See
      {!Logs.msgf}. *)

  type 'a log = ('a, unit) msgf -> unit
  (** The type for log functions. See {!Logs.log}. *)

  val msg : level -> 'a log
  (** See {!Logs.msg}. *)

  val maybe : level option -> 'a log
  (** [maybe] is like {!msg} but logs nothing if [level] is [None]. *)

  val app : 'a log
  (** [app] is [msg App]. *)

  val err : 'a log
  (** [err] is [msg Error]. *)

  val warn : 'a log
  (** [warn] is [msg Warning]. *)

  val info : 'a log
  (** [info] is [msg Info]. *)

  val debug : 'a log
  (** [debug] is [msg Debug]. *)

  val kmsg : (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b
  (** [kmsg k level m] logs [m] with level [level] and continues with [k]. *)

  (** {2:result Logging [result] value [Error]s} *)

  val on_error :
    ?level:level -> ?header:string -> pp:'b Fmt.t ->
    use:('b -> 'a) -> ('a, 'b) Pervasives.result -> 'a
  (** See {!Logs.on_error}. *)

  val on_error_msg :
    ?level:level -> ?header:string -> use:(unit -> 'a) ->
    ('a, [`Msg of string]) Pervasives.result -> 'a
  (** See {!Logs.on_error_msg}. *)

  (** {2:timing Logging timings} *)

  val time :
    ?level:level ->
    ('a -> (('b, Format.formatter, unit, 'a) format4 -> 'b) -> 'a) ->
    ('c -> 'a) -> 'c -> 'a
  (** [time ~level m f v] logs [m] with level [level] (defaults to
      [Info]) and the time [f v] took as the log header. *)

  (** {1:monitoring Log monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level
      [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level
      [Warning]. *)

  (** {1:logger Logger}

      The following function allows to change the logging backend.
      Note that in this case {{!monitoring}monitoring} and {{!levels}level}
      functions are no longer relevant. *)

  type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }
  (** The type for the basic logging function. *)

  val set_kmsg : kmsg -> unit
  (** [set_kmsg kmsg] sets the logging function to [kmsg]. *)
end

(** Named value definitions.

    End-user interaction with B0 occurs via unique names representing
    values which are defined in various description files and
    libraries.

    This modules abstracts the management of these names. It allows to
    track the {{!Loc}location} of their definition, guarantee their
    unicity, index them and, if needed, namespace them via the file
    system.

    {b WARNING.} As a decription library author, need only to use
    {!Loc.lib} otherwise this module should not be used and especially
    not by [B0.ml] description files, the {{!B0_driver}driver library} is
    in charge of handling this.

    FIXME we are exposing more than needed here. Trim that. *)
module Def : sig

  (** {1:defs Definitions} *)

  type loc
  (** See {!Loc.t} *)

  (** Definition locations. *)
  module Loc : sig

    (** {1:loc Locations} *)

    type t = loc
    (** The type for definition locations. *)

    val none : loc
    (** [none] locates nothing. *)

    val lib : string -> loc
    (** [lib l] locates in library [l]. *)

    val file : fpath -> loc
    (** [file f] locates in [f]. *)

    val is_none : loc -> bool
    (** [is_none l] is [true] iff [l] is {!none}. *)

    val find_file : loc -> fpath option
    (** [find_file l] is [Some f] is [l] is located in [f]. *)

    val equal : loc -> loc -> bool
    (** [equal l0 l1] is [true] iff [l0] and [l1] are the same location. *)

    val compare : loc -> loc -> int
    (** [compare l0 l1] totally orders [l0] and [l1]. *)

    val pp : loc Fmt.t
    (** [pp ppf l] prints and unspecified representation of [l] on [ppf]. *)

    (** {1 Private}

        {b WARNING.} Do not invoke these function they are invoked
        by driver libraries (e.g. {!B0_driver}). *)

    val set_root : fpath option -> unit
    (** [set_root r] sets the file source root to [r]. *)

    val get_root : unit -> fpath option
    (** [get_root ()] is the current file source root. *)

    val set_sub_root : fpath option -> unit
    (** [set_sub_root r] sets the file source sub root to [r]. *)

    val get_sub_root : unit -> fpath option
    (** [get_sub_root ()] is the file source sub root. *)

    val set_current : loc -> unit
    (** [set_current loc] sets the current location of definitions. *)

    val get_current : unit -> loc
    (** [get_current ()] is the current location of definitions. *)
  end

  (** Definition names. *)
  module Name : sig

    (** {1:namespaces Namespaces} *)

    val space : unit -> string
    (** [space ()] is the current namespace prefix. The prefix can be
        empty.

        The prefix is defined by relativizing a
        {{!Loc.get_current}current file source} according to the
        current {{!Loc.get_root}file root}. If neither of these exist
        the namespace prefix is [""]. *)

    val spaced : string -> string
    (** [spaced n] namespaces [n] if there is a namespace prefix. If the
        namespace is [""] this [n] itself. *)

    (** {1:rename Renaming} *)

    exception Panic
    (** This exception is raised if whenever renaming fails. It should not
        be handled, this indicates a serious error condition in the
        system. *)
  end

  type t
  (** The type for named definitions. *)

  type def = t
  (** The type for named definitions (again). *)

  (** The base module type for defined values.

      This module allows to configure how names are handled for
      a given type. *)
  module type DEFINED = sig
    type t
    (** The type for defined values. *)

    val def_kind : string
    (** [def_kind] is an uncapitalized string used in user interface to
        stand for the kind of defined values. *)

    val def_get : t -> def
    (** [def_get v] is [v]'s definition. *)

    val def_namespaced : bool
    (** [def_namespaced] is [true] iff definition names are namespaced. *)

    val def_name_tty_color : Tty.color
    (** [def_name_tty_color] is a tty color to render the value's names. *)

    val def_pp_info : t Fmt.t
    (** [def_pp_info ppf v] is the value field printing function
        use to derive {!S.pp_info}. *)
  end

  (** Named value interface.

      The module guarantees name unicity and provides indexing and lookup
      of the named values. *)
  module type S = sig
    type t
    (** The type for named values. *)

    val value_kind : string
    (** The kind of value. *)

    val name : t -> string
    (** [name v] is [v]'s name. *)

    val loc : t -> loc
    (** [loc v] is [v]'s source. *)

    val doc : t -> string
    (** [doc v] is [v]'s documentation string. *)

    val equal : t -> t -> bool
    (** [equal v0 v1] is [true] iff [v0] and [v1] and have the same name. *)

    val compare : t -> t -> int
    (** [compare v0 v1] is a total order on values compatible with {!equal}. *)

    val compare_by_name : t -> t -> int
    (** [compare_by_name v0 v1] totally orders [v0] and [v1] by name
        in increasing lexicographical order. *)

    val find : string -> t option
    (** [find n] is the value named [n] (if any). *)

    val get : string -> t
    (** [get n] is like {!find} but @raise Invalid_argument if no value
        [n] exists. *)

    val get_or_suggest : string -> (t, string list) Pervasives.result
    (** [get_or_suggest n] is the value with name [n] if it exists or
        a list of suggested values whose name could match [n]. *)

    val list : unit -> t list
    (** [list ()] is the list of existing value lexicographically ordered
        by increasing value name. *)

    (** {1:pp Pretty-printing} *)

    val pp_name_str : string Fmt.t
    (** [pp_name_str ppf s] prints [s] as if it was a name on [ppf]. *)

    val pp_name : t Fmt.t
    (** [pp_name ppf v] prints [v]'s name on [ppf]. *)

    val pp_synopsis : t Fmt.t
    (** [pp_synopsis ppf v] prints [v]'s synopsis:
        its name and documentation. *)

    val pp_info : t Fmt.t
    (** [pp_info ppf v] prints [v]'s synopsis and fields. *)

    val pp_info_ext : t Fmt.t -> t Fmt.t
    (** [pp_info ext ppf v] prints [v]'s synopsis and fields extended with
        [ext]. *)
  end

  (** Like {!S} but with access to underlying {!type:def}. *)
  module type S_DEF = sig
    include S

    val def : ?loc:loc -> ?doc:string -> string -> def
    (** [def ~loc ~doc n] is a definition named [n] at location
        [loc] with documentation string [doc] (defaults to
        ["Undocumented."]). [loc] default to {!Loc.none}.

        The resulting definition is guaranteed to have a unique name
        and this name won't be reused as long as it is {!def_add}ed to
        the index.

        A {{!Log.warn}warning} is issued if the definition had to be
        renamed.

        {b WARNING.} [loc] must be different from {!Loc.none} {e if
        and only if} the {{!Loc.get_current}current source} is {!Loc.none}
        or [Invalid_argument] is raised. FIXME should we rather log &
        ignore ? *)

    val def_add : t -> unit
    (** [def_add v] adds [v]'s definition to the underlying index. *)

    val def_rem : t -> unit
    (** [def_rem v] removes [v]'s definition from the underlying index. *)
  end

  (** [Make (V)] is uniquely named defined values for the type [V.t]. *)
  module Make (V : DEFINED) : S_DEF with type t = V.t
end

(** Type-safe, serializable, heterogeneous value maps.

    Type-safe serialization is enabled by mandating, on
    {{!S.Key.v}key creation} unique key names and a key value
    {{!Conv}converter}.

    Deserialization must be aware of key definitions, this means that
    all the keys in a serialized map must be created before attempting
    to deserialize the map. If a deserialized key does not exist this
    will be reported as such. If a key was serialized with a different
    converter than the current existing key this will likely lead to a
    key value decoding error, see {!MAP.serialization}. *)
module Hmap : sig

  (** {1:maps Maps} *)

  (** Signature for map keys. *)
  module type KEY = sig

    (** {1:keys Keys} *)

    type 'a typed
    (** The type for keys whose lookup value is of type ['a]. *)

    type 'a info
    (** The type for key information. *)

    type t = V : 'a typed -> t (** *)
    (** The type for existential keys. *)

    val v :
      ?loc:Def.loc -> ?doc:string -> string -> 'a Conv.t -> 'a info -> 'a typed
    (** [v n conv info ~doc] is a new key with name [n] using
        [conv] to convert values, [info] as key information and
        documented by [doc] (default to [""]).

        {b Warning.} Keys must be defined at toplevel and subject
        to namespacing, {{!descr_values}see details}. *)

    val conv : 'a typed -> 'a Conv.t
    (** [conv k] is [k]'s converter. *)

    val info : 'a typed -> 'a info
    (** [info k] is [k]'s key information. *)

    val of_typed : 'a typed -> t
    (** [of_typed k] is [k]'s existential key. *)

    include Def.S with type t := t
  end

  (** Signature for client defined key information. *)
  module type KEY_INFO = sig
    type 'a t
    (** The type for key information. *)

    val key_kind : string
    (** [key_kind] is an uncapitalized string used in user interface to
        stand for the kind of keys. *)

    val key_namespaced : bool
    (** [def_namespaced] is [true] iff key names are namespaced. *)

    val key_name_tty_color : Tty.color
    (** [key_name_tty_color] is a tty color to render the key's names. *)

    val pp : 'a Fmt.t -> 'a t Fmt.t
    (** [pp pp_v ppf i] is a printer for key information. *)
  end

  (** Signature for heterogeneous maps. *)
  module type MAP = sig

    (** {1:maps Maps} *)

    type 'a key
    (** The type for keys whose lookup value is of type ['a]. *)

    type t
    (** The type for serializable heterogeneous value maps. *)

    val empty : t
    (** [empty] is the empty map. *)

    val is_empty : t -> bool
    (** [is_empty m] is [true] iff [m] is empty. *)

    val mem : 'a key -> t -> bool
    (** [mem k m] is [true] iff [k] is bound in [m]. *)

    val add : 'a key -> 'a -> t -> t
    (** [add k v m] is [m] with [k] bound to [v]. *)

    val add_tag : bool key -> t -> t
    (** [add_tag k m] is [add k true m]. *)

    val singleton : 'a key -> 'a -> t
    (** [singleton k v] is [add k v empty]. *)

    val rem : 'a key -> t -> t
    (** [rem k m] is [m] with [k] unbound. *)

    val find : 'a key -> t -> 'a option
    (** [find k m] is the value of [k]'s binding in [m], if any. *)

    val get : 'a key -> t -> 'a
    (** [get k m] is the value of [k]'s binding in [m].
        @raise Invalid_argument if [k] is not bound in [m]. *)

    val get_or_suggest : 'a key -> t -> ('a, string list) Pervasives.result
    (** [get_or_suggest k m] is the value of [k]'s binding in [m] or,
        if [k] is not bound in [m] it suggests alternative, key names
        found in [m]. *)

    val flag : ?absent:bool -> bool key -> t -> bool
    (** [flag ~absent k m] is the value of [k] in [m] or [absent] (defaults
        to [false]) if [k] is not bound in [m]. *)

    type binding = B : 'a key * 'a -> binding (** *)
    (** The type for bindings. *)

    val iter : (binding -> unit) -> t -> unit
    (** [iter f m] applies [f] to all bindings of [m]. *)

    val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f m acc] folds over the bindings of [m] with [f], starting with
        [acc] *)

    val for_all : (binding -> bool) -> t -> bool
    (** [for_all p m] is [true] iff all bindings of [m] satisfy [p]. *)

    val exists : (binding -> bool) -> t -> bool
    (** [exists p m] is [true] iff there exists a bindings of [m] that
        satisfies [p]. *)

    val filter : (binding -> bool) -> t -> t
    (** [filter p m] are the bindings of [m] that satisfy [p]. *)

    val cardinal : t -> int
    (** [cardinal m] is the number of bindings in [m]. *)

    val any_binding : t -> binding option
    (** [any_binding m] is a binding of [m] (if not empty). *)

    val get_any_binding : t -> binding
    (** [get_any_binding m] is a binding of [m].

        @raise Invalid_argument if [m] is empty. *)

    val pp : t Fmt.t

    (** {1:serialization Serialization} *)

    type encode_error = string * [ `Msg of string ]
    (** The type for encode errors. The key name and the encoder error. *)

    val encode : t -> (string * string) list * encode_error list
    (** [encode m] is [(l,errs)] with [l] the bindings of [m] whose
        encoding suceeded and [errs] the bindings of [m] whose encoding
        failed. *)

    type decode_error = string * [ `Msg of string | `Unknown ]
    (** The type for decoded binding errors. The key name and the
        error. [`Unknown] occurs if the key name does not exist in
        {!Key.list} when the decode is performed. *)

    val decode : (string * string) list -> t * decode_error list
    (** [decode bs] is a map and a list of errors resulting from
        decoding the encoded binding list [l]. *)
  end

  (** Signature for a universe of heterogeneous maps sharing
      the same key space. *)
  module type S = sig

    (** {1:maps Maps} *)

    (** Keys. *)
    module Key : KEY

    type 'a key = 'a Key.typed
    (** The type for keys whose lookup value is of type ['a]. *)

    (** Maps. *)
    include MAP with type 'a key := 'a key
  end

  module Make (Key_info : KEY_INFO) () : S with type 'a Key.info = 'a Key_info.t
  (** [Make (Key_info) ()] creates a new universe of heterogeneous
      maps with information key [Key_info]. Maps in different
      universes can create keys with the same name without being
      subject to {{!S.Key.uniq}renaming}. *)
end

(** File paths.

    A file system {e path} specifies a file or a directory in a file
    system hierarchy. It is made of three parts:

    {ol
    {- An optional, platform-dependent, volume.}
    {- An optional root directory separator {!dir_sep} whose presence
       distiguishes absolute paths (["/a"]) from {e relative} ones
       (["a"])}
    {- A non-empty list of {!dir_sep} separated segments. {e Segments}
       are non empty strings except for maybe the last one. The latter
       syntactically distiguishes {e directory paths} (["a/b/"]) from
       file paths (["a/b"]).}}

    The paths segments ["."] and [".."] are relative path segments
    that respectively denote the current and parent directory. The
    {{!basename}basename} of a path is its last non-empty segment if
    it is not a relative path segment or the empty string otherwise (e.g.
    on ["/"]). *)
module Fpath : sig

  (** {1:segments Separators and segments} *)

  val dir_sep : string
  (** [dir_sep] is the platform dependent natural directory seperator.
      This is / on POSIX and \ on Windows. *)

  val is_seg : string -> bool
  (** [is_seg s] is [true] iff [s] does not contain a {!dir_sep}. *)

  val is_rel_seg : string -> bool
  (** [is_rel_seg s] is [true] iff [s] is a relative segment in other
      words either ["."] or [".."]. *)

  (** {1:paths File paths} *)

  type t = fpath
  (** The type for paths *)

  val v : string -> t
  (** [v s] is the string [s] as a path.

      {b Warning.} In code only use ["/"] as the directory separator
      even on Windows platforms (don't be upset, the module will give them
      back to you with backslashes).

      @raise Invalid_argument if [s] is not a {{!of_string}valid
      path}.  Use {!of_string} to deal with untrusted input. *)

  val of_string : string -> t result
  (** [of_string s] is the string [s] as a path. The following transformations
      are performed on the string:
      {ul
      {- On Windows any / occurence is converted to \ }}
      An error returned if [s] is [""]. *)

  val to_string : t -> string
  (** [to_string p] is the path [p] as a string. The result can
      be safely converted back with {!v}. *)

  val add_seg : t -> string -> t
  (** [add_seg p seg] if [p]'s last segment is non-empty this is
      [p] with [seg] added. If [p]'s last segment is empty, this is
      [p] with the empty segment replaced by [seg].

      @raise Invalid_argument if [is_seg seg] is [false]. *)

  val append : t -> t -> t
  (** [append p q] appends [q] to [p] as follows:
      {ul
      {- [q] is absolute or has a non-empty volume then [q] is returned.}
      {- Otherwise appends [q]'s segment to [p] using {!add_seg}.}} *)

  val ( / ) : t -> string -> t
  (** [p / seg] is [add_seg p seg]. Left associative. *)

  val ( // ) : t -> t -> t
  (** [p // p'] is [append p p']. Left associative. *)

  (** {1:filedirpaths File and directory paths}

      {b Note.} The following functions use syntactic semantic
      properties of paths. Given a path, these properties can be
      different from the ones your file system attributes to it. *)

  val is_dir_path : t -> bool
  (** [is_dir_path p] is [true] iff [p] syntactically represents
      a directory. This means that [p] is [.], [..] or ends
      with [/], [/.] or [/..]. *)

  val is_file_path : t -> bool
  (** [is_file_path p] is [true] iff [p] syntactically represents
      a file. This is the negation of {!is_dir_path}. *)

  val to_dir_path : t -> t
  (** [to_dir_path p] is [add_seg p ""]. It ensures that the resulting
      path represents a {{!is_dir_path}directory} and, if converted
      to a string, that it ends with a {!dir_sep}. *)

  val filename : t -> string
  (** [filename p] is the file name of [p]. This is the last segment
      of [p] if [p] is a {{!is_file_path}file path} and the empty
      string otherwise. See also {!basename}. *)

  val filename_equal : t -> t -> bool
  (** [filename_equal p0 p1] is [String.equal (filename p0) (filename p1)]. *)

  val basename : t -> string
  (** [basename p] is the last non-empty segment of [p] or the empty
      string otherwise. The latter occurs only on root paths and on paths
      whose last non-empty segment is a relative segment. *)

  val basename_equal : t -> t -> bool
  (** [basename p0 p1] is [String.equal (filename p0) (basename p1)]. *)

  val parent : t -> t
  (** [parent p] is a {{!is_dir_path}directory path} that contains
      [p]. If [p] is a {{!is_root}root path} this is [p] itself. *)

  (** {1:preds Predicates and comparison} *)

  val is_rel : t -> bool
  (** [is_rel p] is [true] iff [p] is a relative path, i.e. the root
      directory separator is missing in [p]. *)

  val is_abs : t -> bool
  (** [is_abs p] is [true] iff [p] is an absolute path, i.e. the root
      directory separator is present in [p]. *)

  val is_root : t -> bool
  (** [is_root p] is [true] iff [p] is a root directory, i.e. [p] has
      the root directory separator and a single, empty, segment. *)

  val equal : t -> t -> bool
  (** [equal p0 p1] is true iff [p0] and [p1] are stringwise equal. *)

  val compare : t -> t -> int
  (** [compare p0 p1] is a total order on paths compatible with {!equal}. *)

  (** {1 File extensions}

      The {e file extension} (resp. {e multiple file extension}) of a
      path segment is the suffix that starts at the last (resp. first)
      occurence of a ['.'] that is preceeded by at least one non ['.']
      character.  If there is no such occurence in the segment, the
      extension is empty.  With these definitions, ["."], [".."],
      ["..."] and dot files like [".ocamlinit"] or ["..ocamlinit"] have
      no extension, but [".emacs.d"] and ["..emacs.d"] do have one. *)

  type ext = string
  (** The type for file extensions, ['.'] seperator included.  *)

  val get_ext : ?multi:bool -> t -> ext
  (** [get_ext p] is [p]'s {{!basename}basename} file extension or the empty
      string if there is no extension. If [multi] is [true] (defaults to
      [false]), returns the mutiple file extension. *)

  val has_ext : ext -> t -> bool
  (** [has_ext ext p] is [true] iff
      [get_ext p = e || get_ext ~multi:true p = e]. *)

  val mem_ext : ext list -> t -> bool
  (** [mem_ext exts p] is [List.exists (fun e -> has_ext e p) exts] *)

  val add_ext : ext -> t -> t
  (** [add_ext ext p] is [p] with [ext] concatenated to [p]'s
      {{!basename}basename}. *)

  val rem_ext : ?multi:bool -> t -> t
  (** [rem_ext ?multi p] is [p] with the extension of [p]'s
      {{!basename}basename} removed. If [multi] is [true] (defaults to
      [false]), the multiple file extension is removed. *)

  val set_ext : ?multi:bool -> ext -> t -> t
  (** [set_ext ?multi p] is [add_ext ext (rem_ext ?multi p)]. *)

  val split_ext : ?multi:bool -> t -> t * ext
  (** [split_ext ?multi p] is [(rem_ext ?multi p, get_ext ?multi p)]. *)

  val ( + ) : t -> ext -> t
  (** [p + ext] is [add_ext p ext]. Left associative. *)

  val ( -+ ) : t -> ext -> t
  (** [p -+ ext] is [set_ext p ext]. Left associative. *)

  (** {1:pp Pretty printing} *)

  val pp : t Fmt.t
  (** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

  val dump : t Fmt.t
  (** [dump ppf p] prints path [p] on [ppf] using {!String.dump}. *)

  (** {1:unique Uniqueness} *)

  val uniquify : t list -> t list
  (** [uniquify ps] is [ps] without duplicates, the list order is
      preserved. *)

  (** {1:setmap Paths map and sets} *)

  type set
  (** The type for sets of paths. *)

  (** Path sets. *)
  module Set : sig

    (** {1 Path sets} *)

    include Set.S with type elt := t
                   and type t := set

    val pp : ?sep:unit Fmt.t -> t Fmt.t -> set Fmt.t
    (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}). If the set is empty leaves [ppf]
        untouched. *)

    val dump : set Fmt.t
    (** [dump ppf ss] prints an unspecified representation of [ss] on
        [ppf]. *)

    type t = set
    (** The type for path sets. *)
  end

  type +'a map
  (** The type for maps from paths to values of type ['a]. *)

  (** Path maps. *)
  module Map : sig

    (** {1 Path maps} *)

    include Map.S with type key := t
                   and type 'a t := 'a map

    val dom : 'a map -> set
    (** [dom m] is the domain of [m]. *)

    val of_list : (t * 'a) list -> 'a map
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    val pp : ?sep:unit Fmt.t -> (t * 'a) Fmt.t -> 'a map Fmt.t
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val dump : 'a Fmt.t -> 'a map Fmt.t
    (** [dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)

    type 'a t = 'a map
    (** The type for path maps. *)
  end

  (** {1:metadata Metadata} *)

  (** Heterogeneous value maps for storing file path metadata. *)
  module Meta : Hmap.S with type 'a Key.info = unit

  (** Map file paths to metadata. *)
  module Meta_map : sig

    type t = Meta.t map
    (** The type for maps from file paths to metadata. *)

    val empty : t
    (** [empty] is the empty map. *)

    val mem : fpath -> 'a Meta.key -> t -> bool
    (** [mem p k m] is [true] iff path [p] has a binding for [k] in [m]. *)

    val add : fpath -> 'a Meta.key -> 'a -> t -> t
    (** [add p k v m] adds key [k] with value [v] to [p]'s metadata in [m]. *)

    val rem : fpath -> 'a Meta.key -> t -> t
    (** [rem p k m] removes the key [k] from [p]'s metadata in [m]. *)

    val find : fpath -> 'a Meta.key -> t -> 'a option
    (** [find p k m] is the value of [p]'s metadata key [k] in [m]. *)

    val get : fpath -> 'a Meta.key -> t -> 'a
    (** [get p k m] is like {!find} but @raise Invalid_argument if [k] is no
        in [m]. *)

    val get_all : fpath -> t -> Meta.t
    (** [get_all p m] is all the metadata for [p] in [m]. *)
  end
end

(** Command lines.

    Both command lines and command line fragments using the same are
    represented with the same {{!t}type}.

    When a command line is {{!section:OS.Cmd.run}run}, the first
    element of the line defines the program name and each other
    element is an argument that will be passed {e as is} in the
    program's [argv] array: no shell interpretation or any form of
    argument quoting and/or concatenation occurs.

    See {{!ex}examples}. *)
module Cmd : sig

  (** {1:frags Command line fragments} *)

  type t
  (** The type for command line fragments. *)

  val v : string -> t
  (** [v cmd] is a new command line (or command line fragment)
      whose first argument is [cmd]. *)

  val empty : t
  (** [empty] is an empty command line. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is empty. *)

  val ( % ) : t -> string -> t
    (** [l % arg] adds [arg] to the command line [l]. *)

  val ( %% ) : t -> t -> t
  (** [l %% frag] appends the line fragment [frag] to [l]. *)

  val add_arg : t -> string -> t
  (** [add_arg l arg] is [l % arg]. *)

  val add_args : t -> t -> t
  (** [add_args l frag] is [l %% frag]. *)

  val on : bool -> t -> t
  (** [on bool line] is [line] if [bool] is [true] and {!empty}
      otherwise. *)

  val p : Fpath.t -> string
  (** [p] is {!Fpath.to_string}. This combinator makes path argument
      specification brief. *)

  (** {1:lines Command lines} *)

  val line_exec : t -> string option
  (** [line_exec l] is [l]'s first element, usually the executable name. *)

  val get_line_exec : t -> string
  (** [get_line_exec l] is like {!line_exec} but @raise Invalid_argument
      if there's no first element. *)

  val line_args : t -> string list
  (** [line_args] is [l]'s command line arguments, the elements of [l] without
      the command name. *)

  (** {1:predicates Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal l l'] is [true] iff [l] and [l'] are litterally equal. *)

  val compare : t -> t -> int
  (** [compare l l'] is a total order on command lines. *)

  (** {1:convert Conversions and pretty printing} *)

  val of_string : string -> t result
  (** [of_string s] tokenizes [s] into a command line. The tokens
      are recognized according to the [token] production of the following
      grammar which should be mostly be compatible with POSIX shell
      tokenization.
{v
white   ::= ' ' | '\t' | '\n' | '\x0B' | '\x0C' | '\r'
squot   ::= '\''
dquot   ::= '\"'
bslash  ::= '\\'
tokens  ::= white+ tokens | token tokens | ϵ
token   ::= ([^squot dquot white] | squoted | dquoted) token | ϵ
squoted ::= squot [^squot]* squot
dquoted ::= dquot (qchar | [^dquot])* dquot
qchar   ::= bslash (bslash | dquot | '$' | '`' | '\n')
v}
      [qchar] are substitued by the byte they escape except for ['\n']
      which removes the backslash and newline from the byte stream.
      [squoted] and [dquoted] represent the bytes they enclose. *)

  val to_string : t -> string
  (** [to_string l] converts [l] to a string that can be passed
      to the
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/system.html}
      [command(3)]} POSIX system call. *)

  val to_list : t -> string list
  (** [to_list l] is [l] as a list of strings. *)

  val to_rev_list : t -> string list
  (** [to_rev_list l] is [l] as a reversed list of strings. *)

  val of_list : ?slip:string -> string list -> t
  (** [of_list ?slip l] is a command line from the list of arguments
      [l].  If [slip] is specified it is added on the command line
      before each element of [l]. *)

  val of_rev_list : string list -> t
  (** [of_rev_list l] is a command line from the reversed list of
      arguments [l]. *)

  val of_values : ?slip:string -> ('a -> string) -> 'a list -> t
  (** [of_values ?slip conv l] is like {!of_list} but acts on a list
      of values, each converted to an argument with [conv]. *)

  val pp : t Fmt.t
  (** [pp ppf l] formats an unspecified representation of [l] on
      [ppf]. *)

  val dump : t Fmt.t
  (** [dump ppf l] dumps and unspecified representation of [l]
      on [ppf]. *)

  (** {1:ex Examples}
{[
let ls path = Cmd.(v "ls" % "-a" % p path)

let tar archive path = Cmd.(v "tar" % "-cvf" % p archive % p path)

let opam cmd = Cmd.(v "opam" % cmd)

let opam_install pkgs = Cmd.(opam "install" %% of_list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(v "ocamlc" % "-c" %% on debug (v "-g") % p file)

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(on profile @@ v "-p") in
  let debug = Cmd.(on debug @@ v "-g") in
  let incs = Cmd.of_list ~slip:"-I" incs in
  Cmd.(v "ocamlopt" % "-c" %% debug %% profile %% incs % p file)
]} *)
end

(** OS interaction. *)
module OS : sig

  (** Environment variables *)
  module Env : sig

    (** {1:var Variables} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name],
        if defined. *)

    val opt_var : string -> absent:string -> string
    (** [opt_var name ~absent] is the value of the optionally defined
        environment variable [name], if defined and absent if undefined. *)

    (** {1:env Process environement} *)

    type t = string String.map
    (** The type for process environments. *)

    val empty : t
    (** [empty] is {!String.Map.empty}. *)

    val current : unit -> t result
    (** [current ()] is the current process environment. *)

    val override : t -> by:t -> t
    (** [override env ~by:o] overrides the definitions in [env] by [o]. *)

    val of_assignments : ?init:t -> string list -> t result
    (** [of_assignments ~init ss] folds over strings in [ss],
        {{!String.cut}cuts} them at the first ['='] character and adds
        the resulting pair to [init] (defaults to {!empty}).  If the
        same variable is bound more than once, the last one takes
        over. *)

    val to_assignments : t -> string list
    (** [to_assignments env] is [env]'s bindings as a list of strings
        of the form ["var=value"]. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:paths Famous file paths} *)

    val null : Fpath.t
    (** [null] represents a file on the OS that discards all writes
        and returns end of file on reads. *)

    val dash : Fpath.t
    (** [dash] is ["-"]. This value is is used by {!read} and {!write}
        to respectively denote [stdin] and [stdout]. *)

    (** {1:exdel Existence and deletion} *)

    val exists : Fpath.t -> bool result
    (** [exists file] is [true] if [file] is a regular in the file
        system and false otherwise. Symbolic links are followed. *)

    val must_exist : Fpath.t -> Fpath.t result
    (** [must_exist file] is [file] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    val delete : ?must_exist:bool -> Fpath.t -> unit result
    (** [delete ~must_exist file] deletes file [file]. If [must_exist]
        is [true] (defaults to [false]) an error is returned if [file]
        doesn't exist. *)

    (** {1:links Hard links} *)

    val link : force:bool -> target:Fpath.t -> Fpath.t -> unit result
    (** [link ~force ~target p] hard links [target] to path [p]. If [force]
        is [true] and [p] exists it is unlinked first. FIXME should we
        [rmdir -r] if [p] is a directory ? *)

    (** {1:input Input}

        {b Stdin.} In the following functions if the path is {!dash},
        bytes are read from [stdin]. *)

    val with_ic :
      Fpath.t -> (in_channel -> 'a -> 'b) -> 'a -> 'b result
    (** [with_ic file f v] opens [file] as a channel [ic] and returns
        [Ok (f ic v)]. After the function returns (normally or via an
        exception), [ic] is ensured to be closed.  If [file] is
        {!dash}, [ic] is {!Pervasives.stdin} and not closed when the
        function returns. [End_of_file] exceptions raised by [f] are
        turned it into an error message. *)

    val read : Fpath.t -> string result
    (** [read file] is [file]'s content as a string. *)

    (** {1:output Output}

        The following applies to every function in this section:
        {ul
        {- Stdout. If the path is {!dash}, bytes are written to
           [stdout].}
        {- Default permission mode. The optional [mode] argument
           specifies the permissions of the created file. It defaults to
           [0o644] (readable by everyone writeable by the user).}
        {- Atomic writes. Files are written atomically by the
           functions. They create a temporary file [t] in the directory
           of the file [f] to write, write the contents to [t] and
           renames it to [f] on success. In case of error [t] is
           deleted and [f] left intact.}} *)

    val with_oc :
      ?mode:int -> Fpath.t ->
      (out_channel -> 'a -> (('c, 'd) Pervasives.result as 'b)) ->
      'a -> 'b result
    (** [with_oc file f v] opens [file] as a channel [oc] and returns
        [Ok (f oc v)]. After the function returns (normally or via an
        exception) [oc] is closed. [file] is not written if [f]
        returns an error. If [file] is {!dash}, [oc] is
        {!Pervasives.stdout} and not closed when the function
        returns. *)

    val write :
      ?mode:int -> Fpath.t -> string -> unit result
    (** [write file content] outputs [content] to [file]. If [file]
        is {!dash}, writes to {!Pervasives.stdout}. If an error is
        returned [file] is left untouched except if {!Pervasives.stdout}
        is written. *)


    (** {1:tmpfiles Temporary files}

        FIXME. Make that bos-like. *)

    val with_tmp_oc :
      ?mode:int -> Fpath.t -> (Fpath.t -> out_channel -> 'a -> 'b) ->
      'a -> 'b result
    (** [with_tmp_oc mode dir pat f v] is a new temporary file in
        [dir] (defaults to {!Dir.default_tmp}) named according to
        [pat] and atomically created and opened with permission [mode]
        (defaults to [0o600] only readable and writable by the
        user). Returns [Ok (f file oc v)] with [file] the file path
        and [oc] an output channel to write the file. After the
        function returns (normally or via an exception), [oc] is
        closed and [file] is deleted. *)

  end

  (** Directory operations. *)
  module Dir : sig

    (** {1:dirops Existence, creation, deletion and contents} *)

    val exists : Fpath.t -> bool result
    (** [exists dir] is [true] if [dir] is a directory in the file system
        and [false] otherwise. Symbolic links are followed. *)

    val must_exist : Fpath.t -> Fpath.t result
    (** [must_exist dir] is [Ok dir] if [dir] is a directory in the file system
        and an error otherwise. Symbolic links are followed. *)

    val create : ?path:bool -> ?mode:int -> Fpath.t -> bool result
    (** [create ~path ~mode dir] creates, if needed, the directory [dir] with
        file permission [mode] (defaults [0o755] readable and traversable
        by everyone, writeable by the user). If [path] is [true]
        (default) intermediate directories are created with the same
        [mode], otherwise missing intermediate directories lead to an
        error. The result is [false] if [dir] already exists.
        {b Note.} The mode of existing directories, including
        [dir] if this is the case is kept unchanged. *)

    val delete : ?must_exist:bool -> contents:bool -> Fpath.t -> unit result
    (** [delete ~must_exist ~contents dir] deletes the directory
        [dir]. If [contents] is [true] no error occurs if the
        directory is non-empty: its contents is recursively deleted
        first. If [must_exist] is [true] (defaults to [false]) an
        error is returned if [dir] doesn't exist. *)

    val contents : ?dotfiles:bool -> ?rel:bool -> Fpath.t -> Fpath.t list result
    (** [contents ~dotfiles ~rel dir] is the list of directories and
        files in [dir]. If [rel] is [false] (default) the resulting
        path have [dir] prepended, if [true] they are relative to
        [dir]. If [dotfiles] is [false] (default) elements that start
        with a [.] are omitted. *)

    val files : ?dotfiles:bool -> ?rel:bool -> Fpath.t -> Fpath.t list result
    (** [files] is like {!contents} but only returns files. *)

    val dirs : ?dotfiles:bool -> ?rel:bool -> Fpath.t -> Fpath.t list result
    (** [dirs] is like {!contents} but only returns directories. *)

    (** {1:current Current working directory} *)

    val current : unit -> Fpath.t result
    (** [current ()] is the current working directory. The resulting
        path is guaranteed to be absolute. *)

    val set_current : Fpath.t -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)

    val with_current : Fpath.t -> ('a -> 'b) -> 'a -> 'b result
    (** [with_current dir f v] is [f v] with the current working directory
        bound to [dir]. After the function returns the current working
        directory is back to its initial value. *)

    (** {1:tmp Default Temporary directory} *)

    val default_tmp : unit -> Fpath.t
    (** [default_tmp ()] is the directory used as a default value for
        creating {{!File.tmpfiles}temporary files} and
        {{!tmpdirs}directories}. If {!set_default_tmp} hasn't been
        called this is:
        {ul
        {- On POSIX, the value of the [TMPDIR] environment variable or
           [Fpath.v "/tmp"] if the variable is not set or empty.}
        {- On Windows, the value of the [TEMP] environment variable or
           {!Fpath.cur_dir} if it is not set or empty}} *)

    val set_default_tmp : Fpath.t -> unit
    (** [set_default_tmp p] sets the value returned by {!default_tmp} to
        [p]. *)
  end

  (** Running commands. *)
  module Cmd : sig

    (** {1:exists Command existence} *)

    val exists : Cmd.t -> bool result
    (** [exists cmd] is [true] if the executable of [cmd] can be found
        by the [PATH] lookup procedure and [false] otherwise. *)

    val must_exist : Cmd.t -> Cmd.t result
    (** [must_exist cmd] is [cmd] if the executable of [cmd] can be found
        in the [PATH] and an error otherwise. *)

    val which : Cmd.t -> Fpath.t option result
    (** [which cmd] is the full path to the executable of [cmd] as found
        in byte the [PATH] lookup procedure. *)

    (**/**)
    val which_raw : string -> string option
    val execv_raw : string -> string array -> unit result
    val execve_raw : string -> string array -> env:string array -> unit result
    (**/**)

    (** {1:run Running commands} *)

    type status = [`Exited of int | `Signaled of int ]

    val pp_status : status Fmt.t

    val run : ?err:Fpath.t -> Cmd.t -> unit result
    (** [run cmd] runs the command [cmd]. [std{i,o,err}] are connected
        to the invoking process' standard channels. If [err] is specified
        [stderr] is redirected to the given file (e.g. {!File.null}). *)

    val run_status : ?err:Fpath.t -> Cmd.t -> status result
    (** [run_status cmd] is like {!run}, but doesn't error on non-zero
        exit status. *)

    (** {1:stdout Capturing standard output} *)

    type run_status = Cmd.t * status
    (** The type for run statuses, the command that was run and the run
        status. *)

    val success : ('a * run_status) result -> 'a result
    (** [success r] is:
        {ul
        {- [Ok v] if [r = Ok (v, (_, `Exited 0))]}
        {- [Error _] otherwise. Non [`Exited 0] statuses are turned into
           an error message.}} *)

    type run_out
    (** The type for representing the standard output of a command run. *)

    val out_string : ?trim:bool -> run_out -> (string * run_status) result
    (** [out_string ~trim o] captures the standard output [o] as a [string].
        If [trim] is [true] (default) the result is passed through
        {!String.trim}. *)

    val out_file : Fpath.t -> run_out -> (unit * run_status) result
    (** [out_file f o] writes the standard output [o] to file [f]. *)

    val out_stdout : run_out -> (unit * run_status) result
    (** [out_stdout o] redirects the standard output [o] to the current
        process standard output. *)

    val to_string : ?trim:bool -> run_out -> string result
    (** [to_string] is [(out_string ?trim o |> success)]. *)

    val to_file : Fpath.t -> run_out -> unit result
    (** [to_file f o] is [(out_file f o |> success)] *)

    val to_null : run_out -> unit result
    (** [to_null] is [to_file Fpath.null]. *)

    val run_out : ?err:Fpath.t -> Cmd.t -> run_out
    (** [run_out cmd] represents the standard output of the command run [cmd].
        [std{i,err}] are connected to the invoking prcoess stream and standard
        output can be consumed with {!to_string}, {!to_file}.
        If [err] is specified [stderr] is redirected to the given file. *)
  end
end

(** Measuring time.

    Support to measure monotonic wall-clock {{!monotonic}time} and CPU
    {{!cpu}time}. *)
module Time : sig

  (** {1:monotonic Monotonic wall-clock time} *)

  type span
  (** The type for non-negative monotonic time spans. They represent
      the difference between two monotonic clock readings. *)

  val zero : span
  (** [zero] is a span of 0ns. *)

  val one : span
  (** [one] is a span of 1ns. *)

  val add : span -> span -> span
  (** [add s0 s1] is [s0] + [s1]. {b Warning.} Rolls over on overflow. *)

  val abs_diff : span -> span -> span
  (** [abs_diff s0 s1] is the aboslute difference between [s0] and [s1]. *)

  val to_ns : span -> float
  (** [to_ns s] is [s] in nanoseconds (1e-9). *)

  val to_uint64_ns : span -> int64
  (** [to_uint64_ns s] is [s] as an {e unsigned} 64-bit integer nanosecond
      span. *)

  val of_uint64_ns : int64 -> span
  (** [of_uint64_ns u] is the unsigned 64-bit integer nanosecond span [u]
      as a span. *)

  val pp_span : span Fmt.t
  (** [pp_span ppf s] prints an unspecified representation of [s] on [ppf].
      The representation is not fixed-width, depends on the magnitude of
      [s] and uses locale independent standard time scale abbreviations. *)

  val pp_span_uint_ns : span Fmt.t
  (** [pp_span_uint_ns ppf s] prints [s] as an unsigned 64-bit integer
      nanosecond span. *)

  val compare_span : span -> span -> int
  (** [compare_span s0 s1] orders span by increasing duration. *)

  (** {2:count Monotonic time counters} *)

  type counter
  (** The type for monotonic wall-clock time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting from now on. *)

  val count : counter -> span
  (** [count c] is the monotonic time span elapsed since [c] was created. *)

  (** {1:cpu CPU time} *)

  type cpu
  (** The type for CPU execution time. *)

  val cpu_zero : cpu
  (** [cpu_zero] is zero CPU times. *)

  val cpu_utime_s : cpu -> float
  (** [cpu_utime_s cpu] is [cpu]'s user time in seconds. *)

  val cpu_stime_s : cpu -> float
  (** [cpu_stime_s cpu] is [cpu]'s system time in seconds. *)

  val cpu_children_utime_s : cpu -> float
  (** [cpu_utime_s cpu] is [cpu]'s user time in seconds for children
      processes. *)

  val cpu_children_stime_s : cpu -> float
  (** [cpu_utime_s cpu] is [cpu]'s system time in seconds for children
      processes. *)

  (** {2:cpu_count CPU counters} *)

  type cpu_counter
  (** The type for CPU time counters. *)

  val cpu_counter : unit -> cpu_counter
  (** [cpu_counter ()] is a counter counting from now on. *)

  val cpu_count : cpu_counter -> cpu
  (** [cpu_count c] are CPU times since [c] was created. *)

  (** {1:pp Pretty-printing [float] seconds} *)

  val pp_float_s : float Fmt.t
  (** [pp_span ppf s] prints an unspecified representation of [s] on [ppf].
      The representation is not fixed-width, depends on the magnitude of
      [s] and uses locale independent standard time scale abbreviations. *)
end

(** Hashing. *)
module Hash : sig

  (** {1 Hashes} *)

  type t
  (** The type for hashes. The algorithm is unspecified but should be
      collision resistant. *)

  val zero : t
  (** [zero] is the zero hash, a sequence of null bytes. *)

  val string : string -> t
  (** [string s] is the hash of [s]. *)

  val file : Fpath.t -> t
  (** [file p] is the hash of file path [p].

      {b FIXME} This raises Sys_error. *)

  (**/**)
  (* FIXME remove this *)
  val raw_file : string -> t
  (**/**)

  val to_byte_string : t -> string
  (** [to_byte_string h] is the sequence of bytes of the hash [h]. *)

  val to_hex : t -> string
  (** [to_hex h] is the sequence of bytes of the hash [h] as US-ASCII
      hexadecimal digits. *)

  val of_hex : string -> t option
  (** [of_hex s] parses a sequence of US-ASCII hexadecimal digits from [s] into
      a hash. Guaranteed to succeed if [s] has been produced by {!to_hex}. *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [compare h0 h1] is a totally order [h0] and [h1]. The order is
      compatible with {!equal}. *)

  val pp : t Fmt.t
  (** [pp ppf h] prints an unspecified reprsentation of [h] on [ppf]. *)

  (** {1:setmap Hash map and sets} *)

  module Set : Set.S with type elt = t
  type set = Set.t

  module Map : Map.S with type key = t
  type 'a map = 'a Map.t
end

(** Freshness stamps. *)
module Stamp : sig
  include module type of Hash
  val hash : t -> Hash.t
  val of_hash : Hash.t -> t
end

(** {1:builds Builds} *)

type build
(** The type for builds. See {!Build.t}. *)

(** Build environment.

    The build environment controls build tool lookups and spawn
    processes environments. *)
module Env : sig

  (** {1:build_aim Build aims} *)

  type build_aim = [ `Build_os | `Host_os ]
  (** The type for build aims. Whenever a build unit is built, its
      build outcome may be aimed at either the [`Build_os] or the
      [`Host_os]:

      {ul
      {- [`Build_os] is the OS on which the build tools are executed.}
      {- [`Host_os] is the OS on which the build outcomes are eventually
         hosted and run.}}

      Most of the time the build OS coincides with the host OS. The
      build OS does however differ in cross-compiled builds. In this
      case tool lookup, process environments and configuration needs
      to be distinguished in case the build system is creating build
      outcomes to be used by the build system itself.

      This happens for example if pre-processors or programs
      generating source files are built and used by the build itself.
      If these pre-processors also need to be used on the host OS this
      lead those units being built twice: once for the build OS and
      once for the host OS. Units can {{!Unit.create}specify} that
      they support only one mode of building, this is mainly used for
      programs that are built for the build but are not deployed to
      the host OS. *)

  (** {1:lookup Tool lookups} *)

  type tool_lookup = Fpath.t list -> Fpath.t result
  (** The type for tool lookups. Given a list of tool alternatives
      returns the exectuable of the first one which can be found
      in the environment.

      The expected semantics is similar to a lookup made in a POSIX
      [PATH] environment variable. Notably if a given path has
      {!Fpath.dir_sep} no search should occur and the path be returned
      as is. However this is left to the discretion of the entity that
      setups the environment.

      If no tool can be found the function should return an
      intellligible error message that mentions the given tools and
      where exactly it was looked up. *)

  val pp_tool_alts : Fpath.t list Fmt.t
  (** [pp_tool_alts] can be used as a preamble for reporting failures
      to lookup tools e.g. [R.error_msgf "%a: Not found in ..."
      pp_tool_alts tools]. *)

  val env_tool_lookup : ?sep:string -> ?var:string -> OS.Env.t -> tool_lookup
  (** [env_tool_lookup ~sep ~var env] is a tool lookup that gets
      the value of the [var] variable (defaults to [PATH]) in [env] and
      performs a {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08_03}POSIX PATH variable} search.
      Directories in the search path are separated by [sep] (defaults
      to [";"] if {!Sys.win32} is [true] and [":"] otherwise). *)

  (** {1:env Environments} *)

  type t
  (** The type for build environments. *)

  val v :
    ?build_lookup:tool_lookup -> ?build_forced_env:OS.Env.t ->
    ?build_env:OS.Env.t -> ?host_lookup:tool_lookup ->
    ?host_forced_env:OS.Env.t -> OS.Env.t -> t
  (** [v ~build_lookup ~build_forced_env ~build_env ~host_lookup
         ~host_forced_env host_env] is an environment with:
      {ul
      {- [host_env], the {{!Tool.v}consulted} process environment for tools
         which aim at producing artefacts for the [`Host_os].}
      {- [host_forced_env], an overriding process environment passed to all
         tools which aim at producing artefacts for the [`Host_os] (defaults
         to {!String.Map.empty}).}
      {- [host_lookup], the tool lookup function for tools which aim
          at producing artefacts for the [`Host_os] (defaults to
         [env_tool_lookup host_env]).}
      {- [build_env], the {{!Tool.v}consulted} process environment for tools
         which aim at producing artefacts for the [`Build_os] (defaults
         to [host_env]).}
      {- [build_forced_env], an overriding environment passed to all tools
         which aim at producing artefacts for the [`Build_os] (defaults
         to [host_forced_env]).}
      {- [build_lookup], the tool lookup function for tools which aim
         at producing artefacts for the [`Build_os] (defaults to
         [env_tool_lookup build_env] if [build_env] is defined explicitely
         or host_lookup otherwise).}} *)

  val env : t -> build_aim -> OS.Env.t
  (** [env e aim] is [e]'s {{!Tool.v}consulted} process environment for tools
      runs which aim at producing artefacts for [aim]. *)

  val forced_env : t -> build_aim -> OS.Env.t
  (** [forced_env e aim] is [e]'s forced process environment for tools which
      aim at producing artefacts for [aim]. *)

  val tool : t -> build_aim -> Fpath.t list -> Fpath.t result
  (** [tool e aim tools] is the first file executable which can be found
      among [tools] for runs which aim at producing artefacts for [aim].

      @raise Invalid_argument if [tools] is empty. *)
end

(** Build configuration.

    Typed, persistable, key-value bindings to configure builds. See
    {{!configuration}the manual} for a conceptual introduction.

    {b TODO.} It's unclear whether this is only build configuration
    we might use it for deployement configuration aswell. *)
module Conf : sig

  (** {1:confs Configurations} *)

  (** Configurations. *)
  include Hmap.MAP

  (** {2:persist Persistence} *)

  val load : Fpath.t -> (t * decode_error list) result
  (** [load file] loads a configuration from [file]. *)

  val save : t -> Fpath.t -> encode_error list result
  (** [save c file] saves configuration [c] to [file]. *)

  (** {1:key_values Key value} *)

  type 'a value
  (** The type for key value of type ['a]. *)

  val const : 'a -> 'a value
  (** [const v] is the value [v]. *)

  val discover :
    ?store:bool -> (Env.t -> Env.build_aim -> 'a key -> t -> 'a result) ->
    'a value
  (** [discover ~store f] is the value [f k c] with [c] the
      configuration in which the value must be defined and [k] the
      actual key. If [store] is [true] (default) and the result of the
      invocation is not an error the result of the key discovery will
      be persisted for future runs. If [store] is [false], [f] is
      always executed on {{!value}lookup} unless the key is
      explicitely defined in [c].

      {b FIXME.}
      {ul
      {- Review this simple scheme for allowing keys to depend
         on each other. One of the problem is in bulk set maybe we should
         still have an app functor for that.}
      {- Rather than {!Env.t} and aim as is now we should give it a {!build}
         this allows to support oblivous cross, and we benefit of the
         build cache for unstored discovery.}} *)

  val value : Env.t -> Env.build_aim -> 'a key -> t -> 'a value -> 'a result
  (** [value k c v] determines the value of [v] with respect
      to [k] and [c] (for {!discover}ed). *)

  val value_const : 'a value -> 'a option
  (** [value_const d] is [d]'s constant value (if any). *)

  val value_store : 'a value -> bool
  (** [value_store d] is [true] if the discovered value should be stored.
      This is [false] on {!const}. *)

  val pp_value : 'a Fmt.t -> 'a value Fmt.t
  (** [pp_value k ppf v] prints [v]'s constant value or the
      fact that it is discovered. *)

  (** {1:key_groups Key groups} *)

  (** Key groups.

      Key groups are named sets of related keys. Each key can belong
      to at most one group that is specified a {{!key}key creation}
      time. Key groups offer a convenience for user interaction. *)
  module rec Group : sig

    (** {1 Key groups} *)

    type t
    (** The type for key groups.  *)

    val v : ?loc:Def.loc -> ?doc:string -> string -> t
    (** [v n ~doc] is a key group with unique name [n]
        documented by [doc].

        {b Warning.} Groups must be defined at toplevel and are subject
        to namespacing, {{!descr_values}see details}. *)

    val none : t
    (** [none] is the group attached to keys that do not specify an
        explicit group. Its name is ["none"]. *)

    val keys : t -> Key.t list
    (** [keys g] are the keys that belong to group [g]. *)

    include Def.S with type t := t
  end

  (** {1:keys Keys} *)

  (** Configuration keys. *)
  and Key : sig
    include Hmap.KEY with type 'a typed = 'a key

    val v :
      ?loc:Def.loc -> ?doc:string -> ?group:Group.t -> string -> 'a Conv.t ->
      default:'a value -> 'a typed
    (** See {!Conf.key}. *)

    val default : 'a typed -> 'a value
    (** [default k] is [k]'s default value. *)

    val group : t -> Group.t
    (** [group k] is [k]'s group. *)
  end

  val key :
    ?loc:Def.loc -> ?doc:string -> ?group:Group.t -> string -> 'a Conv.t ->
    default:'a value -> 'a key
  (** [key n conv ~default ~group ~doc] is a configuration key with
      unique name [n] whose value is converted using [conv]. If the
      key is undefined when looked up in a configuration it is
      determined according to [default]. If [group] is provided the
      key belongs to the given [group] (defaults to {!Group.none}).

      {b Warning.} Keys need to be defined at toplevel and are subject to
      namespacing, {{!descr_values}see details}. *)

  val get_default : Env.t -> Env.build_aim -> 'a key -> t -> 'a result
  (** [get_default k c] is [k]'s default value in [c]. *)

  val get_effective :
    Env.t -> Env.build_aim -> 'a key -> t ->
    ([`Env | `Default | `Conf ] * 'a) result
  (** [get_effective env k c] gets the effective value of key [k] in environment
      [env] and configuration [c]. This is the first value determined by the
      following steps:
      {ol
      {- If [B0_C_$KEY] is defined in [env], decode a value using [k]'s
         converter. [$KEY] is [Key.name k] uppercased and with '.' substituted
         by '_'.}
      {- If [k] is defined in [c], use its value.}
      {- Otherwise, use [k]'s default value in [c].}}
      An error is returned if decoding the environment variable fails
      or if the default value discovery errors. *)

  val keys : t -> Key.t list
  (** [keys c] are the keys defined in [c]. *)

  (** {1:presets Presets} *)

  (** Configuration presets.

      A configuration preset is a named set of key-value binding
      definitions. Presets allow users to quickly setup a given sub
      configuration. *)
  module Preset : sig

    (** {1:key_defs Preset value definition} *)

    type conf = t

    val const : 'a -> 'a value
    (** [const] is {!Conf.const}. *)

    val discover :
      ?store:bool -> (Env.t -> Env.build_aim -> 'a key -> t -> 'a result) ->
      'a value
    (** [discover] is {!Conf.discover}. *)

    type def
    (** The type for a preset value definition. *)

    val def : ?doc:string -> 'a key -> 'a value -> def
    (** [def k d] is a preset element that sets [k] to definition [d].

        {b Note.} The [stored] argument of {!Conf.discover} is
        meaningless for presets, as the only way to use a preset's
        value is to {{!add}add} its bindings to a
        configuration. *)

    val def_key : def -> Key.t
    (** [def_key d] is [d]'s key. *)

    val def_doc : def -> string
    (** [def_doc d] is [d]'s documentation string. *)

    type binding = B : 'a key * 'a value -> binding (** *)
    (** The type for preset key-value bindings. *)

    val def_binding : def -> binding
    (** [def_binding d] is [d]'s key-value binding. *)

    (** {1:presets Presets} *)

    type t
    (** The type for configuration presets. *)

    val v : ?loc:Def.loc -> ?doc:string -> string -> def list -> t
    (** [v name ~doc defs] is a preset with unique name [n], key-value
        definitions [defs] an documented by [doc].

        {b Warning.} Presets need to be defined at toplevel and are
        subject to namespacing, {{!descr_values}see details}. *)

    include Def.S with type t := t

    val defs : t -> def list
    (** [defs p] are [p]'s key-value definitions, sorted by key name. *)

    val find_def : string -> t -> def option
    (** [find_def name] finds a definition for a key named [name]. *)

    val get_or_suggest_def : string -> t -> (def, string list) Pervasives.result
    (** [get_or_suggest_def name] is the definition for a key named [name]
        if it exists or a list of suggested key names that could match
        [name]. *)

    val keys : t -> Key.t list
    (** [keys p] are the keys that are preset by [p]. *)

    (** {1:apply Apply and remove} *)

    type def_error = string * [`Msg of string]
    (** The type for preset definition errors, the failed key name and
        the error message. *)

    val add : Env.t -> Env.build_aim -> t -> conf -> conf * def_error list
    (** [add p c] is [(c', errs)] where [c'] is [c] with the bindings
        of [p] added. If a preset value discovery functions the failed
        key name and error message is in [erros] and the binding is left
        unchanged. *)

    val rem : t -> conf -> conf
    (** [rem_preset p c] is [c] with no bindings for [p]'s keys. *)
  end

  val of_preset :
    Env.t -> Env.build_aim -> Preset.t -> t * Preset.def_error list
  (** [of_preset p] is [Preset.add p empty]. *)
end

(** Packages.

    Packages have no fundamental purpose in the build system, they
    are simply a way to collect a set of {{!Unit}build units} under
    a common name and attach arbitrary metadata to it. At the user
    interface level they allow to perform coarse grained action on build
    units. For description developers they can be used to collect unit
    metadata in order to generate packaging and deployement bureaucracy
    and checks. *)
module Pkg : sig

  (** {1:pkg Packages} *)

  (** Heterogeneous value maps for storing package metadata. *)
  module Meta : Hmap.S with type 'a Key.info = unit

  type t
  (** The type for packages. *)

  type id = int
  (** The type for package unique identifiers. {b Warning.} The same
      package can be given different ids in different program runs. *)

  val create : ?loc:Def.loc -> ?doc:string -> ?meta:Meta.t -> string -> t
  (** [create ~doc n b] is a package with name [n] documented by
      [doc].

      {b Warning.} Packges must be defined at toplevel and are subject
      to namespacing, {{!descr_values}see details}. *)

  include Def.S with type t := t

  val basename : t -> string
  (** [basename u] is [u]'s given base name, without the namespacing. *)

  val id : t -> id
  (** [id u] is [u]'s unique identifier for the program run. *)

  val meta : t -> Meta.t
  (** [meta u] is [u]'s metadata. *)

  val meta_mem : 'a Meta.key -> t -> bool
  (** [meta_mem k u] is [true] iff [k] is defined in [u]'s meta. *)

  val meta_find : 'a Meta.key -> t -> 'a option
  (** [meta_find k u] is [k]'s value in [u]'s meta (if any). *)

  val meta_get : 'a Meta.key -> t -> 'a
  (** [meta_get k u] is like {!meta_find} but @raise Invalid_argument
      if [k] is not bound in [u]. *)

  val has_tag : bool Meta.key -> t -> bool
  (** [has_tag k u] is [true] iff [k]'s value in [u]'s meta is [true]. *)

  (** {1:idsetmap Package id maps and sets} *)

  (** Sets of package identifiers. *)
  module Idset : sig
    include Set.S with type elt := id
    val pp : ?sep:unit Fmt.t -> id Fmt.t -> t Fmt.t
  end

  (** Maps of package identifiers. *)
  module Idmap : sig
    include Map.S with type key := id
    val pp : ?sep:unit Fmt.t -> (id * 'a) Fmt.t -> 'a t Fmt.t
  end

  (** {1:setmap Package map and sets} *)

  type set
  (** The type for sets of packages. *)

  (** Package sets. *)
  module Set : sig
    include Set.S with type elt := t
                   and type t := set

    val pp : ?sep:unit Fmt.t -> t Fmt.t -> set Fmt.t
    type t = set
    (** The type for package sets. *)
  end

  type +'a map
  (** The type for maps from units to values of type ['a]. *)

  (** Package maps. *)
  module Map : sig

    (** {1 Package maps} *)

    include Map.S with type key := t
                   and type 'a t := 'a map

    val of_list : (t * 'a) list -> 'a map
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    val pp : ?sep:unit Fmt.t -> (t * 'a) Fmt.t -> 'a map Fmt.t

    type 'a t = 'a map
    (** The type for package maps. *)

    val dom : 'a map -> set
    (** [dom m] is the domain of [m]. *)
  end
end

(** Build units.

    Builds are organized into statically known and named {e units} which
    allows coarse grained action on builds. *)
module Unit : sig

  (** {1:units Units} *)

  (** Heterogeneous value maps for storing unit metadata. *)
  module Meta : Hmap.S with type 'a Key.info = unit

  type t
  (** The type for build units. *)

  type id = int
  (** The type for build unit unique identifiers. {b Warning.} The same
      unit can be given different ids in different program runs. *)

  val create :
    ?loc:Def.loc ->
    ?src_root:Fpath.t ->
    ?doc:string ->
    ?doc_outcome:string ->
    ?only_aim:Env.build_aim ->
    ?pkg:Pkg.t ->
    ?meta:Meta.t ->
    string -> (build -> unit) -> t
  (** [create ~doc n b] is a build unit with unique name [n] build
      function [b] and documented by [doc] and which logs
      {!doc_outcome} once built.

      [src_root] should only be defined if you are not using
      description files it determines the root of sources for the
      unit.

      By default build units can be built for either {!Env.build_aim},
      [only_aim] allows to restrict this. It is mainly used with
      [`Build_os] for units that build private internal tools used by
      the build itself.

      {b Warning.} Units must be defined at toplevel and are subject
      to namespacing, {{!descr_values}see details}. *)

  include Def.S with type t := t

  val func : t -> (build -> unit)
  (** [func u] is [u]'s build function. *)

  val src_root : t -> Fpath.t
  (** [src_root u] is the root of sources for the units. It determines
      the value of {!Build.src_dir} when the unit is being built. *)

  val basename : t -> string
  (** [basename u] is [u]'s given base name, without the namespacing. *)

  val doc_outcome : t -> string
  (** [doc_outcome u] is a string logged when the unit builds successfully. *)

  val id : t -> id
  (** [id u] is [u]'s unique identifier for the program run. *)

  val pkg : t -> Pkg.t option
  (** [pkg u] is [u]'s package (if any). *)

  val meta : t -> Meta.t
  (** [meta u] is [u]'s metadata. *)

  val meta_mem : 'a Meta.key -> t -> bool
  (** [meta_mem k u] is [true] iff [k] is defined in [u]'s meta. *)

  val meta_add : 'a Meta.key -> 'a -> t -> unit
  (** [meta_add k v u] binds [k] to [v] in [u]'s meta. *)

  val meta_find : 'a Meta.key -> t -> 'a option
  (** [meta_find k u] is [k]'s value in [u]'s meta (if any). *)

  val meta_get : 'a Meta.key -> t -> 'a
  (** [meta_get k u] is like {!meta_find} but @raise Invalid_argument
      if [k] is not bound in [u]. *)

  val has_tag : bool Meta.key -> t -> bool
  (** [has_tag k u] is [true] iff [k]'s value in [u]'s meta is [true]. *)

  (** {1:idsetmap Unit id map and sets} *)

  (** Sets of unit identifiers. *)
  module Idset : sig
    include Set.S with type elt := id
    val pp : ?sep:unit Fmt.t -> id Fmt.t -> t Fmt.t
  end

  (** Maps of unit identifiers. *)
  module Idmap : sig
    include Map.S with type key := id
    val pp : ?sep:unit Fmt.t -> (id * 'a) Fmt.t -> 'a t Fmt.t
  end

  (** {1:setmap Unit map and sets} *)

  type set
  (** The type for sets of units. *)

  (** Unit sets. *)
  module Set : sig
    include Set.S with type elt := t
                   and type t := set

    val pp : ?sep:unit Fmt.t -> t Fmt.t -> set Fmt.t
    type t = set
    (** The type for sets. *)
  end

  type +'a map
  (** The type for maps from units to values of type ['a]. *)

  (** Unit maps. *)
  module Map : sig

    (** {1 Unit maps} *)

    include Map.S with type key := t
                   and type 'a t := 'a map

    val of_list : (t * 'a) list -> 'a map
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    val pp : ?sep:unit Fmt.t -> (t * 'a) Fmt.t -> 'a map Fmt.t

    type 'a t = 'a map
    (** The type for unit maps. *)

    val dom : 'a map -> set
    (** [dom m] is the domain of [m]. *)
  end
end

(** Build tools.

    Define command line tools used by the build.

    In order to improve build reproduciblity [B0] runs command line
    tools with minimal process environments. As such tools need to
    declare the environment variables they want to access. Only these
    variables are automatically passed to the tool, provided they are
    present in the build's environment.

    The final environment given to a tool on a {{!Build.spawn}build spawn}
    is the value of these variables in the build's
    {{!Env.env}environment}, overriden by the variables specified
    during the {{!Build.spawn}spawn} operation overriden by the
    build's {{!Env.forced_env}forced environment}. *)
module Tool : sig

  (** {1:env_vars Environment variables} *)

  type env_vars = string list
  (** The type for lists of environment variable names. *)

  val tmp_vars : env_vars
  (** [tmp_vars] is [["TMPDIR"; "TEMP"]]. *)

  (** {1:tools Tools} *)

  type t
  (** The type for tools. *)

  val v : ?internal:env_vars -> ?env_vars:env_vars -> string -> t
  (** [v ~internal ~env_vars name] is a tool looked up in the build's
      search path as [name]. [env_vars] are the environment variables
      read from the build's environment and that affect the tool's
      outputs (defaults to [[]]). [internal] are the environment
      variables that are read from the build's environment and that do
      not affect the tool's output (defaults to {!tmp_vars}).

      {b WARNING Windows users.} Never add the [.exe] extension to the
      tool name, this will be done by the {!Env.tool_lookup} function as
      needed.

      @raise Invalid_argument if [name] contains {!Fpath.dir_sep}, use
      {!of_file} to specify tools as file paths. *)

  val of_file : ?internal:env_vars -> ?env_vars:env_vars -> Fpath.t -> t
  (** [of_file ~internal ~env_vars file] is like {!v} but if
      [file] is:
      {ul
      {- A relative file path with a single segment, the tool is looked
         up in the build's search path like in {!v}}
      {- Otherwise, the tool is the corresponding file.}} *)

  val of_unit_file :
    Unit.t -> ?internal:env_vars -> ?env_vars:env_vars -> Fpath.t -> t
  (** [of_unit_file ~internal_env_vars ~env_vars file] is like {!of_file}
      but the given relative [file] is located in the build directory
      of {!Unit}. Build spawns that need this tool automatically
      build the unit in [`Build_os] mode and make sure to add
      the tool to the read set of the spawn.

      {b FIXME.} This feels ad-hoc maybe better primitives are needed
      in Build.t and then {!of_file} can be used, this can then be hidden
      in a larger combinator. *)

  (** {1:conf Tool configuration keys} *)

  val key :
    ?loc:Def.loc -> ?doc:string -> ?group:Conf.Group.t ->
    ?internal:env_vars -> ?env_vars:env_vars -> ?tools:Fpath.t list ->
    string -> t Conf.key
  (** [key ~internal ~env_vars ~tools name] is a command line tool and
      configuration key named [name], and that can manifest itself as
      one of [tools] (defaults to [[Fpath.v name]]). The first match
      is taken, files are looked up like in {!of_file} and [None] is
      returned if none can be found. *)
end

(** Build outcomes.

    A build outcome holds information about a finished build. In particular
    remembers all the build operation and their metadata.

    {b TODO} This should be revamped a bit. *)
module Outcome : sig

  (** {1:outcomes Outcomes} *)

  type t
  (** The type for build outcomes.  *)

  val read : Fpath.t -> t result
  (** [read file] reads a build outcome from [file]. *)

  val write : Fpath.t -> t -> unit result
  (** [write file o] writes the build outcome [o] to file [file]. *)

  val fpath_meta : t -> Fpath.Meta_map.t
  (** [fpath_meta o] is the file metadata for the build. *)

  val conf : t -> Conf.t
  (** [conf o] is the effective configuration used by the build. *)

  val unit_names : t -> string list
  (** [unit_names o] are the names of the units that were built. *)

  val unit_id_name_map : t -> string Unit.Idmap.t
  (** [unit_id_name_map o] maps identifiers of units built in [o] to their
      name. *)

  val unit_id_set : string list -> t -> Unit.Idset.t
  (** [unit_id_set names o] is the set of unit identifers that matches
      names in [o]. *)

  val root_files : t -> Fpath.set
  (** [root_file o] is the set of root files in [o]. *)

  val built_files : t -> Fpath.set
  (** [built_files o] is the of files that are written in [o]. *)

  (** {1:ops Build operations} *)

  (** Build operations *)
  module Op : sig

    (** {1:op Operations} *)

    type id = int
    (** The type for build operation identifiers. *)

    type t
    (** The type for build operations. *)

    val id : t -> id
    (** [id o] is the identifier of operation [o]. *)

    val unit_id : t -> Unit.id
    (** [unit_id o] is the identifier of the unit in which the operation was
        submitted. {b Warning} This should be used to look up units defined
        in the current program run. But only matched against unit metadata
        present in the build outcome itself, see {!unit_id_name_map}. *)

    val aim : t -> Env.build_aim
    (** [aim o] is the build target for which the operation was submitted. *)

    val reads : t -> Fpath.set
    (** [reads o] are the file paths read by operation. *)

    val writes : t -> Fpath.set
    (** [writes o] writes are the file paths written by [o]. *)

    val creation_time : t -> Time.span
    (** [creation_time o] is [o]'s monotonic creation time. *)

    val exec_start_time : t -> Time.span
    (** [exec_start_time o] is [o]'s monotonic operating system starting
        execution time. This is different from {!Time.zero} once the
        operation has been submitted to the OS for execution. *)

    val exec_end_time : t -> Time.span
    (** [exec_end_time o] is [o]'s monotonic time when the operation's has
        been processed by the operating system. This is different from
        {!Time.zero} once the operation has been completed by the OS
        and collected and processed into [Executed] status by the build
        program. *)

    val cached : t -> bool
    (** [cached o] is [true] if the result of the operation was looked up
        in the cache. *)

    type status =
      | Guarded
      | Ready
      | Executed
      | Finished (** *)
      (** The type for operation statuses.
          {ul
          {- [Guarded] the operation is guarded from execution (initial state).}
          {- [Ready] the operation is ready to be submitted for execution.}
          {- [Executed] is [true] iff [o] has been executed by the OS
             (but it may not have succeded), see the individual operation
             results.}
          {- [Finished] the effects of the operation have been applied.}} *)

    val status : t -> status
    (** [status o] is [o]'s status. *)

    val pp_status : status Fmt.t
    (** [pp_status] is a formatter for statuses. *)

    val stamp : t -> Stamp.t
    (** [stamp o] is [o]'s stamp. *)

    (** {1:spawn Process spawns} *)

    type spawn_pid = int
    (** The type for OS specific process identifiers. *)

    type spawn_stdo = [ `Ui | `File of Fpath.t | `Tee of Fpath.t ]
    (** The type for spawn standard output redirections. *)

    type spawn_stdo_ui =
      [ `Tmp_file of Fpath.t | `Stdo of string result | `None ]
    (** The type for spawn standard output [`Ui] redirection result. When
        submitted this becomes [`Tmp_file], once read back this is [`Stdo]. *)

    type spawn_env = string array
    (** The type for spawn process environments. *)

    type spawn_success_codes = int list option
    (** The list of exit codes that indicates success. If this is [None]
        only zero is success. If the list is empty this any exit code. *)

    type spawn
    (** The type for process spawn operations. *)

    val spawn_cmd : spawn -> Cmd.t
    (** [spawn_cmd s] is [s]'s invocation. *)

    val spawn_env : spawn -> spawn_env
    (** [spawn_env s] is the environment in which [s] was run. *)

    val spawn_cwd : spawn -> Fpath.t
    (** [spawn_cwd s] is the current working directory in which [s] was run. *)

    val spawn_stdin : spawn -> Fpath.t option
    (** [spawn_stdin s] is file where stdin was read from for [s] (if any). *)

    val spawn_stdout : spawn -> spawn_stdo
    (** [spawn_stdout s] is destination to which stdout was written for [s]. *)

    val spawn_stderr : spawn -> spawn_stdo
    (** [spawn_stderr s] is destination to which stderr was written for [s]. *)

    val spawn_success_codes : spawn -> int list option
    (** [spawn_success_codes s] is the list of exit codes denoting success
        for the oparation. *)

    val spawn_stdo_ui : spawn -> spawn_stdo_ui
    (** [spawn_stdo_ui s] is the [`Ui] redirection result of [s]. *)

    val set_spawn_stdo_ui : spawn -> spawn_stdo_ui -> unit
    (** [set_spawn_stdo_ui s st] sets the [`Ui] redirection result to
        [st]. *)

    val spawn_result : spawn -> (spawn_pid * OS.Cmd.status) result
    (** [spawn_result s] is [s]'s OS completion result or an error. *)

    (** {1:read File reads} *)

    type read
    (** The type for file read operations. *)

    val read_file : read -> Fpath.t
    (** [read_file r] is [r]'s read file. *)

    val read_result : read -> string result
    (** [read_result r] is [r]'s result. Either the read data or an
    error. *)

    (** {1:write File writes} *)

    type write
    (** The type for (atomic) file write operations. *)

    val write :
      Unit.t -> Env.build_aim -> Time.span -> reads:Fpath.set ->
      Fpath.t -> t
    (** [write u f], when submitted, starts to atomically set the
        contents of [f] to [write_data w]. *)

    val write_file : write -> Fpath.t
    (** [write_file w] is [w]'s written file. *)

    val write_data : write -> string
    (** [write_data w] is [w]'s written data. *)

    val set_write_data : write -> string -> unit
    (** [write_data w] sets [w]'s written data to [w]. *)

    val write_result : write -> unit result
    (** [write_result w] is [w]'s result. Either unit or an error. *)

    (** {1:copyfile File copies} *)

    type copy_file
    (** The type for file copies. *)

    val copy_file_src : copy_file -> Fpath.t
    (** [copy_file_src c] is [c]'s source file. *)

    val copy_file_dst : copy_file -> Fpath.t
    (** [copy_file_dst c] is [c]'s destination file. *)

    val copy_file_linenum : copy_file -> int option
    (** [copy_file_linenum c]  is [c]'s line number directive value (if any). *)

    val copy_file_result : copy_file -> unit result
    (** [copy_file_result c] is [c]'s result. Either the unit or an error. *)

    (** {1:delete File deletions} *)

    type delete
    (** The type for file deletions. *)

    val delete_file : delete -> Fpath.t
    (** [delete_file d] is [d]'s deleted file. *)

    val delete_result : delete -> unit result
    (** [delete_result d] is [d]'s result. *)

    (** {1:mkdir Directory creation} *)

    type mkdir
    (** The type for directory creation operations. *)

    val mkdir_dir : mkdir -> Fpath.t
    (** [mkdir_dir mk] is [mk]'s created directory. *)

    val mkdir_result : mkdir -> unit result
    (** [mkdir_result mk] is [mk]'s result. *)

    (** {1:unit Unit sychronisation} *)

    type sync
    (** The type for unit synchronisation. *)

    val sync_units : sync -> Unit.Idset.t
    (** [sync_units s] is the units on which [s] synchronizes. *)

    (** {1:kind Operation kinds} *)

    type kind =
      | Spawn of spawn
      | Read of read
      | Write of write
      | Copy_file of copy_file
      | Delete of delete
      | Mkdir of mkdir
      | Sync of sync (** *)
    (** The type for asynchronous operation kinds. *)

    val kind : t -> kind
    (** [kind o] is [o]'s kind. *)

    (** {1:preds Predicates} *)

    val cycle : t -> t -> (Fpath.t * Fpath.t) option
    (** [cycle o0 o1] is [Some (r0, r1)] with [r0] a file read by [o0]
        and written by [o1] and [r1] a file read by [o1] and written by
        [o0] or [None] if there no such two files. *)

    val equal : t -> t -> bool
    (** [equal o0 o1] is [true] iff [o0] and [o1] are the same operation. *)

    val compare : t -> t -> int
    (** [compare o0 o1] is a total order on operation compatible with
        {!equal}. *)

    val compare_exec_start_time : t -> t -> int
    (** [compare o0 o1] is a total order on operations according to their
        {!exec_start_time}. *)

    (** {1:pretty Pretty printing} *)

    val pp : t Fmt.t
    val dump : t Fmt.t
    val pp_spawn_fail : t Fmt.t
    val pp_log_line : t Fmt.t
    val pp_long : t Fmt.t
    (** {1:setmap Sets and maps} *)

    type set
    (** The type for sets of operations. *)

    (** Sets of operations. *)
    module Set : sig
      include Set.S with type elt := t
                     and type t = set
    end

    type +'a map
    (** The type for operation maps. *)

    (** Operation maps. *)
    module Map : sig
      include Map.S with type key := t
                     and type 'a t := 'a map
      val dom : 'a map -> set
      val of_list : (t * 'a) list -> 'a map
      type 'a t = 'a map
    end
  end

  val ops : t -> Op.t list
  (** [ops o] are the build's operations. *)

  val ops_to_json : t -> Op.t list -> string
  (** [ops_to_json o ops] outputs JSON text in
      {{:https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview}Trace Event Format}.

      {b TODO} This should be moved somewhere else so that we can
      get rid of {!Json} in the core library. *)

  (** {1:stats Run statistics} *)

(*
    val age : outcome -> int
    (** [age o] is the build age of [o]. *)
*)

(*
    val op_count : outcome -> int
    (** [op_count o] is the number of build operations performed
        (cached or not). *)

    val prev_op_count : outcome -> int
    (** [prev_op_count] is the {!op_count} of the previous age. *)

    val duration : outcome -> int64
    (** [duration o] is the monotonic wall-clock time span it took
        between the build {!init} and {!finish} as an unsigned [int64]
        number. *)

    val prev_duration : outcome -> int64
    (** [prev_duration] is the {!duration} of the previous age. *)

*)
  val pp_stats : t Fmt.t
  (** [pp_stats ppf o] prints [o]'s statistics on [ppf]. *)
end

(** Build cache. *)
module Cache : sig

(**/**)

  (* TODO remove *)

  (** {1:keys Cache keys} *)

  type key = Hash.t
  (** The type for cache keys, stamp of the build operation that writes the
      file. *)

  val key_of_string : string -> key result
  (** [key_of_string s] parses a key from [s]. *)

  val key_to_string : key -> string
  (** [key_to_string k] is [k] in human readable from and parseable
      by {!key_of_string}. *)

  val pp_key : key Fmt.t
  (** [pp_key ppf k] prints an unspecified representation of [k] on ppf. *)

  (** {1:elts Cache elements} *)

  type elt
  (** The type for information about files stored in the cache index. *)

  val elt :
    variant:string -> age:int -> op:Cmd.t -> key:Hash.t ->
    Fpath.t -> file_stamp:Stamp.t -> elt

  val elt_variant : elt -> string
  val elt_age : elt -> int
  val elt_op : elt -> Cmd.t
  val elt_key : elt -> key
  val elt_file_path : elt -> Fpath.t
  val elt_file_stamp : elt -> Stamp.t
(**/**)

  (** {1:cache Cache} *)

  type t
  (** The type for caches. *)

  val empty : index_file:Fpath.t -> dir:Fpath.t -> t
  (** [empty ~index_file ~dir] is an empty cache with index [index] and
      directory [dir]. The file system is left untouched, in particular [dir]
      is not created, see also {!load}. *)

  val is_empty : t -> bool
  (** [is_empty c] is [true] iff [c] is empty. *)

  val dir : t -> Fpath.t
  (** [dir c] is [c]'s directory. *)

  val index_file : t -> Fpath.t
  (** [index_file c] is [c]'s index file. *)

  (** {1:persist Persist} *)

  val exists : index_file:Fpath.t -> bool result
  (** [exists ~index_file ~dir] is [true] if [index_file] exists. *)

  val load : index_file:Fpath.t -> dir:Fpath.t -> t result
  (** [load ~index_file ~dir] loads a cache from [index_file] and
      [dir]. If [index_file] doesn't exist an {!empty} index is
      created (but [index_file] is not written yet). If [dir] doesn't
      exist it is created. *)

  val save : t -> unit result
  (** [save t] saves the cache. *)

(**/**)

  (* TODO remove *)

  (** {1:ops Operations} *)

  val mem : t -> key -> bool
  (** [mem c k] is [true] iff an element keyed by [k] exists in [c]'s index. *)

  val add : t -> elt -> unit result
  (** [add c elt] adds element [elt] to [c]. {!elt_file_path}[ elt] must
      exist on disk and is hard linked to a file in the cache named
      after {!elt_key}[ elt]. *)

  val rem : t -> key -> unit result
  (** [rem c k] removes the element keyed by [k] and its associated file
      from the cache. Does nothing if [k] is not in [c]'s index. *)

  val find : t -> key -> elt option
  (** [find c k] is the element keyed by [k] in [c]'s index. *)

  val use : t -> key -> bool result
  (** [use c k] uses the element keyed by [k]. If [k] is in the cache index
      this hard links the cache file to the element's path and returns
      [true] if [k] is not in the cache index nothing happens and [false] is
      returned. *)

  val verify :
    repair:bool -> t -> key ->
    [ `Ok | `Miss_index | `Miss_file | `Stamp_mismatch | `Unknown ] result
  (** [verify ~repair c k] checks [k] is in the cache index and directory
      and that the file stamp matches. [`Unknown] is returned if [key] is
      neither in the index nor in the directory. If [repair] is [true]
      the cache is changed as follows before the call returns:
      {ul
      {- [`Miss_file], [k]'s index entry was dropped from the cache.}
      {- [`Stamp_mismatch], [k]'s index entry stamp was udpated to match
         the cached filed stamp.}
      {- Otherwise the cache is left unchanged.}} *)

  val foreign : ignore_keys:bool -> t -> ([`Key | `Other] * Fpath.t) list result
  (** [unknown ~ignore_keys c] looks for files in [c]'s cache directory that are
      unknown to the cache. If [ignore_keys] is [true] files unknown to the
      cache but whose filename are keys are not listed as foreign. *)

  (** {1:traverse Traverse} *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f c acc] starting with [acc] folds [f] over the elements of [c]. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f c] iters [f] on the elements of [c]. *)

  val path_map : t -> elt Fpath.Map.t
  (** [path_map t] indexes the file paths of elements. *)

(**/**)
end

(** Builds

    This module is all you need to define your own build functions.
    It allows to access {{!dirs}directories and files}, consult
    the {{!conf}build configuration}, submit {{!ops} build operations}
    and read and write {{!fpathmeta}file path metadata}. *)
module Build : sig

  (** {1:builds Builds} *)

  type t = build
  (** The type for builds. A build gathers the build environment,
      configuration, cache and schedules the build operations. *)

  type run
  (** The type for tool runs. *)

  type 'a fiber = ('a -> unit) -> unit
  (** The type for build operation fibers. *)

  (** {1:dirs Directories and files} *)

  val build_dir : build -> Fpath.t
  (** [build_dir b] is the path to [b]'s current, unit-specific, build
      directory. This is where you should produce your build
      artefacts. *)

  val build_file : build -> string -> Fpath.t
  (** [build_file b f] is [Fpath.(build_dir b / f)]. *)

  val src_dir : build -> Fpath.t
  (** [src_dir b] is the path to [b]'s current source root, where
      non-built (source) files are expected too be looked up. If you
      are using [b0]'s description files this is the directory of the
      [B0.ml] file in which the unit is described. *)

  val unit_build_dir : build -> Unit.t -> Fpath.t
  (** [unit_build_dir b u] is the build directory of unit [u]. This
      should only be used to lookup files in other build units.

      {b FIXME.} Do the build unit request API. *)

  (** {1:conf Tool and configuration lookup} *)

  val conf : build -> 'a Conf.key -> 'a
  (** [conf b k] is [k]'s effective value for the build. *)

  val tool : build -> Tool.t -> Cmd.t -> run
  (** [tool b t args] is [t]'s tool for the build is a run for
      the tool [t] with [args].

      {b Warning.} The resulting run must only be used to
      {!spawn} with [b]. *)

  val conf_tool : build -> Tool.t Conf.key -> Cmd.t -> run
  (** [conf_tool b k] is tool b (conf b k). *)

  (** {1:ops Build operations}

      {b TODO} Explain that basically build operations read and write
      files. For an operation to be executed all the files it reads need to be
      {e ready}. A file becomes ready either if it has been {{!ready}declared}
      as such or if it has been written by a build operation. *)

  (** {2:op_roots Build roots}

      Build roots are non-built files, typically sources or external
      libraries. In order to read them in a build operation
      (i.e. introduce a dependency on them) you need to declare them
      as being {!ready}, otherwise the operation waits for it to be
      produced and fails if that doesn't eventually happen. *)

  val ready : build -> Fpath.t -> unit
  (** [ready b p] declares path [p] to be ready in [b]. This
      is typically used with sources files and files external
      to the build (e.g. intalled libraries). See also {!src}. *)

  val src : build -> Fpath.t -> Fpath.t
  (** [src b src] is:
{[
let src = Fpath.(src_dir b // src) in
ready b src; src
]} *)

  (** {2:op_spawn Tool spawns}

      Tool spawns are asynchronous and can be issued in any order.
      Actual execution occurs once the all files the files that are
      declared to be read by the spawn are ready. Produce spawn
      operations as soon as possible to maximize parallelism. *)

  type stdo = [ `Ui | `File of Fpath.t | `Tee of Fpath.t ]
  (** The type for standard output redirections.
      {ul
      {- [`Ui] redirects the output to the user interface of the
         build system (usually only actually shown in case of failure).
         Outputs are always first redirected to a file
         and read back by the program running the build on completion,
         this means that in the spawned program isatty will be [false]
         on the fds.}
      {- [`File p] redirect from/to file path [p].}
      {- [`Tee p], for outputs perform both [`Ui] and [`File p].}} *)

  val spawn :
    build -> ?reads:Fpath.t list -> ?writes:Fpath.t list ->
    ?success:int list -> ?env:OS.Env.t -> ?cwd:Fpath.t -> ?stdin:Fpath.t ->
    ?stdout:stdo -> ?stderr:stdo -> run -> unit
  (** [spawn b ~reads ~writes ~success ~env ~cwd ~stdin ~stdout ~stderr
      run] spawns [run] once [reads] files are ready and makes files
      [writes] ready if the spawn succeeds and the file exists. The rest
      of the arguments are:
      {ul
      {- [stdin] reads input from the given file. If unspecified reads
         from the standard input of the program running the build.  {b
         Warning.} The file is not automatically added to [reads],
         this allows for example to use {!OS.File.null}.}
      {- [stdout] and [stderr], the redirections for the standard
         outputs of the command, see {!stdo}. {b Warning.} File redirections
         are not automatically added to [writes]; this allows for example
         to use {!OS.File.null}.}
      {- [success] the exit codes that determine if the build operation
         is successful (defaults to [0], use [[]] to always succeed)}
      {- [env], environment variables added to the build environment.
         This overrides environment variables read by the tool in the
         build environment except forced one. It also allows to
         specify environment that may not be mentioned by running tool's
         {{!Tool.v}environment specification}.}
      {- [cwd] the current working directory. Default is {!dir}. In
         general it's better to avoid using relative file paths and
         tweaking the [cwd]. Construct your paths using the absolute
         {{!dirs}directory functions} and make your invocations
         independent from the [cwd].}}

      {b TODO} This is already quite complete, however the following
      could be added (maybe through another primitive):
      {ol
      {- Support to refine the read and write set after the operation
         returns.}
      {- Support to write a stamp if the tool spawn doesn't write
         any file but needs to be synchronized with another op.}
      {- Support to call a fiber once the operation as completed.}} *)

  (** {2:files File operations} *)

  val read : build -> Fpath.t -> string fiber
  (** [read b file k] reads the contents of [file] when it becomes
      ready and invokes [k s]. *)

  val write :
    build -> ?reads:Fpath.t list -> Fpath.t -> (unit -> string) -> unit
  (** [write b ~reads f gen] once [reads] are ready writes [gen ()] to [f]
      assuming the result depends only on [reads]. This means that if [reads]
      do not change and [f] was already computed [k] is not invoked. If
      [reads] is empty (default) [gen] is invoked on every run.

      {b FIXME} Not implemented. *)

  val copy_file : ?linenum:int -> build -> Fpath.t -> Fpath.t -> unit
  (** [copy_file ?linenum src dst], copies [src] to [dst]. If
      [linenum] is specified the following line number directive
      is prepended to the contents of [src] in [dst]:
{[
#line $(linenum) "$(src)"
]}
     with [$(linenum)] and [$(src)] substituted with the values of
     [linenum] and [src]. *)

  val mkdir : build -> Fpath.t -> unit fiber
  (** [mkdir b d k] creates [dir] and proceeds with [k].
      {b Note.} No file is being made ready.
*)

  (** {2:op_units Unit synchronisation} *)

  val await_units : build -> Unit.t list -> unit fiber
  (** [await_units b us k] waits for the build of [us] to be finished before
      proceeding with [k us].

      {b TODO.} [request_unit] API. *)

  (** {2:op_fail Failing operations} *)

  val fail :
    ((('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a) -> unit) ->
    'b
  (** [fail (fun m -> m fmt ...)] fails the current build unit with the
      formatted message [m]. *)

  val fail_on_error_msg : 'a result -> 'a
  (** [fail_on_error_msg r] is:
      {ul
      {- [v], if [r] is [Ok v].}
      {- [fail (fun m -> m "%s" e)], if [r] is [Error (`Msg e)].}} *)

  (** {1:fpathmeta File path metadata} *)

  val find_path_meta : build -> Fpath.t -> 'a Fpath.Meta.key -> 'a option
  (** [find_path_meta b f k] is [f]'s meta binding for [k] in build [b]
      (if any). *)

  val add_path_meta :
    ?force:bool -> build -> Fpath.t -> 'a Fpath.Meta.key -> 'a -> unit
  (** [add_path_meta ~force b f k v] sets [f]'s meta binding for [k] to [v].
      Unless [force] is [true] (defaults to [false]) the built unit fails
      if [k] is already bound for [f]. *)

  (** {1:units Units} *)

  val unit : build -> Unit.t
  (** [unit b] is the unit being built. *)

  val units : build -> Unit.t list
  (** [units b] are the unit that are being built. *)


  (** {1 Creating and running builds}

      {b Note.} Unless you are using B0 as a library or creating your
      own driver you need not be concered by this. TODO maybe
      move that to a private sub-module. *)

  type ctrl
  (** The type for build controls. Controls how the build is performed. *)

  val ctrl : ?tty_cap:Tty.cap -> ?max_spawn:int -> unit -> ctrl
  (** [ctrl ~max_spawn] controls a build as follows:
      {ul
      {- [max_spawn] is the maximal number of commands that are
         spawned concurrently. Defaults to [1].}
      {- [tty_cap] is the capabilities of the output terminal. Defaults
         to [Tty.None].}} *)

  val create :
    ?prev_outcome:Outcome.t -> Cache.t -> ctrl -> Env.t -> Conf.t ->
    Fpath.Meta_map.t -> dir:Fpath.t -> universe:Unit.t list -> Unit.t list ->
    build result
  (** [init cache ctrl ~dir] initializes a new build using [dir] as the build
      directory.

      {b WARNING.} Anything can happen in that directory do not use
      a [dir] that contains data you might want to keep. *)

  val dir : build -> Fpath.t
  (** [dir b] is the absolute path to [b]'s build directory. It is unlikely
      you will have to use this. *)

  val cache : build -> Cache.t
  val stored_conf : build -> Conf.t
  val outcome : build -> Outcome.t
  (** [outcome b] is [b]'s outcome.

      @raise Invalid_argument if [b] is not {{!finish}finished}. *)

  (** {1:running Running the build} *)

  val start : build -> unit
  (** [start b] starts the build. *)

  val finish : build -> unit result
  (** [finish b] finishes the build [b]. *)
end

(** {1 Organizing and deploying builds}

    These concepts are not core to the build system they allow to
    organize multiple, independent builds and their deployment. *)

(** Build variants.

    Build variants define a configuration, build directory and environment
    to run a build.

    The environment of a build variant is defined via a variant
    {{!Scheme}scheme} which is responsible for setting up and
    destroying the environment. *)
module Variant : sig

  (** {1:variant Variant} *)

  (** Variant schemes.

      A variant scheme sets up the environment for a build
      {{!Variant}variant}. *)
  module Scheme : sig

    type t
    (** The type for variant schemes. *)

    (** {1:direct Direct schemes} *)

    type trigger = [ `Before | `After ]
    (** The type for event triggers. *)

    type direct
    (** The type for direct variant schemes. These schemes
        can only run on the same machine as the build program. *)

    val direct :
      ?preset:Conf.Preset.t ->
      ?setup:(Conf.t -> unit result) ->
      ?delete:(Conf.t -> unit result) ->
      ?env:(unit -> Env.t result) ->
      ?build:(trigger -> Build.t -> unit result) ->
      ?stage:(trigger -> Outcome.t -> unit result) -> unit -> direct
    (** [direct ~preset ~setup ~build ~stage ~delete ()] is direct
        variant which applies preset [preset] to the empty
        configuration on variant creation, uses [setup] to setup the
        variant, [build] before and after building in the variant,
        [stage] before and after executing a program in the variant's
        deployments install environment [post_build] after building in
        the variant, and [delete] when the variant is deleted. *)

    val direct_preset : direct -> Conf.Preset.t option
    (** [direct_preset d] is [d]'s direct preset. *)

    val direct_env : direct -> unit -> Env.t result
    (** [direct_env d] is [d]'s build environment setup
        function. *)

    (** {1:proxy Proxy schemes} *)

    type proxy_conf
    (** The type for proxy configuration. *)

    val proxy_conf :
      root_dir:Fpath.t -> b0_dir:Fpath.t -> variant_dir:Fpath.t ->
      unit -> proxy_conf

    val proxy_conf_root_dir : proxy_conf -> Fpath.t
    val proxy_conf_b0_dir : proxy_conf -> Fpath.t
    val proxy_conf_variant_dir : proxy_conf -> Fpath.t

    type proxy
    (** The type for proxy variant schemes. These schemes forward the
        build command to a variant scheme in another context. *)

    val proxy :
      ?create:(proxy_conf -> unit result) ->
      ?setup:(proxy_conf -> unit result) ->
      ?delete:(proxy_conf -> unit result) ->
      run:(proxy_conf -> Cmd.t -> OS.Cmd.status result) -> t -> proxy
    (** [proxy ?create ?setup ?delete run scheme] is a proxy scheme
        variant using [run] to proxy the given build program command
        to build with variant [scheme].

        TODO semantics of setup should be to put in a workable state
        if it's not. Basically this should do whathever happens in
        create assuming it failed or a condition changed. In general
        [run] should check conditions an invoke run if it's not satisfied. *)

    val proxy_run : proxy -> proxy_conf -> Cmd.t -> OS.Cmd.status result
    (** [proxy_run p cmd] is the exit status of the run of the build
        command [cmd] via the proxy [p]. *)

    val proxy_create : proxy -> proxy_conf -> unit result
    val proxy_delete : proxy -> proxy_conf -> unit result
    val proxy_setup : proxy -> proxy_conf -> unit result

    (** {1:scheme Schemes} *)

    type kind = [ `Direct of direct | `Proxy of proxy  ]
    (** The kind of variant scheme.
        {ul
        {- [ `Direct direct], the build is performed using the build program
           that was launched by the user.}
        {- [`Proxy t], the build is forwarded to another build program
           {{!proxy}More details}.}} *)

    val v : ?loc:Def.loc -> ?doc:string -> string -> kind -> t
    (** [v ?src ?doc name kind] is the variant scheme named [name]
        with kind [kind].

        {b Warning.} Schemes need to be defined at toplevel and are
        subject to namespacing, {{!descr_values}see details}. *)

    val kind : t -> kind
    (** [kind s] is [s]'s scheme kind. *)

    val nop : t
    (** The [nop] scheme. The nop scheme does nothing, it is a nop on
        all amost every acccounts. It runs builds in the environment
        in which the [b0] tool itself is run. *)

    val with_preset :
      ?loc:Def.loc -> ?doc:string -> t -> Conf.Preset.t option -> string -> t
    (** [with_preset s preset n] is the scheme [s] but with name [n]
        and preset [preset].

        @raise Invalid_argument on proxy presets. *)

    val wrap : t -> t -> string -> t
    (** [wrap snd fst n] is a scheme named [n] that behaviourly performs
        the functional composition [snd o fst]. *)

    include Def.S with type t := t

    (** {1:proxy Proxy schemes}

        Proxy schemes can build in containers or on virtual or remote
        machines. Exactly how this is performed depends on the
        scheme but in general an proxy scheme [n] tries to:
        {ol
        {- Mount or sync the description root directory on a different
           machine.}
        {- Run the build there using the direct build variant [n] and
           the local directory [_b0/v/n/proxy] as the build directory.}
        {- Mount or sync the resulting direct variant directory
           to the [_b0/v/n] directory.}} *)
  end

  type t
  (** The type for variants. *)

  val create :
    ?preset:bool -> dir:Fpath.t -> string -> Scheme.t ->
    (t * Conf.Preset.def_error list) result
  (** [create ~preset ~dir n scheme] creates a variant named [n] with scheme
      [scheme] in directory [dir] and if [preset] is [true] (default)
      applies the scheme's preset on the empty configuration. *)

  val reset : t -> unit result
  (** [reset v] resets variant [v]. FIXME After this has been called the variant
      should be in a state that is similar to [create]. *)

  val delete :
    ?force:bool -> t -> (unit, [`Scheme of R.msg | R.msg ]) Pervasives.result
  (** [delete ~force v] deletes variant [v]. If [force] is [true] (defaults
      to [false]) the deletion occurs even if the scheme hook errors. *)

  val scheme : t -> Scheme.t
  (** [scheme v] is [v]'s scheme. *)

  val conf_path : t -> Fpath.t
  (** [conf_path v] is the path to [v]'s configuration. *)

  val outcome_path : t -> Fpath.t
  (** [outcome_path v] is the path to [v]'s outcome *)

  val build_path : t -> Fpath.t
  (** [build_path v] is the path to [v]'s build directory.  *)

  val cache_index_path : t -> Fpath.t
  (** [cache_index_path v] is the path to [v]'s cache index. *)

  (* TODO abstract that in Dir_def *)

  val value_kind : string

  val name : t -> string
  (** [name v] is [v]'s name. *)

  val path : t -> Fpath.t
  (** [path v] is the path to [v]. *)

  val equal : t -> t -> bool
  (** [equal v0 v1] is [true] iff [v0] and [v1] are the same variant. *)

  val compare : t -> t -> int
  (** [equal v0 v1] is a total order on variants compatible with {!equal}. *)

  val compare_by_name : t -> t -> int
  (** [compare v0 v1] totally orders [v0] and [v1] in increasing
      lexicographical variant name order. *)

  type unknown_scheme = [`Unknown_scheme of string * t ]
  (** The type for variant with unknown schemes. If the scheme is unknown
      it is replaced by {!Scheme.nop}. *)

  val of_unknown_scheme : unknown_scheme -> t
  (** [of_unknown_scheme u] is the variant in [u] with {!nop} scheme. *)

  val pp_unknown_scheme : unknown_scheme Fmt.t
  (** [pp_unknown_scheme ppf u] prints an unspecified representation of [u]
      on [ppf]. *)

  type load = (t, [`Unknown_scheme of string * t]) Pervasives.result
  (** The type for variant loads. If the scheme is unknown it is
      replaced by {!nop}. *)

  val of_load : load -> t
  (** [of_load l] is the variant in [l]. *)

  val exists : dir:Fpath.t -> string -> bool result
  (** [exists ~dir n] is [Ok true] if variant named [n] exists
      in [dir]. *)

  val find : dir:Fpath.t -> string -> load option result
  (** [find ~dir n] is the variant named [n] in directory [dir] (if any). *)

  val get : dir:Fpath.t -> string -> load result
  (** [get ~dir n] is like {!find} but @raise Invalid_argument
      if no variant named [n] exists. *)

  val get_or_suggest :
    dir:Fpath.t -> string -> (load, string list) Pervasives.result result
  (** [get_or_suggest ~dir n] is the variant with name [n] if it exists or
      a list of suggested variant names that could match [n]. *)

  val list : dir:Fpath.t -> load list result
  (** [list ~dir] is the list of variants located in directory [dir] in
      increasing lexicographical variant name order. *)

  val list_empty : dir:Fpath.t -> bool result
  (** [list_empty ~dir] is [Ok true] if there are no variants [dir] or
      if [dir] doesn't exist. *)

  (** {1:pp Pretty-printing} *)

  val pp_name_str : string Fmt.t
  (** [pp_name_str ppf s] prints [s] as if it was a name on [ppf]. *)

  val pp_name : t Fmt.t
  (** [pp_name ppf v] prints [v]'s name on [ppf]. *)

  val pp_synopsis : t Fmt.t
  (** [pp_synopsis ppf v] prints [v]'s synopsis:
      its name and documentation. *)

  val pp_info : t Fmt.t
  (** [pp_info ppf v] prints [v]'s synopsis and fields. *)

  val pp_info_ext : t Fmt.t -> t Fmt.t
  (** [pp_info ext ppf v] prints [v]'s synopsis and fields extended with
      [ext]. *)
end

(** {1:codec Codec}

    {b FIXME.} Move that out of the core build API
    to driver libraries or [b0]/[d0] or B0_care. *)

(** S-expression decoder. *)
module Sexp : sig

  (** {1:src_pos Source positions} *)

  type pos = int
  (** The type for byte positions. *)

  type range = pos * pos
  (** The type for position ranges. *)

  type src = File of Fpath.t
  (** The type for input sources. *)

  type loc = src * range
  (** The type for locations. *)

  val pp_loc : loc Fmt.t
  (** [pp_loc ppf l] prints an unspecified representation of [l] on [ppf]. *)

  (** {1:sexp S-expressions} *)

  type t = [ `Atom of string | `List of t list ] * loc
  (** The type for s-expressions. *)

  val get_atom : t -> string
  (** [get_atom s] is the atom in [s]. @raise Invalid_argument if [s]
      is not an atom. *)

  val get_list : t -> t list
  (** [get_list s] is the list in [s]. @raise Invalid_argument if [s]
      is not a list. *)

  (** {1:parsing Parsing s-expressions} *)

  val of_string : src:src -> string -> t result
  (** [of_string ~src s] inputs a top-level {e sequence} of s-expressions
      from [s] and returns it as a list that spans the whole input. *)

  val of_file : Fpath.t -> t result
  (** [of_file f] inputs a top-level {e sequence} of s-expressions
      from [s] and returns it as a list that spans the whole input. *)

  val dump_locs : t Fmt.t
  (** [dump_locs ppf se] dumps the location of the s-expressions [se]
      on [ppf]. *)

  (** {1:key_value Parsing key-value maps}

      {b WARNING.} Except {!to_string_map} all these functions and the
      {{!type:key}keys} raise [Failure] on error. Catch the exception
      in higher-level parsing functions on propagate them via [Error
      (`Msg m)]. *)

  type map = (t * loc) String.Map.t * loc
  (** The type for key value maps. A string map that binds key name to their
      location and value tupled with the location of the map. *)

  type 'a key = map -> 'a
  (** The type for key-value binding accessor keys.  Given an
      s-expression map returns the key value. *)

  val to_string_map : ?known:(string -> bool) -> t -> (map * map) result
  (** [to_string_map ~known se] is [(known, unknown)]. [se] must be a
      list of key-value bindings [(k v)] where [k] is an atom and [v]
      an s-expression. The resulting binding for [k] is added to
      [known] or [unknown] tupled with the binding location depending
      on whether [known s] is [true] or not. [known] defaults to [fun
      _ -> true]. *)

  val key : ?absent:'a -> (string -> t -> 'a) -> string -> 'a key
  (** [key ~absent parse name ] looks up key [name], parse its value
      according [parse] (which should raise Failure with an error
      that includes the location in case of error). If the key is
      not in the key map and [absent] is returned, otherwise an
      error is raised. *)

  val atom_key : ?absent:'a -> (string -> 'a) -> string -> 'a key
  (** [atom_key] is like {!key} but requires and parses an atom. *)

  val list_key :
    ?empty:bool -> ?absent:'a list -> (string -> t -> 'a) ->
    string -> 'a list key
  (** [list_key] is like {!key} but requires and parses a list of
      elements. If [empty] is [false] (defaults to [true]) an error is
      raised if the list is empty. *)

  val atom_list_key :
    ?empty:bool -> ?absent:'a list -> (string -> 'a) -> string -> 'a list key
  (** [atom_list] is like {!list_key} but parses a list of atoms. *)

  (** {2:valparse Value parsers} *)

  val parse_atom : string -> t -> string
  (** [parse_atom key se] parses an atom from [se] assuming this is for
      a key named [key] (used for error reporting). *)

  val parse_list :?empty:bool -> string -> t -> t list
  (** [parse_list key se] parses a list from [se] assuming this is for
      a key named [key] (used for error reporting). If [empty] is [false]
      (defaults to [true]) the parse errors if the list is empty. *)

  val parse_atom_kind : (string -> 'a) -> string -> t -> 'a
  (** [parse_atom_kind parse key se] is like {!parse_atom} but
      uses [parse] to transform the atom. [parse] should raise
      [Failure] in case of error. *)

  val parse_list_kind :
    ?empty:bool -> (string -> t -> 'a) -> string -> t -> 'a list
    (** [parse_list_kind parse key se] is like {!parse_list} but
        uses [parse] to transform the atom. [parse] should raise
        [Failure] in case of error. *)
end

(** JSON text encoder.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)
module Json : sig
  (** {1 Generation sequences} *)

  type 'a seq
  (** The type for sequences. *)

  val empty : 'a seq
  (** An empty sequence. *)

  val ( ++ ) : 'a seq -> 'a seq -> 'a seq
  (** [s ++ s'] is sequence [s'] concatenated to [s]. *)

  (** {1 JSON values} *)

  type t
  (** The type for JSON values. *)

  type mem
  (** The type for JSON members. *)

  type el
  (** The type for JSON array elements. *)

  val null : t
  (** [null] is the JSON null value. *)

  val bool : bool -> t
  (** [bool b] is [b] as a JSON boolean value. *)

  val int : int -> t
  (** [int i] is [i] as a JSON number. *)

  val str : string -> t
  (** [str s] is [s] as a JSON string value. *)

  val el : t -> el seq
  (** [el v] is [v] as a JSON array element. *)

  val el_if : bool -> (unit -> t) -> el seq
  (** [el_if c v] is [el (v ())] if [c] is [true] and {!empty} otherwise. *)

  val arr : el seq -> t
  (** [arr els] is an array whose values are defined by the elements [els]. *)

  val mem : string -> t -> mem seq
  (** [mem n v] is an object member whose name is [n] and value is [v]. *)

  val mem_if : bool -> string -> (unit -> t) -> mem seq
  (** [mem_if c n v] is [mem n v] if [c] is [true] and {!empty} otherwise. *)

  val obj : mem seq -> t
  (** [obj mems] is an object whose members are [mems]. *)

  (** {1 Output} *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b j] adds the JSON value [j] to [b]. *)

  val to_string : t -> string
  (** [to_string j] is the JSON value [j] as a string. *)

  val output : out_channel -> t -> unit
  (** [output oc j] outputs the JSON value [j] on [oc]. *)
end

(** {1:manual Manual}

    {ul
    {- {!quickstart}}
    {- {!concepts}}
    {- {!configuration}}
    {- {!variants}}
    {- {!deployments}}
    {- {!descriptions}}
    {- {!recipes}}}

    {1:quickstart Quick start}

    TODO show a few examples, using B0_ocaml care and the naked
    system. For now have a look at the [examples] directory in the
    distribution.

    {1:concepts Conceptual overview}

    {2:core Build model}

    Effectively a [B0] build system is an OCaml program that executes
    arbitrary external commands in parallel and whose effects on the
    file system are memoized across program runs with an on-disk
    cache.

    There is no notion of build rule in [B0]: you simply generate and
    execute program commands using arbitrary OCaml functions. This
    allows to define modular and rich data structures for describing
    builds that are "compiled" down, on each build, to parallel
    invocations of memoized commands.

    {2:concept_conf Configuration}

    Next to this simple build model [B0] adds a configuration mecanism
    under the form of a typed, persisted, key-value store which builds
    can consult and depend on.

    Since outputs from previous builds are kept in the cache, build
    configurations can be switched over and back almost
    instantaneously without loosing the earlier CPU cycles.

    The configuration layer is also cross-compilation ready: any
    configuration key value can differ for the build and host
    operating system and the build system of B0 keeps tracks of build
    tools that are build and used by the build system to make sure
    they are built with the build OS toolchain. For programmers of the
    build system, cross compilation is oblivious in B0, it happens
    without the user having to perform anything special.

    More on {{!configuration}configuration}.

    {2:concept_units Build units}

    Build units are statically known and named entities with metadata
    that gather sets of related build operations. Typical build units
    are sequences of commands that build a library, an executable,
    etc. Dependencies can be defined statically among units on dynamically
    on builds.

    Build units structure builds in well identified fragments,
    allowing to run them independently and perform coarse grained actions
    on their build outcomes.

    {2:concept_packages Packages}

    Packages are statically known named entities with metadata that
    represent a set of build units. They are units of deployment, they
    allow to build a set of units in isolation from the other and
    define deployment over them. They have no special functionality
    expect to structure the build units.

    {2:concept_variants Build variants}

    The basic build library and model allows build operations to act
    anywhere and can be used as such. However to structure the builds,
    the notion of {e build variant} is added on top of that. Build
    variant allow builds with different configurations to live next to
    each other or be performed in containers or on remote
    machines. They define a basic directory layout in which the build
    occurs and setup the {{!Env}build environment} in which the configuration
    occurs and build tools are looked up.

    More {{!variants}on variants}.

    {2:concept_deploy Deployments}

    Deployments abstract the general process of extracting part of the
    sources and/or build artefacts of your software to a new location.

    Examples of deployments are: installing build artefacts in a
    system (FIXME unclear), pushing build artefacts to a remote server
    or device, making source or binary distribution tarballs and
    pushing them to a location, interacting with package manager
    repositories.

    More {{!deployments}on deployments}.

    {2:concept_b0 The [b0] and [d0] tools}

    The [b0] and [d0] tool allow to build projects that are described
    by writing one or more [B0.ml] OCaml files in a source tree or a
    composition thereof.

    More {{!descriptions}on description files}.

    {2:concept_b0_dir A tour of the _b0 directory}

    Generally the layout of the build directory is as follows:

    {ul
    {- [_b0/cache], holds the build cache.}
    {- [_b0/defaults], holds description defaults.}
    {- [_b0/i], holds the
       {{!root}root description} and compiled {{!B0_driver}driver instances}.}
    {- [_b0/v], path to build variants.}
    {- [_b0/d], path to deployments.}}

    The structure of a build variant [n] is as follows:

    {ul
    {- [_b0/v/n/scheme], holds the variant's scheme name.}
    {- [_b0/v/n/outcome], holds the variant's build outcome (if any).}
    {- [_b0/v/n/conf], holds the variant's configuration (if any).}
    {- [_b0/v/n/trash], holds the build variant unit trash (if any).}
    {- [_b0/v/n/index], holds the variant's cache index (if any).}
    {- [_b0/v/n/proxy], path to data related to a proxy build (if any).}
    {- [_b0/v/n/b], path to the variant's build directory}
    {- [_b0/v/n/b/u1], holds the build of unit [u1] of the build variant.}
    {- [_b0/v/n/b/u2], holds the build of unit [u2] of the build variant.}}

    The structure of a deployment [n] is as follows:

    {ul
    {- [_b0/d/n/scheme], holds the deployment scheme name.}
    {- [_b0/d/n/conf], holds the deployment configuration (if any).}
    {- [_b0/d/n/s], holds the deployment's staging directory.}}

    {1:configuration Configuration}

    A {{!Conf}{e configuration}} is a set of typed key-value bindings
    consulted by descriptions and build procedures to adjust their
    outcomes to the build environment and desires of the end user.

    A {{!Conf.key}configuration key} the user did not explicitely set
    has a {e default} value, specified in the description at key
    creation time.  This value is either {{!Conf.const}constant} or
    {{!Conf.discover}discovered} via a function.

    A key can belong to at most one {{!Conf.Group}{e group}} which is
    simply a named set of related keys. Groups are used to easily
    select a subset of keys for user interaction. For example on [b0
    key get] command invocations, using the [-g ocaml] option will
    report the value of all configuration keys that declared
    themselves to be part of the [ocaml] group.

    Configuration {{!Conf.Preset}{e presets}} are named sets of
    key-value bindings defined in descriptions. They are a convenience
    to set key subsets in bulk in configurations.

    {3:last_stored_conf Last and stored configuration}

    The configuration used by the last build is persisted in the build
    outcome and called the {e last configuration}. It is immutable and
    contains only the key-value pairs of the configuration that were
    accessed by the last build. It can be accessed via the [b0 key get
    --last] command.

    The mutable {e stored configuration} is the configuration to be used
    by the next build. It can be acted upon via the [b0 key get] and
    [b0 key set] commands.

    {3:key_values Key value terminology}

    A configuration key has different values depending where and in
    which context it is looked up:
    {ul
    {- {e Last value}. The value used by the last build and stored in the
       last build outcome, if any.}
    {- {e Stored value}. The value from the stored configuration,
       if any.}
    {- {e Default value}. The value given at key definition time.
       This is either a constant or a value discovered by a
       function. The default value is used by a build if the key has
       no stored or environment value (see below). If the default
       value is discovered, the discovered value gets saved in the
       stored which is persisted at the end of the build to avoid
       repeated discovery; this behaviour may however be prevented by
       the default value {{!Conf.discover}definition}.}
    {- {e Environment value}. The value found, if any, for a key named
       [key] in a [B0_C_$KEY] environment variable where [$KEY] is
       [key] uppercased and with ['.'] replaced by ['_']. In a build
       this value overrides both the stored and default value of a
       key. It ends up defining the last value of a key but it doesn't
       get saved in the stored configuration.}
    {- {e Effective value} (for a build). The value used during a
       build: the environment value or if undefined, the stored value
       or if undefined, the default value.}
    {- {e Preset value}. The value of the key in a configuration
       {{!Conf.Preset}preset}. This is either a constant or a discovered
       value. It is used to set the stored value of a key to a user
       preference defined in the description.}}

    During a build the effective value of keys is looked up using the
    stored configuration. As a side effect new key-value pairs may be
    added to the stored configuration for keys whose default value is
    used {e and discovered} during the build. This modified stored
    configuration is persisted at the end of the build.

    {1:variants Build variants and variant schemes}

    A build variant is a build performed in a particular environment with
    a particular configuration.

    The build environment is defined by a variant {e scheme} which is
    responsible for setting up the environment for the variant. For
    example this can be: configuring and setting up the environment
    for an opam switch, spin a container or ssh to a remote machine.

    Build variants are identified by a name [n] which is used to
    operate on the variant. The build directory of a variant [n] is
    isolated from the others in [_b0/v/n]. Variants are created via:
{[
b0 variant create [SCHEME] [-n NAME]
]}
    or implicitely on the first [b0 build] if there's no existing
    variant (see {!initial_variant}). If you don't specify a variant
    name on creation a unique one will be automatically derived from
    the scheme name. If you don't specify a scheme, the default
    scheme (likely the {{!nop}nop scheme}) will be used.

    [b0] allows variants to exist and be acted upon side by side, use
    [b0 variant list] to list them. Most [b0] commands act on the
    variant specified explicitely via the [-w] or [--variant] argument
    or on the {e default variant} as reported by [b0 variant get].  If
    there is no default variant or if it doesn't exist commands might
    error.

    {2:nop The nop variant scheme}

    The variant scheme {!Variant.Scheme.nop} available under the name
    [nop] is the simplest variant scheme. It does nothing, it runs builds in
    the environment where the build tool [b0] itself is run.

    {2:default_variant The default variant and variant schemes}

    The default variant can be consulted, set or cleared via:
{[
b0 variant get [--effective | --stored]
b0 variant set [--clear | VARIANT]
]}
    If the [B0_VARIANT] environment variable is defined, it's
    value will define the default. The default variant is automatically
    set to a newly created variant this can be prevented with the [-c]
    option:
{[
b0 variant create -c SCHEME  # Do not set the new variant as the default
]}
    {2:initial_variant The initial variant}

    If no variant exists and there is no default variant when [b0
    build] (or equivalently [b0]) is run, a variant is created using
    the default variant scheme. So on a fresh checkout of your project:
{[
b0
]}
    automatically creates a variant, set it as the default one and
    builds your project.

    {1:descr_values Description values}

    [B0] descriptions are made of a grab bag of OCaml values,
    configuration keys, build units, packages, variants, variant
    schemes, deployments, etc. In order to operate on these values
    from end-user interfaces (e.g. the [b0] and [d0] tools), the
    following must be guaranteed:

    {ol
    {- Values and their names need to be defined during the toplevel
       initialization phase of the program without being conditioned
       by external factors [B0] may not be aware of (FIXME implement
       {!Def} locking).}
    {- Values names need to be unique to ensure all the values are
       accessible and can be operated on.}}

    As far as 1. is concerned, [B0] relies on the discpline of [B0.ml]
    file writers. They should define all their description values
    through toplevel [let] definitions and {b never} conditionalize
    their existence or the definition of their components.  For
    examples this should {b NOT} be done:
{[
let myprogram =
  (* NEVER DO THIS *)
  let default = Conf.const (if Sys.win32 then "tool.exe" else "tool") in
  Conf.(key "myprogram" ~default)
]}
    As far as 2. is concerned. [B0] handles this automatically. In two manners:
    {ul
    {- If a name used in a description clashes with a name defined by a library
       [B0] logs a warning and automatically rename the new definition.}
    {- When multiple [B0.ml] are composed toghether. The names defined
       by subdescriptions get automatically namespaced by their position
       in the file hierarchy. TODO Give example.}}

    {1:deployments Deployments}

    Deployements are handled via the [d0] tool.  They do not
    necessarily need a build to exist but can request for builds of
    specific packages to exist. They occur through a sequence of
    steps, all of which are configurable and made available through {e
    deployment schemes}.

    {ol
    {- Pre-stage check and build requirements.}
    {- Stage function, prepare deploy artefacts in the deployment
       staging directory.}
    {- Post-stage check.}
    {- Pre-push check.}
    {- Deployment push, push the staged artefacts.}
    {- Post-push check.}}

    {1:descriptions Descriptions files}

    A description file is either:

    {ol
    {- A [B0.b0] file that {{!b0b0}describes} how to compile a description.}
    {- A [B0.ml] OCaml file in a directory without a [B0.b0] file.}}

    If your description is simple or uses only the default [B0]
    library then a simple [B0.ml] description will do. If not, a
    [B0.b0] file is an s-expression based {{!b0b0}configuration file}
    that describes how to compile a self-contained and isolated
    description.  It can specify additional (and conditional) sources
    and libraries to use, compilation flags and control how
    subdescriptions (see below) are looked up.

    {2:root Root description and directory}

    [b0] supports file hierarchies that contain more than one
    description file. In general, to ease build setup understanding,
    it is better to keep a single description per project.

    However multiple descriptions allow to merge the description of
    multiple parallel and interdependent projects into a {e root
    description} that is built in a {e root directory}.

    We first explain formally how an invocation of [b0] finds the root
    directory, examples follow. Given the root directory we can
    proceed to describe which descriptions belong to the root
    description.

    When started in a directory [dir], [b0], unless invoked with
    [--root] option, finds a {e root directory} for the build as
    follows:
    {ol
    {- Starting with [dir] (included) and moving up in the hierarchy, find
       the first [start] directory with a description file (a [B0.b0] or
       [B0.ml] file). If there is no such directory there is no root directory
       and no build description.}
    {- From [start] move to the parent directory [up] and:
      {ul
      {- If [up] has a description file and does not exclude [start] via the
       {{!b0_key_ref}[subs] key} of an [up/B0.b0] file, let
       [start] be [up] and go to 2.}
      {- If there is no description in [up] or if it excludes [start] then
         [start] is the root directory.}}}}



    Here's an example of a file hierarchy with multiple descriptions:
{v
d
└── root
    ├── B0.b0
    ├── B0.ml
    ├── p1
    │   ├── B0.b0
    │   └── B0.ml
    ├── p2
    │   ├── B0.ml
    │   ├── hop
    │   │   └── B0.ml
    │   └── sub
    │       ├── a
    │       │   └── B0.ml
    │       └── b
    └── src
        ├── bin
        └── lib
v}

    In the example above starting a driver in [d/root],
    [d/root/src/bin], [d/root/p1], [d/root/p2/sub/b] will all find the
    root directory [d/root].  However starting a driver in
    [d/root/p2/sub/a] will find the root directory [d/root/p2/sub/a]
    as there is no description in [root/p2/sub]. Adding an empty file
    [d/root/p2/sub/B0.b0] would allow to find [d/root].

    Given a root directory with (a possibly empty) description, [b0]
    gathers and {{!b0_merge}merge} the descriptions files of all {e
    direct} subdirectories and recursively into the {e root
    description}. The {{!b0_key_ref}[subs] key} of [B0.b0] files
    can be used to control which {e direct} subdirectories are looked
    up. The OCaml sources of different sub descriptions cannot refer
    to each other directly; they are properly isolated and linked in
    any (but deterministic) order.

    Assuming no [B0.b0] file makes use of the [subs] key in the
    above example, the root description in [root] takes into account
    all descriptions files except [d/root/p2/sub/a/B0.ml]. Here again
    adding an empty file [d/root/p2/sub/B0.b0] would allow to take it
    into account.

    {1:b0b0 B0.b0 description files}

    A [B0.b0] description file is a possibly empty sequence of
    s-expressions of the form [(key value)]. Here's an annoted example:
{v
(b0-version 0)       ; Mandatory, except if the file is empty
(libs (b0_cmdliner)) ; Always compile with the external b0_cmdliner library

; Describe the sources that make up the description in dependency order.
; As a convention if you split your build in many build files put them
; in a B0.d/ directory. If the [srcs] key is absent and a B0.ml file
; exists next to the B0.b0 file it is always automatically added as if
; ("B0.ml" () "B0.ml file") was appended at the end of srcs.
(srcs
  ; If the source path has no suffix looks up both for an .ml and mli file
  ((B0.d/util () "Utility module")

   ; The following source needs the b0_jsoo library and is only added to
   ; the description if the library is found to be installed.
   (B0.d/with_jsoo.ml (b0_jsoo) "Description with jsoo support")))

(compile (-w -23)) ; Disable warning 23 for compiling the description
v}

    {2:b0_key_sem Key parsing and semantics}

    An [B0.b0] file without keys and without a [B0.ml] file sitting
    next to it is an {e empty} and valid description.

    If a key is defined more than once, the last one takes over;
    other than that the key order is irrelevant. Except for keys
    that start with [x-], unknown keys trigger parse warnings.

    {b Relative file paths.} Relative file paths are relative to the
    description file directory.

    {b Library lookup.} FIXME. Library lookup is currently quite restricted
    and done according to the following name mapping:
    {ul
    {- [libname], [$LIBDIR/libname/libname.cm[x]a]}
    {- [libname.sub], [$LIBDIR/libname/libname_sub.cm[x]a]}}
    With [$LIBDIR] being defined (first match) by:
    {ol
    {- The value of the environment variable [B0_DRIVER_LIBDIR]}
    {- The value of the environment variable [OPAM_SWITCH_PREFIX] post}
    {- The value of [$(ocamlc -where)/..]}}
    Dependency resolution on the libraries is not performed and [cmi] files
    have to be in the corresponding [libname] directory.

    {2:b0_key_ref Key reference}

    {ul
    {- [(b0-version 0)]. File format version, mandatory, except if
       the description is empty.}
    {- [(libs (<libname>...))]. Libraries unconditionally used to compile
        and link.}
    {- [(drop-libs (<libname>...))]. Libraries dropped from compile and link
       (e.g. to drop [B0] or [B0_care]).}
    {- [(srcs ((<path> (<libname>...) <docstring>)...))]
       OCaml source files to use for the description. Each source is
       described by a path to an ml file, libraries whose existence
       condition their usage and a documentation string describing their
       purpose. If [<path>] doesn't end with [.ml] assumes both [<path>.ml]
       and [<path>.mli] exist and are used. A [B0.ml] file sitting
       next to the [B0.b0] is always automatically added at the end of
       the list.}
    {- [(subs (<op> (<dirname>...)))]. Subdescription lookup.
       {ul
       {- By default, if unspecified, all the subdirectories of the directory
          in which the [B0.b0] resides that do not start with [.] or [_]
          are looked up for descriptions.}
       {- If [<op>] is [include] only the specified list of directory names
          are looked up.}
       {- If [<op>] is [exclude] excludes the given list of directory names
          from the default lookup.}}}
    {- [(compile-kind <kind>)] with [<kind>] one of [byte], [native], [auto].
       The kind of binary to compile to. Allows to force
       the use of [ocamlc] or [ocamlopt]. Defaults to [auto] which selects
       native code if available and bytecode otherwise. Subdescriptions
       propagate their constraint to the root. Inconsistent compilation kind in
       subdescripitions lead to failure. Can be overriden with
       the `--d-compile-kind` option or by the [B0_D_COMPILE_KIND]
       environment variable.}
    {- [(b0-dir <path>)]. The b0 directoy to use. Can be overriden on
       the command line with the `--b0-dir` option or by the
       [B0_DIR] environment variable. Defaults to [_b0].}
    {- [(driver-dir <path>)]. The driver directory to use. Can be overriden
       on the command line or by the [B0_DRIVER_DIR] environment variable.}
    {- [(compile (<arg>...))]. Arguments added to byte and native compilation.
       More can be added on the command line via the `--d-compile` option.}
    {- [(compile-byte (<arg>...))]. Arguments added to byte compilation.}
    {- [(compile-native (<arg>...))]. Arguments added to native compilation.}
    {- [(link (<arg>...))]. Arguments added to byte and native linking.
       More can be added on the command line via theh `--d-link` option.}
    {- [(link-byte (<arg>...))]. Arguments added to byte linking.}
    {- [(link-native (<arg>...))]. Arguments added to native linking.}
    {- [(ocamlfind <bin>)]. [ocamlfind] binary to use. Ignored in
       subdescriptions.}
    {- [(ocamlc <bin>)]. [ocamlc] binary to use. Ignored in subdescriptions.
       Can be overriden with the `--d-ocamlc` option or by the [B0_D_OCAMLC]
       environment variable.}
    {- [(ocamlopt <bin>)]. [ocamlopt] binary to use. Ignored in
       subdescriptions. Can be overriden with the `--d-ocamlopt` option
       or by the [B0_D_OCAMLOPT] environment variable.}
    {- [(x-<key> value)]. Arbitrary user defined key.}}

    {2:b0_merge [B0.b0] key merges}

    When multiple [B0.b0] file are used, their specification is merged
    with the root description. During this process the key values of
    subdescriptions are either:

    {ul
    {- Ignored (e.g. [ocamlc], [ocamlopt], etc.).}
    {- Merged according to key specific strategies which can
       fail; one example of failure is when one subdescription mandates
       [(compile-kind native)] and another one [(compile-kind byte)].}} *)

(**/**)
(*
    {1:more More building concepts}

    {2:concept_cmd_stamps Output command digests}

    Commands are assumed to be pure functions of their inputs
    and declared process environment.

    Concatenate and digest: The digest of the executable, the command
    line arguments, the spawn process environment, the digest of the
    contents of inputs, the output path. This becomes the name of the
    file in the cache.

    {2:cleaning Cleaning build}

    In a cleaning run, outputs that are not rebuild and were present
    in the previous run are deleted at the end of the run.

    {2:correct Build correctness}

    If you spend some time thinking about building software
    incrementally and correctly you quickly realize that our current
    file system and tool based approach is entirely hopless. The fact
    that you can't guarantee noone fiddles with the outputs of your
    build steps across build system runs. [B0] is not different and
    you can entirely trip it by fiddling with the contents of its
    [_b0] dir.
*)
(**/**)
(**
    {1:recipes Recipes and menagerie}

    {2:conf Writing conf discovery}

    Error only if really needed. Otherwise log with warning and
    default to a reasonable deafult value. Build units can still abort
    if they can't use the value.
*)


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
