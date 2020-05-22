(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Standard library needs.

    Open this module to use it. Only redefines a few standard
    modules and introduces a few new ones. *)

(** {1:std Std} *)

(**/**)
module Stdlib_set = Set
(**/**)

(** Type identifiers. *)
module Tid : sig

  (** {1:typeids Type identifiers} *)

  type 'a t
  (** The type for type identifiers for a type ['a]. *)

  val create : unit -> 'a t
  (** [create ()] is a new type identifier. *)

  type ('a, 'b) eq = Eq : ('a, 'a) eq (** *)
  (** The type for type identifier equality testing. *)

  val equal : 'a t -> 'b t -> ('a, 'b) eq option
  (** [equal t0 t1] determines if [t0] and [t1] are equal. *)
end

(** {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
      ANSI terminal} interaction. *)
module Tty : sig

  (** {1:terminals Terminals} *)

  type t = [ `Dumb | `Term of string ] option
  (** The type for terminals. Either no terminal, a dumb one or
      a named terminal from the [TERM] environment variable. *)

  val of_fd : Unix.file_descr -> t
  (** [of_fd fd] determines the terminal for file descriptor [fd] by
      using {!Unix.isatty}[ fd] and consulting the [TERM] environment
      variable. *)

  (** {1:caps Capabilities} *)

  type cap = [ `None (** No capability. *) | `Ansi (** ANSI terminal. *)  ]
  (** The type for terminal capabilities. Either no capability or
      ANSI capability. *)

  val cap : t -> cap
  (** [cap tty] determines [tty]'s capabilities. *)

  (** {1:style ANSI escapes and styling} *)

  type color =
  [ `Default | `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan
  | `White ]
  (** The type for ANSI colors. *)

  type style =
  [ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Slow | `Rapid ]
  | `Reverse | `Fg of [ color | `Hi of color ]
  | `Bg of [ color | `Hi of color ] ]
  (** The type for ANSI styles. *)

  val styled_str : cap -> style list -> string -> string
  (** [styled_str cap styles s] styles [s] according to [styles] and [cap]. *)

  val strip_escapes : string -> string
  (** [strip_escapes s] removes ANSI escapes from [s]. *)
end

(** Textual formatters.

    Helpers for dealing with {!Format}. *)
module Fmt : sig

  (** {1:stdoutput Standard outputs} *)

  val stdout : Format.formatter
  (** [stdout] outputs to standard output. *)

  val stderr : Format.formatter
  (** [stderr] outputs to standard error. *)

  (** {1:formatting Formatting} *)

  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.fprintf}. *)

  val pr : ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.printf}. *)

  val epr : ('a, Format.formatter, unit) format -> 'a
  (** [epr] is {!Format.eprintf}. *)

  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  (** str is {!Format.asprintf}. *)

  val kpf :
    (Format.formatter -> 'a) -> Format.formatter ->
    ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kpf] is {!Format.kfprintf}. *)

  val kstr : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** kstr is {!Format.kasprintf}. *)

  val failwith : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [failwith fmt ...] is [kstr failwith fmt ...] *)

  val failwith_notrace : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [failwith_notrace] is like {!failwith} but [Failure] is raised with
      {!raise_notrace}. *)

  val invalid_arg : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [invalid_arg fmt ...] is [kstr invalid_arg fmt ...] *)

  val error : ('b, Format.formatter , unit, ('a, string) result) format4 -> 'b
  (** [error fmt ...] is [kstr (fun s -> Error s) fmt ...] *)

  (** {1:formatters Formatters} *)

  type 'a t = Format.formatter -> 'a -> unit
  (** The type for formatter of values of type ['a]. *)

  val flush : 'a t
  (** [flush] has the effect of {!Format.pp_print_flush}. *)

  val flush_nl : 'a t
  (** [flush_nl] has the effect of {!Format.pp_print_newline}. *)

  val nop : 'a t
  (** [nop] formats nothing. *)

  val any : (unit, Format.formatter, unit) format -> 'a t
  (** [any fmt] formats any value with [fmt]. *)

  val using : ('a -> 'b) -> 'b t -> 'a t
  (** [using f pp ppf v] is [pp ppf (f v)]. *)

  (** {1:separators Separators} *)

  val cut : 'a t
  (** [cut] has the effect of {!Format.pp_print_cut}. *)

  val sp : 'a t
  (** [sp] has the effect of {!Format.pp_print_space}. *)

  val sps : int -> 'a t
  (** [sps n] has the effect of {!Format.pp_print_break}[ n 0]. *)

  val comma : 'a t
  (** [comma] is {!Fmt.any}[ ",@ "]. *)

  val semi : 'a t
  (** [semi] is {!Fmt.any}[ ";@ "]. *)

  (** {1:sequencing Sequencing} *)

  val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t
  (** [iter ~sep iter pp_elt] formats the iterations of [iter] over a
      value using [pp_elt]. Iterations are separated by [sep] (defaults to
      {!cut}). *)

  val iter_bindings :
    ?sep:unit t -> (('a -> 'b -> unit) -> 'c -> unit) -> ('a * 'b) t -> 'c t
  (** [iter_bindings ~sep iter pp_binding] formats the iterations of
      [iter] over a value using [pp_binding]. Iterations are separated
      by [sep] (defaults to {!cut}). *)

  val append : 'a t -> 'a t -> 'a t
  (** [append pp_v0 pp_v1 ppf v] is [pp_v0 ppf v; pp_v1 ppf v]. *)

  val (++) : 'a t -> 'a t -> 'a t
  (** ( ++ ) is {!append}. *)

  val concat : ?sep:unit t -> 'a t list -> 'a t
  (** [concat ~sep pps] concatenates the formatters [pps] separating them
      with [sep] (defaults to {!cut}). *)

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

  val hovbox : ?indent:int -> 'a t -> 'a t
  (** [hovbox] is a condensed {!box}, see {!Format.pp_open_hovbox}. *)

  (** {1:bracks Brackets} *)

  val parens : 'a t -> 'a t
  (** [parens pp_v ppf] is [pf "@[<1>(%a)@]" pp_v]. *)

  val brackets : 'a t -> 'a t
  (** [brackets pp_v ppf] is [pf "@[<1>[%a]@]" pp_v]. *)

  val braces : 'a t -> 'a t
  (** [braces pp_v ppf] is [pf "@[<1>{%a}@]" pp_v]. *)

  val quote : ?mark:string -> 'a t -> 'a t
  (** [quote ~mark pp_v ppf] is [pf "@[<1>@<1>%s%a@<1>%s@]" mark pp_v mark],
      [mark] defaults to ["\""], it is always counted as spanning as single
      column (this allows for UTF-8 encoded marks). *)

  (** {1:records Records} *)

  val id : 'a -> 'a
  (** [id] is {!Fun.id}. *)

  val field :
    ?label:string t -> ?sep:unit t -> string -> ('b -> 'a) -> 'a t -> 'b t
  (** [field ~label ~sep l prj pp_v] pretty prints a labelled field value as
      [pf "@[<1>%a%a%a@]" label l sep () (using prj pp_v)]. [label] defaults
      to [tty_string [`Yellow]] and [sep] to [any ":@ "]. *)

  val record : ?sep:unit t -> 'a t list -> 'a t
  (** [record ~sep fields] pretty-prints a value using the concatenation of
      [fields], separated by [sep] (defaults to [cut]) and framed in a vertical
      box. *)

  (** {1:stdlib Stdlib types}

      Formatters for structures give full control to the client over
      the formatting process and do not wrap the formatted structures
      with boxes. *)

  val bool : bool t
  (** [bool] is {!Format.pp_print_bool}. *)

  val int : int t
  (** [int] is {!Format.pp_print_int}. *)

  val int32 : int32 t
  (** [int32] is [pf ppf "%ld"]. *)

  val int64 : int64 t
  (** [int64] is [pf ppf "%Ld"]. *)

  val float : float t
  (** [float] is [pf ppf "%g"]. *)

  val char : char t
  (** [char] is {!Format.pp_print_char}. *)

  val string : string t
  (** [string] is {!Format.pp_print_string}. *)

  val sys_signal : int t
  (** [sys_signal] formats an OCaml {{!Sys.sigabrt}signal number} as
      a C POSIX {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/signal.h.html}constant}
      or ["SIG(%d)"] if the signal number is unknown. *)

  val backtrace : Printexc.raw_backtrace t
  (** [backtrace] formats a backtrace. *)

  val exn : exn t
  (** [exn] formats an exception. *)

  val exn_backtrace : (exn * Printexc.raw_backtrace) t
  (** [exn_backtrace] formats an exception backtrace. *)

  val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
  (** [pair ~sep pp_fst pp_snd] formats a pair. The first and second
      projection are formatted using [pp_fst] and [pp_snd] and are
      separated by [sep] (defaults to {!cut}). *)

  val option : ?none:unit t -> 'a t -> 'a option t
  (** [option ~none pp_v] formats an option. The [Some] case uses
      [pp_v] and [None] uses [none] (defaults to {!nop}). *)

  val none : unit t
  (** [none] is [any "<none>"]. *)

  val list : ?empty:unit t -> ?sep:unit t -> 'a t -> 'a list t
  (** [list ~sep pp_v] formats list elements. Each element of the list is
      formatted in order with [pp_v]. Elements are separated by [sep]
      (defaults to {!cut}). If the list is empty, this is [empty]
      (defaults to {!nop}). *)

  val array : ?empty:unit t -> ?sep:unit t -> 'a t -> 'a array t
  (** [array ~sep pp_v] formats array elements. Each element of the
      array is formatted in in order with [pp_v]. Elements are
      seperated by [sep] (defaults to {!cut}). If the array is empty
      this is [empty] (defauls to {!nop}). *)

    (** {1:mag Magnitudes} *)

  val si_size : scale:int -> string -> int t
  (** [si_size ~scale unit] formats a non negative integer
      representing unit [unit] at scale 10{^scale * 3}, depending on
      its magnitude, using power of 3
      {{:https://www.bipm.org/en/publications/si-brochure/chapter3.html}
      SI prefixes} (i.e. all of them except deca, hector, deci and
      centi). Only US-ASCII characters are used, [µ] (10{^-6}) is
      written using [u].

      [scale] indicates the scale 10{^scale * 3} an integer
      represents, for example [-1] for m[unit] (10{^-3}), [0] for
      [unit] (10{^0}), [1] for [kunit] (10{^3}); it must be in the
      range \[[-8];[8]\] or [Invalid_argument] is raised.

      Except at the maximal yotta scale always tries to show three
      digits of data with trailing fractional zeros omited. Rounds
      towards positive infinity (over approximates).  *)

  val byte_size : int t
  (** [byte_size] is [si_size ~scale:0 "B"]. *)

  val uint64_ns_span : int64 t
  (** [uint64_ns_span] formats an {e unsigned} nanosecond time span
      according to its magnitude using
      {{:http://www.bipm.org/en/publications/si-brochure/chapter3.html}SI
      prefixes} on seconds and
      {{:http://www.bipm.org/en/publications/si-brochure/table6.html}accepted
      non-SI units}. Years are counted in Julian years (365.25
      SI-accepted days) as
      {{:http://www.iau.org/publications/proceedings_rules/units/}defined}
      by the International Astronomical Union (IAU). Only US-ASCII characters
      are used ([us] is used for [µs]). *)

  (** {1:text_lines Text and lines} *)

  val text : string t
  (** [text] is {!Format.pp_print_text}. *)

  val lines : string t
  (** [lines] formats lines by replacing newlines (['\n']) in the string
      with calls to {!Format.pp_force_newline}. *)

  val truncated : max:int -> string t
  (** [truncated ~max] formats a string using at most [max]
      characters. If the string doesn't fit, it is truncated and ended
      with three consecutive dots which do count towards [max]. *)

  (** {1:hci HCI fragments} *)

  val and_enum : ?empty:unit t -> 'a t -> 'a list t
  (** [and_enum ~empty pp_v ppf l] formats [l] according to its length.
      {ul
      {- [0], formats {!empty} (defaults to {!nop}).}
      {- [1], formats the element with [pp_v].}
      {- [2], formats ["%a and %a"] with the list elements}
      {- [n], formats ["%a, ... and %a"] with the list elements}} *)

  val or_enum : ?empty:unit t -> 'a t -> 'a list t
  (** [or_enum] is like {!and_enum} but uses "or" instead of "and". *)

  val did_you_mean : 'a t -> 'a list t
  (** [did_you_mean pp_v] formats ["Did you mean %a ?"] with {!or_enum}
      if the list is non-empty and {!nop} otherwise. *)

  val must_be : 'a t -> 'a list t
  (** [must_be pp_v] formats ["Must be %a."] with {!or_enum} if the list
      is non-empty and {!nop} otherwise. *)

  val unknown : kind:unit t -> 'a t -> 'a t
  (** [unknown ~kind pp_v] formats ["Unknown %a %a." kind () pp_v]. *)

  val unknown' :
    kind:unit t -> 'a t -> hint:('a t -> 'a list t) -> ('a * 'a list) t
  (** [unknown ~kind pp_v ~hint (v, hints)] formats {!unknown} followed
      by a space and [hint pp_v hints] if [hints] is non-empty. *)

  (** {1:tty ANSI TTY styling} *)

  val set_tty_styling_cap : Tty.cap -> unit
  (** [set_tty_styling_cap c] sets the global styling capabilities to
      [c]. Affects the output of {!tty_str} and {!tty}. *)

  val tty_styling_cap : unit -> Tty.cap
  (** [tty_styling_cap ()] is the global styling capability. *)

  val tty_string : Tty.style list -> string t
  (** [tty_string styles ppf s] prints [s] on [ppf] according to [styles]
      and the value of {!tty_styling_cap}. *)

  val tty : Tty.style list -> 'a t -> 'a t
  (** [tty styles pp_v ppf v] prints [v] with [pp_v] on [ppf]
      according to [styles] and the value of {!tty_styling_cap}. *)

  val code : 'a t -> 'a t
  (** [code] is [tty `Bold]. *)
end

(** Option values (as in [4.08)]. *)
module Option : sig

  (** {1:options Options} *)

  type 'a t = 'a option = None | Some of 'a (** *)
  (** The type for option values. Either [None] or a value [Some v]. *)

  val none : 'a option
  (** [none] is [None]. *)

  val some : 'a -> 'a option
  (** [some v] is [Some v]. *)

  val value : 'a option -> default:'a -> 'a
  (** [value o ~default] is [v] if [o] is [Some v] and [default] otherwise. *)

  val get : 'a option -> 'a
  (** [get o] is [v] if [o] is [Some v] and raises {!Invalid_argument}
      otherwise. *)

  val bind : 'a option -> ('a -> 'b option) -> 'b option
  (** [bind o f] is [f v] if [o] is [Some v] and [None] if [o] is
      [None]. *)

  val join : 'a option option -> 'a option
  (** [join oo] is [Some v] if [oo] is [Some (Some v)] and [None] otherwise. *)

  val map : ('a -> 'b) -> 'a option -> 'b option
  (** [map f o] is [None] if [o] is [None] and [Some (f v)] is [o] is
      [Some v]. *)

  val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
  (** [fold ~none ~some o] is [none] if [o] is [None] and [some v] if [o] is
      [Some v]. *)

  val iter : ('a -> unit) -> 'a option -> unit
  (** [iter f o] is [f v] if [o] is [Some v] and [()] otherwise. *)

  (** {1:preds Predicates and comparisons} *)

  val is_none : 'a option -> bool
  (** [is_none o] is [true] iff [o] is [None]. *)

  val is_some : 'a option -> bool
  (** [is_some o] is [true] iff [o] is [Some o]. *)

  val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
  (** [equal eq o0 o1] is [true] iff [o0] and [o1] are both [None] or if
      they are [Some v0] and [Some v1] and [eq v0 v1] is [true]. *)

  val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
  (** [compare cmp o0 o1] is a total order on options using [cmp] to compare
      values wrapped by [Some _]. [None] is smaller than [Some _] values. *)

  (** {1:converting Converting} *)

  val to_result : none:'e -> 'a option -> ('a, 'e) result
  (** [to_result ~none o] is [Ok v] if [o] is [Some v] and [Error none]
      otherwise. *)

  val to_list : 'a option -> 'a list
  (** [to_list o] is [[]] if [o] is [None] and [[v]] if [o] is [Some v]. *)
end

(** Result values (as in [4.08])

    Except for the function of {{!exn}this section}
    that's the [Result] module that went into [4.08]. *)
module Result : sig

  (** {1:results Results} *)

  type ('a, 'e) t = ('a, 'e) result = Ok of 'a | Error of 'e (** *)
  (** The type for result values. Either a value [Ok v] or an error
      [Error e]. *)

  val ok : 'a -> ('a, 'e) result
  (** [ok v] is [Ok v]. *)

  val error : 'e -> ('a, 'e) result
  (** [error e] is [Error e]. *)

  val value : ('a, 'e) result -> default:'a -> 'a
  (** [value r ~default] is [v] if [r] is [Ok v] and [default] otherwise. *)

  val get_ok : ('a, 'e) result -> 'a
  (** [get_ok r] is [v] if [r] is [Ok v] and raises {!Invalid_argument}
      otherwise. *)

  val get_error : ('a, 'e) result -> 'e
  (** [get_error r] is [e] if [r] is [Error e] and raises {!Invalid_argument}
      otherwise. *)

  val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  (** [bind r f] is [f v] if [r] is [Ok v] and [r] if [r] is [Error _]. *)

  val join : (('a, 'e) result, 'e) result -> ('a, 'e) result
  (** [join rr] is [r] if [rr] is [Ok r] and [rr] if [rr] is [Error _]. *)

  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
  (** [map f r] is [Ok (f v)] if [r] is [Ok v] and [r] if [r] is [Error _]. *)

  val map_error : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
  (** [map_error f r] is [Error (f e)] if [r] is [Error e] and [r] if
      [r] is [Ok _]. *)

  val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c
  (** [fold ~ok ~error r] is [ok v] if [r] is [Ok v] and [error e] if [r]
      is [Error e]. *)

  val iter : ('a -> unit) -> ('a, 'e) result -> unit
  (** [iter f r] is [f v] if [r] is [Ok v] and [()] otherwise. *)

  val iter_error : ('e -> unit) -> ('a, 'e) result -> unit
  (** [iter_error f r] is [f e] if [r] is [Error e] and [()] otherwise. *)

  (** {1:preds Predicates and comparisons} *)

  val is_ok : ('a, 'e) result -> bool
  (** [is_ok r] is [true] iff [r] is [Ok _]. *)

  val is_error : ('a, 'e) result -> bool
  (** [is_error r] is [true] iff [r] is [Error _]. *)

  val equal :
    ok:('a -> 'a -> bool) -> error:('e -> 'e -> bool) -> ('a, 'e) result ->
    ('a, 'e) result -> bool
  (** [equal ~ok ~error r0 r1] tests equality of [r0] and [r1] using [ok]
      and [error] to respectively compare values wrapped by [Ok _] and
      [Error _]. *)

  val compare :
    ok:('a -> 'a -> int) -> error:('e -> 'e -> int) -> ('a, 'e) result ->
    ('a, 'e) result -> int
  (** [compare ~ok ~error r0 r1] totally orders [r0] and [r1] using [ok] and
      [error] to respectively compare values wrapped by [Ok _ ] and [Error _].
      [Ok _] values are smaller than [Error _] values. *)

  (** {1:exn Interacting with [Stdlib] exceptions} *)

  val to_failure : ('a, string) result -> 'a
  (** [to_failure r] is [failwith e] if [r] is [Error e] and [v]
      if [r] is [Ok v]. *)

  val catch_failure : (unit -> 'a) -> ('a, string) result
  (** [catch_failure f] is [try Ok (f ()) with Failure e -> Error e] *)

  val catch_sys_error : (unit -> 'a) -> ('a, string) result
  (** [catch_sys_error f] is [try Ok (f ()) with Sys_error e -> Error e] *)

  (** {1:converting Converting} *)

  val to_option : ('a, 'e) result -> 'a option
  (** [to_option r] is [r] as an option, mapping [Ok v] to [Some v] and
      [Error _] to [None]. *)

  val to_list : ('a, 'e) result -> 'a list
  (** [to_list r] is [[v]] if [r] is [Ok v] and [[]] otherwise. *)
end

(** Characters (bytes in fact). *)
module Char : sig

  (** {1:stdlib_char Stdlib [Char]} *)

  include module type of Char

  (** {1:ascii Bytes as US-ASCII characters} *)

  (** US-ASCII character support.

      The following functions act only on US-ASCII code points, that
      is on the bytes in range \[[0x00];[0x7F]\]. The functions can be
      safely used on UTF-8 encoded strings, they will of course only
      deal with US-ASCII related matters.

      {b References.}
      {ul
      {- Vint Cerf.
      {{:http://tools.ietf.org/html/rfc20}
      {e ASCII format for Network Interchange}}. RFC 20, 1969.}} *)
  module Ascii : sig

    (** {1:digits Decimal and hexadecimal digits} *)

    val is_digit : char -> bool
    (** [is_digit c] is [true] iff [c] is an US-ASCII digit
        ['0'] ... ['9'], that is a byte in the range \[[0x30];[0x39]\]. *)

    val is_hex_digit : char -> bool
    (** [is_hex_digit c] is [true] iff [c] is an US-ASCII hexadecimal
        digit ['0'] ... ['9'], ['a'] ... ['f'], ['A'] ... ['F'],
        that is a byte in one of the ranges \[[0x30];[0x39]\],
        \[[0x41];[0x46]\], \[[0x61];[0x66]\]. *)

    val hex_digit_value : char -> int
    (** [hex_digit_value c] is the numerical value of a digit that
        satisfies {!is_hex_digit}. Raises {!Invalid_argument} if
        [is_hex_digit c] is [false]. *)

    val lower_hex_digit : int -> char
    (** [lower_hex_digit n] is an hexadecimal digit for the integer
        [n] truncated to its lowest 4 bits. *)

    val upper_hex_digit : int -> char
    (** [upper_hex_digit n] is an hexadecimal digit for the integer
        [n] truncated to its lowest 4 bits. *)

    (** {1:preds Predicates} *)

    val is_valid : char -> bool
    (** [is_valid c] is [true] iff [c] is an US-ASCII character,
        that is a byte in the range \[[0x00];[0x7F]\]. *)

    val is_upper : char -> bool
    (** [is_upper c] is [true] iff [c] is an US-ASCII uppercase
        letter ['A'] ... ['Z'], that is a byte in the range
        \[[0x41];[0x5A]\]. *)

    val is_lower : char -> bool
    (** [is_lower c] is [true] iff [c] is an US-ASCII lowercase
        letter ['a'] ... ['z'], that is a byte in the range
        \[[0x61];[0x7A]\]. *)

    val is_letter : char -> bool
    (** [is_letter c] is [is_lower c || is_upper c]. *)

    val is_alphanum : char -> bool
    (** [is_alphanum c] is [is_letter c || is_digit c]. *)

    val is_white : char -> bool
    (** [is_white c] is [true] iff [c] is an US-ASCII white space
        character, that is one of space [' '] ([0x20]), tab ['\t']
        ([0x09]), newline ['\n'] ([0x0A]), vertical tab ([0x0B]), form
        feed ([0x0C]), carriage return ['\r'] ([0x0D]). *)

    val is_blank : char -> bool
    (** [is_blank c] is [true] iff [c] is an US-ASCII blank character,
        that is either space [' '] ([0x20]) or tab ['\t'] ([0x09]). *)

    val is_graphic : char -> bool
    (** [is_graphic c] is [true] iff [c] is an US-ASCII graphic
        character that is a byte in the range \[[0x21];[0x7E]\]. *)

    val is_print : char -> bool
    (** [is_print c] is [is_graphic c || c = ' ']. *)

    val is_control : char -> bool
    (** [is_control c] is [true] iff [c] is an US-ASCII control character,
        that is a byte in the range \[[0x00];[0x1F]\] or [0x7F]. *)

    (** {1:case Casing transforms} *)

    val uppercase : char -> char
    (** [uppercase c] is [c] with US-ASCII characters ['a'] to ['z'] mapped
        to ['A'] to ['Z']. *)

    val lowercase : char -> char
    (** [lowercase c] is [c] with US-ASCII characters ['A'] to ['Z'] mapped
        to ['a'] to ['z']. *)
  end
end

(** Strings. *)
module String : sig

  (** {1:stdlib_string Stdlib [String]} *)

  include module type of String

  (** {1:strings Strings} *)

  val empty : string
  (** [empty] is [""]. *)

  val head : string -> char option
  (** [head s] if [Some s.[0]] if [s <> ""] and [None] otherwise. *)

  val of_char : char -> string
  (** [of_char c] is [c] as a string. *)

  (** {1:preds Predicates} *)

  val is_empty : string -> bool
  (** [is_empty s] is [equal empty s]. *)

  val is_prefix : affix:string -> string -> bool
  (** [is_prefix ~affix s] is [true] iff [affix.[i] = s.[i]] for
      all indices [i] of [affix]. *)

  val is_infix : affix:string -> string -> bool
  (** [is_infix ~affix s] is [true] iff there exists an index [j]
      such that for all indices [i] of [affix], [affix.[i] = s.[j+ 1]]. *)

  val is_suffix : affix:string -> string -> bool
  (** [is_suffix ~affix s] is true iff [affix.[i] = s.[m - i]] for all
      indices [i] of [affix] and with [m = String.length s - 1]. *)

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] is [true] iff for all indices [i] of [s], [p s.[i]
      = true]. *)

  val exists : (char -> bool) -> string -> bool
  (** [exists p s] is [true] iff there exists an index [i] of [s] with
      [p s.[i] = true]. *)

  (** {1:subs Extracting substrings} *)

  val subrange : ?first:int -> ?last:int -> string -> string
  (** [subrange ~first ~last s] are the consecutive bytes of [s] whose
      indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  (** {1:break Breaking} *)

  (** {2:break_mag Breaking with magnitudes} *)

  val take_left : int -> string -> string
  (** [take_left n s] are the first [n] bytes of [s]. This is [s] if
      [n >= length s] and [""] if [n <= 0]. *)

  val take_right : int -> string -> string
  (** [take_right n s] are the last [n] bytes of [s].  This is [s] if
      [n >= length s] and [""] if [n <= 0]. *)

  val drop_left : int -> string -> string
  (** [drop_left n s] is [s] without the first [n] bytes of [s]. This is [""]
      if [n >= length s] and [s] if [n <= 0]. *)

  val drop_right : int -> string -> string
  (** [drop_right n s] is [s] without the last [n] bytes of [s]. This is [""]
      if [n >= length s] and [s] if [n <= 0]. *)

  val break_left : int -> string -> string * string
  (** [break_left n v] is [(take_left n v, drop_left n v)]. *)

  val break_right : int -> string -> string * string
  (** [break_right n v] is [(drop_left n v, take_right n v)]. *)

  (** {2:break_pred Breaking with predicates} *)

  val keep_left : (char -> bool) -> string -> string
  (** [keep_left sat s] are the first consecutive [sat] statisfying
      bytes of [s]. *)

  val keep_right : (char -> bool) -> string -> string
  (** [keep_right sat s] are the last consecutive [sat] satisfying
      bytes of [s]. *)

  val lose_left : (char -> bool) -> string -> string
  (** [lose_left sat s] is [s] without the first consecutive [sat]
      satisfying bytes of [s]. *)

  val lose_right : (char -> bool) -> string -> string
  (** [lose_right sat s] is [s] without the last consecutive [sat]
      satisfying bytes of [s]. *)

  val span_left : (char -> bool) -> string -> string * string
  (** [span_left sat s] is [(keep_left sat s, lose_left sat s)]. *)

  val span_right : (char -> bool) -> string -> string * string
  (** [span_right sat s] is [(lose_right sat s, keep_right sat s)]. *)

  (** {2:break_sep Breaking with separators} *)

  val cut_left : sep:string -> string -> (string * string) option
  (** [cut ~sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the
      first match of the separator character [sep] or [None] if
      [sep] can't be matched in [s]. Matching starts from the
      left of [s].

      The invariant [l ^ sep ^ r = s] holds.

      @raise Invalid_argument if [sep] is the empty string. *)

  val cut_right : sep:string -> string -> (string * string) option
  (** [cut_right ~sep s] is like {!cut_left} but matching starts
      on the right of [s]. *)

  val cuts_left : ?drop_empty:bool -> sep:string -> string -> string list
  (** [cuts_left sep s] is the list of all substrings of [s] that are
      delimited by matches of the non empty separator string
      [sep]. Empty substrings are omitted in the list if [drop_empty]
      is [true] (defaults to [false]).

      Matching separators in [s] starts from the left of [s] ([rev] is
      [false], default) or the end ([rev] is [true]). Once one is
      found, the separator is skipped and matching starts again, that
      is separator matches can't overlap. If there is no separator
      match in [s], the list [[s]] is returned.

      The following invariants hold:
      {ul
      {- [concat ~sep (cuts ~drop_empty:false ~sep s) = s]}
      {- [cuts ~drop_empty:false ~sep s <> []]}}

      @raise Invalid_argument if [sep] is the empty string. *)

  val cuts_right : ?drop_empty:bool -> sep:string -> string -> string list
  (** [cuts_right sep s] is like {!cuts_left} but matching starts on the
      right of [s]. *)

  (** {1:fmt Formatting} *)

  val pp : string Fmt.t
  (** [pp ppf s] prints [s]'s bytes on [ppf]. *)

  val pp_dump : string Fmt.t
  (** [pp_dump ppf s] prints [s] as a syntactically valid OCaml string
      on [ppf]. *)

  (** {1:unique Uniqueness} *)

  val uniquify : string list -> string list
  (** [uniquify ss] is [ss] without duplicates, the list order is
      preserved. *)

  val unique :
    exists:(string -> bool) -> string -> (string, string) result
  (** [unique ~exist n] is [n] if [exists n] is [false] or [r = strf
      "%s~%d" n d] with [d] the smallest integer in \[[1];[1e9]\] such
      that [exists r] is [false] or an error if there is no such
      string. *)

  (** {1:suggesting Suggesting} *)

  val edit_distance : string -> string -> int
  (** [edit_distance s0 s1] is the number of single character edits (insertion,
      deletion, substitution) that are needed to change [s0] into [s1]. *)

  val suggest : ?dist:int -> string list -> string -> string list
  (** [suggest ~dist candidates s] are the elements of [candidates]
      whose {{!edit_distance}edit distance} is the smallest to [s] and
      at most at a distance of [dist] of [s] (defaults to [2]). If
      multiple results are returned the order of [candidates] is
      preserved. *)

  (** {1:escunesc (Un)escaping bytes}

      The following functions can only (un)escape a single byte.  See
      also {{!Ascii.escunesc}these functions} to convert a string to
      printable US-ASCII characters. *)

  val byte_escaper :
    (char -> int) -> (bytes -> int -> char -> int) -> string -> string
  (** [byte_escaper char_len set_char] is a byte escaper such that:
      {ul
      {- [char_len c] is the length of the unescaped byte [c] in the
         escaped form. If [1] is returned then [c] is assumed
         to be unchanged use {!byte_replacer} if that does not hold}
      {- [set_char b i c] sets an unescaped byte [c] to its escaped
         form at index [i] in [b] and returns the next writable
         index. [set_char] is called regardless if [c] needs to be
         escaped or not in the latter case {b you must} write [c] (use
         {!byte_replacer} if that is not the case). No bounds check
         need to be performed on [i] or the returned value.}}

      For any [b], [c] and [i] the invariant
      [i + char_len c = set_char b i c] must hold. *)

  val byte_replacer :
    (char -> int) -> (bytes -> int -> char -> int) -> string -> string
  (** [byte_replacer char_len set_char] is like {!byte_escaper} but
      a byte can be substituted by another one by [set_char]. *)

  exception Illegal_escape of int
  (** See {!unescaper}. *)

  val byte_unescaper :
    (string -> int -> int) -> (bytes -> int -> string -> int -> int) ->
    string -> (string, int) result
  (** [byte_unescaper char_len_at set_char] is a byte unescaper such that:
      {ul
      {- [char_len_at s i] is the length of an escaped byte at index
         [i] of [s]. If [1] is returned then the byte is assumed
         to be unchanged by the unescape, use {!byte_unreplace}
         if that does not hold.}
      {- [set_char b k s i] sets at index [k] in [b] the unescaped
         byte read at index [i] in [s] and returns the next readable
         index in [s]. [set_char] is called regardless of wheter the
         byte at [i] must be unescaped or not in the latter case {b
         you must} write s.[i] only (use {!byte_unreplacer} if that is
         not the case). No bounds check need to be performed on [k],
         [i] or the returned value.}}

      For any [b], [s], [k] and [i] the invariant [i + char_len_at s i
      = set_char b k s i] must hold.

      Both [char_len_at] and [set_char] may raise [Illegal_escape i]
      if the given index [i] has an illegal or truncated escape. The
      unescaper turns this exception into [Error i] if that happens. *)

  val byte_unreplacer :
    (string -> int -> int) -> (bytes -> int -> string -> int -> int) ->
    string -> (string, int) result
  (** [byte_unreplacer char_len_at set_char] is like {!byte_unscaper}
      except [set_char] can set a different byte whenever [char_len_at]
      returns [1]. *)

  (** {1:ascii US-ASCII strings} *)

  (** US-ASCII string support.

      The following functions act only on US-ASCII code points, that
      is on the bytes in range \[[0x00];[0x7F]\]. The functions can be
      safely used on UTF-8 encoded strings but they will, of course,
      only deal with US-ASCII related matters.

      {b References.}
      {ul
      {- Vint Cerf.
      {{:http://tools.ietf.org/html/rfc20}
      {e ASCII format for Network Interchange}}. RFC 20, 1969.}} *)
  module Ascii : sig

    (** {1:pred Predicates} *)

    val is_valid : string -> bool
    (** [is_valid s] is [true] iff only for all indices [i] of [s],
        [s.[i]] is an US-ASCII character, i.e. a byte in the range
        \[[0x00];[0x1F]\]. *)

    (** {1:case Casing transforms}

        The functions can be safely used on UTF-8 encoded strings;
        they will of course only deal with US-ASCII casings. *)

    val uppercase : string -> string
    (** [uppercase s] is [s] with US-ASCII characters ['a'] to ['z'] mapped
        to ['A'] to ['Z']. *)

    val lowercase : string -> string
    (** [lowercase s] is [s] with US-ASCII characters ['A'] to ['Z'] mapped
        to ['a'] to ['z']. *)

    val capitalize : string -> string
    (** [capitalize s] is like {!uppercase} but performs the map only
        on [s.[0]]. *)

    val uncapitalize : string -> string
    (** [uncapitalize s] is like {!lowercase} but performs the map only
        on [s.[0]]. *)

    (** {1:hex Converting to US-ASCII hexadecimal characters} *)

    val to_hex : string -> string
    (** [to_hex s] is the sequence of bytes of [s] as US-ASCII lowercase
        hexadecimal digits. *)

    val of_hex : string -> (string, int) result
    (** [of_hex h] parses a sequence of US-ASCII (lower or upper
        cased) hexadecimal digits from [h] into its corresponding byte
        sequence.  [Error n] is returned either with [n] an index in
        the string which is not a hexadecimal digit or the length of
        [h] if it there is a missing digit at the end. *)

    (** {1:escunesc Converting to printable US-ASCII characters} *)

    val escape : string -> string
    (** [escape s] escapes bytes of [s] to a representation that uses only
        US-ASCII printable characters. More precisely:
        {ul
        {- \[[0x20];[0x5B]\] and \[[0x5D];[0x7E]\] are left unchanged.
           These are the {{!Char.Ascii.is_print}printable} US-ASCII bytes,
           except ['\\'] ([0x5C]).}
        {- \[[0x00];[0x1F]\], [0x5C] and
           \[[0x7F];[0xFF]\] are escaped by an {e hexadecimal} ["\xHH"]
           escape with [H] a capital hexadecimal number. These bytes
           are the US-ASCII control characters, the non US-ASCII bytes
           and ['\\'] ([0x5C]).}}
        Use {!unescape} to unescape. The invariant
        [unescape (escape s) = Ok s] holds. *)

    val unescape : string -> (string, int) result
    (** [unescape s] unescapes from [s] the escapes performed by {!escape}.
        More precisely:
        {ul
        {- ["\xHH"] with [H] a lower or upper case hexadecimal number
           is unescaped to the corresponding byte value.}}
        Any other escape following a ['\\'] not defined above makes
        the function return [Error i] with [i] the index of the
        error in the string. *)

    val ocaml_string_escape : string -> string
    (** [ocaml_string_escape s] escapes the bytes of [s] to a representation
        that uses only US-ASCII printable characters and according to OCaml's
        conventions for [string] literals. More precisely:
        {ul
        {- ['\b'] ([0x08]) is escaped to ["\\b"] ([0x5C,0x62]).}
        {- ['\t'] ([0x09]) is escaped to ["\\t"] ([0x5C,0x74]).}
        {- ['\n'] ([0x0A]) is escaped to ["\\n"] ([0x5C,0x6E]).}
        {- ['\r'] ([0x0D]) is escaped to ["\\r"] ([0x5C,0x72]).}
        {- ['\"'] ([0x22]) is escaped to ["\\\""] ([0x5C,0x22]).}
        {- ['\\'] ([0x5C]) is escaped to ["\\\\"] ([0x5C],[0x5C]).}
        {- [0x20], [0x21], \[[0x23];[0x5B]\] and \[[0x5D];[0x7E]\] are
           left unchanged. These are the
           {{!Char.Ascii.is_print}printable} US-ASCII bytes, except
           ['\"'] ([0x22]) and ['\\'] ([0x5C]).}
        {- Remaining bytes are escaped by an {e hexadecimal} ["\xHH"]
           escape with [H] an uppercase hexadecimal number. These bytes
           are the US-ASCII control characters not mentioned above
           and non US-ASCII bytes.}}
        Use {!ocaml_unescape} to unescape. The invariant
        [ocaml_unescape (ocaml_string_escape s) = Ok s]
        holds. *)

    val ocaml_unescape : string -> (string, int) result
    (** [ocaml_unescape s] unescapes from [s] the escape sequences
        afforded by OCaml [string] and [char] literals. More precisely:
        {ul
        {- ["\\b"] ([0x5C,0x62]) is unescaped to ['\b'] ([0x08]).}
        {- ["\\t"] ([0x5C,0x74]) is unescaped to ['\t'] ([0x09]).}
        {- ["\\n"] ([0x5C,0x6E]) is unescaped to ['\n'] ([0x0A]).}
        {- ["\\r"] ([0x5C,0x72]) is unescaped to ['\r'] ([0x0D]).}
        {- ["\\ "] ([0x5C,0x20]) is unescaped to [' '] ([0x20]).}
        {- ["\\\""] ([0x5C,0x22]) is unescaped to ['\"'] ([0x22]).}
        {- ["\\'"] ([0x5C,0x27]) is unescaped to ['\''] ([0x27]).}
        {- ["\\\\"] ([0x5C],[0x5C]) is unescaped to ['\\'] ([0x5C]).}
        {- ["\xHH"] with [H] a lower or upper case hexadecimal number
           is unescaped to the corresponding byte value.}
        {- ["\\DDD"] with [D] a decimal number such that [DDD]
           is unescaped to the corresponding byte value.}
        {- ["\\oOOO"] with [O] an octal number is unescaped to the
           corresponding byte value.}}

        Any other escape following a ['\\'] not defined above makes
        the function return [Error i] with [i] the location of the
        error in the string. *)
  end

  (** {1:version Version strings} *)

  val to_version : string -> (int * int * int * string option) option
  (** [to_version] parses version strings of the form:
      {[
        "[v|V]major.minor[.patchlevel][+additional-info]"
      ]}
      into [(major, minor, patch, additional_info)] tuples. If no
      [patchlevel] is found [0] is used. *)

  val drop_initial_v : string -> string
  (** [drop_initial_v s] drops a leading ['v'] or ['V'] from [s]. *)

  (** {1:setmap Sets and maps} *)

  (** String sets. *)
  module Set : sig

    (** {1 String sets} *)

    include Set.S with type elt := string

    type elt = string

    val pp : ?sep:unit Fmt.t -> string Fmt.t -> t Fmt.t
    (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}). If the set is empty leaves [ppf]
        untouched. *)

    val pp_dump : t Fmt.t
    (** [pp_dump ppf ss] prints an unspecified representation of [ss] on
        [ppf]. *)
  end with type t = Set.Make(String).t

  (** String maps. *)
  module Map : sig

    (** {1 String maps} *)

    include Map.S with type key := string

    type key = string

    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)

    val of_list : (string * 'a) list -> 'a t
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    (** {1:add Additional adds} *)

    val add_to_list : string -> 'a -> 'a list t -> 'a list t
    (** [add k v m] is [m] with [k] mapping to [l] such that [l] is
        [v :: find k m] if [k] was bound in [m] and [[v]] otherwise. *)

    val add_to_set :
      (module Stdlib_set.S with type elt = 'a and type t = 'set) ->
      string -> 'a -> 'set t -> 'set t
    (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
        [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
        otherwise. *)

    (** {1:fmt Formatting} *)

    val pp : ?sep:unit Fmt.t -> (string * 'a) Fmt.t -> 'a t Fmt.t
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val pp_dump : 'a Fmt.t -> 'a t Fmt.t
    (** [pp_dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)

    val pp_dump_string_map : string t Fmt.t
    (** [pp_dump_string_map ppf m] prints an unspecified representation of the
        string map [m] on [ppf]. *)
  end with type 'a t = 'a Map.Make(String).t

  (** {1:var_subst Variable substitution} *)

  val subst_pct_vars : ?buf:Buffer.t -> string Map.t -> string -> string option
  (** [subst_pct_vars ~buf vars s] substitutes in [s] strings of the
      form [%%VAR%%] by the value of ["VAR"] in [vars] (if any).
      [None] is returned if no substitution was performed. *)
end

(** Lists. *)
module List : sig

  (** {1:stdlib_list Stdlib [List]} *)

  include module type of List

  (** {1:adds Additions} *)

  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** [find_map f l] is the first element of [l] such that
      [f v] is [Some r] or [None] otherwise.

      {b Note.} Available in 4.10. *)

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] applies [f] to the elements of [l]
      in order and keeps the results of the form [Some v].

      {b Note.} Available in 4.08. *)

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  (** [concat_map f l] maps [l] with [f] and concatenates
      the result. Tail recursive.

      {b Note.} Available in 4.10. *)

  val classify :
    ?cmp_elts:('a -> 'a -> int) ->
    ?cmp_classes:('b -> 'b -> int) -> classes:('a -> 'b list) -> 'a list ->
    ('b * 'a list) list
  (** [classify ~cmp_elts ~cmp_classes ~classes els] bins elements [els]
      into classes as determined by [classes]. [cmp_elts] is used to
      compare elements and [cmp_classes] to compare classes, both
      default to {!compare}. *)
end

(** File paths.

    A file system {e path} specifies a file or a directory in a file
    system hierarchy. It is made of three parts:

    {ol
    {- An optional, platform-dependent, volume.}
    {- An optional root directory separator {!dir_sep} whose presence
       distinguishes absolute paths (["/a"]) from {e relative} ones
       (["a"])}
    {- A non-empty list of {!dir_sep} separated segments. {e Segments}
       are non empty strings except for maybe the last one. The latter
       syntactically distinguishes {e directory paths} (["a/b/"]) from
       file paths (["a/b"]).}}

    The paths segments ["."] and [".."] are relative path segments
    that respectively denote the current and parent directory. The
    {{!basename}basename} of a path is its last non-empty segment if
    it is not a relative path segment or the empty string otherwise (e.g.
    on ["/"] or [".."]). *)
module Fpath : sig

  (** {1:segments Separators and segments} *)

  val dir_sep_char : char
  (** [dir_sep_char] is the platform dependent natural directory
      separator.  This is / on POSIX and \ on Windows. *)

  val dir_sep : string
  (** [dir_sep] is {!dir_sep_char} as a string. *)

  val has_dir_sep : string -> bool
  (** [has_dir_sep s] is [true] iff [s] contains {!dir_sep_char} (on
      Windows also if it contains ['/']). *)

  val is_seg : string -> bool
  (** [is_seg s] is [true] iff [s] does not contain a {!dir_sep_char}
      (on Windows also that it does not contain ['/']) or a null byte. *)

  val is_rel_seg : string -> bool
  (** [is_rel_seg s] is [true] iff [s] is a relative segment in other
      words either ["."] or [".."]. *)

  (** {1:paths Paths} *)

  type t
  (** The type for paths *)

  val v : string -> t
  (** [v s] is the string [s] as a path.

      {b Warning.} In code only use ["/"] as the directory separator
      even on Windows platforms (don't be upset, the module gives them
      back to you with backslashes).

      @raise Invalid_argument if [s] is not a {{!of_string}valid
      path}. Use {!of_string} to deal with untrusted input. *)

  val add_seg : t -> string -> t
  (** [add_seg p seg] if [p]'s last segment is non-empty this is
      [p] with [seg] added. If [p]'s last segment is empty, this is
      [p] with the empty segment replaced by [seg].

      @raise Invalid_argument if [is_seg seg] is [false]. *)

  val append : t -> t -> t
  (** [append p q] appends [q] to [p] as follows:
      {ul
      {- If [q] is {{!is_abs}absolute} or has a non-empty volume then
         [q] is returned.}
      {- Otherwise appends [q]'s segments to [p] using {!add_seg}.}} *)

  val ( / ) : t -> string -> t
  (** [p / seg] is [add_seg p seg]. Left associative. *)

  val ( // ) : t -> t -> t
  (** [p // p'] is [append p p']. Left associative. *)

  (** {1:dirpaths Directory paths}

      {b Note.} The following functions use syntactic semantic
      properties of paths. Given a path, these properties can be
      different from the ones your file system attributes to it. *)

  val is_dir_path : t -> bool
  (** [is_dir_path p] is [true] iff [p] syntactically represents
      a directory. This means that [p] is [.], [..] or ends
      with [/], [/.] or [/..]. *)

  val to_dir_path : t -> t
  (** [to_dir_path p] is [add_seg p ""]. It ensures that the resulting
      path syntactically represents a {{!is_dir_path}directory} and thus,
      if converted to a string, that it ends with a {!dir_sep}. *)

  val rem_empty_seg : t -> t
  (** [rem_empty_seg p] is [p] without an existing last empty segment
      when [p] is not a root path, ensuring the result has no trailing
      {!dir_sep} when converted to a string. *)

  (** {1:baseparent Basename and parent directory}

      {b Note.} The following functions use syntactic semantic
      properties of paths. Given a path, these properties can be
      different from the ones your file system attributes to it. *)

  val basename : ?no_ext:bool -> t -> string
  (** [basename p] is the last non-empty segment of [p] or the empty
      string otherwise. The latter occurs only on root paths and on
      paths whose last non-empty segment is a {{!is_rel_seg}relative
      segment}. If [no_ext] is [true] (default to [false]) the basename's
      {{!file_exts}multiple extension}, if any, is removed from the result. *)

  val parent : t -> t
  (** [parent p] is a {{!is_dir_path}directory path} that contains
      [p]. If [p] is a {{!is_root}root path} this is [p] itself.
      If [p] is in the current directory this is [./]. *)

  (** {1:prefix Strict prefixes and roots} *)

  val is_prefix : t -> t -> bool
  (** [is_prefix prefix p] is [true] iff [prefix] is a strict prefix
      of [p] that respects path segments. More formally iff the following
      two conditions hold:
      {ol
      {- [not Fpath.(equal (to_dir_path prefix) (to_dir_path p))]}
      {- [Fpath.(String.is_prefix (to_string (to_dir_path prefix)
         (to_string p)))] is [true]}}

      {b Warning.} By definition [is_prefix p p] is [false]. Note
      also that the prefix relation does not entail directory
      containement; for example [is_prefix (v "..")  (v "../..")]  holds. *)

  val rem_prefix : t -> t -> t option
  (** [rem_prefix prefix p] is:
      {ul
      {- [None] if {!is_prefix}[ prefix p] is [false].}
      {- [Some q] otherwise where [q] is [p] without the string prefix
         [Fpath.to_dir_path prefix]. This means that [q] is always
         relative, that it preserves [p]'s
         {{!is_dir_path}directoryness} and that [Fpath.(equal (prefix
         // q) p)] holds.}}

      {b Warning.} By definition [rem_prefix p p] is [None]. *)

  val drop_prefixed : t list -> t list
  (** [drop_prefixed ps] is [ps] without elements that have a
      {{!is_prefix}strict prefixes} in [ps]. The list order is
      preserved. Duplicates are not removed use {!uniquify} for
      this. *)

  val reroot : root:t -> dst:t -> t -> t
  (** [reroot ~root ~dst p] assumes [root] {{!is_prefix}prefixes} [p]
      removes the prefix and prepends [dst] to the result.

      @raise Invalid_argument if [root] is not a prefix of [src].
      In particular note that [p] cannot be [root]. *)

  val relative : to_dir:t -> t -> t
  (** [relative to_dir p] is [q] such that [to_dir // q] represents
      the same path as [p]. Note that [q] is not necessarily relative:
      if [to_dir] is relative and [p] is absolute [p] is returned.

      @raise Invalid_argument if path [to_dir] contains "..". *)

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

  val is_current_dir : t -> bool
  (** [is_current_dir p] is [true] iff [p] is either ["."] or ["./"]. *)

  val is_parent_dir : t -> bool
  (** [is_parent_dir p] is [true] iff [p] is either [".."] or ["../"]. *)

  val equal : t -> t -> bool
  (** [equal p0 p1] is true iff [p0] and [p1] are stringwise equal. *)

  val equal_basename : t -> t -> bool
  (** [equal_basename p0 p1] is [String.equal (basename p0) (basename p1)]. *)

  val compare : t -> t -> int
  (** [compare p0 p1] is a total order on paths compatible with {!equal}. *)

  (** {1:file_exts File extensions}

      The {e file extension} (resp. {e multiple file extension}) of a
      path segment is the suffix that starts at the last (resp. first)
      occurence of a ['.'] that is preceeded by at least one non ['.']
      character.  If there is no such occurence in the segment, the
      extension is empty.  With these definitions, ["."], [".."],
      ["..."] and dot files like [".ocamlinit"] or ["..ocamlinit"] have
      no extension, but [".emacs.d"] and ["..emacs.d"] do have one. *)

  type ext = string
  (** The type for file extensions, ['.'] separator included.  *)

  val get_ext : ?multi:bool -> t -> ext
  (** [get_ext p] is [p]'s {{!basename}basename} file extension or the empty
      string if there is no extension. If [multi] is [true] (defaults to
      [false]), returns the multiple file extension. *)

  val has_ext : ext -> t -> bool
  (** [has_ext ext p] is [true] iff
      [String.equal (get_ext p) e || String.equal (get_ext ~multi:true p) e]. *)

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

  val cut_ext : ?multi:bool -> t -> t * ext
  (** [cut_ext ?multi p] is [(rem_ext ?multi p, get_ext ?multi p)]. *)

  val ( + ) : t -> ext -> t
  (** [p + ext] is [add_ext p ext]. Left associative. *)

  val ( -+ ) : t -> ext -> t
  (** [p -+ ext] is [set_ext p ext]. Left associative. *)

  (** {1:converting Converting} *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the string [s] as a path. The following transformations
      are performed on the string:
      {ul
      {- On Windows any / ([0x2F]) occurence is converted to \ ([0x5C])}
      {- Non initial empty segments are suppressed; "a//b" becomes "a/b",
         "//a////b//" becomes "//a/b/", etc}}
      An error returned if [s] is [""] or if it contains a null byte. The
      error string mentions [s]. *)

  val to_string : t -> string
  (** [to_string p] is the path [p] as a string. The result can
      be safely converted back with {!v}. *)

  val to_uri_path : ?escape_space:bool -> t -> string
  (** [to_uri_path p] is the path [p] as an URI path. This is [p] with
      the system specific {!dir_sep_char} directory separator replaced
      by ['/'] and with the following characters percent encoded:
      ['%'], ['?'], ['#'], [' '] (unless [escape_space] is [false],
      defaults to [true]), and the US-ASCII
      {{!Char.Ascii.is_control}control characters}.

      {b Note.} In 2019, the standard definition of URIs is in a sorry
      state. Assuming [p] is UTF-8 encoded. It is {e believed} the
      above function should lead to an URI path component that can be
      parsed by HTML5's
      {{:https://dev.w3.org/html5/spec-LC/urls.html#parsing-urls}
      definition} of URI parsing. *)

(*
  val pp : t Fmt.t
  (** [pp ppf p] prints path [p] on [ppf] using {!Filename.quote}. *)
*)

  val pp_quoted : t Fmt.t
  (** [pp_quoted ppf p] prints path [p] on [ppf] using {!Filename.quote}. *)

  val pp_unquoted : t Fmt.t
  (** [pp_unquoted p] prints path [p] on [ppf] using {!to_string}. *)

  val pp_dump : t Fmt.t
  (** [pp_dump ppf p] prints path [p] on [ppf] using {!String.dump}. *)

  (** {1:unique Uniqueness} *)

  val uniquify : t list -> t list
  (** [uniquify ps] is [ps] without duplicates, the list order is
      preserved. *)

  (** {1:setmap Paths map and sets} *)

  type path = t

  (** Path sets. *)
  module Set : sig

    (** {1 Path sets} *)

    include Set.S with type elt := t
    type elt = path

    val pp : ?sep:unit Fmt.t -> path Fmt.t -> t Fmt.t
    (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
        [ppf]. Each element is formatted with [pp_elt] and elements
        are separated by [~sep] (defaults to
        {!Format.pp_print_cut}). If the set is empty leaves [ppf]
        untouched. *)

    val pp_set : t Fmt.t
    (** [pp_set ppf ss] prints an unspecified set-like representation
        of [ss] on [ppf] using {!Fpath.pp}. *)

    val pp_dump : t Fmt.t
    (** [pp_dump ppf ss] prints an unspecified representation of [ss] on
        [ppf] with {!Fpath.pp_dump}. *)
  end

  (** Path maps. *)
  module Map : sig

    (** {1 Path maps} *)

    include Map.S with type key := t
    type key = path

    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)

    val of_list : (path * 'a) list -> 'a t
    (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty
        bs]. *)

    (** {1:add Additional adds} *)

    val add_to_list : path -> 'a -> 'a list t -> 'a list t
    (** [add k v m] is [m] with [k] mapping to [l] such that [l] is
        [v :: find k m] if [k] was bound in [m] and [[v]] otherwise. *)

    val add_to_set :
      (module Stdlib_set.S with type elt = 'a and type t = 'set) ->
      path -> 'a -> 'set t -> 'set t
    (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
        [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
        otherwise. *)

    (** {1:fmt Formatting} *)

    val pp : ?sep:unit Fmt.t -> (path * 'a) Fmt.t -> 'a t Fmt.t
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
        [ppf]. Each binding is formatted with [pp_binding] and
        bindings are separated by [sep] (defaults to
        {!Format.pp_print_cut}). If the map is empty leaves [ppf]
        untouched. *)

    val pp_dump : 'a Fmt.t -> 'a t Fmt.t
    (** [pp_dump pp_v ppf m] prints an unspecified representation of [m] on
        [ppf] using [pp_v] to print the map codomain elements. *)
  end

  (** {1:sorts Sorts} *)

  val sort_by_parent : Set.t -> Set.t Map.t
  (** [sort_by_parent ps] maps elements of [ps] by their {!Fpath.parent}. *)

  val sort_by_ext : ?multi:bool -> Set.t -> Set.t String.Map.t
  (** [sort_by_ext ~multi ps] maps elements of [ps] by their extension as
      determined by {!Fpath.get_ext}[ ~multi]. *)

  (** {1:sp Search paths}

      A {e search path} is a list of paths separated by a designated
      separator. A well known search path is [PATH] in which executable
      binaries are looked up. *)

  val search_path_sep : string
  (** [search_path_sep] is the default platform specific separator for
      search paths, this is [";"] if {!Sys.win32} is [true] and [":"]
      otherwise. *)

  val list_of_search_path : ?sep:string -> string -> (t list, string) result
  (** [list_of_search_path ~sep s] parses [sep] separated file paths
      from [s]. [sep] is not allowed to appear in the file paths, it
      defaults to {!search_path_sep}. The order in the list
      matches the order from left to right in [s]. *)
end

(** Hash values and functions.

    The property we want from these functions is speed and collision
    resistance. Build correctness depends on the latter. *)
module Hash : sig

  (** {1:values Hash values} *)

  type t
  (** The type for hash values. All hash functions use this representation.
      It is not possible to distinguish them, except for their {!length}
      which might vary, or not. *)

  val nil : t
  (** [nil] is the only hash value of {!length} [0]. *)

  val length : t -> int
  (** [length h] is the length of [h] in bytes. *)

  (** {1:preds Predicate and comparisons} *)

  val is_nil : t -> bool
  (** [is_nil h] is [true] iff [h] is {!nil}. *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [compare h0 h1] is a total order on hashes compatible with {!equal}. *)

  (** {1:converting Converting} *)

  val to_bytes : t -> string
  (** [to_bytes h] is the sequence of bytes of [h]. *)

  val of_bytes : string -> t
  (** [of_bytes s] is the sequences of bytes of [s] as a hash value. *)

  val to_hex : t -> string
  (** [to_hex h] is {!String.Ascii.to_hex}[ (to_bytes h)]. *)

  val of_hex : string -> (t, int) result
  (** [of_hex s] is [Result.map of_bytes (]{!String.Ascii.of_hex}[ s)]. *)

  val pp : t Fmt.t
  (** [pp] formats using {!to_hex} or, if the hash is {!nil},
      formats ["nil"]. *)

  (** {1:funs Hash functions} *)

  (** The type for hash functions. *)
  module type T = sig

    (** {1:hash Hash function} *)

    val id : string
    (** [id] is an US-ASCII string identifying the hash function. *)

    val length : int
    (** [length] is the byte length of hashes produced by the function. *)

    val string : string -> t
    (** [string s] is the hash of [s]. *)

    val fd : Unix.file_descr -> t
    (** [fd fd] [mmap(2)]s and hashes the object pointed by [fd].
        @raise Sys_error if [mmap] fails. *)

    val file : Fpath.t -> (t, string) result
    (** [file f] is the hash of file [f]. *)
  end

  module Murmur3_128 : T
  (** [Murmur3_128] is the
      {{:https://github.com/aappleby/smhasher}MurmurHash3 128-bit} hash. *)

  module Xxh_64 : T
  (** [Xxh_64] is the {{:http://cyan4973.github.io/xxHash/}xxHash 64-bit}
      hash. *)

  val funs : unit -> (module T) list
  (** [funs ()] is the list of available hash functions. *)

  val add_fun : (module T) -> unit
  (** [add_fun m] adds [m] to the list returned by [funs]. *)

  val get_fun : string -> ((module T), string) result
  (** [get_fun id] is the hash function with identifier [id] or an
      error message. *)
end

(** Measuring time.

    Support to measure monotonic wall-clock time, CPU
    user and CPU system time. *)
module Time : sig

  (** {1:span Monotonic time spans} *)

  type span
  (** The type for non-negative monotonic time spans. They represent
      the difference between two clock readings with nanosecond precision
      (1e-9s). *)

  (** Time spans *)
  module Span : sig

    (** {1:span Time spans} *)

    type t = span
    (** See {!type:span}. *)

    val zero : span
    (** [zero] is a span of 0ns. *)

    val one : span
    (** [one] is a span of 1ns. *)

    val max : span
    (** [max_span] is a span of [2^64-1]ns. *)

    val add : span -> span -> span
    (** [add s0 s1] is [s0] + [s1]. {b Warning.} Rolls over on overflow. *)

    val abs_diff : span -> span -> span
    (** [abs_diff s0 s1] is the absolute difference between [s0] and [s1]. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : span -> span -> bool
    (** [equal s0 s1] is [s0 = s1]. *)

    val compare : span -> span -> int
    (** [compare s0 s1] orders span by increasing duration. *)

    (** {1:conv Conversions} *)

    val to_uint64_ns : span -> int64
    (** [to_uint64_ns s] is [s] as an {e unsigned} 64-bit integer nanosecond
        span. *)

    val of_uint64_ns : int64 -> span
    (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond span [u]
        as a span. *)

    val pp : span Fmt.t
    (** [pp] formats with {!Fmt.uint64_ns_span}. *)

    val pp_ns : span Fmt.t
    (** [pp_ns ppf s] prints [s] as an unsigned 64-bit integer nanosecond
        span. *)
  end

  (** {1:monotonic_counters Monotonic wall-clock time counters} *)

  type counter
  (** The type for monotonic wall-clock time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting from now on. *)

  val count : counter -> span
  (** [count c] is the monotonic time span elapsed since [c] was created. *)

  (** {1:cpu_span CPU time spans} *)

  type cpu_span
  (** The type for CPU execution time spans. *)

  val cpu_span :
    cpu_utime:span -> cpu_stime:span -> cpu_children_utime:span ->
    cpu_children_stime:span -> cpu_span
  (** [cpu_span ~cpu_utime ~cpu_stime ~cpu_children_utime
      ~cpu_children_stime] is a cpu span with the given fields. See
      accessors for semantics. *)

  val cpu_zero : cpu_span
  (** [cpu_zero] is zero CPU times. *)

  val cpu_utime : cpu_span -> span
  (** [cpu_utime_s cpu] is [cpu]'s user time in seconds. *)

  val cpu_stime : cpu_span -> span
  (** [cpu_stime_s cpu] is [cpu]'s system time in seconds. *)

  val cpu_children_utime : cpu_span -> span
  (** [cpu_utime_s cpu] is [cpu]'s user time in seconds for children
      processes. *)

  val cpu_children_stime : cpu_span -> span
  (** [cpu_utime_s cpu] is [cpu]'s system time in seconds for children
      processes. *)

  (** {1:cpu_counter CPU time counters} *)

  type cpu_counter
  (** The type for CPU time counters. *)

  val cpu_counter : unit -> cpu_counter
  (** [cpu_counter ()] is a counter counting from now on. *)

  val cpu_count : cpu_counter -> cpu_span
  (** [cpu_count c] are CPU times since [c] was created. *)
end

(** Command lines.

    Command line values specify the command line arguments given to
    tools spawns. In certain contexts the command line value is the
    full specification of the tool spawn, in this case the first
    element of the line defines the program to invoke. In other
    contexts the tool to invoke and its arguments are kept separate.

    {!examples}.

    {b B00 artefact.}  This module allows to {!unstamp} command
    arguments. Unstamped arguments have no special semantics as far as
    the command line is concerned they simply indicate that the
    argument value itself does not influence the outputs of the
    tool. Unstamped arguments do not appear in the command line
    {{!to_list_and_stamp}stamp} which is used to memoize tool
    spawns. A typical example of unstamped arguments are file paths to
    inputs: it's often the file contents not the actual file path that
    determines the tool output; beware though that some tool use both
    the file path contents and the actual file path in their
    outputs. *)
module Cmd : sig

  (** {1:cl Command lines} *)

  type t
  (** The type for command lines. A command line is a list of command
      line arguments. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is an empty list of arguments. *)

  val empty : t
  (** [empty] is an empty list of arguments. *)

  val arg : string -> t
  (** [arg a] is the argument [a]. *)

  val append : t -> t -> t
  (** [append l1 l2] appends arguments [l2] to [l1]. *)

  val unstamp : t -> t
  (** [unstamp l] indicates that arguments [l] do not influence the
      tool's invocation outputs. These arguments are omitted from
      the command line's {{!to_list_and_stamp}stamp}. *)

  (** {1:derived Derived combinators} *)

  val ( % ) : t -> string -> t
  (** [l % a] is [append l (arg a)]. *)

  val ( %% ) : t -> t -> t
  (** [l1 % l2] is [append l1 l2]. *)

  val if' : bool -> t -> t
  (** [if' cond l] is [l] if [cond] is [true] and {!empty} otherwise. *)

  val path : Fpath.t -> t
  (** [path p] is [arg (Fpath.to_string p)]. *)

  val args : ?slip:string -> string list -> t
  (** [args ?slip l] is a command line from the list of arguments [l].
      If [slip] is specified it is added on the command line before
      each element of [l]. *)

  val rev_args : ?slip:string -> string list -> t
  (** [rev_args ?slip l] is {!args}[ ?slip (List.rev l)]. *)

  val of_list : ?slip:string -> ('a -> string) -> 'a list -> t
  (** [of_list ?slip conv l] is {!args}[ ?slip (List.map conv l)]. *)

  val of_rev_list : ?slip:string -> ('a -> string) -> 'a list -> t
  (** [of_rev_list ?slip conv l] is {!args}[ ?slip (List.rev_map conv l)]. *)

  val paths : ?slip:string -> Fpath.t list -> t
  (** [paths ?slip ps] is {!of_list}[ ?slip Fpath.to_string ps]. *)

  val rev_paths : ?slip:string -> Fpath.t list -> t
  (** [rev_paths ?slip ps] is {!of_rev_list}[ ?slip Fpath.to_string ps]. *)

  (** {1:tool Tools} *)

  type tool = Fpath.t
  (** The type for command line tools. A command line tool is
      represented by a file path according to the POSIX convention for
      [exec(3)]. If it is made of a single segment, for example
      [Fpath.v "ocaml"], it represents a program name to be looked up
      via a search procedure; for example in the [PATH] environment
      variable. If it is a file path with multiple segments (POSIX
      would say if they contain a slash characters) the program is the
      file itself. *)

  val tool : t -> tool option
  (** [tool l] is [l]'s first element. This is [None] if the line is
      {!empty} or if the first element can't be parsed to a {!tool}. *)

  val set_tool : tool -> t -> t option
  (** [set_tool tool l] replaces [l]'s first element with [tool]. This
      is [None] if [l] is {!empty}. *)

  val get_tool : t -> tool
  (** [get_tool] is like {!tool} but raises {!Invalid_argument} in case
      of error. *)

  val pp_tool : tool Fmt.t
  (** [pp_tool] formats a tool for the TTY. *)

  (** {1:preds Predicates} *)

  val is_singleton : t -> bool
  (** [is_singleton l] is [true] iff [l] has a single argument. *)

  (** {1:converting Converting} *)

  val fold :
    arg:(string -> 'a) -> unstamp:('a -> 'a) -> append:('a -> 'a -> 'a) ->
    empty:'a -> t -> 'a
  (** [fold ~arg ~unstamp ~append ~empty l] folds over [l]'s structure. *)

  val iter_enc :
    arg:('a -> string -> unit) ->
    unstamp:('a -> unit) ->
    append:('a -> unit) ->
    empty:('a -> unit) -> 'a -> t -> unit

  val to_list : t -> string list
  (** [to_list l] converts [l] to a list of strings. *)

  val to_stamp : t -> string list
  (** [to_stamp l] is the sequence of stamped arguments. *)

  val to_list_and_stamp : t -> string list * string list
  (** [to_list_and_stamp l] is a [l] as a list of strings tuppled with
      its stamp: the sequence of stamped arguments. *)

  val to_string : t -> string
  (** [to_string l] converts [l] to a string that can be passed
      to the
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/system.html}
      [command(3)]} POSIX system call. *)

  val of_string : string -> (t, string) result
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

  val pp : t Fmt.t
  (** [pp ppf l] formats an unspecified representation of [l] on
      [ppf]. *)

  val pp_dump : t Fmt.t
  (** [pp_dump ppf l] dumps and unspecified representation of [l]
      on [ppf]. *)

  (** {1:examples Examples}
{[
let ls p = Cmd.(arg "ls" % "-a" % path p)
let tar archive dir =
  Cmd.(arg "tar" % "-cvf" %% unstamp (path archive) %% path dir)

let opam cmd = Cmd.(arg "opam" % cmd)
let opam_install pkgs = Cmd.(opam "install" %% args pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(arg "ocamlc" % "-c" % if' debug (arg "-g") %% path file)

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(if' profile (arg "-p")) in
  let debug = Cmd.(if' debug (arg "-g")) in
  let incs = Cmd.(unstamp (paths ~slip:"-I" incs)) in
  Cmd.(arg "ocamlopt" % "-c" %% debug %% profile %% incs %% unstamp (path file))
]} *)
end

(** OS interaction. *)
module Os : sig

  (** CPU information. *)
  module Cpu : sig
    val logical_count : unit -> int
    (** [logical_count ()] is the number of logical CPUs available
        on the running machine. *)
  end

  (** Environment variables. *)
  module Env : sig

    (** {1:var Variables} *)

    val find : empty_is_none:bool -> string -> string option
    (** [find ~empty_is_none name] is the value of the environment
        variable [name] in the current process environment, if
        defined. If [empty_is_none] is [true], [None] is returned if
        the variable value is the empty string. *)

    val find' :
      empty_is_none:bool -> (string -> ('a, string) result) -> string ->
      ('a option, string) result
    (** [find' ~empty_is_none parse name] is like {!find} but
        the value is parsed with [parse]. If the latter errors
        with [Error e], [Error (Fmt.str "%s env: %s" name e)]
        is returned. *)

    (** {1:env Process environement} *)

    type t = string String.Map.t
    (** The type for process environments. *)

    val empty : t
    (** [empty] is {!String.Map.empty}. *)

    val override : t -> by:t -> t
    (** [override env ~by:o] overrides the definitions in [env] by [o]. *)

    val current : unit -> (t, string) result
    (** [current ()] is the current process environment. *)

    (** {1:assign Process environments as assignments} *)

    type assignments = string list
    (** The type for environments as lists of strings of the form
        ["var=value"]. *)

    val current_assignments : unit -> (assignments, string) result
    (** [current_assignments ()] is the current process environment as
        assignments. *)

    val of_assignments : ?init:t -> string list -> (t, string) result
    (** [of_assignments ~init ss] folds over strings in [ss],
        {{!String.cut}cuts} them at the leftmost ['='] character and
        adds the resulting pair to [init] (defaults to {!empty}). If
        the same variable is bound more than once, the last one takes
        over. *)

    val to_assignments : t -> assignments
    (** [to_assignments env] is [env]'s bindings as assignments. *)
  end

  (** File system path operations.

      These functions operate on files and directories
      equally. Specific function operating on either kind of path are
      in the {!File} and {!Dir} modules. *)
  module Path : sig

    (** {1:existence Existence} *)

    val exists : Fpath.t -> (bool, string) result
    (** [exists p] is [Ok true] if [p] exists in the file system
        and [Ok false] otherwise. Symbolic links are followed. *)

    val must_exist : Fpath.t -> (unit, string) result
    (** [must_exist p] is [Ok ()] if [p] exists in the file system
        and an error otherwise. Symbolic links are followed. *)

    (** {1:renaming Deleting and renaming} *)

    val delete : recurse:bool -> Fpath.t -> (bool, string) result
    (** [delete ~recurse p] deletes [p] from the file system. If [p]
        is a symbolic link this only deletes the link, not the linked
        object. If [recurse] is [true] and [p] is a non-empty
        directory, no error occurs, its contents is recursively
        deleted.  The result is:
        {ul
        {- [Ok true], if [p] existed and was deleted.}
        {- [Ok false], if the path [p] did not exist on the file system.}
        {- [Error _ ] in case of error, in particular if [p] is a non-empty
           directory and [recurse] is [false].}}
        See also {!File.delete}. *)

    val rename :
      force:bool -> make_path:bool -> src:Fpath.t -> Fpath.t ->
      (unit, string) result
    (** [rename ~force ~make_path ~src dst] renames [src] to [dst].
        {ul
        {- If [force] is [true] and [dst] exists it tries to delete it
           using {!File.delete}[ dst]. If [force] is [false]
           and [dst] exists the function errors.}
        {- If [make_path] is [true] and the parent directory of [dst] does
           not exist the whole path to the parent is created as needed
           with permission [0o755] (readable and traversable by everyone,
           writable by the user).}} *)

    (** {1:resolving Resolving} *)

    val realpath : Fpath.t -> (Fpath.t, string) result
    (** [realpath p] expands all symbolic links and resolves all
        references to [.] and [..] segments. The function errors if
        [p] does not exist. *)

    (** {1:copy Copying} *)

    val copy :
      ?rel:bool -> ?atomic:bool -> ?follow_symlinks:bool ->
      ?prune:(Unix.stats -> string -> Fpath.t -> bool) -> make_path:bool ->
      recurse:bool -> src:Fpath.t -> Fpath.t -> (unit, string) result
    (** [copy ~make_path ~recurse ~src dst] copies the file or file
        hierarchy rooted at [src] to [dst]. The function errors if
        [dst] exists. The semantics and arguments correspond to those
        of {!Os.Dir.copy}, except this function also works if [src] is
        not a directory. Note that [prune] is never called on [src]
        itself {b FIXME is that a good idea ?} also {b FIXME} this should
        error if [src] is a directory and [recurse] is false.

        See also {!Os.Dir.copy} and {!Os.File.copy}. *)

    (** {1:stat_mode File mode and stat}

        See also {!File.is_executable}. *)

    val get_mode : Fpath.t -> (int, string) result
    (** [get_mode p] is the file mode of [p]. Symbolic links are followed. *)

    val set_mode : Fpath.t -> int -> (unit, string) result
    (** [set_mode file p] sets the file mode of [file] to [p]. Symbolic
        links are followed. *)

    val stat : Fpath.t -> (Unix.stats, string) result
    (** [stat p] is [p]'s file information. Symbolic links are followed. *)

    (** {1:symlinks Symbolic links}

        For hard links see {!File.hard_links}. *)

    val symlink :
      force:bool -> make_path:bool -> src:Fpath.t -> Fpath.t ->
      (unit, string) result
    (** [symlink ~force ~src p] symbolically links [src] to [p].
        {ul
        {- If [force] is [true] and [p] exists it tries to delete it
           using {!File.delete}[ p]. If [force] is [false]
           and [p] exists the function errors.}
        {- If [make_path] is [true] and the parent directory of [file] does
           not exist the whole path to the parent is created as needed
           with permission [0o755] (readable and traversable by everyone,
           writable by the user).}} *)

    val symlink_link : Fpath.t -> (Fpath.t, string) result
    (** [symlink_link p] is [Ok l] if [p] is a symbolic link to [l]. *)

    val symlink_stat : Fpath.t -> (Unix.stats, string) result
    (** [symlink_stat p] is like {!stat} but if [p] is a symlink returns
        information about the link itself. If [p] is not a symlink then
        this is {!stat}. *)

    (** {1:tmppaths Temporary paths} *)

    type tmp_name = (string -> string, unit, string) format
    (** The type for temporary file name patterns. The string format
        is replaced by random hexadecimal US-ASCII characters. *)

    val tmp :
      ?make_path:bool -> ?dir:Fpath.t -> ?name:tmp_name -> unit ->
      (Fpath.t, string) result
    (** [tmp ~make_path ~dir name ()] is a file system path in [dir] that
        did not exist when the name was found. It may exist once the function
        returns though, prefer temporary {{!File.tmpfiles}files} and
        {{!Dir.tmpdirs}directories} creation functions to guarantee the
        creation of the temporary objects.
        {ul
        {- [name] is used to construct the filename of the file,
           see {!type:tmp_name} for details. It defaults to ["tmp-%s"].}
        {- [dir] is the directory in which the temporary file is created.
           It defaults to {!Os.Dir.default_tmp}[ ()].}
        {- If [make_path] is [true] (default) and [dir] does not exist the
           whole path to it is created as needed with permission [0o755]
           (readable and traversable by everyone, writable by the user).}} *)
  end

  (** Regular file operations.

      This module operates on regular files, most functions error if
      they are applied to other file kinds. *)
  module File : sig

    (** {1:paths Famous file paths} *)

    val null : Fpath.t
    (** [null] represents a file on the OS that discards all writes
        and returns end of file on reads. *)

    val dash : Fpath.t
    (** [dash] is ["-"]. This value is used by {!read} and {!write} to
        respectively denote {!stdin} and {!stdout}. *)

    (** {1:existence Existence} *)

    val exists : Fpath.t -> (bool, string) result
    (** [exists file] is [Ok true] if [file] is a regular file in the
        file system and [Ok false] otherwise. Symbolic links are
        followed. *)

    val must_exist : Fpath.t -> (unit, string) result
    (** [must_exist file] is [Ok ()] if [file] is a regular file in
        the file system and an error otherwise. Symbolic links are
        followed. *)

    val is_executable : Fpath.t -> bool
    (** [is_executable file] is [true] iff [file] exists and is executable. *)

    (** {1:delete_truncate Deleting and truncating} *)

    val delete : Fpath.t -> (bool, string) result
    (** [delete file] deletes file [file] from the file system. If
        [file] is a symbolic link this only deletes the link, not the
        linked file. The result is:
        {ul
        {- [Ok true], if [file] existed and was deleted.}
        {- [Ok false], if the path [file] did not exist on the file system.}
        {- [Error _] in case of error and in particular if [file] is a
           directory.}}
        See also {!Path.delete}. *)

    val truncate : Fpath.t -> int -> (unit, string) result
    (** [trunacte file size] truncates [file] to [size]. *)

    (** {1:hard_links Hard links}

        For symbolic links see {!Path.symlinks}. *)

    val link :
      force:bool -> make_path:bool -> src:Fpath.t -> Fpath.t ->
      (unit, string) result
    (** [link ~force ~src p] hard links file path [p] to the file [src].
        {ul
        {- If [force] is [true] and [p] exists an attempt to delete
           it is performed with {!File.delete}[ p]. If [force] is [false]
           and [p] exists the function errors.}
        {- If [make_path] is [true] and the parent directory of [p] does
           not exist the whole path to the parent is created as needed
           with permission [0o755] (readable and traversable by everyone,
           writable by the user).}} *)

    (** {1:reads Reading} *)

    val read_with_fd :
      Fpath.t -> (Unix.file_descr -> 'b) -> ('b, string) result
    (** [read_with_ic file f] opens [file] as a file descriptor [fdi]
        and returns [Ok (f ic)]. If [file] is {!dash}, [ic] is
        {!stdin}.  After the function returns (normally or via an
        exception raised by [f]), [ic] is ensured to be closed, except
        if it is {!stdin}. The function errors if opening [file]
        fails. Errors have the form [Fmt.str "%s: %s" file err]. *)

    val read_with_ic : Fpath.t -> (in_channel -> 'b) -> ('b, string) result
    (** [read_with_ic file f] is exactly like {!read_with_fd} but
        opens an OCaml input channel. *)

    val read : Fpath.t -> (string, string) result
    (** [read file] is [file]'s content as a string. If [file] is
        {!dash} the contents of {!stdin} is read. {b Warning.} The
        signature of this function limits files to be at most
        {!Sys.max_string_length} in size. On 32-bit platforms this is
        {b only around [16MB]}. Errors have the form
        [Fmt.str "%s: %s" file err]. *)

    (** {1:writes Writing and copying} *)

    val write_with_fd :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool -> Fpath.t ->
      (Unix.file_descr -> ('a, 'b) result) -> (('a, 'b) result, string) result
    (** [write_with_fd ~atomic ~mode ~force ~make_path file f] opens
        an output file descriptor [fdo] to write to [file] and returns
        [Ok (f fdo)].  If [file] is {!dash}, [fdo] is
        {!Unix.stdout}. After the function returns (normally or via an
        exception) [fdo] is ensured to be closed except if it is
        {!Unix.stdout}.
        {ul
        {- If [make_path] is [true] and the parent directory of [file]
           does not exist the whole path to the parent is created as
           needed with permission [0o755] (readable and traversable by
           everyone, writable by the user).}
        {- If [force] is [true] and [file] exists at call time as a
           regular file it tries to overwrite it, in all other cases
           the function errors if [file] exists.}
        {- [mode] are the permissions of the written file; they default to
           [0o644], readable by everyone, writable by the user.}
        {- If [atomic] is [true] (default) and the function or [f]
           errors [file] is left untouched. To write atomically, a
           temporary file [t] in the parent directory of [file] is
           created. On write success [t] is renamed to [file]; an
           operation which is {e more or less} atomic. On error [t] is
           deleted and [file] left intact.  This means the user needs
           write permissions in the parent directory of [file], in
           practice this is almost always the case but fails for some
           directories (e.g. writing to [/sys] on Linux®).
           {b XXX} An improvement would be to automatically disable
           [atomic] on non {!Unix.S_REG} files at the cost of a [stat(2)].}} *)

    val write_with_oc :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool -> Fpath.t ->
      (out_channel -> ('a, 'b) result) -> (('a, 'b) result, string) result
    (** [write_with_oc ~atomic ~mode ~force ~make_path file f] operates like
        {!write_with_fd} but opens an OCaml channel. *)

    val write :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool -> Fpath.t ->
      string -> (unit, string) result
    (** [write ~atomic ~mode ~force ~make_path file s] operates like
        {!write_with_fd} but directly writes [s] to [file]. *)

    val copy :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
      src:Fpath.t -> Fpath.t -> (unit, string) result
    (** [copy ~atomic ~mode ~force ~path ~make_path ~src file]
        operates like {!write_with_fd} but directly writes the content
        of [src] (or {!stdin} if [src] is {!dash}) to [file]. [mode] defaults
        to the permissions of [src] if available and [0o644] otherwise. *)

    (** {1:tmpfiles Temporary files} *)

    val with_tmp_fd :
      ?flags:Unix.open_flag list -> ?mode:int -> ?make_path:bool ->
      ?dir:Fpath.t -> ?name:Path.tmp_name ->
      (Fpath.t -> Unix.file_descr -> 'b) -> ('b, string) result
    (** [with_tmp_fd ~flags ~mode ~make_path ~dir ~name f] opens an output file
        descriptor [fdo] to a temporary file and returns [Ok (f fdo)].
        After the function returns (normally or via an exception) [fdo] is
        ensured to be closed and the temporary file is deleted.
        {ul
        {- [name] is used to construct the filename of the file,
           see {!type:tmp_name} for details. It defaults to ["tmp-%s"].}
        {- [dir] is the directory in which the temporary file is created.
           It defaults to {!Dir.default_tmp ()}.}
        {- If [make_path] is [true] (default) and [dir] doesn't exist the
           whole path to it is created as needed with permission [0o755]
           (readable and traversable by everyone, writable by the user).}
        {- [mode] are the permissions of the written file; they
           default to [0o600], only readable and writeable by the user}
        {- [flags] are the flags used to open the file.  They default
           to [Unix.[O_WRONLY; O_CREAT; O_EXCL; O_SHARE_DELETE;
           O_CLOEXEC]]}} *)

    val open_tmp_fd :
      ?flags:Unix.open_flag list -> ?mode:int -> ?make_path:bool ->
      ?dir:Fpath.t -> ?name:Path.tmp_name -> unit ->
      (Fpath.t * Unix.file_descr, string) result
    (** [open_tmp_fd] is like {!with_tmp_fd} except it is the client's
        duty to close the file descriptor and delete the file (if the
        file is not deleted it will be when the program exits). *)

    val with_tmp_oc :
      ?flags:Unix.open_flag list -> ?mode:int -> ?make_path:bool ->
      ?dir:Fpath.t -> ?name:Path.tmp_name -> (Fpath.t -> out_channel -> 'b) ->
      ('b, string) result
    (** [with_tmp_oc] is like {!with_tmp_fd} but uses an OCaml output channel
        instead of a file decriptor. *)
  end

  (** Directory operations.

      This module operates on directories, most functions error if
      they are applied to other file kinds. *)
  module Dir : sig

    (** {1:existence Existence} *)

    val exists : Fpath.t -> (bool, string) result
    (** [exists dir] is [Ok true] if [dir] is a directory in the file system
        and [Ok false] otherwise. Symbolic links are followed. *)

    val must_exist : Fpath.t -> (unit, string) result
    (** [must_exist dir] is [Ok ()] if [dir] is a directory in the file system
        and an error otherwise. Symbolic links are followed. *)

    (** {1:create_delete Creating} *)

    val create : ?mode:int -> make_path:bool -> Fpath.t -> (bool, string) result
    (** [create ~mode ~make_path dir] creates the directory [dir].
        {ul
        {- [mode] are the file permission of [dir]. They default to
           [0o755] (readable and traversable by everyone, writeable by the
           user).}
        {- If [make_path] is [true] and the parent directory of [p] does not
           exist the whole path to the parent is created as needed with
           permission [0o755]
           (readable and traversable by everyone, writable by the user)}}
        The result is:
        {ul
        {- [Ok true] if [dir] did not exist and was created.}
        {- [Ok false] if [dir] did exist as (possibly a symlink to) a
           directory. In this case the mode of [dir] and any other
           directory is kept unchanged.}
        {- [Error _] otherwise and in particular if [dir] exists as a
           non-directory.}} *)

    (** {1:content Contents} *)

    val fold :
      ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
      ?prune:(Unix.stats -> string -> Fpath.t -> 'a -> bool) -> recurse:bool ->
      (Unix.stats -> string -> Fpath.t -> 'a -> 'a) -> Fpath.t -> 'a ->
      ('a, string) result
    (** [fold ~rel ~dotfiles ~follow_symlinks ~prune ~recurse f dir
        acc] folds [f] over the contents of [dir] starting with
        [acc]. If [dir] does not exist the function errors.
        Paths given to [prune] and [f] do not have a trailing [/].
        {ul
        {- [f st name p acc] is called with each path [p] folded over
           with [st] its stat information, [name] its filename and [acc]
           the accumulator.}
        {- If [recurse] is [true] sub-directories [dir] are
           folded over recursively modulo [prune] (see below). If [recurse]
           is false only the direct contents of [dir] is folded over.}
        {- [prune] is called only when [recurse] is [true] as [prune st d]
           with [d] any sub-directory to be folded over and [st] its stat
           information. If the result is [true] [d] and its contents
           are not folded over. Defaults to [fun _ _ _ _ -> false]}
        {- [follow_symlinks] if [true] (default), symbolic links
           are followed. If [false] symbolic links are not followed
           and the stat information given to [prune] and [f] is
           given by {!Path.symlink_stat}.}
        {- If [dotfiles] is [false] (default) elements whose filename start
           with a [.] are not folded over}
        {- If [rel] is [false] (default) the paths given to [f] and [prune]
           have [dir] prepended, if [true] they are relative to [dir].}}

        {b Fold order.} The fold order is generally undefined. The only
        guarantee is that directory paths are folded over before their
        content.

        {b Warning.} Given the raciness of the POSIX file API it
        cannot be guaranteed that really all existing files will be
        folded over in presence of other processes. *)

    val fold_files :
      ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
      ?prune:(Unix.stats -> string -> Fpath.t -> 'a -> bool) -> recurse:bool ->
      (Unix.stats -> string -> Fpath.t -> 'a -> 'a) -> Fpath.t -> 'a ->
      ('a, string) result
    (** [fold_files] is like {!fold} but [f] is only applied to
        non-directory files. *)

    val fold_dirs :
      ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
      ?prune:(Unix.stats -> string -> Fpath.t -> 'a -> bool) -> recurse:bool ->
      (Unix.stats -> string -> Fpath.t -> 'a -> 'a) -> Fpath.t -> 'a ->
      ('a, string) result
    (** [fold_dirs] is like {!fold} but [f] is only applied
        to directory files. *)

    val path_list :
      Unix.stats -> string -> Fpath.t -> Fpath.t list -> Fpath.t list
    (** [path_list] is a {{!fold}folding} function to get a (reverse w.r.t.
        list of paths). Paths which are directories satisfy
        {!Fpath.is_dir_path}. *)

    (** {1:copy Copying} *)

    val copy :
      ?rel:bool -> ?atomic:bool -> ?follow_symlinks:bool ->
      ?prune:(Unix.stats -> string -> Fpath.t -> bool) -> make_path:bool ->
      recurse:bool -> src:Fpath.t -> Fpath.t -> (unit, string) result
    (** [copy ~rel ~atomic ~prune ~follow_symlinks ~make_path ~recurse
        ~src dst] copies the directory [src] to [dst]. File modes of
        [src] and its contents are preserved in [dst]. The function
        errors if [dst] exists.
        {ul
        {- If [recurse] is [true] sub-directories of [dir] are also
           copied recursively, unless they are [prune]d (see below).
           If [false] only the files of [src] are copied modulo [prune].
           {b FIXME} I think this is weird}
        {- If [make_path] is [true] and the parent directory of [dst]
           does not exist the whole path to the parent is created as
           needed with permission [0o755] (readable and traversable by
           everyone, writable by the user).}
        {- [prune st name p] is called on each path [p] to copy
           with [st] its stat information and [name] its filename.
           If the function returns [true] the directory or file is not
           copied over. Defaults to [fun _ _ _ -> false].}
        {- If [follow_symlinks] is [true] (default), symlinks are followed.
           If [false] symbolic links are not followed, the actual
           symlinks are copied and the stat information given to [prune]
           is given by {!Os.Path.symlink_stat}.}
        {- [atomic] if atomic is [true] and the function errors then
           [dst] should not exist. To write atomically, a temporary
           directory [t] in the parent directory of [dst] is created.
           On copy success [t] is renamed to [dst]. On error [t] is
           deleted and [dst] left intact.  This means the user needs
           write permissions in the parent directory of [dst], in
           practice this is almost always the case but fails for some
           directories (e.g. writing in [/sys] on Linux®).}
        {- If [rel] is [false] (default) the paths given to [prune]
           have [src] prepended. If [true] they are relative to
           [src].}} *)

    (** {1:cwd Current working directory (cwd)} *)

    val cwd : unit -> (Fpath.t, string) result
    (** [cwd ()] is the current working directory. The resulting path
        is guaranteed to be absolute. *)

    val set_cwd : Fpath.t -> (unit, string) result
    (** [set_cwd dir] sets the current working directory to [dir]. *)

    val with_cwd : Fpath.t -> (unit -> 'a) -> ('a, string) result
    (** [with_cwd dir f] is [f ()] with the current working directory
        bound to [dir]. After the function returns the current working
        directory is back to its initial value. *)

    (** {1:tmp_default Default temporary directory} *)

    val default_tmp : unit -> Fpath.t
    (** [default_tmp ()] is a default directory that can be used
        as a default directory for
        creating {{!File.tmpfiles}temporary files} and
        {{!tmpdirs}directories}. If {!set_default_tmp} hasn't been
        called this is:
        {ul
        {- On POSIX, the value of the [TMPDIR] environment variable or
           [Fpath.v "/tmp"] if the variable is not set or empty.}
        {- On Windows, the value of the [TEMP] environment variable or
           [Fpath.v "."] if it is not set or empty.}} *)

    val set_default_tmp : Fpath.t -> unit
    (** [set_default_tmp p] sets the value returned by {!default_tmp} to
        [p]. *)

    (** {1:tmpdirs Temporary directories} *)

    val with_tmp :
      ?mode:int -> ?make_path:bool -> ?dir:Fpath.t -> ?name:Path.tmp_name ->
      (Fpath.t -> 'a) -> ('a, string) result
    (** [with_tmp ~mode ~make_path ~dir ~name f] creates a temporary empty
        directory [t] and returns Ok (f t). After the function returns
        (normally or via an exception) [t] and its content are deleted.
        {ul
        {- [name] is used to construct the filename of the directory,
           see {!type:File.tmp_name} for details. It defaults to
           ["tmp-%s"].}
        {- [dir] is the directory in which the temporary file is created.
           It defaults to {!Dir.default_tmp ()}.}
        {- If [make_path] is [true] (default) and [dir] doesn't exist the
           whole path to it is created as needed with permission [0o755]
           (readable and traversable by everyone, writable by the user).}
        {- [mode] are the permissions of the temporary directory; they
           default to [0o700], only readable, writeable and traversable
           by the user}} *)

    val tmp :
      ?mode:int -> ?make_path:bool -> ?dir:Fpath.t -> ?name:Path.tmp_name ->
      unit -> (Fpath.t, string) result
    (** [tmp] is like {!with_tmp} except the directory and its content
        is only deleted at the end of program execution if the client
        doesn't do it before. *)

    (** {1:base Base directories}

        The directories returned by these functions are not guaranteed
        to exist. *)

    val user : unit -> (Fpath.t, string) result
    (** [user ()] is the home directory of the user executing the
        process.  Determined by consulting [passwd] database with the
        user if of the process. If this fails falls back to parse
        a path from the [HOME] environment variables. On Windows
        no special fallback is implemented. *)

    val config : unit -> (Fpath.t, string) result
    (** [config ()] is the directory used to store user-specific program
        configurations. This is in order:
        {ol
        {- If set the value of [XDG_CONFIG_HOME].}
        {- If set and on Windows® the value of [APPDATA].}
        {- If [user ()] is [Ok home], [Fpath.(home / ".config")].}} *)

    val data : unit -> (Fpath.t, string) result
    (** [data ()] is the directory used to store user-specific program
        data. This is in order:
        {ol
        {- If set the value of [XDG_DATA_HOME].}
        {- If set and on Windows® the value of [APPDATA].}
        {- If [user ()] is [Ok home], [Fpath.(home / ".local" / "share")].}} *)

    val cache : unit -> (Fpath.t, string) result
    (** [cache ()] is the directory used to store user-specific
        non-essential data. This is in order:
        {ol
        {- If set the value of [XDG_CACHE_HOME].}
        {- If set and on Windows® the value of [%TEMP%]}
        {- If [user ()] is [Ok home], [Fpath.(home / ".cache")]}} *)

    val runtime : unit -> (Fpath.t, string) result
    (** [runtime ()] is the directory used to store user-specific runtime
        files. This is in order:
        {ol
        {- If set the value of [XDG_RUNTIME_HOME].}
        {- The value of {!default_tmp}.}} *)
  end

  (** File descriptors operations. *)
  module Fd : sig

    val unix_buffer_size : int
    (** [unix_buffer_size] is the value of the OCaml runtime
        system buffer size for I/O operations. *)

    val apply :
      close:(Unix.file_descr -> unit) -> Unix.file_descr ->
      (Unix.file_descr -> 'a) -> 'a
    (** [apply ~close fd f] calls [f fd] and ensure [close fd] is
        is called whenever the function returns. Any {!Unix.Unix_error}
        raised by [close fd] is ignored. *)

    val copy : ?buf:Bytes.t -> src:Unix.file_descr -> Unix.file_descr -> unit
    (** [copy ~buf ~src dst] reads [src] and writes it to [dst] using
        [buf] as a buffer; if unspecified a buffer of length
        {!unix_buffer_size} is created for the call. Raise {!Unix.Unix_error}
        if that happens *)

    val to_string : Unix.file_descr -> string
    (** [to_string fd] reads [fd] to a string. Raises {!Unix.Unix_error} in
        case of error. *)

    val read_file : string -> Unix.file_descr -> string
    (** [read_file fn fd] reads [fd] to a string assuming it is a file
        descriptor open on file path [fn].

        @raise Failure in case of error with an error message that
        mentions [fn]. *)
  end

  (** Executing commands. *)
  module Cmd : sig

    (** {1:search Tool search}

      {b Portability.} In order to maximize portability no [.exe]
      suffix should be added to executable names on Windows, tool
      search adds the suffix during the tool search procedure. *)

    val find_tool :
      ?search:Fpath.t list -> Cmd.tool -> (Fpath.t option, string) result
    (** [find_tool ~search tool] is the file path, if any, to the program
        executable for the tool specification [tool].
        {ul
        {- If [tool] has a single path segment. The [tool] file is
           searched, in list order, for the first matching executable
           file in the directories of [search]. These directories
           default to those that result from parsing [PATH] with
           {!Fpath.list_of_search_path}.}
        {- If [tool] has multiple path segments the corresponding file
           is simply tested for {{!File.is_executable}existence and
           executability}.  [Ok (Some tool)] is returned if that is
           case and [Ok None] otherwise.}} *)

    val must_find_tool :
      ?search:Fpath.t list -> Cmd.tool -> (Fpath.t, string) result
    (** [must_find_tool] is like {!find_tool} except it errors if [Ok None]
        is returned. *)

    val find_first_tool :
      ?search:Fpath.t list -> Cmd.tool list -> (Fpath.t option, string) result
    (** [find_first_tool] is the first tool that can be found in the list
        with {!find_tool}. *)

    val find :
      ?search:Fpath.t list -> Cmd.t -> (Cmd.t option, string) result
    (** [find ~search cmd] resolves [cmd]'s tool as {!find_tool} does. *)

    val must_find :
      ?search:Fpath.t list -> Cmd.t -> (Cmd.t, string) result
    (** [must_find ~search cmd] resolves [cmd]'s tool as {!must_find_tool}
        does. *)

    val find_first :
      ?search:Fpath.t list -> Cmd.t list -> (Cmd.t option, string) result
    (** [find_first ~search cmds] resolves [cmds]'s {!Cmd.too}s
        as {!find_first_tool} does. *)

    (** {1:statuses Process completion statuses} *)

    type status = [ `Exited of int | `Signaled of int ]
    (** The type for process exit statuses. *)

    val pp_status : status Fmt.t
    (** [pp_status] is a formatter for process exit statuses. *)

    val pp_cmd_status : (Cmd.t * status) Fmt.t
    (** [pp_cmd_status] is a formatter for command process exit statuses. *)

    (** {1:stdis Process standard inputs} *)

    type stdi
    (** The type for representing the standard input of a process. *)

    val in_string : string -> stdi
    (** [in_string s] is a standard input that reads the string [s]. *)

    val in_file : Fpath.t -> stdi
    (** [in_file f] is a standard input that reads from file [f]. *)

    val in_fd : close:bool -> Unix.file_descr -> stdi
    (** [in_fd ~close fd] is a standard input that reads from file
        descriptor [fd]. If [close] is [true], [fd] is closed after
        the process is spawn. *)

    val in_stdin : stdi
    (** [in_stdin] is [in_fd ~close:false Unix.stdin], a standard
        input that reads from the current process standard input. *)

    val in_null : stdi
    (** [in_null] is [in_file File.null]. *)

    (** {1:stdos Process standard outputs} *)

    type stdo
    (** The type for representing the standard output of a process. *)

    val out_file : ?mode:int -> force:bool -> make_path:bool -> Fpath.t -> stdo
    (** [out_file ~force ~make_path file] is a standard output that writes
        to file [file].
        {ul
        {- If [make_path] is [true] and the parent directory of [file]
           does not exist the whole path to the parent is created as
           needed with permission [0o755] (readable and traversable by
           everyone, writable by the user).}
        {- If [force] is [true] and [file] exists at call time as a
           regular file it tries to overwrite it, in all other cases
           the function errors if [file] exists.}
        {- [mode] are the permissions of the written file; they default to
           [0o644], readable by everyone, writable by the user.}} *)

    val out_fd : close:bool -> Unix.file_descr -> stdo
    (** [out_fd ~close fd] is a standard output that writes to file
        descriptor [fd]. If [close] is [true], [fd] is closed after
        the process spawn. *)

    val out_stdout : stdo
    (** [out_stdout] is [out_fd ~close:false Unix.stdout] *)

    val out_stderr : stdo
    (** [out_stderr] is [out_fd ~close:false Unix.stderr] *)

    val out_null : stdo
    (** [out_null] is [out_file File.null] *)

    (** {1:run Command execution} *)

    (** {2:run_block Blocking}

        These functions wait for the command to complete before
        proceeding. *)

    val run_status :
      ?env:Env.assignments -> ?cwd:Fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
      ?stderr:stdo -> Cmd.t -> (status, string) result
    (** [run_status ~env ~cwd ~stdin ~stdout ~stderr cmd] runs and
        waits for the completion of [cmd] in environment [env] with
        current directory [cwd] and standard IO connections [stdin],
        [stdout] and [stderr].
        {ul
        {- [env] defaults to {!Env.current_assignments}[ ()]}
        {- [cwd] defaults to {!Dir.cwd}[ ()]}
        {- [stdin] defaults to {!in_stdin}}
        {- [stdout] defaults to {!out_stdout}}
        {- [stderr] defaults to {!out_stderr}}} *)

    val run_status_out :
      ?env:Env.assignments -> ?cwd:Fpath.t -> ?stdin:stdi ->
      ?stderr:[`Stdo of stdo | `Out] -> trim:bool -> Cmd.t ->
      (status * string, string) result
    (** [run_status_out] is like {!run_status} except [stdout] is read
        from the process to a string. The string is {!String.trim}ed
        if [trim] is [true] (default). If [stderr] is [`Out] the
        process' [stderr] is redirected to [stdout] and thus read back
        in the string aswell. *)

    val run :
      ?env:Env.assignments -> ?cwd:Fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
      ?stderr:stdo -> Cmd.t -> (unit, string) result
    (** [run] is {!run_status} with non-[`Exited 0] statuses turned
        into errors via {!pp_cmd_status}. *)

    val run_out :
      ?env:Env.assignments -> ?cwd:Fpath.t -> ?stdin:stdi ->
      ?stderr:[`Stdo of stdo | `Out] -> trim:bool -> Cmd.t ->
      (string, string) result
    (** [run] is {!run_status_out} with non-[`Exited 0] statuses
        turned into errors via {!pp_cmd_status}. *)

    (** {2:spawn Non-blocking}

        {b Note.} In contrast to [waitpid(2)] the following API does
        not allow to collect {e any} child process completion. There
        are two reasons: first this is not supported on Windows,
        second this is anti-modular. *)

    type pid
    (** The type for process identifiers. *)

    val pid_to_int : pid -> int
    (** [pid_to_int pid] is the system identifier for process
        identifier [pid]. *)

    val spawn :
      ?env:Env.assignments -> ?cwd:Fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
      ?stderr:stdo -> Cmd.t -> (pid, string) result
    (** [spawn ~env ~cwd ~stdin ~stdout ~stderr cmd] spawns command
        [cmd] in environment [env] with current directory [cwd] and
        standard IO connections [stdin], [stdout] and [stderr]. [env]
        defaults to {!Env.current_assignments}[ ()], [cwd] to {!Dir.current}[
        ()], [stdin] to {!in_stdin}, [stdout] to {!out_stdout} and
        [stderr] to {!out_stderr}. *)

    val spawn_poll_status : pid -> (status option, string) result
    (** [spawn_poll_status pid] tries to collect the exit status of
        command spawn [pid]. If [block] is [false], [Ok None] is immediately
        returned if [pid] has not terinated yet. *)

    val spawn_wait_status : pid -> (status, string) result
    (** [spawn_wait_status] blocks and waits for [pid]'s termination status to
        become available. *)

    (** {2:tracing Tracing} *)

    type spawn_tracer =
      pid option -> Env.assignments option -> cwd:Fpath.t option -> Cmd.t ->
      unit
    (** The type for spawn tracers. Called with each blocking and
        non-blocking spawned command aswell as {!execv}. The function
        is given the process identifier of the spawn (or [None] in
        case of {!execv}), the environment if different from the
        program's one, the current working directory if different from
        the program's one and the actual command. *)

    val spawn_tracer_nop : spawn_tracer
    (** [spawn_tracer_nop] is a spawn tracer that does nothing.
        This is the initial spawn tracer. *)

    val spawn_tracer : unit -> spawn_tracer
    (** [tracer ()] is the current spawn tracer. Initially this is
        {!spawn_tracer_nop}. *)

    val set_spawn_tracer : spawn_tracer -> unit
    (** [set_tracer t] sets the current spawn tracer to [t]. *)

    (** {1:exec Executing files}

        {b Windows.} On Windows a program executing an [execv*]
        function yields back control to the terminal as soon as the
        child starts (vs. ends on POSIX). This entails all sorts of
        unwanted behaviours. To workaround this, the following
        function executes, on Windows, the file as a spawned child
        process which is waited on for completion via
        [waitpid(2)]. Once the child process has terminated the
        calling process is immediately [exit]ed with the status of the
        child. *)

    val execv :
      ?env:Env.assignments -> ?cwd:Fpath.t -> Fpath.t -> Cmd.t ->
      (unit, string) result
    (** [execv ~env ~cwd f argv] executes file [f] as a new process in
        environment [env] with [args] as the {!Sys.argv} of this
        process (in particular [Sys.argv.(0)] is the name of the
        program not the first argument to the program). The function
        only recturns in case of error. [env] defaults to
        {!B00.OS.Env.current_assignments}[ ()], [cwd] to {!Dir.current}[ ()]. *)
  end

  (** Signal exit hooks. *)
  module Sig_exit : sig

    val on_sigint :  hook:(unit -> unit) -> (unit -> 'a) -> 'a
    (** [on_sigint ~hook f] calls [f ()] and returns its value. If [SIGINT]
        is signalled during that time [hook] is called followed by [exit 130]
        – that is the exit code a [SIGINT] would produce.

        [on_sigint] replaces an existing signal handler for
        {!Sys.sigint} during time of the function call. It is restored
        when the function returns.

        {b Note.} Since {!exit} is called {!at_exit} functions are
        called if a [SIGINT] occurs during the function call. This is not
        the case on an unhandled [SIGINT]. *)
  end
end

(** Program log.

    Support for program logging. Not to be used by build logic.

    The module is modelled after {!Logs} logging, see
    {{!Logs.basics}this quick introduction}. It can be made
    to log on a {!Logs} source, see {{!logger}here}.

    {b FIXME} This should maybe moved to B00_ui. Make the doc self
    contained (cf. references to Logs). OTOH it's nice to simply open
    B00_std and be done. *)
module Log : sig

  (** {1:levels Reporting levels} *)

  type level = Quiet | App | Error | Warning | Info | Debug (** *)
  (** The type for reporting levels. They are meant to be used
      as follows:
      {ul
      {- [Quiet] doesn't report anything.}
      {- [App] can be used for the standard output or console
         of an application. Using this instead of [stdout] directly
         allows the output to be silenced by [Quiet] which may
         be desirable, or not.}
      {- [Error] is an error condition that prevents the program from
          running.}
      {- [Warning] is a suspicious condition that does not prevent
         the program from running normally but may eventually lead to
         an error condition.}
      {- [Info] is a condition that allows the program {e user} to
         get a better understanding of what the program is doing.}
      {- [Debug] is a condition that allows the program {e developer}
         to get a better understanding of what the program is doing.}} *)

  val level : unit -> level
  (** [level ()] is the current reporting level. The initial level
      is set to {!Warning}. *)

  val set_level : level -> unit
  (** [set_level l] sets the current reporting level to [l]. *)

  val pp_level : level Fmt.t
  (** [pp_level ppf l] prints and unspecified representation of [l]
      on [ppf]. *)

  val level_to_string : level -> string
  (** [level_to_string l] converts [l] to a string representation. *)

  val level_of_string : string -> (level, string) result
  (** [level_of_string s] parses a level from [s] according to the
      representation of {!level_to_string}. *)

  (** {1:func Log functions} *)

  type ('a, 'b) msgf =
    (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  (** The type for client specified message formatting functions. See
      {!Logs.msgf}.

      [header] interpretation is up to the reported but [None] should
      automatially output headers that depend on the level and [Some ""]
      should not output any header leaving full control to the client. *)

  type 'a log = ('a, unit) msgf -> unit
  (** The type for log functions. See {!Logs.log}. *)

  val msg : level -> 'a log
  (** See {!Logs.msg}. *)

  val quiet : 'a log
  (** [quiet] is [msg Quiet]. *)

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

  (** {2:result Logging [result] value [Error] messages} *)

  val if_error :
    ?level:level -> ?header:string -> use:'a -> ('a, string) result -> 'a
  (** [if_error ~level ~use v r] is:
      {ul
      {- [v], if [r] is [Ok v]}
      {- [use] and [e] is logged with [level] (defaults to [Error]), if
         [r] is [Error e].}} *)

  val if_error' :
    ?level:level -> ?header:string -> use:'a -> ('a, string) result ->
    ('a, 'b) result
  (** [if_error'] is {!if_error} wrapped by {!Result.ok}. *)

  val if_error_pp :
    ?level:level -> ?header:string -> 'b Fmt.t -> use:'a -> ('a, 'b) result ->
    'a
  (** [if_error_pp ~level pp ~use r] is
      {ul
      {- [v], if [r] is [Ok v].}
      {- [use] and [e] is logged with [level] (defaults to [Error]) using
         [pp], if [r] is [Error e].}} *)

  val if_error_pp' :
    ?level:level -> ?header:string -> 'b Fmt.t -> use:'a -> ('a, 'b) result ->
    ('a, 'b) result
  (** [if_error_pp'] is {!if_error_pp'} wrapped by {!Result.ok} *)

  (** {2:timings Timings logging} *)

  val time :
    ?level:level ->
    ('a -> (('b, Format.formatter, unit, 'a) format4 -> 'b) -> 'a) ->
    (unit -> 'a) -> 'a
  (** [time ~level m f] logs [m] with level [level] (defaults to
      [Info]) and the time [f ()] took as the log header.

      {b Note.} The current log level is determined after [f] has been
      called this means [f] can change it to affect the log
      operation. This allows [f] to be the main function of your
      program and let it set the log level. *)

  (** {2:spawns Spawn logging} *)

  val spawn_tracer : level -> Os.Cmd.spawn_tracer
  (** [spawn_tracer level] is a {{!B00_std.Os.Cmd.tracing}spawn tracer}
      that logs with level [level]. If [level] is {!Log.Quiet} this is
      {!B00_std.Os.Cmd.spawn_tracer_nop}. *)

  (** {1:monitoring Log monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level
      [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level
      [Warning]. *)

  (** {1:logger Logger}

      The following function allows to change the logging backend.
      Note that in this case {{!monitoring}monitoring} and
      {{!levels}level} functions are no longer relevant. *)

  type kmsg = { kmsg : 'a 'b. (unit -> 'b) -> level -> ('a, 'b) msgf -> 'b }
  (** The type for the basic logging function. The function is never
      invoked with a level of [Quiet]. *)

  val kmsg_nop : kmsg
  (** [nop_kmsg] is a logger that does nothing. *)

  val kmsg_default : kmsg
  (** [kmsg_default] is the default logger that logs messages on
      {!Fmt.stderr} except for {!Log.App} level which logs on
      {!Fmt.stdout}. *)

  val set_kmsg : kmsg -> unit
  (** [set_kmsg kmsg] sets the logging function to [kmsg]. *)
end

(** Random queue *)
module Rqueue : sig
  type 'a t
  (** The type for random queues with elements of type ['a]. *)

  val empty : ?rand:Random.State.t -> unit -> 'a t
  (** [emtpy ~rand ()] is an empty random queue using [rand] as random
      state (defaults to {!Random.State.make_self_init}). *)

  val add : 'a t -> 'a -> unit
  (** [add q v] adds [v] to the queue. *)

  val take : 'a t -> 'a option
  (** [take q] removes and returns a random element in [q] (if any). *)

  val length : 'a t -> int
  (** [length q] is the number of elements in [q]. *)
end

(** Binary coding of values. *)
module Bincode : sig

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

  val v : 'a enc -> 'a dec -> 'a t
  (** [v enc dec] is a decoder using [enc] to encode and [dec] to decode. *)

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
      message (defaults to {!Os.File.dash}). *)

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
  (** [enc_fpath] encodes an {!Fpath.t}. *)

  val dec_fpath : Fpath.t dec
  (** [dec_fpath] decodes an {!Fpath.t}. *)

  val fpath : Fpath.t t
  (** [fpath] is a coded for [!Fpath.t]. *)

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
  (** [enc_hash] encodes a {!Hash.t}. *)

  val dec_hash : Hash.t dec
  (** [dec_hash] decodes a {!Hash.t}. *)

  val hash : Hash.t t
  (** [hash] is a codec for {!Hash.t} *)

  (** {2:time_span [Time.span]} *)

  val enc_time_span : Time.span enc
  (** [enc_time_span] encodes a {!type:Time.span}. *)

  val dec_time_span : Time.span dec
  (** [dec_time_span] decodes a {!type:Time.span}. *)

  val time_span : Time.span t
  (** [time_span] is a codec for {!type:Time.span}. *)

  (** {2:time_span [Time.cpu_span]} *)

  val enc_time_cpu_span : Time.cpu_span enc
  (** [enc_time_cpu_span] encodes a {!type:Time.cpu_span}. *)

  val dec_time_cpu_span : Time.cpu_span dec
  (** [dec_time_cpu_span] decodes a {!type:Time.cpu_span}. *)

  val time_cpu_span : Time.cpu_span t
  (** [time_span] is a codec for {!type:Time.cpu_span}. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
