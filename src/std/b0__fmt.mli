(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Textual formatters.

    Helpers for dealing with {!Format}. *)

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

val failwith_line : int -> ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [failwith_line fmt …] is [failwith ("%d:" ^^ fmt) …]. *)

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

val suffix_lines : suffix:string -> 'a t -> 'a t
(** [suffix_lines ~suffix pp] adds [suffix] to the lines formatted
    by [pp]. {b Note.} Not very compositional: first formats to a
    string, cuts the string and then formats the lines. *)

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

val field :
  ?label:string t -> ?sep:unit t -> string -> ('b -> 'a) -> 'a t -> 'b t
(** [field ~label ~sep l prj pp_v] pretty prints a labelled field value as
    [pf "@[<1>%a%a%a@]" label l sep () (using prj pp_v)]. [label] defaults
    to [st [`Yellow]] and [sep] to [any ":@ "]. *)

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

val uint32 : int32 t
(** [uint32] is [pf ppf "%lu"]. *)

val int32 : int32 t
(** [int32] is [pf ppf "%ld"]. *)

val int64 : int64 t
(** [int64] is [pf ppf "%Ld"]. *)

val uint64 : int64 t
(** [uint64] is [pf ppf "%Lu"]. *)

val float : float t
(** [float] is [pf ppf "%g"]. *)

val char : char t
(** [char] is {!Format.pp_print_char}. *)

val string : string t
(** [string] is {!Format.pp_print_string}. See also {!text_lines}. *)

val binary_string : string t
(** [binary_string] formats strings as ASCII hex. See also {!text_lines}. *)

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

val none : unit t
(** [none] is [any "<none>"]. *)

val option : ?none:unit t -> 'a t -> 'a option t
(** [option ~none pp_v] formats an option. The [Some] case uses
    [pp_v] and [None] uses [none] (defaults to {!nop}). *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result ~ok ~error] formats a result value. The [Ok _] case uses
    [ok] and the [Error _] case uses [error]. *)

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

(** {1:text_lines Text and lines} *)

val text : string t
(** [text] is {!Format.pp_print_text}. *)

val lines : string t
(** [lines] formats lines by replacing newlines (['\n']) in the string
    with calls to {!Format.pp_force_newline}. *)

val truncated : max:int -> string t
(** [truncated ~max] formats a string using at most [max]
    characters. If the string doesn't fit, it is truncated and ended
    with […] (U+2026) which does count towards [max]. *)

val ascii_char : char t
(** [ascii_char] prints {!Char.Ascii.is_print} characters
    and hex escapes for other characters. *)

val ascii_string : string t
(** [ascii_string] prints {!Char.Ascii.is_print} characters and [' ']
    and hex escapes for other characters. *)

val ascii_string_literal : string t
(** [ascii_string_literal] is like {!ascii_string} but between
    double quotes ['\"']. Double quotes are also escaped and CR an
    LF are escaped by ['\r'] and ['\n']. The result is a valid, single
    line, OCaml string. *)

val text_uchar : Uchar.t t
(** [text_uchar] formats an UTF-8 encoded Unicode character or
    its scalar value in [U+%04X] representation if [u] is in
    C0 control characters (U+0000-U+001F), C1 control characters
    (U+0080-U+009F), line separator (U+2028), paragraph separator
    (U+2029), left-to-right mark (U+200E) or right-to-left mark
    (U+200F). *)

val text_string : string t
(** [text_string] formats an UTF-8 encoded string but escapes: C0
    control characters (U+0000-U+001F), C1 control characters
    (U+0080-U+009F), line separator (U+2028) and paragraph separator
    (U+2029). Decoding errors are replaced by literal {!Uchar.rep}
    characters. *)

val text_string_literal : string t
(** [text_string_literal] is like {!text_string} but between
    double quotes ['\"']. Double quotes are also escaped an CR and LF
    are escaped by ['\r'] and ['\n']. The result is a valid, single line,
    OCaml string. *)

val styled_text_string : string t
(** [styled_text_string] is like {!text_string} but ANSI escape
    sequences are detected and output as zero width strings. *)

val styled_text_string_literal : string t
(** [styled_text_string_literal] combines {!text_string_literal}
    and {!ansi_styled_text_string}. *)

(** {1:mag Magnitudes} *)

val si_size : scale:int -> string -> int t
(** [si_size ~scale unit] formats a non negative integer
    representing unit [unit] at scale 10{^scale * 3}, depending on
    its magnitude, using power of 3
    {{:https://www.bipm.org/en/publications/si-brochure/chapter3.html}
    SI prefixes} (i.e. all of them except deca, hector, deci and
    centi). The output is UTF-8 encoded, it uses U+03BC for [µ] (10{^-6}).

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
    according to its magnitude using SI prefixes on seconds and
    non-SI units. Years are counted in Julian years (365.25
    SI-accepted days) as
    {{:http://www.iau.org/publications/proceedings_rules/units/}defined}
    by the International Astronomical Union.

    Rounds towards positive infinity, i.e. over approximates, no
    duration is formatter shorter than it is.

    The output is UTF-8 encoded, it uses U+03BC for [µs] (10{^-6}[s]). *)

(** {1:hci HCI fragments} *)

val and_enum : ?empty:unit t -> 'a t -> 'a list t
(** [and_enum ~empty pp_v ppf l] formats [l] according to its length.
    {ul
    {- [0], formats [empty] (defaults to {!nop}).}
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

(** {1:styling Text styling}

    Text styling control happens via ANSI escape sequences and is
    handled globally. If you want to make sure no escape sequences
    are produced disable them with {!set_styler}. Otherwise they can
    be selectively stripped on strings or formatters with the
    {!B0_std.String.strip_ansi_escapes} and {!strip_styles} functions. *)

type styler =
| Ansi (** Style with ANSI escapes. *)
| Plain (** No styles, plain text. *)
(** The kind of styler. *)

val set_styler : styler -> unit
(** [set_styler st] sets the global styler to [st]. See {!styler}. *)

val styler : unit -> styler
(** [styler] is the global styler. The initial styler is only
    set to [Plain] if the value of the [TERM] environment variable is
    set to [dumb] or if it undefined and the {!Sys.backend_type}
    is not [js_of_ocaml] (browser consoles support ANSI escapes). *)

val strip_styles : Format.formatter -> unit
(** [strip_styles ppf], this overrides the formatter's [out_string]
    function to strip out styling information. See also
    {!B0_std.String.strip_ansi_escapes}.

    {b Note.} This assumes [ppf] is not used to write data that uses
    raw [U+001B] characters. *)

type color =
  [ `Default
  | `Black
     (** {%html:<span style="background:rgb(0,0,0)">    </span>%} *)
  | `Black_bright
     (** {%html:<span style="background:rgb(85,85,85)">    </span>%} *)
  | `Red
     (** {%html:<span style="background:rgb(170,0,0)">    </span>%} *)
  | `Red_bright
     (** {%html:<span style="background:rgb(255,0,0)">    </span>%} *)
  | `Green
     (** {%html:<span style="background:rgb(0,170,0)">    </span>%} *)
  | `Green_bright
    (** {%html:<span style="background:rgb(85,255,85)">    </span>%} *)
  | `Yellow
     (** {%html:<span style="background:rgb(187,187,0)">    </span>%} *)
  | `Yellow_bright
     (** {%html:<span style="background:rgb(255,255,85)">    </span>%} *)
  | `Blue
     (** {%html:<span style="background:rgb(0,0,170)">    </span>%} *)
  | `Blue_bright
     (** {%html:<span style="background:rgb(85,85,255)">    </span>%} *)
  | `Magenta
     (** {%html:<span style="background:rgb(170,0,170)">    </span>%} *)
  | `Magenta_bright
     (** {%html:<span style="background:rgb(255,85,255)">    </span>%} *)
  | `Cyan
     (** {%html:<span style="background:rgb(0,170,170)">    </span>%} *)
  | `Cyan_bright
     (** {%html:<span style="background:rgb(85,255,255)">    </span>%} *)
  | `White
     (** {%html:<span style="background:rgb(170,170,170)">    </span>%} *)
  | `White_bright
     (** {%html:<span style="background:rgb(255,255,255)">    </span>%} *) ]
(** The type for colors.

    {b Note.} The actual color may differ in terminals.
    See for example the table
    {{:https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit}here}
    and check out the output of the [b0-sttyle] tool. *)

type style =
[ `Bold | `Faint | `Italic | `Underline | `Blink of [ `Slow | `Rapid ]
| `Reverse | `Fg of color | `Bg of color ]
(** The type for text styles. *)

val st : style list -> string t
(** [st styles ppf s] formats [s] on [ppf] styled by [styles]. *)

val st' : style list -> 'a t -> 'a t
(** [st' styles pp_v ppf v] formats [v] with [pp_v] on [ppf] styled
    by [styles]. *)

val code : string t
(** [code] is [st [`Bold]]. *)

val code' : 'a t -> 'a t
(** [code'] is [st' [`Bold]]. *)

val hey : string t
(** [hey] is [st [`Bold; `Fg `Red]]. *)

val puterr : unit t
(** [puterr] formats [Error:] with [[`Fg `Red]]. *)

val putwarn : unit t
(** [putwarn] formats [Warning:] with [[`Fg `Yellow]]. *)

val putnote : unit t
(** [putnote] formats [Note:] with [[`Fg `Blue]]. *)
