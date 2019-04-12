(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** S-expression support.

    Consult information about the {{!sexp_syntax}syntax} of s-expressions.

    Open this module to use it, this only introduces modules in your scope. *)

open B0_std
open B0_tlex

(** {1:sexp S-expressions} *)

(** S-expression definitions and codec.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)
module Sexp : sig

  (** {1:sexp S-expressions} *)

  type loc = Tloc.t
  (** The type for text locations. *)

  val loc_nil : loc
  (** [loc_nil] is an invalid input location. *)

  type t = [ `A of string * loc | `L of t list * loc ]
  (** The type for generic s-expression representations. Either an atom or
      a list. *)

  val atom : string -> t
  (** [atom a] is [`A (a, loc_nil)]. *)

  val list : t list -> t
  (** [list l] is [`L (l, loc_nil)]. *)

  val to_atom : t -> (string, string) result
  (** [to_atom s] extracts an atom from [s]. If [s] is a list an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val to_list : t -> (t list, string) result
  (** [to_list s] extracts a list from [s]. If [s] is an atom an
      error with the location formatted according to {!Tloc.pp} is returned. *)

  val get_atom : t -> string
  (** [get_atom s] is like {!to_atom} but @raise Invalid_argument if
      [s] is not an atom. *)

  val get_list : t -> t list
  (** [get_atom s] is like {!to_list} but @raise Invalid_argument if
      [s] is not an list. *)

  val loc : t -> loc
  (** [loc s] is [s]'s input location. *)

  val pp : t Fmt.t
  (** [pp] formats an s-expression. *)

  val pp_seq : t Fmt.t
  (** [pp_seq] formats an s-expression but if it is a list the
      outer list separators are not formatted in the output. *)

  (** {1:codec Codec} *)

  val of_string : ?file:Fpath.t -> string -> (t, string) result
  (** [of_string ?file s] parses from string [s] a {e sequence} of
      s-expressions.  [file] is the file for locations, defaults to
      [Fpath.v "-"]. The sequence is returned as a fake s-expression
      list that spans the whole string; note that this list does not
      exist syntactically in [s]. *)

  val to_string : t -> string
  (** [to_string s] encodes [s] to a sequence of s-expressions. If [s]
      is an s-expression list this wrapping list is not syntactically
      represented in the output (see also {!of_string}). *)
end

(** S-expression generation. *)
module Sexpg : sig

  (** {1:gen Generation} *)

  type t
  (** The type for generated s-expressions. *)

  val atom : string -> t
  (** [atom s] is [s] as an atom. *)

  type lyst
  (** The type for generated s-expression lists. *)

  val ls : lyst
  (** [ls] starts a list. *)

  val le : lyst -> t
  (** [le l] ends lists [l]. *)

  val el : t -> lyst -> lyst
  (** [el e l] is list [l] with [e] added at the end. *)

  val el_if : bool -> (unit -> t) -> lyst -> lyst
  (** [el cond v l] is [el (v ()) l] if [cond] is [true] and
      [l] otherwise. *)

  (** {1:derived Derived generators} *)

  val strf : ('a, Format.formatter, unit, t) format4 -> 'a
  (** [strf fmt ...] is an atom formatted according to [fmt]. *)

  val bool : bool -> t
  (** [bool b] is [strf "%b" b]. *)

  val int : int -> t
  (** [int i] is [strf "%d" i]. *)

  val float : float -> t
  (** [float f] is [strf "%g" f]. *)

  val float_hex : float -> t
  (** [float_hex f] is [strf "%h" f]. *)

  val string : string -> t
  (** [string s] is {!atom}. *)

  val fpath : Fpath.t -> t
  (** [fpath p] is [p] as an atom. *)

  val cmd : Cmd.t -> t
  (** [cmd c] is [c] as a list. *)

  val option : ('a -> t) -> 'a option -> t
  (** [option some o] is [o] as the [none] atom if [o] is
      [none] and a list starting with [some] atom followed by [some v]
      if [o] is [Some v]. *)

  val list : ('a -> t) -> 'a list -> t
  (** [list el l] is [l] as a list whose elements are generated using
      [el]. *)

  val sexp : Sexp.t -> t
  (** [sexp s] is the s-expression [s] as a generated value. *)

  (** {1:output Output} *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b g] adds the generated s-expression value [g] to [b]. *)

  val to_string : t -> string
  (** [to_string g] is the generated s-expression value [g] as a string. *)
end

(** S-expression queries.

    {b TODO} maybe we could expose a bit more options for error
    reporting. In particular the internal [path] type and a combinator
    in the vein of {!loc} to report back the path trace. *)
module Sexpq : sig

  (** {1:query Queries} *)

  type 'a t
  (** The type for s-expression queries. A query either fails or
      succeeds against a s-expression with a value of type ['a]. *)

  val query : 'a t -> Sexp.t -> ('a, string) result
  (** [query q s] is [Ok v] if the query [q] succeeds on [s] and a (multiline)
      [Error e] otherwise. *)

  (** {1:outcome Success and failure} *)

  val succeed : 'a -> 'a t
  (** [succeed v] is a query that succeeds with value [v] on any
      s-expression. *)

  val fail : string -> 'a t
  (** [fail msg] is a query that fails on any s-expression with
      message [msg]. Do not include position information in [msg]: this
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

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind q f] queries a s-expression with [q], applies the result to
      [f] and re-queries the s-expression with the result. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f q] is [app (succeed f) q]. *)

  (** {1:qsexp S-expression queries} *)

  val fold : atom:'a t -> list:'a t -> 'a t
  (** [fold ~atom ~list] queries atoms with [atom] and lists with [list]. *)

  val sexp : Sexp.t t
  (** [sexp] queries any s-expression and returns it. *)

  val loc : Sexp.loc t
  (** [loc] is [map Sexp.loc sexp]. *)

  val with_loc : 'a t -> ('a * Sexp.loc) t
  (** [with_loc q] queries with [q] an returns the result with the
      location of the s-expression. *)

  (** {1:qatom Atom queries}

      These queries fail on lists. *)

  val atom : string t
  (** [atom] queries an atom as a string. *)

  val enum : kind:string -> String.Set.t -> string t
  (** [enum ~kind ss] queries an atom for one of the element of [ss]
      and fails otherwise. [kind] is for the kind of elements in [ss],
      it used for error reporting. *)

  val enum_map : kind:string -> 'a String.Map.t -> 'a t
  (** [enum_map ~kind sm] queries an atom for it's map in [sm] and
      fails if the atom is not bound in [sm]. [kind] is for the kind
      of elements in [sm], it used for error reporting. *)

  val parsed_atom : kind:string -> (string -> ('a, string) result) -> 'a t
  (** [parsed_atom ~kind p] queries and atom and parses it with [p]. In
      case of [Error m] fails with message [m]. [kind] is the kind
      of value parsed, used for the error in case a list is found. *)

  val bool : bool t
  (** [bool] queries an atom for one of [true] or [false]. *)

  val int : int t
  (** [int] queries an atom for an integer value parsed with
      {!int_of_string}. *)

  val int32 : int32 t
  (** [int32] queries an atom for an integer value parsed with
      {!Int32.of_string}. *)

  val int64 : int64 t
  (** [int64] queries an atom for an integer value parsed with
      {!Int64.of_string}. *)

  val float : float t
  (** [float] queries an atom for a float value parsed with
      {!float_of_string}. *)

  val fpath : Fpath.t t
  (** [fpath] queries an atom for a file path value parsed with
      {!Fpath.of_string}. *)

  (** {1:qlist List queries}

      These queries fail on atoms. *)

  val is_empty : bool t
  (** [is_empty] queries a list for emptyness. *)

  val hd : 'a t -> 'a t
  (** [hd q] queries the head of a list with [q]. *)

  val tl : 'a t -> 'a t
  (** [tail q] queries the tail of a list with [q]. *)

  val fold_list : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b t
  (** [fold_list f q acc] queries the elements of a list from left to
      right with [q] and folds the result with [f] starting with
      [acc]. *)

  val list : 'a t -> 'a list t
  (** [list q] queries the elements of a list with [q]. *)

  (** {1:qdict Dictionary queries}

      Queries for s-expression dictionaries. A dictionary is a list of
      bindings. A binding is a list that starts with an atom key, the remaing
      elements are the binding's value. For example in this binding:
{v
(key v0 v1 ...)
v}
      The key is [key] and the value the possibly empty list [v0], [v1],
      ... of s-expressions. The value is always represented by a fake
      (doesn't exist syntactically) s-expression list whose location
      spans the whole binding expression. *)

  val key : string -> 'a t -> 'a t
  (** [key k q] queries the value of key [k] of a dictionary with [q].
      The query fails if [k] is not bound or on atoms. *)

  val key_opt : string -> 'a t -> 'a option t
  (** [key_opt k q] queries the optional key [k] of a dictionary. The
      query fails on atoms. *)

  val key_dom : validate:String.Set.t option -> String.Set.t t
  (** [key_dom validate] queries the key domain of a list of bindings.
      If [validate] is [Some dom], the fails if a key is not in
      [dom]. The query also fails if a binding is not well-formed.  *)

  val batom : 'a t -> 'a t
  (** [batom q] queries a singleton atom list with an atom query
      [q]. Useful for singleton {{!Sexp.dict}dictionary bindings}. In
      error reporting treats the list as if it doesn't exist
      syntactically which is the case in dictionary bindings. *)

  (** {1:ocaml OCaml datatype encoding queries} *)

  val option : 'a t -> 'a option t
  (** [option q] queries with [q] the value of an option represented
      according the encoding of {!Sexpg.option}. *)
end

(** {1:sexp_syntax S-expressions syntax}

    S-expressions are a general way of describing data via atoms
    (sequences of characters) and lists delimited by parentheses.
    Here are a few examples of s-expressions and their syntax:

{v
this-is-an_atom
(this is a list of seven atoms)
(this list contains (a nested) list)

; This is a comment
; Anything that follows a semi-colon is ignored until the next line

(this list ; has three atoms and an embeded ()
 comment)

"this is a quoted atom, it can contain spaces ; and ()"

"quoted atoms can be split ^
 across lines or contain Unicode esc^u\{0061\}pes"
v}

    We define the syntax of s-expressions over a sequence of
    {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode
    characters} in which all US-ASCII {{!Char.Ascii.is_control}control
    characters} except {{!whitespace}whitespace} are forbidden in
    unescaped form.

    {2:sexp S-expressions and sequences thereof}

    An {e s-expression} is either an {{!atoms}{e atom}} or a
    {{!lists}{e list}} of s-expressions interspaced with
    {{!whitespace}{e whitespace}} and {{!comments}{e comments}}. A {e
    sequence of s-expressions} is a succession of s-expressions
    interspaced with whitespace and comments.

    These elements are informally described below and finally made
    precise via an ABNF {{!grammar}grammar}.

    {2:whitespace Whitespace}

    Whitespace is a sequence of whitespace characters, namely, space
    [' '] (U+0020), tab ['\t'] (U+0009), line feed ['\n'] (U+000A),
    vertical tab ['\t'] (U+000B), form feed (U+000C) and carriage return
    ['\r'] (U+000D).

    {2:comments Comments}

    Unless it occurs inside an atom in quoted form (see below)
    anything that follows a semicolon [';'] (U+003B) is ignored until
    the next {e end of line}, that is either a line feed ['\n'] (U+000A), a
    carriage return ['\r']  (U+000D) or a carriage return and a line feed
    ["\r\n"] (<U+000D,U+000A>).

{v
(this is not a comment) ; This is a comment
(this is not a comment)
v}

    {2:atoms Atoms}

    An atom represents ground data as a string of Unicode characters.
    It can, via escapes, represent any sequence of Unicode characters,
    including control characters and U+0000. It cannot represent an
    arbitrary byte sequence except via a client-defined encoding
    convention (e.g. Base64 or {{!string_bytes}hex encoding}).

    Atoms can be specified either via an unquoted or a quoted form. In
    unquoted form the atom is written without delimiters. In quoted
    form the atom is delimited by double quote ['\"'] (U+0022)
    characters, it is mandatory for atoms that contain
    {{!whitespace}whitespace}, parentheses ['('] [')'], semicolons
    [';'], quotes ['\"'], carets ['^'] or characters that need to be
    escaped.

{v
abc        ; a token for the atom "abc"
"abc"      ; a quoted token for the atom "abc"
"abc; (d"  ; a quoted token for the atom "abc; (d"
""         ; the quoted token for the atom ""
v}

    For atoms that do not need to be quoted, both their unquoted and
    quoted form represent the same string; e.g. the string ["true"]
    can be represented both by the atoms {e true} and {e "true"}. The
    empty string can only be represented in quoted form by {e ""}.

    In quoted form escapes are introduced by a caret ['^']. Double
    quotes ['\"'] and carets ['^'] must always be escaped.

{v
"^^"             ; atom for ^
"^n"             ; atom for line feed U+000A
"^u\{0000\}"       ; atom for U+0000
"^"^u\{1F42B\}^""  ; atom with a quote, U+1F42B and a quote
v}

    The following escape sequences are recognized:
    {ul
    {- ["^ "] (<U+005E,U+0020>) for space [' '] (U+0020)}
    {- ["^\""] (<U+005E,U+0022>) for double quote ['\"'] (U+0022)
       {b mandatory}}
    {- ["^^"] (<U+005E,U+005E>) for caret ['^'] (U+005E) {b mandatory}}
    {- ["^n"] (<U+005E,U+006E>) for line feed ['\n'] (U+000A)}
    {- ["^r"] (<U+005E,U+0072>) for carriage return ['\r'] (U+000D)}
    {- ["^u{X}"] with [X] is from 1 to at most 6 upper or lower case
       hexadecimal digits standing for the corresponding
       {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode character}
         U+X.}
    {- Any other character except line feed ['\n'] (U+000A) or
       carriage return ['\r'] (U+000D), following a caret is an
       illegal sequence of characters. In the two former cases the
       atom continues on the next line and white space is ignored.}}

    An atom in quoted form can be split across lines by using a caret
    ['^'] (U+005E) followed by a line feed ['\n'] (U+000A) or a
    carriage return ['\r'] (U+000D); any subsequent
    {{!whitespace}whitespace} is ignored.

{v
"^
  a^
  ^ " ; the atom "a "
v}

    The character ['^'] (U+005E) is used as an escape character rather
    than the usual ['\\'] (U+005C) in order to make quoted Windows®
    file paths decently readable and, not the least, utterly please
    DKM.

    {2:lists Lists}

    Lists are delimited by left ['('] (U+0028) and right [')']
    (U+0029) parentheses. Their elements are s-expressions separated
    by optional {{!whitespace}whitespace} and
    {{!comments}comments}. For example:

{v
(a list (of four) expressions)
(a list(of four)expressions)
("a"list("of"four)expressions)
(a list (of ; This is a comment
four) expressions)
() ; the empty list
v}

    {2:grammar S-expression grammar}

    The following {{:https://tools.ietf.org/html/rfc5234}RFC 5234}
    ABNF grammar is defined on a sequence of
    {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode characters}.

{v
 sexp-seq = *(ws / comment / sexp)
     sexp = atom / list
     list = %x0028 sexp-seq %x0029
     atom = token / qtoken
    token = t-char *(t-char)
   qtoken = %x0022 *(q-char / escape / cont) %x0022
   escape = %x005E (%x0020 / %x0022 / %x005E / %x006E / %x0072 /
                    %x0075 %x007B unum %x007D)
     unum = 1*6(HEXDIG)
     cont = %x005E nl ws
       ws = *(ws-char)
  comment = %x003B *(c-char) nl
       nl = %x000A / %x000D / %x000D %x000A
   t-char = %x0021 / %x0023-0027 / %x002A-%x003A / %x003C-%x005D /
            %x005F-%x007E / %x0080-D7FF / %xE000-10FFFF
   q-char = t-char / ws-char / %x0028 / %x0029 / %x003B
  ws-char = %x0020 / %x0009 / %x000A / %x000B / %x000C / %x000D
   c-char = %x0009 / %x000B / %x000C / %x0020-D7FF / %xE000-10FFFF
v}

    A few additional constraints not expressed by the grammar:
    {ul
    {- [unum] once interpreted as an hexadecimal number must be a
       {{:http://unicode.org/glossary/#unicode_scalar_value}Unicode scalar
       value.}}
    {- A comment can be ended by the end of the character sequence rather
       than [nl]. }} *)

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
