(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Strings.

    @canonical B0_std.String *)

include module type of String (** @closed *)

(** {1:strings Strings} *)

val head : string -> char option
(** [head s] if [Some s.[0]] if [s <> ""] and [None] otherwise. *)

val of_char : char -> string
(** [of_char c] is [c] as a string.

    {b Available} in OCaml 5.5. *)

(** {1:preds Predicates} *)

val is_empty : string -> bool
(** [is_empty s] is [true] if and only [s] is empty.

    {b Available} in OCaml 5.5. *)

val includes : affix:string -> string -> bool
(** [includes ~affix s] is [true] if and only if there exists an index
    [i] of [s] such that for all indices [k] of [affix], [affix.[k] =
    s.[i + k]].

    {b Note.} To test the same [affix] string multiple times, partially
    applying the [~affix] argument and using the resulting function repeatedly
    is more efficient. *)

(** {1:find_indices Finding indices} *)

val find_first_index : (char -> bool) -> ?start:int -> string -> int option
(** [find_first_index p ~start s] is the index of the first character
    of [s] that satisfies predicate [p] at or after the index or
    position [start] (defaults to [0]).

    If [start] is [length s], the result is always [None].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val find_last_index : (char -> bool) -> ?start:int -> string -> int option
(** [find_last_index p ~start s] is the index of the last character of
    [s] that satisfies predicate [p] at or before the index or
    position [start] (defaults to [length s]).

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

(** {1:find_subs Finding substrings}

    {b Note.} To find the same [sub] string multiple times, partially
    applying the [~sub] argument of these functions and using the
    resulting function repeatedly is more efficient *)

val find_first : sub:string -> ?start:int -> string -> int option
(** [find_first ~sub ~start s] is the starting position of the first
    occurrence of [sub] in [s] at or after the index or position [start]
    (defaults to [0]).

    If [sub] is [""] the result is [Some start]. The result of the
    function is always a valid index of [s] except when [sub] is
    [""] and [start] is [length s].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val find_last : sub:string -> ?start:int -> string -> int option
(** [find_last ~sub ~start s] is the starting position of the last
    occurrence of [sub] in [s] at or before the index or position
    [start] (defaults to [String.length s]).

    If [sub] is [""] the result is [Some start]. The result of the
    function is always a valid index of [s] except when [sub] is
    [""] and [start] is [length s].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val find_all :
  sub:string -> (int -> 'acc -> 'acc) -> ?start:int -> string -> 'acc -> 'acc
(** [find_all ~sub f ~start s acc], starting with [acc], folds [f]
    over all non-overlapping starting positions of [sub] in [s] at or
    after the index or position [start] (defaults to [0]). The
    result is [acc] if [sub] could not be found in [s].

    If [sub] is [""], [f] gets invoked on all positions of [s] at or after
    [start].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val rfind_all :
  sub:string -> (int -> 'acc -> 'acc) -> ?start:int -> string -> 'acc -> 'acc
(** [rfind_all ~sub f ~start s acc], starting with [acc], folds [f]
    over all non-overlapping starting positions of [sub] in [s] at or
    before the index or position [start] (defaults to [String.length
    s]). The result is [acc] if [sub] could not be found in [s].

    If [sub] is [""], [f] gets invoked on on all positions of [s] at
    or before [start].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

(** {1:replacing Replacing substrings}

    {b Note.} To replace the same [sub] string multiple times, partially
    applying the [~sub] argument of these functions and using the
    resulting function repeatedly is more efficient. *)

val replace_first : sub:string -> by:string -> ?start:int -> string -> string
(** [replace_first ~sub ~by ~start s] replaces by [by] the first
    occurrence of [sub] in [s] at or after the index or position
    [start] (defaults to [0]).

    If [sub] is [""], this inserts [by] at position [start].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val replace_last : sub:string -> by:string -> ?start:int -> string -> string
(** [replace_last ~sub ~by ~start s] replaces by [by] the last
    occurrence of [sub] in [s] at or after the index or position
    [start] (defaults to [String.length s]).

    If [sub] is [""], this inserts [by] at position [start].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val replace_all : sub:string -> by:string -> ?start:int -> string -> string
(** [replace_all ~sub ~by ~start s] replaces by [by] all non-overlapping
    occurrences of [sub] in [s] at or after the index or position [start]
    (defaults to [0]). Occurences are found in increasing indexing order.

    If [sub] is [""], this inserts [by] on all positions from [start] on.

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

(** {1:subs Extracting substrings} *)

val subrange : ?first:int -> ?last:int -> string -> string
(** [subrange ~first ~last s] are the consecutive bytes of [s] whose
    indices exist in the range \[[first];[last]\].

    [first] defaults to [0] and last to [String.length s - 1].

    Note that both [first] and [last] can be any integer. If [first
    > last] the interval is empty and the empty string is
    returned. *)

(** {1:splitting Splitting} *)

(** {2:splitting_mag Splitting with magnitudes}

    {b All additions available} in OCaml 5.5 *)

val take_first : int -> string -> string
(** [take_first n s] are the first [n] bytes of [s]. This is [s] if
    [n >= length s] and [""] if [n <= 0]. *)

val take_last : int -> string -> string
(** [take_last n s] are the last [n] bytes of [s].  This is [s] if
    [n >= length s] and [""] if [n <= 0]. *)

val drop_first : int -> string -> string
(** [drop_first n s] is [s] without the first [n] bytes of [s]. This is [""]
    if [n >= length s] and [s] if [n <= 0]. *)

val drop_last : int -> string -> string
(** [drop_last n s] is [s] without the last [n] bytes of [s]. This is [""]
    if [n >= length s] and [s] if [n <= 0]. *)

val cut_first : int -> string -> string * string
(** [cut_first n v] is [(take_first n v, drop_first n v)]. *)

val cut_last : int -> string -> string * string
(** [cut_last n v] is [(drop_last n v, take_last n v)]. *)

(** {2:splitting_pred Splitting with predicates}

    {b All additions available} in OCaml 5.5 *)

val take_first_while : (char -> bool) -> string -> string
(** [take_first_while sat s] are the first consecutive [sat] statisfying
    bytes of [s]. *)

val take_last_while : (char -> bool) -> string -> string
(** [take_last_while sat s] are the last consecutive [sat] satisfying
    bytes of [s]. *)

val drop_first_while : (char -> bool) -> string -> string
(** [drop_first_while sat s] is [s] without the first consecutive [sat]
    satisfying bytes of [s]. *)

val drop_last_while : (char -> bool) -> string -> string
(** [drop_last_while sat s] is [s] without the last consecutive [sat]
    satisfying bytes of [s]. *)

val cut_first_while : (char -> bool) -> string -> string * string
(** [cut_first_while sat s] is
    [(take_first_while sat s, drop_first_while sat s)]. *)

val cut_last_while : (char -> bool) -> string -> string * string
(** [cut_last_while sat s] is
    [(drop_last_while sat s, take_last_while sat s)]. *)

(** {2:splitting_sep Splitting with separators}

    {b Note.} To split the same [sep] string multiple times, partially
    applying the [~sep] argument of these functions and using the
    resulting function repeatedly is more efficient. *)

val split_first : sep:string -> string -> (string * string) option
(** [split_first ~sep s] is the pair [Some (left, right)] made of the
    two (possibly empty) substrings of [s] that are delimited by the
    first match of the separator [sep] in [s] or [None] if [sep] can't
    be found. Search for [sep] starts at position [0] and uses
    {!find_first}.

    If [sep] is [""], this is [Some ("", s)].

    The invariant [concat sep [left; right] = s] holds. *)

val split_last : sep:string -> string -> (string * string) option
(** [split_last ~sep s] is the pair [Some (left, right)] made of the
    two (possibly empty) substrings of [s] that are delimited by the
    last match of the separator [sep] in [s] or [None] if [sep] can't
    be found. Search for [sep] starts at position [length s] and uses
    {!find_last}.

    If [sep] is [""], this is [Some (s, "")].

    The invariant [concat sep [left; right] = s] holds. *)

val split_all : sep:string -> ?drop:(string -> bool) -> string -> string list
(** [split_all ~sep s] is the list of all substrings of [s] that are
    delimited by non-overlapping matches of the separator [sep] or the
    list [[s]] if [sep] can't be found. Search for [sep] starts at
    position [0] in increasing indexing order and uses {!find_all}.

    Substrings [sub] for which [drop sub] is [true] are not included
    in the result. [drop] defaults to [Fun.const false].

    If [sep] is [""], this is [[""; c0; …; cn; ""]] with [ci]
    the string [of_char s.[i]].

    The invariant [concat sep (split_all ~sep s) = s] holds. *)

val rsplit_all : sep:string -> ?drop:(string -> bool) -> string -> string list
(** [rsplit_all ~sep s] is the list of all substrings of [s] that are
    delimited by non-overlapping matches of the separator [sep] or
    [[s]] if [sep] can't be found. Search for [sep] starts at position
    [length s] in deacreasing indexing order and uses {!rfind_all}.

    Substrings [sub] for which [drop sub] is [true] are not included
    in the result. [drop] defaults to [Fun.const false].

    If [sep] is [""], this is [[""; c0; …; cn; ""]] with [ci]
    the string [of_char s.[i]].

    The invariant [concat sep (rsplit_all ~sep s) = s] holds. *)

(** {2:split_lines Splitting lines} *)

val fold_ascii_lines :
  drop_newlines:bool -> (int -> 'a -> string -> 'a) -> 'a -> string -> 'a
(** [fold_ascii_lines ~drop_newlines f acc s] folds over the lines of
    [s] by calling [f linenum acc' line] with [linenum] the one-based
    line number count, [acc'] the result of accumulating [acc] with
    [f] so far and [line] the data of the line (without the newline
    found in the data if [strip_newlines] is [true]).

    Lines are delimited by newline sequences which are either one of
    ["\n"], ["\r\n"] or ["\r"]. More precisely the function determines
    lines and line data as follows:

    {ul
    {- If [s = ""], the function considers there are no lines in [s] and
       [acc] is returned without [f] being called.}
    {- If [s <> ""], [s] is repeteadly split on the first newline sequences
       ["\n"], ["\r\n"] or ["\r"] into [(left, newline, right)], [left]
       (or [left ^ newline] when [strip_newlines = false]) is given to [f]
       and the process is repeated with [right] until a split can no longer
       be found. At that point this final string is given to [f] and the
       process stops.}} *)

val cut_ascii_newline : string -> string * string
(** [cut_ascii_newline s] is [(data, endline)] with:
    {ul
    {- [endline] either the suffix ["\n"], ["\r\n"] or ["\r"] of [s] or [""]
       if [s] has no such suffix.}
    {- [data] the bytes before [endline] such that [data ^ newline = s]}} *)

(** {2:tokenize Tokenize} *)

val take_token :
  ?is_sep:(char -> bool) -> ?is_token:(char -> bool) -> string -> string
(** [take_token ~is_sep ~is_token s] skips characters statisfying [is_sep]
    from [s], then gathers zero or more consecutive characters satisfiying
    [is_token] and returns the result. Effectively this is:

    {[take_first_while is_token (drop_first_while is_sep s)]}

    [is_sep] defaults to {!Char.Ascii.is_white} and [is_token] is
    {!Char.Ascii.is_graphic}. *)

val drop_token :
  ?is_sep:(char -> bool) -> ?is_token:(char -> bool) -> string -> string
(** [drop_token ~is_sep ~is_token] skips characters satisfiying [is_sep]
    from [s], then skips characters satisfying [is_token] and returns
    the result. Effectively this is:

    {[drop_first_while is_token (drop_first_while is_sep s)]}

    [is_sep] defaults to {!Char.Ascii.is_white} and [is_token] is
    {!Char.Ascii.is_graphic}. *)

val cut_token :
  ?is_sep:(char -> bool) -> ?is_token:(char -> bool) -> string ->
  string * string
(** [cut_token ~is_sep ~is_token s] skips characters satisfying
    [is_sep] from [s], then gather zero or more consecutive
    characters satisfying [is_token] into a string which is returned
    along the remaining characters after that. Effectively this is:

    {[cut_first_while is_token (drop_first_while is_sep s)]}

    [is_sep] defaults to
    {!Char.Ascii.is_white} and [is_token] is {!Char.Ascii.is_graphic}. *)

val tokens : ?is_sep:(char -> bool) -> string -> string list
(** [tokens s] are the strings separated by sequences of [is_sep]
    characters (default to {!Char.Ascii.is_white}). The empty list is
    returned if [s] is empty or made only of separators. *)

(** {1:unique Uniqueness} *)

val distinct : string list -> string list
(** [distinct ss] is [ss] without duplicates, the list order is
    preserved. *)

val unique : ?limit:int -> exists:(string -> bool) -> string -> string
(** [unique ~limit ~exist n] is [n] if [exists n] is [false] or [r = strf
    "%s~%d" n d] with [d] the smallest integer such that exists [r]
    if [false]. If no [d] in \[[1];[limit]\] satisfies the condition
    [Invalid_argument] is raised, [limit] defaults to [1e6]. *)

(** {1:spellchecking Spellchecking}

    {b All additions available} in OCaml 5.4 *)

val edit_distance : ?limit:int -> t -> t -> int
(** [edit_distance s0 s1] is the number of single character edits
    (understood as insertion, deletion, substitution, transposition)
    that are needed to change [s0] into [s1].

    If [limit] is provided the function returns with [limit] as soon
    as it was determined that [s0] and [s1] have distance of at least
    [limit]. This is faster if you have a fixed limit, for example for
    spellchecking.

    The function assumes the strings are UTF-8 encoded and uses {!Uchar.t}
    for the notion of character. Decoding errors are replaced by
    {!Uchar.rep}. Normalizing the strings to
    {{:https://unicode.org/glossary/#normalization_form_c}NFC} gives
    better results.

    {b Note.} This implements the simpler Optimal String Alignement (OSA)
    distance, not the Damerau-Levenshtein distance. With this function
    ["ca"] and ["abc"] have a distance of 3 not 2. *)

val spellcheck :
  ?max_dist:(string -> int) -> ((string -> unit) -> unit) -> string ->
  string list
(** [spellcheck iter_dict s] are the strings enumerated by the
    iterator [iter_dict] whose {{!edit_distance}edit distance} to [s]
    is the smallest and at most [max_dist s]. If multiple corrections
    are returned their order is as found in [iter_dict]. The default
    [max_dist s] is:

    {ul
    {- [0] if [s] has 0 to 2 Unicode characters.}
    {- [1] if [s] has 3 to 4 Unicode characters.}
    {- [2] otherwise.}}

    If your dictionary is a list [l], a suitable [iter_dict] is given
    by [(fun yield -> List.iter yield l)].

    All strings are assumed to be UTF-8 encoded, decoding
    errors are replaced by {!Uchar.rep} characters. *)

(** {1:escunesc (Un)escaping bytes}

    The following functions can only (un)escape a single byte.  See
    also {{!Ascii.escunesc}these functions} to convert a string to
    printable ASCII characters. *)

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
    [i + char_len c = set_char b i c] must hold.

    Here's a small example that escapes ['"'] by prefixing
    them by backslashes.
    double quotes from strings:
{[
let escape_dquotes s =
  let char_len = function '"' -> 2 | _ -> 1 in
  let set_char b i = function
  | '"' -> Bytes.set b i '\\'; Bytes.set b (i+1) '"'; i + 2
  | c -> Bytes.set b i c; i + 1
  in
  String.byte_escaper char_len set_char s
]}
*)

val byte_replacer :
  (char -> int) -> (bytes -> int -> char -> int) -> string -> string
(** [byte_replacer char_len set_char] is like {!byte_escaper} but
    a byte can be substituted by another one by [set_char]. *)

exception Illegal_escape of int
(** See {!byte_unescaper}. *)

val byte_unescaper :
  (string -> int -> int) -> (bytes -> int -> string -> int -> int) ->
  string -> (string, int) result
(** [byte_unescaper char_len_at set_char] is a byte unescaper such that:
     {ul
     {- [char_len_at s i] is the length of an escaped byte at index
         [i] of [s]. If [1] is returned then the byte is assumed
         to be unchanged by the unescape, use {!byte_unreplacer}
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
  (** [byte_unreplacer char_len_at set_char] is like {!byte_unescaper}
      except [set_char] can set a different byte whenever [char_len_at]
      returns [1]. *)

(** {1:ascii ASCII strings} *)

(** ASCII string support.

    The following functions act only the code points of the
    {{:https://en.wikipedia.org/wiki/ASCII#Character_set}ASCII
    character set}, that is on the bytes in the range
    \[[0x00];[0x7F]\].  The functions can be safely used on UTF-8
    encoded strings but they will, of course, only deal with ASCII
    related matters. *)
module Ascii : sig

  (** {1:pred Predicates} *)

  val is_valid : string -> bool
  (** [is_valid s] is [true] iff only for all indices [i] of [s],
      [s.[i]] is an ASCII character, i.e. a byte in the range
      \[[0x00];[0x1F]\]. *)

  (** {1:case Casing transforms}

      The functions can be safely used on UTF-8 encoded strings;
      they will of course only deal with ASCII casings. *)

  val uppercase : string -> string
  (** [uppercase s] is [s] with ASCII characters ['a'] to ['z'] mapped
      to ['A'] to ['Z']. *)

  val lowercase : string -> string
  (** [lowercase s] is [s] with ASCII characters ['A'] to ['Z'] mapped
      to ['a'] to ['z']. *)

  val capitalize : string -> string
  (** [capitalize s] is like {!Ascii.uppercase} but performs the map only
      on [s.[0]]. *)

  val uncapitalize : string -> string
  (** [uncapitalize s] is like {!Ascii.lowercase} but performs the map only
      on [s.[0]]. *)

  (** {1:hex Converting to ASCII hexadecimal characters} *)

  val to_hex : string -> string
  (** [to_hex s] is the sequence of bytes of [s] as ASCII lowercase
      hexadecimal digits. *)

  val of_hex' : string -> (string, int) result
  (** [of_hex' h] parses a sequence of ASCII (lower or upper
      cased) hexadecimal digits from [h] into its corresponding byte
      sequence. [Error n] is returned either with [n] an index in
      the string which is not a hexadecimal digit or the length of
      [h] if it there is a missing digit at the end. *)

  val of_hex : string -> (string, string) result
  (** [of_hex] is {!of_hex'} but errors with an english error message. *)

  (** {1:escunesc Converting to printable ASCII characters} *)

  val escape : string -> string
  (** [escape s] escapes bytes of [s] to a representation that uses only
      ASCII printable characters. More precisely:
      {ul
      {- \[[0x20];[0x5B]\] and \[[0x5D];[0x7E]\] are left unchanged.
         These are the {{!Char.Ascii.is_print}printable} ASCII bytes,
         except ['\\'] ([0x5C]).}
      {- \[[0x00];[0x1F]\], [0x5C] and
         \[[0x7F];[0xFF]\] are escaped by an {e hexadecimal} ["\xHH"]
         escape with [H] a capital hexadecimal number. These bytes
         are the ASCII control characters, the non ASCII bytes
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
        that uses only ASCII printable characters and according to OCaml's
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
           {{!Char.Ascii.is_print}printable} ASCII bytes, except
           ['\"'] ([0x22]) and ['\\'] ([0x5C]).}
        {- Remaining bytes are escaped by an {e hexadecimal} ["\xHH"]
           escape with [H] an uppercase hexadecimal number. These bytes
           are the ASCII control characters not mentioned above
           and non ASCII bytes.}}
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

(** {1:var_subst Variable substitution} *)

val subst_pct_vars :
  ?buf:Buffer.t -> (string -> string option) -> string -> string
(** [subst_pct_vars ~buf vars s] substitutes in [s] sub-strings of the
    form [%%VAR%%] by the value of [vars "VAR"] (if any). *)

(** {1:ansi_strip ANSI stripping} *)

val strip_ansi_escapes : string -> string
(** [strip_ansi_escapes s] removes ANSI escapes from [s]. *)

(** {1:fmt Formatting} *)

val pp : string B0__fmt.t
(** [pp ppf s] prints [s]'s bytes on [ppf]. *)

(** {1:setmap Sets and maps} *)

(** String sets. *)
module Set : sig

  (** {1 String sets} *)

  include Set.S with type elt := string
  type elt = string

  val pp : ?sep:unit B0__fmt.t -> string B0__fmt.t -> t B0__fmt.t
  (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
      [ppf]. Each element is formatted with [pp_elt] and elements are
      separated by [~sep] (defaults to {!Format.pp_print_cut}). If the
      set is empty leaves [ppf] untouched. *)

  val pp_dump : t B0__fmt.t
  (** [pp_dump ss] formats a set for inspection. *)

end with type t = Set.Make(String).t

(** String maps. *)
module Map : sig

  (** {1 String maps} *)

  include Map.S  with type key := string
  type key = string

  val dom : 'a t -> Set.t
  (** [dom m] is the domain of [m]. *)

  val of_list : (string * 'a) list -> 'a t
  (** [of_list bs] is [List.fold_left (fun m (k, v) -> add k v m) empty bs].

      {b Available} in 5.1 *)

  (** {1:add Additional adds} *)

  val add_to_list : string -> 'a -> 'a list t -> 'a list t
  (** [add k v m] is [m] with [k] mapping to [l] such that [l] is
      [v :: find k m] if [k] was bound in [m] and [[v]] otherwise.

      {b Available} in 5.1. *)

  val add_to_set :
    (module Stdlib.Set.S with type elt = 'a and type t = 'set) ->
    string -> 'a -> 'set t -> 'set t
  (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
      [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
      otherwise. *)

  (** {1:get_or_hint Get or hint} *)

  val get_or_suggest : string -> 'a t -> ('a, string list) result
  (** [get_or_suggest k m] is [Ok v] if [k] is bound to [v] in [m]
      and otherwise a (possibly empty) list of suggested keys
      whose name could match [name]. *)

  val get_or_hint :
    ?pp_key:string B0__fmt.t ->
    kind:string -> string -> 'a t -> ('a, string) result
  (** [get_or_hint] is like {!get_or_suggest} but it formats an error
      message that indicate that the [kind] of key [k] could not be
      found and suggests alternative names (if any).  [pp_key] is used
      to format the keys, it default to {!Fmt.code'}. *)

  (** {1:fmt Formatting} *)

  val pp :
    ?sep:unit B0__fmt.t -> (string * 'a) B0__fmt.t -> 'a t B0__fmt.t
  (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
      [ppf]. Each binding is formatted with [pp_binding] and bindings
      are separated by [sep] (defaults to {!Format.pp_print_cut}). If
      the map is empty leaves [ppf] untouched. *)

  val pp_dump : 'a B0__fmt.t -> 'a t B0__fmt.t
  (** [pp_dump pp_v ppf m] formats a map for inspection using [pp_v]
      to format codomain elements. *)

  val pp_dump_string_map : string t B0__fmt.t
  (** [pp_dump_string_map ppf m] prints an unspecified representation
      of the string map [m] on [ppf]. *)

end with type 'a t = 'a Map.Make(String).t
