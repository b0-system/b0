(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Strings.

    @canonical B0_std.String *)

include module type of String (** @closed *)

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

val includes : affix:string -> string -> bool
(** [includes ~affix s] is [true] iff there exists an index [j]
    such that for all indices [i] of [affix], [sub.[i] = s.[j+ 1]]. *)

(** {1:find_indices Finding indices} *)

val find_index : ?start:int -> (char -> bool) -> string -> int option
(** [find_index ~start sat] is the index of the first character of
    [s] that satisfies [sat] before or at [start] (defaults to [0]). *)

val rfind_index : ?start:int -> (char -> bool) -> string -> int option
(** [rfind_index ~start sat] is the index of the first character of
    [s] that satisfies [sat] before or at [start] (defaults to
    [String.length s - 1]). *)

(** {1:find_subs Finding and replacing substrings} *)

val find_sub : ?start:int -> sub:string -> string -> int option
(** [find_sub ~start ~sub s] is the start position (if any) of the
    first occurence of [sub] in [s] after or at position [start]
    (which includes index [start] if it exists, defaults to [0]).
    Note if you need to search for [sub] multiple times in [s] use
    {!find_all_sub} it is more efficient.

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val rfind_sub : ?start:int -> sub:string -> string -> int option
(** [rfind_sub ~start ~sub s] is the start position (if any) of the
    first occurences of [sub] in [s] before or at position [start]
    (which includes index [start] if it exists, defaults to
    [String.length s]).

    Note if you need to search for [sub] multiple times in [s] use
    {!rfind_all_sub} it is more efficient.

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val find_all_sub :
  ?start:int -> (int -> 'acc -> 'acc) -> sub:string -> string -> 'acc -> 'acc
(** [find_all_sub ~start f ~sub s acc], starting with [acc], folds [f] over
    all non-overlapping starting positions of [sub] in [s] after or at
    position [start] (which includes index [start] if it exists, defaults
    to [0]). This is [acc] if [sub] could not be found in [s].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val rfind_all_sub :
  ?start:int -> (int -> 'acc -> 'acc) -> sub:string -> string -> 'acc -> 'acc
(** [rfind_all_sub ~start f ~sub s acc], starting with [acc], folds
    [f] over all non-overlapping starting positions of [sub] in [s]
    before or at position [start] (which includes index [start] if
    it exists, defaults to [String.length s]). This is [acc] if
    [sub] could not be found in [s].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val replace_first : ?start:int -> sub:string -> by:string -> string -> string
(** [replace_first ~start ~sub ~by s] replaces in [s] the first occurence
    of [sub] at or after position [start] (defaults to [0]) by [by].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

val replace_all : ?start:int -> sub:string -> by:string -> string -> string
(** [replace_all ~start ~sub ~by] replaces in [s] all
    non-overlapping occurences of [sub] at or after position [start]
    (default to [0]) by [by].

    @raise Invalid_argument if [start] is not a valid position of [s]. *)

(** {1:subs Extracting substrings} *)

val subrange : ?first:int -> ?last:int -> string -> string
(** [subrange ~first ~last s] are the consecutive bytes of [s] whose
    indices exist in the range \[[first];[last]\].

    [first] defaults to [0] and last to [String.length s - 1].

    Note that both [first] and [last] can be any integer. If [first
    > last] the interval is empty and the empty string is
    returned. *)

(** {1:break Breaking} *)

(** {2:break_mag Breaking with magnitudes} *)

val take : int -> string -> string
(** [take n s] are the first [n] bytes of [s]. This is [s] if
    [n >= length s] and [""] if [n <= 0]. *)

val rtake : int -> string -> string
(** [rtake n s] are the last [n] bytes of [s].  This is [s] if
    [n >= length s] and [""] if [n <= 0]. *)

val drop : int -> string -> string
(** [drop n s] is [s] without the first [n] bytes of [s]. This is [""]
    if [n >= length s] and [s] if [n <= 0]. *)

val rdrop : int -> string -> string
(** [rdrop n s] is [s] without the last [n] bytes of [s]. This is [""]
    if [n >= length s] and [s] if [n <= 0]. *)

val span : int -> string -> string * string
(** [span n v] is [(take n v, drop n v)]. *)

val rspan : int -> string -> string * string
(** [rspan n v] is [(rdrop n v, rtake n v)]. *)

(** {2:break_pred Breaking with predicates} *)

val take_while : (char -> bool) -> string -> string
(** [take_while sat s] are the first consecutive [sat] statisfying
    bytes of [s]. *)

val rtake_while : (char -> bool) -> string -> string
(** [keep_right sat s] are the last consecutive [sat] satisfying
    bytes of [s]. *)

val drop_while : (char -> bool) -> string -> string
(** [drop_while sat s] is [s] without the first consecutive [sat]
    satisfying bytes of [s]. *)

val rdrop_while : (char -> bool) -> string -> string
(** [rdrop_while sat s] is [s] without the last consecutive [sat]
    satisfying bytes of [s]. *)

val span_while : (char -> bool) -> string -> string * string
(** [span_while sat s] is [(take_while sat s, drop_while sat s)]. *)

val rspan_while : (char -> bool) -> string -> string * string
(** [rspan_while sat s] is [(rdrop_while sat s, rtake_while sat s)]. *)

(** {2:break_sep Breaking with separators} *)

val cut : sep:string -> string -> (string * string) option
(** [cut ~sep s] is either the pair [Some (l,r)] of the two
    (possibly empty) substrings of [s] that are delimited by the
    first match of the separator character [sep] or [None] if [sep]
    can't be matched in [s]. Matching starts from the left of [s].

    The invariant [l ^ sep ^ r = s] holds.

    @raise Invalid_argument if [sep] is the empty string. *)

val rcut : sep:string -> string -> (string * string) option
(** [rcut ~sep s] is like {!cut} but matching starts
    on the right of [s]. *)

val split : ?drop_empty:bool -> sep:string -> string -> string list
(** [split sep s] is the list of all substrings of [s] that are
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

val rsplit : ?drop_empty:bool -> sep:string -> string -> string list
(** [rsplit sep s] is like {!split} but matching starts on the
    right of [s]. *)

(** {2:break_lines Breaking lines} *)

val fold_ascii_lines :
  strip_newlines:bool -> (int -> 'a -> string -> 'a) -> 'a -> string -> 'a
(** [fold_ascii_lines ~strip_newlines f acc s] folds over the lines of [s] by
    calling [f linenum acc' line] with [linenum] the one-based line number
    count, [acc'] the result of accumulating [acc] with [f] so far and [line]
    the data of the line (without the newline found in the data if
    [strip_newlines] is [true]).

    Lines are delimited by newline sequences which are either one of
    ["\n"], ["\r\n"] or ["\r"]. More precisely the function determines lines
    and line data as follows:
    {ul
    {- If [s = ""], the function considers there are no lines in [s] and
       [acc] is returned without [f] being called.}
    {- If [s <> ""], [s] is repeteadly split on the first newline sequences
       ["\n"], ["\r\n"] or ["\r"] into [(left, newline, right)], [left]
       (or [left ^ newline] when [strip_newlines = false]) is given to [f]
       and the process is repeated with [right] until a split can no longer
       be found. At that point this final string is given to [f] and the
       process stops.}} *)

val detach_ascii_newline : string -> string * string
(** [detach_ascii_newline s] is [(data, endline)] with:
    {ul
    {- [endline] either the suffix ["\n"], ["\r\n"] or ["\r"] of [s] or [""]
       if [s] has no such suffix.}
    {- [data] the bytes before [endline] such that [data ^ newline = s]}} *)

(** {2:tokenize Tokenize} *)

val next_token :
  ?is_sep:(char -> bool) -> ?is_token:(char -> bool) -> string ->
  string * string
(** [next_token ~is_sep ~is_token s] skips characters satisfying
    [is_sep] from [s], then gather zero or more consecutive
    characters satisfying [is_token] into a string which is returned
    along the remaining characters after that. [is_sep] defaults to
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
(** [unique ~exist n] is [n] if [exists n] is [false] or [r = strf
    "%s~%d" n d] with [d] the smallest integer such that exists [r]
    if [false]. If no [d] in \[[1];[1e9]\] satisfies the condition
    [Invalid_argument] is raised, [limit] defaults to [1e9]. *)

(** {1:spellchecking Spellchecking} *)

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
    ["ca"] and ["abc"] have a distance of 3 not 2.

    {b Available} in 5.4.  *)

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
    errors are replaced by {!Uchar.rep} characters.

    {b Available} in 5.4.  *)

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
