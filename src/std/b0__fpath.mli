(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File paths.

    A file {e path} is a syntactic specification of a file or a
    directory location in a file system hierarchy. It is made of three
    parts:

    {ol
    {- An optional, platform-dependent, volume (e.g. ["C:"]).}
    {- An optional root {{!is_dir_sep_char}directory separator} whose presence
       distinguishes absolute paths from {e relative} ones
       (e.g. ["/a"] versus ["a"]).}
    {- A non-empty list of segments delimited by
       {{!is_dir_sep_char}directory separators}. {e Segments} are non empty
       strings except for maybe the last one. The latter syntactically
       distinguishes {e directory paths} from file paths
       (e.g. ["a/b/"] versus ["a/b"]). But a directory can also be
       specified by a file path and a syntactic directory path may not
       be a directory (e.g. a symlink).}}

    The paths segments ["."] and [".."] are relative path segments
    that respectively denote the current and parent directory. The
    {{!Fpath.basename}basename} of a path is its last non-empty segment if
    it is not a relative path segment or the empty string otherwise (e.g.
    on ["/"] or [".."]).

    @canonical B0_std.Fpath *)

(** {1:seps Directory separators} *)

val natural_dir_sep_char : char
(** [natural_dir_sep_char] is the natural directory separator for the
    platform. This is ['\\'] if {!Sys.win32} and ['/'] otherwise.

    {b Warning.} Do not test for equality against this character to
    assert directory separators. Use {!is_dir_sep_char}, some platforms support
    more than one directory separator. *)

val natural_dir_sep : string
(** [natural_dir_sep] is {!natural_dir_sep_char} as a string.

    {b Warning.} Do not test for equality against this string to
    assert directory separators. Use {!is_dir_sep}, some platforms support more
    than one directory separator. *)

val is_dir_sep_char : char -> bool
(** [is_dir_sep_char c] is [true] iff [c] is a directory separator
    on the platform. This means [c] is ['/'] or it is ['\\'] and {!Sys.win32}
    is [true]. *)

val is_dir_sep : string -> bool
(** [is_dir_sep s] is [true] if [s] is a singelton string and
    {!is_dir_sep_char}[ s.[0]] is [true]. *)

(** {1:paths File paths} *)

type t
(** The type for file paths. *)

val v : string -> t
(** [v s] is the string [s] as a file path. Raises [Invalid_argument] if [s]
    is not a {{!of_string}valid path}. Use {!of_string} to deal with
    untrusted input.

    {b Warning.} In code only use ["/"] in string literals as the directory
    separator even on Windows platforms (don't be upset, the module gives them
    back to you with backslashes). *)

val fmt : ('a, Format.formatter, unit, t) format4 -> 'a
(** [fmt …] is [Fmt.kstr v …]. *)

val append : t -> t -> t
(** [append p q] appends [q] to [p] as follows:
    {ul
    {- If [q] is {{!is_absolute}absolute} or has a non-empty volume then
       [q] is returned.}
    {- Otherwise appends [q]'s segments to [p] using {!add_segment}.}} *)

val ( // ) : t -> t -> t
(** [p // p'] is [append p p']. Left associative. *)

(** {1:famous Famous file paths} *)

val null : t
(** [null] represents a file on the OS that discards all writes
    and returns end of file on reads. This is [NUL] if {!Sys.win32}
    is [true] and [/dev/null] otherwise. See also {!is_null}. *)

val dash : t
(** [dash] is ["-"]. This value is used in command line interfaces
    to denote standard input or output. See also {!is_dash}. *)

(** {1:segs Segments} *)

val is_segment : string -> bool
(** [is_segment s] is [true] iff [s] does not contain a null byte or
    a directory separator as asserted by {!is_dir_sep_char}. *)

val is_segment_relative : string -> bool
(** [is_segment_relative s] is [true] iff [s] is either ["."] or [".."]. *)

val add_segment : t -> string -> (t, string) result
(** [add_segment p seg] is:
    {ul
    {- [p] with the last segment replaced by [seg], if the last
       segment is empty.}
    {- [p] with segment [seg] added, otherwise}}

    This errors if {!is_segment}[ seg] is [false]. {b FIXME} error
    format. *)

val ( / ) : t -> string -> t
(** [p / seg] is {!add_segment}[ p seg |> Result.get_ok'].
    Left associative. {b Warning.} Use {!add_segment} to deal with
    untrusted [seg] values. *)

(** {1:syntacticdirpaths Syntactic directory paths} *)

val is_syntactic_dir : t -> bool
(** [is_syntactic_dir p] is [true] iff [p] syntactically represents
    a directory. This means that [p] is ["."], [".."] or ends
    with ["/"], ["/."] or ["/.."].

    {b Note.} This a syntactic check, the file system may attribute
    different properties to these paths. *)

val ensure_trailing_dir_sep : t -> t
(** [ensure_trailing_dir_sep p] is [add_segment p ""]. This ensures that
    [p] has a final empty segment and thus a trailing directory separator when
    converted to a string. *)

val strip_trailing_dir_sep : t -> t
(** [strip_trailing_dir_sep p] ensures [p] has no final empty segment unless
    [p] is a root path. When [p] is not a root path this ensure that
    [p] has no trailing directory separator when converted to a string. *)

(** {1:baseparent Basename and parent directory}

    {b Note.} The following functions use syntactic semantic
    properties of paths. Given a path, these properties can be
    different from the ones your file system attributes to it. *)

val basename : ?strip_exts:bool -> t -> string
(** [basename p] is the last non-empty segment of [p] or the empty
    string otherwise. The latter occurs only on root paths and on
    paths whose last non-empty segment is a {{!is_segment_relative}relative
    segment}. If [strip_exts] is [true] (default to [false]) the basename's
    {{!file_exts}multiple extension}, if any, is removed from the segment. *)

val parent : t -> t
(** [parent p] is a {{!is_syntactic_dir}directory path} that contains
    [p]. If [p] is a {{!is_root}root path} this is [p] itself.
    If [p] is in the current directory this is ["./"]. *)

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

val strip_prefix : t -> t -> t option
(** [strip_prefix prefix p] is:
    {ul
    {- [None] if {!is_prefix}[ prefix p] is [false].}
    {- [Some q] otherwise where [q] is [p] without the string prefix
       [Fpath.to_dir_path prefix]. This means that [q] is always
       relative, that it preserves [p]'s
       {{!is_syntactic_dir}syntactic directoryness} and
       that [Fpath.(equal (prefix // q) p)] holds.}}

    {b Warning.} By definition [strip_prefix p p] is [None]. *)

val drop_prefixed : t list -> t list
(** [drop_prefixed ps] is [ps] without elements that have a
    {{!is_prefix}strict prefixes} in [ps]. The list order is
    preserved. Duplicates are not removed use {!distinct} for
    this. *)

val reroot : src_root:t -> dst_root:t -> t -> t
(** [reroot ~src_root ~dst_root p] assumes [src_root] {{!is_prefix}prefixes}
    [p] removes the prefix and prepends [dst_root] to the result.

    Raises [Invalid_argument] if [dst_root] is not a prefix of [src].
    In particular note that [p] cannot be [src_root]. *)

val relative : to_dir:t -> t -> t
(** [relative ~to_dir p] is [q] such that [to_dir // q] represents
    the same path as [p]. Note that [q] is not necessarily relative:
    if [to_dir] is relative and [p] is absolute [p] is returned.

    {b Warning.} This function is mostly broken at the moment.

    Raises [Invalid_argument] if path [to_dir] contains [".."]. *)

(** {1:preds Predicates and comparison} *)

val is_relative : t -> bool
(** [is_relative p] is [true] iff [p] is a relative path, i.e. the root
    directory separator is missing in [p]. *)

val is_absolute : t -> bool
(** [is_absolute p] is [true] iff [p] is an absolute path, i.e. the root
    directory separator is present in [p]. *)

val is_root : t -> bool
(** [is_root p] is [true] iff [p] is a root directory, i.e. [p] has
    the root directory separator and a single, empty, segment. *)

val is_null : t -> bool
(** [is_null p] is [equal p null]. *)

val is_dash : t -> bool
(** [is_dash p] is [equal p dash]. *)

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

    The {e file extension} (resp. {e multiple file extensions}) of a
    path segment is the suffix that starts at the last (resp. first)
    occurence of a ['.'] that is preceeded by at least one non ['.']
    character. If there is no such occurence in the segment, the
    extension is empty.  With these definitions, ["."], [".."],
    ["..."] and dot files like [".ocamlinit"] or ["..ocamlinit"] have
    no extension, but [".emacs.d"] and ["..emacs.d"] do have a single one.

    {b TODO} In the [fpath] package we correct e.g. [has_ext] if the
    given [ext] has no [ext]. I think we should either check and raise
    [Invalid_argument] or correct. *)

type ext = string
(** The type for file or multiple file extensions, ['.'] separator
    included. *)

val exists_ext : t -> bool
(** [exists_ext p] is [true] iff [p] has a file or multiple file extension. *)

val exists_multi_ext : t -> bool
(** [exists_multi_ext p] is [true] iff [p] has a multiple file extension. *)

val get_ext : multi:bool -> t -> ext
(** [get_ext ~multi p] is the file extension or multiple file extension
    if [multi] is [true] of the {!basename} of [p].

    The empty string is returned if there is no extension. By definition
    this works on the directory name of directory paths.

    {b TODO} Is returning the empty string rather than an option really a
    good idea ? I vaguely remember option was an annoyance though. *)

val has_ext : ext -> t -> bool
(** [has_ext ext p] is [true] iff [String.equal (get_ext n multi:false p) e ||
    String.equal (get_ext ~multi:true p) e]. *)

val mem_ext : ext list -> t -> bool
(** [mem_ext exts p] is [List.exists (fun e -> has_ext e p) exts] *)

val add_ext : ext -> t -> t
(** [add_ext ext p] is [p] with [ext] concatenated to [p]'s
    {{!basename}basename}. *)

val strip_ext : multi:bool -> t -> t
(** [strip_ext multi p] is [p] with the extension of [p]'s
    {{!basename}basename} removed. If [multi] is [true] (defaults to
    [false]), the multiple file extension is removed. *)

val set_ext : multi:bool -> ext -> t -> t
(** [set_ext ~multi p] is [add_ext ext (strip_ext ~multi p)]. *)

val cut_ext : multi:bool -> t -> t * ext
(** [cut_ext ~multi p] is [(strip_ext ~multi p, get_ext ~multi p)]. *)

val ( + ) : t -> ext -> t
(** [p + ext] is [add_ext p ext]. Left associative. *)

val ( -+ ) : t -> ext -> t
(** [p -+ ext] is [set_ext ~multi:false p ext]. Left associative. *)

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

val to_url_path : ?escape_space:bool -> t -> string
(** [to_url_path ~escape_space p] is the path [p] as an URL path. This
    is [p] with the directory separator replaced by ['/'] and with the
    following characters percent encoded: ['%'], ['?'], ['#'], [' ']
    (if [escape_space] is [true], default), and the US-ASCII
    {{!Char.Ascii.is_control}control characters}.

    {b Note.} In 2019, the standard definition of URLs is in a sorry
    state. Assuming [p] is UTF-8 encoded. It is {e believed} the
    above function should lead to an URL path component that can be
    parsed by HTML5's
    {{:https://dev.w3.org/html5/spec-LC/urls.html#parsing-urls}
    definition} of URL parsing. *)

val to_segments : t -> string list
(** [to_segments p] is [p]'s {e non-empty} list of segments. Absolute
    paths have an empty string added, this allows to recover the path's
    string with [String.concat dir_sep], note however that you may
    have lost the {!volume} along the way. *)

(** {1:fmt Formatting} *)

val pp : t B0__fmt.t
(** [pp ppf p] prints path [p] on [ppf]. The path is quoted with
    {!Filename.quote} if needed. For now this means if it contains
    spaces (U+0020). *)

val pp_quoted : t B0__fmt.t
(** [pp_quoted ppf p] prints path [p] on [ppf] using {!Filename.quote}. *)

val pp_unquoted : t B0__fmt.t
(** [pp_unquoted ppf p] prints path [p] on [ppf] using {!to_string}. *)

val pp_dump : t B0__fmt.t
(** [pp_dump ppf p] prints path [p] on [ppf] using {!String.pp_dump}. *)

val error :
  t -> ('b, Format.formatter , unit, ('a, string) result) format4 -> 'b
(** [error p fmt …] is [Fmt.error ("%a:" ^^ fmt) pp_unquoted p … ]. *)

val prefix_msg : t -> string -> string
(** [prefix_msg p msg] is [Fmt.str "%a: %s" pp_unquoted msg]. *)

(** {1:unique Uniqueness} *)

val distinct : t list -> t list
(** [distinct ps] is [ps] without duplicates, the list order is
    preserved. *)

(** {1:setmap Paths map and sets} *)

type path = t

(** Path sets. *)
module Set : sig

  (** {1 Path sets} *)

  include Set.S with type elt := t
  type elt = path

  val pp : ?sep:unit B0__fmt.t -> path B0__fmt.t -> t B0__fmt.t
  (** [pp ~sep pp_elt ppf ss] formats the elements of [ss] on
      [ppf]. Each element is formatted with [pp_elt] and elements
      are separated by [~sep] (defaults to
      {!Format.pp_print_cut}). If the set is empty leaves [ppf]
      untouched. *)

  val pp_set : t B0__fmt.t
  (** [pp_set ppf ss] prints an unspecified set-like representation
      of [ss] on [ppf] using {!Fpath.pp}. *)

  val pp_dump : t B0__fmt.t
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
    (module Stdlib.Set.S with type elt = 'a and type t = 'set) ->
    path -> 'a -> 'set t -> 'set t
  (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
      [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
      otherwise. *)

  (** {1:fmt Formatting} *)

  val pp : ?sep:unit B0__fmt.t -> (path * 'a) B0__fmt.t -> 'a t B0__fmt.t
  (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on
      [ppf]. Each binding is formatted with [pp_binding] and
      bindings are separated by [sep] (defaults to
      {!Format.pp_print_cut}). If the map is empty leaves [ppf]
      untouched. *)

  val pp_dump : 'a B0__fmt.t -> 'a t B0__fmt.t
  (** [pp_dump pp_v ppf m] prints an unspecified representation of [m] on
      [ppf] using [pp_v] to print the map codomain elements. *)
end

(** {1:sorts Sorts} *)

val sort_by_parent : Set.t -> Set.t Map.t
(** [sort_by_parent ps] maps elements of [ps] by their {!Fpath.parent}. *)

val sort_by_ext : multi:bool -> Set.t -> Set.t B0__string.Map.t
(** [sort_by_ext ~multi ps] maps elements of [ps] by their extension as
    determined by {!Fpath.get_ext}[ ~multi]. *)

(** {1:sp Search paths}

    A {e search path} is a list of paths separated by a designated
    separator in which elements are looked up in left to right
    priority order. A well known search path is [PATH] in which
    executable binaries are looked up. *)

val search_path_sep : string
(** [search_path_sep] is the default platform specific separator for
    search paths. This is [";"] if {!Sys.win32} and [":"]
    otherwise. *)

val list_of_search_path : ?sep:string -> string -> (t list, string) result
(** [list_of_search_path ~sep s] splits [s] on [sep] (defaults to
    {!search_path_sep}) and parses the result with {!of_string},
    ignoring empty strings.

    This means that [sep] is not allowed to appear in the file
    paths, consecutive [sep] are ignored and the order in the
    resulting list matches the left-to-right order of paths in [s].

    If one {!of_string} errors on a path [p] with [e] the function errors
    with the message:
    {[Fmt.str "Illegal path %a in search path: %s" pp p e]} *)
