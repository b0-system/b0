(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File paths.

    A file system {e path} specifies a file or a directory in a file
    system hierarchy. It is made of three parts:

    {ol
    {- An optional, platform-dependent, volume (e.g. [C:]).}
    {- An optional root directory separator {!Fpath.dir_sep} whose presence
       distinguishes absolute paths (["/a"]) from {e relative} ones
       (["a"])}
    {- A non-empty list of {!Fpath.dir_sep} separated segments. {e Segments}
       are non empty strings except for maybe the last one. The latter
       syntactically distinguishes {e directory paths} (["a/b/"]) from
       file paths (["a/b"]).}}

    The paths segments ["."] and [".."] are relative path segments
    that respectively denote the current and parent directory. The
    {{!Fpath.basename}basename} of a path is its last non-empty segment if
    it is not a relative path segment or the empty string otherwise (e.g.
    on ["/"] or [".."]).

    @canonical B0_std.Fpath *)



 (** {1:segments Directory separators and segments} *)

 val dir_sep_char : char
 (** [dir_sep_char] is the platform dependent natural directory
     separator.  This is character ['\\'] if {!Sys.win32} and ['/']
     otherwise. *)

 val char_is_dir_sep : char -> bool

 val dir_sep : string
 (** [dir_sep] is {!dir_sep_char} as a string. *)

 val has_dir_sep : string -> bool
 (** [has_dir_sep s] is [true] iff [s] contains {!dir_sep_char} (and
     if {!Sys.win32} also if it contains ['/']). *)

 val is_seg : string -> bool
 (** [is_seg s] is [true] iff [s] does not contain a null byte or a
     {!dir_sep_char} (and if {!Sys.win32} also that it does not
     contain ['/']). *)

 val is_rel_seg : string -> bool
 (** [is_rel_seg s] is [true] iff [s] is a relative segment, that is
     either ["."] or [".."]. *)

 (** {1:paths File paths} *)

 type t
 (** The type for file paths. *)

 val v : string -> t
 (** [v s] is the string [s] as a file path.

     {b Warning.} In code only use ["/"] as the directory separator
     even on Windows platforms (don't be upset, the module gives them
     back to you with backslashes).

     Raises [Invalid_argument] if [s] is not a {{!of_string}valid
     path}. Use {!of_string} to deal with untrusted input. *)

 val fmt : ('a, Format.formatter, unit, t) format4 -> 'a
 (** [fmt …] is [Fmt.kstr v …]. *)

 val add_seg : t -> string -> (t, string) result
 (** [add_seg p seg] if [p]'s last segment is non-empty this is [p]
     with [seg] added. If [p]'s last segment is empty, this is [p]
     with the empty segment replaced by [seg].

     This errors if [is_seg seg] is [false]. *)

 val append : t -> t -> t
 (** [append p q] appends [q] to [p] as follows:
     {ul
     {- If [q] is {{!is_abs}absolute} or has a non-empty volume then
        [q] is returned.}
     {- Otherwise appends [q]'s segments to [p] using {!add_seg}.}} *)

 val ( / ) : t -> string -> t
 (** [p / seg] is [add_seg p seg |> Result.get_ok'].
     Left associative. {b Warning.} Use {!add_seg} to deal with
     untrusted [seg] values. *)

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

 (** {1:dirpaths Directory paths}

     {b Note.} The following functions use syntactic semantic
     properties of paths. Given a path, these properties can be
     different from the ones your file system attributes to it. *)

 val is_dir_path : t -> bool
 (** [is_dir_path p] is [true] iff [p] syntactically represents
     a directory. This means that [p] is [.], [..] or ends
     with [/], [/.] or [/..]. *)

 val add_dir_sep : t -> t
 (** [add_dir_sep p] is [add_seg p ""]. It ensures that the resulting
     path syntactically represents a {{!is_dir_path}directory} and thus,
     if converted to a string, that it ends with a {!dir_sep}. *)

 val strip_dir_sep : t -> t
 (** [strip_dir_sep p] is [p] without an existing last empty segment
     when [p] is not a root path, ensuring the result has no trailing
     {!dir_sep} when converted to a string. *)

 (** {1:baseparent Basename and parent directory}

     {b Note.} The following functions use syntactic semantic
     properties of paths. Given a path, these properties can be
     different from the ones your file system attributes to it. *)

 val basename : ?strip_exts:bool -> t -> string
 (** [basename p] is the last non-empty segment of [p] or the empty
     string otherwise. The latter occurs only on root paths and on
     paths whose last non-empty segment is a {{!is_rel_seg}relative
     segment}. If [strip_exts] is [true] (default to [false]) the basename's
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

 val strip_prefix : t -> t -> t option
 (** [strip_prefix prefix p] is:
     {ul
     {- [None] if {!is_prefix}[ prefix p] is [false].}
     {- [Some q] otherwise where [q] is [p] without the string prefix
        [Fpath.to_dir_path prefix]. This means that [q] is always
        relative, that it preserves [p]'s
        {{!is_dir_path}directoryness} and that [Fpath.(equal (prefix
        // q) p)] holds.}}

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

     Raises [Invalid_argument] if path [to_dir] contains "..". *)

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

     {b TODO} Is returning the empty string really a good idea ? I vaguely
     remember that this was an annoyance though. *)

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
     is [p] with the system specific {!dir_sep_char} directory separator
     replaced by ['/'] and with the following characters percent encoded:
     ['%'], ['?'], ['#'], [' '] (if [escape_space] is [true], default),
     and the US-ASCII {{!Char.Ascii.is_control}control characters}.

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
