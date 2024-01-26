(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [tar] file archives.

    For making reproducible tar archives, an (us)tar archiver OCaml
    implementation is {{!of_dir}provided}. Unarchiving and compression
    support occurs via shell outs.

    {b References}.
    {ul
    {- {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_06}
      ustar Interchange Format} in POSIX 1003.1, 2013.}} *)

open B0_std

(** {1:encode Ustar encoder} *)

type ptime = int
(** The type for POSIX times in seconds since the epoch. *)

type member =
[ `Dir (** Directory. *)
| `File of string (** File with given content. *) ]
(** The type for archive member. *)

type t
(** The type for ustar archives. *)

val empty : t
(** [empty] is the empty ustar archive. *)

val add :
  t -> Fpath.t -> mode:int -> mtime:ptime -> member ->
  (t, string) result
(** [add a path ~mode ~mtime member] adds [member] to archive [a]
    with file path [path], permission mode [mode] and modificaton
      time [mtime]. *)

val to_string : t -> string
(** [to_string a] is the byte serialization of the archive [a]. *)

val of_dir :
  dir:Fpath.t -> exclude_paths:Fpath.Set.t -> root:Fpath.t -> mtime:int ->
  (string, string) result
(** [of_dir ~dir ~exclude_paths ~root ~mtime] is a (us)tar archive that
    contains the file hierarchy [dir] except the relative
    hierarchies and files present in [exclude_paths]. Symbolic links are
    followed.

    In the archive, members of [dir] are rerooted at [root] and sorted
    according to {!B0_std.Fpath.compare} (for determinism). They have their
    modification time set to [mtime] and their file permissions are
    [0o775] for directories and files executable by the user and
    [0o664] for other files. No other file metadata is preserved. *)

(** {1:compressing Compressing} *)

val compress :
  ?search:Cmd.tool_search -> force:bool -> make_path:bool ->
  Fpath.t -> archive:string -> (unit, string) result
(** [compress ~force ~make_path file ~archive] compresses archive
    [archive] to [file]. For [force] and [make_path] see
    {!B0_std.Os.Cmd.out_file}.

    The compression algorithm and tool looked up with [search] (defaults
    to [Os.Cmd.get ?search]) depends on the file extension of [file]:
    {ul
    {- For [.tar] no tool is used.}
    {- For [.tgz] or [.gz] the [Cmd.tool "gzip"] tool is used.}
    {- For [.tbz] or [.bzip2] the [Cmd.tool "bzip2"] tool is used.}
    {- For [.xz] then the [Cmd.tool "lzma"] tool is used.}
    {- For [.zst] then the [Cmt.tool "zstd"] tool is used.}
    {- Otherwise the function errors.}} *)

(** {1:unarchive Unarchiving} *)

val unarchive :
  ?search:Cmd.tool_search -> make_path:bool -> verbose:bool -> src:Fpath.t ->
  in_dir:Fpath.t -> unit -> (unit, string) result
(** [unarchive ~make_path ~src ~in_dir] unarchives [src] in directory
    [in_dir] which is created if it doesn't exist. For [make_path]
    see {!Os.Dir.create}. If [verbose] is [true] [-v] is passed to [tar].

    The compression algorithm and tool is to use is looked up with [search]
    like in {!compress}. *)
