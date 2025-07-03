(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Standard needs for b0 programs.

    Open this module to use it. It redefines a few standard
    modules and introduces a few new ones. *)

(** {1:std Std} *)

module Char = B0__char
module Fmt = B0__fmt
module Fpath = B0__fpath
module List = B0__list
module Mtime = B0__mtime
module Result = B0__result
module String = B0__string
module Type = B0__type

(** Command lines.

    Command line values specify the command line arguments given to
    tool spawns. Depending on the context this may represent either
    only tool arguments or the full command specification with
    the tool to spawn as the first argument.

    See {{!Cmd.examples}examples}. *)
module Cmd : sig

  (** {1:cl Command lines} *)

  type t
  (** The type for command lines.

      A command line is a list of command line arguments. The first
      argument usually denotes the {{!section-tool}tool or executable}
      to invoke. *)

  val is_empty : t -> bool
  (** [is_empty cmd] is [true] iff [cmd] is an empty list of arguments. *)

  val empty : t
  (** [empty] is an empty list of arguments. *)

  val arg : string -> t
  (** [arg a] is the atomic argument [a]. *)

  val append : t -> t -> t
  (** [append cmd1 cmd2] appends arguments [cmd2] to [cmd1]. *)

  val unstamp : t -> t
  (** [unstamp cmd] indicates that arguments [cmd] do not influence the
      tool's invocation outputs. These arguments are omitted from
      the command line's {{!to_list_and_stamp}stamp}, see {!section-stamps}
      for more details and {{!examples}examples}. *)

  (** {1:derived Derived combinators} *)

  val ( % ) : t -> string -> t
  (** [cmd % a] is [append cmd (arg a)]. *)

  val ( %% ) : t -> t -> t
  (** [cmd1 %% cmd2] is [append cmd1 cmd2]. *)

  val if' : bool -> t -> t
  (** [if' cond cmd] is [cmd] if [cond] is [true] and {!empty} otherwise. *)

  val if_some : t option -> t
  (** [if_some o] is [cmd] if [o] is [Some cmd] and {!empty} otherwise. *)

  val int : int -> t
  (** [int i] is [arg (string_of_int i)]. *)

  val float : float -> t
  (** [float f] is [arg (float_of_int f)]. *)

  val path : Fpath.t -> t
  (** [path p] is [arg (Fpath.to_string p)]. *)

  val list : ?slip:string -> string list -> t
  (** [list ?slip l] is a command line from the list of arguments [l].
      If [slip] is specified it is added on the command line before
      each element of [l]. *)

  val paths : ?slip:string -> Fpath.t list -> t
  (** [paths ?slip ps] is {!of_list}[ ?slip Fpath.to_string ps]. *)

  val of_list : ?slip:string -> ('a -> string) -> 'a list -> t
  (** [of_list ?slip conv l] is {!list}[ ?slip (List.map conv l)]. *)

  (** {1:tool Tools}

      Tools are the first argument of commands. *)

  type tool = Fpath.t
  (** The type for command line tools.

      A command line tool is represented by a file path according to
      the POSIX convention for [exec(3)]. If it is made of a single
      segment, for example [Fpath.v "ocaml"], it represents a program
      name to be looked up via a search procedure; for example in the
      [PATH] environment variable. If it is a file path with multiple
      segments (POSIX would say if they contain a slash characters)
      the program is the file itself.

      {b Note.} For portability one should not use the [.exe] suffix on
      Windows on tools. This should be handled transparently by
      {!type-tool_search} procedures. *)

  val tool : string -> t
  (** [tool t] is [arg t], used for reading clarity. *)

  val find_tool : t -> tool option
  (** [find_tool cmd] is [cmd]'s first argument. This is [None] if the
      command is {!empty} or if the first element can't be parsed to a
      {!type-tool}. *)

  val get_tool : t -> (tool, string) result
  (** [get_tool] is like {!val-find_tool} but returns an english [Error msg] on
      [None]. *)

  val set_tool : tool -> t -> t
  (** [set_tool t cmd] replaces [cmd]'s first element with [t]. This
      is [path t] if [cmd] is {!empty}. *)

  (** {2:tool_search Tool search} *)

  type tool_search = t -> (t, string) result
  (** The type for tool search functions.

      These are functions that resolve and {{!set_tool}set} the
      {!get_tool} argument of commands to a concrete program executable.
      Or return an error message if the tool cannot be resolved.
      See {!B0_std.Os.Cmd.section-tool_search}
      for implementations. *)

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
  (** [pp] is an unspecified formatter for commands. *)

  val pp_shell : t Fmt.t
  (** [pp_shell] formats a command as a multiline shell command
      that can be cut and pasted. {b Note.} Currently this may
      overflow your boxes. *)

  val pp_dump : t Fmt.t
  (** [pp_dump] formats raw data for debugging. *)

  (** {1:stamp Stamps}

    This module allows to {!Cmd.unstamp} command arguments.

    Unstamped arguments have no special semantics as far as
    the command line is concerned they simply indicate that the
    argument value itself does not influence the outputs of the
    tool.

    Unstamped arguments do not appear in the command line
    {{!Cmd.to_list_and_stamp}stamp} which is used to memoize tool
    spawns.

    A typical example of unstamped arguments are file paths to
    inputs: it's often the file contents not the actual file path that
    determines the tool output; beware though that some tool use both
    the file path contents and the actual file path in their
    outputs. See {{!examples}examples}. *)

  (** {1:examples Examples}
{[
let ls p = Cmd.(atom "ls" % "-a" % path p)
let tar archive dir =
  Cmd.(atom "tar" % "-cvf" %% unstamp (path archive) %% path dir)

let opam cmd = Cmd.(atom "opam" % cmd)
let opam_install pkgs = Cmd.(opam "install" %% list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(atom "ocamlc" % "-c" % if' debug (atom "-g") %% path file)

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(if' profile (atom "-p")) in
  let debug = Cmd.(if' debug (atom "-g")) in
  let incs = Cmd.(unstamp (paths ~slip:"-I" incs)) in
  Cmd.(atom "ocamlopt" % "-c" %% debug %% profile %% incs %%
       unstamp (path file))
]} *)

end


(** OS interaction. *)
module Os : sig

  (** {1:file_system File system} *)

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
      force:bool -> make_path:bool -> Fpath.t -> dst:Fpath.t ->
      (unit, string) result
    (** [rename ~force ~make_path src ~dst] renames [src] to [dst].
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
      recurse:bool -> Fpath.t -> dst:Fpath.t -> (unit, string) result
    (** [copy ~make_path ~recurse src ~dst] copies the file or file
        hierarchy rooted at [src] to [dst]. The function errors if
        [dst] exists. The semantics and arguments correspond to those
        of {!Os.Dir.val-copy}, except this function also works if [src] is
        not a directory. Note that [prune] is never called on [src]
        itself {b FIXME is that a good idea ?} also {b FIXME} this should
        error if [src] is a directory and [recurse] is false.

        See also {!Os.Dir.val-copy} and {!Os.File.val-copy}. *)

    (** {1:stat_mode File mode, stat and mounts}

        See also {!File.is_executable}. *)

    val get_mode : Fpath.t -> (int, string) result
    (** [get_mode p] is the file mode of [p]. Symbolic links are followed. *)

    val set_mode : Fpath.t -> int -> (unit, string) result
    (** [set_mode file p] sets the file mode of [file] to [p]. Symbolic
        links are followed. *)

    val stat : Fpath.t -> (Unix.stats, string) result
    (** [stat p] is [p]'s file information. Symbolic links are followed. *)

    val is_mount_point : Fpath.t -> (bool, string) result
    (** [is_mount_point p] is [true] if [p] looks like a mount point. The
        criterion is if [p] and [p/..]'s {!stat} have a differing
        {!Unix.stat.std_dev} field. *)

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
           It defaults to {!B0_std.Os.Dir.default_tmp}[ ()].}
        {- If [make_path] is [true] (default) and [dir] does not exist the
           whole path to it is created as needed with permission [0o755]
           (readable and traversable by everyone, writable by the user).}} *)
  end

  (** Regular file operations.

      This module operates on regular files, most functions error if
      they are applied to other file kinds. *)
  module File : sig

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
        and returns [Ok (f ic)]. If [file] is {!Fpath.dash}, [ic] is
        {!stdin}.  After the function returns (normally or via an
        exception raised by [f]), [ic] is ensured to be closed, except
        if it is {!stdin}. The function errors if opening [file]
        fails. Errors have the form [Fmt.str "%s: %s" file err]. *)

    val read_with_ic : Fpath.t -> (in_channel -> 'b) -> ('b, string) result
    (** [read_with_ic file f] is exactly like {!read_with_fd} but
        opens an OCaml input channel in binary mode. *)

    val read : Fpath.t -> (string, string) result
    (** [read file] is [file]'s content as a string. If [file] is
        {!Fpath.dash} the contents of {!stdin} is read. {b Warning.} The
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
        [Ok (f fdo)].  If [file] is {!Fpath.dash}, [fdo] is
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
        {!write_with_fd} but opens an OCaml channel in binary mode. *)

    val write :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool -> Fpath.t ->
      string -> (unit, string) result
    (** [write ~atomic ~mode ~force ~make_path file s] operates like
        {!write_with_fd} but directly writes [s] to [file]. *)

    val copy :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
      Fpath.t -> dst:Fpath.t -> (unit, string) result
    (** [copy ~atomic ~mode ~force ~path ~make_path src ~dst:file]
        operates like {!write_with_fd} but directly writes the content
        of [src] (or {!stdin} if [src] is {!Fpath.dash}) to [file].
        [mode] defaults to the permissions of [src] if available and
        [0o644] otherwise. *)

    val copy_to_dir :
      ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
      ?src_root:Fpath.t -> Fpath.t -> dir:Fpath.t -> (unit, string) result
   (** [copy ~force ~make_path src ~dir] is
       [copy ~force ~make_path src ~dst] with [dst] equal to
       {!Fpath.reroot}[ ~src_root ~dst_root:dir src] and [src_root]
       defaulting to {!Fpath.parent}[ src]. *)

    (** {1:tmpfiles Temporary files}

        See also {{!B0_std.Os.Path.tmppaths}temporary paths}. *)

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
           see {!type:Path.tmp_name} for details. It defaults to ["tmp-%s"].}
        {- [dir] is the directory in which the temporary file is created.
           It defaults to {!B0_std.Os.Dir.default_tmp}.}
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
      ?prune_dir:(Unix.stats -> string -> Fpath.t -> 'a -> bool) ->
      recurse:bool -> (Unix.stats -> string -> Fpath.t -> 'a -> 'a) ->
      Fpath.t -> 'a -> ('a, string) result
    (** [fold ~rel ~dotfiles ~follow_symlinks ~prune_dir ~recurse f dir
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
        {- [prune_dir] is called only when [recurse] is [true] as [prune st d]
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
      ?prune_dir:(Unix.stats -> string -> Fpath.t -> 'a -> bool) ->
      recurse:bool -> (Unix.stats -> string -> Fpath.t -> 'a -> 'a) ->
      Fpath.t -> 'a -> ('a, string) result
    (** [fold_files] is like {!fold} but [f] is only applied to
        non-directory files. *)

    val fold_dirs :
      ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
      ?prune_dir:(Unix.stats -> string -> Fpath.t -> 'a -> bool) ->
      recurse:bool -> (Unix.stats -> string -> Fpath.t -> 'a -> 'a) ->
      Fpath.t -> 'a -> ('a, string) result
    (** [fold_dirs] is like {!fold} but [f] is only applied
        to directory files. *)

    val prune_denied : (Unix.stats -> string -> Fpath.t -> 'a -> bool)
    (** [prune_denied] is a [prune] function for {!fold}s to
        skip directories for which the user has no [R_OK] and [X_OK]
        permissions. *)

    val path_list :
      Unix.stats -> string -> Fpath.t -> Fpath.t list -> Fpath.t list
    (** [path_list] is a {{!fold}folding} function to get a (reverse w.r.t.
        list of paths). Paths in the result that correspond to directories
        satisfy {!Fpath.is_dir_path}. *)

    (** {1:copy Copying} *)

    val copy :
      ?rel:bool -> ?atomic:bool -> ?follow_symlinks:bool ->
      ?prune:(Unix.stats -> string -> Fpath.t -> bool) -> make_path:bool ->
      recurse:bool -> Fpath.t -> dst:Fpath.t -> (unit, string) result
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

    (** {1:tmpdirs Temporary directories}

        See also {{!B0_std.Os.Path.tmppaths}temporary paths}. *)

    val with_tmp :
      ?mode:int -> ?make_path:bool -> ?dir:Fpath.t -> ?name:Path.tmp_name ->
      (Fpath.t -> 'a) -> ('a, string) result
    (** [with_tmp ~mode ~make_path ~dir ~name f] creates a temporary empty
        directory [t] and returns Ok (f t). After the function returns
        (normally or via an exception) [t] and its content are deleted.
        {ul
        {- [name] is used to construct the filename of the directory,
           see {!type:B0_std.Os.Path.tmp_name} for details. It defaults to
           ["tmp-%s"].}
        {- [dir] is the directory in which the temporary file is created.
           It defaults to {!B0_std.Os.Dir.default_tmp}.}
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

  (** {1:fd File descriptors and sockets} *)

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

    val copy : ?buf:Bytes.t -> Unix.file_descr -> dst:Unix.file_descr -> unit
    (** [copy ~buf src ~dst] reads [src] and writes it to [dst] using
        [buf] as a buffer; if unspecified a buffer of length
        {!unix_buffer_size} is created for the call. Raise {!Unix.Unix_error}
        if that happens *)

    val to_string : Unix.file_descr -> string
    (** [to_string fd] reads [fd] to a string. Raises {!Unix.Unix_error} in
        case of error. *)

    val read_file : string -> Unix.file_descr -> string
    (** [read_file fn fd] reads [fd] to a string assuming it is a file
        descriptor open on file path [fn]. Raises [Failure] in case of error
        with an error message that mentions [fn]. *)
  end

    (** Socket operations. *)
  module Socket : sig

    (** Endpoints. *)
    module Endpoint : sig
      type t =
      [ `Host of string * int (** Hostname and port. *)
      | `Sockaddr of Unix.sockaddr (** Given socket address. *)
      | `Fd of Unix.file_descr (** Direct file descriptor. *) ]
      (** The type for specifying a socket endpoint to connect or to listen
          to on. *)

      val of_string : default_port:int -> string -> (t, string) result
      (** [of_string ~default_port s] parses a socket endpoint
          specification from [s]. The format is [ADDR[:PORT]] or [PATH] for
          a Unix domain socket (detected by the the presence of a
          {{!Stdlib.Filename.dir_sep}directory separator}). [default_port]
          port is used if no [PORT] is specified. *)

      val pp : Format.formatter -> t -> unit
      (** [pp] formats endpoints. *)

      val wait_connectable :
        ?socket_type:Unix.socket_type -> timeout:Mtime.Span.t -> t ->
        ([`Ready | `Timeout], string) result
      (** [wait_connectable ~timeout ep st] blocks until [fd]
          becomes connectable or duration [timeout] elapses.

          [socket_type] defines the kind of socket, it defaults to
          {!Unix.SOCK_STREAM}. *)

      val wait_connectable' :
        ?socket_type:Unix.socket_type -> timeout:Mtime.Span.t -> t ->
        (unit, string) result
        (** [wait_connectable'] is like {!wait_connectable} but errors with a
            message on timeout. *)
    end

    val of_endpoint :
      Endpoint.t -> Unix.socket_type ->
      (Unix.sockaddr option * Unix.file_descr * bool, string) result
    (** [socket_of_endpoint e st] is [Ok (addr, fd, close)] with:
        {ul
        {- [addr], the address for the socket, if any.}
        {- [fd], the file descriptor for the socket. If [c] is [`Fd fd]
           this is [fd] untouched. Otherwise [fd] is a new file descriptor
           set to {{!Unix.set_nonblock}non-blocking mode} and has
           {{!Unix.set_close_on_exec}close on exec} set to [true].}
        {- [close] is [true] if the caller is in charge of closing it. This
           is [false] iff [c] is [`Fd _].}} *)

    val pp_sockaddr : Format.formatter -> Unix.sockaddr -> unit
    (** [pp_sockaddr] formats a socket address. *)
  end

  (** {1:process Processes} *)

  (** Environment variables.

      {b Note.} On Windows environment variable names are case
      insensitive. All the operations performed by this module take this
      into account when {!Sys.win32} is [true]. Be careful if you deal
      with {!Env.assignments} directly. *)
  module Env : sig

    (** {1:var Variables} *)

    type var_name = string
    (** The type for environment variable names. Case insensitive
        on Windows. *)

    val var : empty_is_none:bool -> var_name -> string option
    (** [var ~empty_is_none name] is the value of the environment
        variable [name] in the current process environment, if
        defined. If [empty_is_none] is [true], [None] is returned if
        the variable value is the empty string [""]. *)

    val var' :
      empty_is_none:bool -> (var_name -> ('a, string) result) -> var_name ->
      ('a option, string) result
    (** [var' ~empty_is_none parse name] is like {!var} but the value
        is parsed with [parse]. If the latter errors with [Error e],
        [Error (Fmt.str "%s env: %s" name e)] is returned. *)

    (** {1:env Process environement} *)

    type t
    (** The type for process environments. *)

    val empty : t
    (** [empty] is an empty environment. *)

    val current : unit -> (t, string) result
    (** [current ()] is the current process environment. *)

    val find : var_name -> t -> string option
    (** [find name env] lookups variable [name] in [env]. *)

    val override : t -> by:t -> t
    (** [override env ~by:over] overrides the definitions in [env] by
        those in [by]. *)

    val add : var_name -> string -> t -> t
    (** [add name v env] is [env] but with variable [name] bound to [v] *)

    val fold : (var_name -> string -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f env init] folds [f] on [env]'s bindings starting with
        [init]. *)

    val remove : var_name -> t -> t
    (** [remove name env] is [env] without a binding for variable [name]. *)

    val mem : var_name -> t -> bool
    (** [mem name env] is [true] iff variable [name] is bound in [env]. *)

    val pp : t Fmt.t
    (** [pp] formats environments for inspection. *)

    (** {1:assign Process environments as assignments} *)

    type assignments = string list
    (** The type for environments as lists of strings of the form
        ["VAR=value"]. *)

    val current_assignments : unit -> (assignments, string) result
    (** [current_assignments ()] is the current process environment as
        assignments. *)

    val of_assignments : ?init:t -> string list -> (t, string) result
    (** [of_assignments ~init ss] folds over strings in [ss],
        {{!B0_std.String.cut_left}cuts} them at the leftmost ['='] character and
        adds the resulting pair to [init] (defaults to {!empty}). If
        the same variable is bound more than once, the last one takes
        over. *)

    val to_assignments : t -> assignments
    (** [to_assignments env] is [env]'s bindings as assignments. *)

    val pp_assignments : assignments Fmt.t
    (** [pp] formats assignments for inspection. *)
  end

  (** Executing commands. *)
  module Cmd : sig

    (** {1:tool_search Tool search}  *)

    val path_search :
      ?win_exe:bool -> ?path:Fpath.t list -> unit -> Cmd.tool_search
    (** [path_search ~win_exe ~path () cmd] searches the
        {{!B0_std.Cmd.type-tool}tool} of [cmd] in the [path]
        directories. If the tool:

        {ul
        {- Has a single path segment: that {e filename} is
           searched, in list order, for the first matching
           {{!File.is_executable}executable file} in the directories
           of [path]. [path] defaults to the environment variable
           [PATH] parsed with {!Fpath.list_of_search_path}.}
        {- Has multiple path segments: the {e file path} is simply tested for
           {{!File.is_executable}existence and executability}
           and [cmd] is returned if that is the case (possibly by altered
           by the [win_exe] behaviour, see below). If the path is relative
           it is tested relative to the process' current working directory.}}

        If [win_exe] is [true] (defaults to {!Stdlib.Sys.win32}) an
        [.exe] suffix is added to the command's tool if it doesn't
        already have one. . *)

    val find : ?search:Cmd.tool_search -> Cmd.t -> Cmd.t option
    (** [find ~search cmd] is [cmd] with its {!B0_std.Cmd.val-tool}
        resolved to the executable file for the tool specified by [cmd]
        using [search] (defaults to [path_search ()]) or [Ok None] if
        the tool cannot be found. *)

    val find_first : ?search:Cmd.tool_search -> Cmd.t list -> Cmd.t option
    (** [find_first ?search cmds] is [List.find_map (find ?search) cmds]. *)

    val get : ?search:Cmd.tool_search -> Cmd.t ->
      (Cmd.t, string) result
    (** [get] is like {!find} except but return an error message if [Ok None]
        is returned. *)

    val get_first :
      ?search:Cmd.tool_search -> Cmd.t list -> (Cmd.t, string) result
    (** [get_first_tool cmds] is the first command of [cmds] that can be found
        with {!find} or an error if none is found. *)

    (** {1:statuses Process completion statuses} *)

    type status = [ `Exited of int | `Signaled of int ]
    (** The type for process exit statuses. *)

    val pp_status : status Fmt.t
    (** [pp_status] is a formatter for process exit statuses of the form:
        {ul
        {- ["exited %d"] for [`Exited _] values}
        {- ["signaled %s"] for [`Signaled _] value}} *)

    val pp_cmd_status : (Cmd.t * status) Fmt.t
    (** [pp_cmd_status] is a formatter for command process exit statuses
        of the form: ["cmd [%a]: %a"]. *)

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
        {- [cwd] defaults to {!Dir.val-cwd}[ ()]}
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
    (** [run_out] is {!run_status_out} with non-[`Exited 0] statuses
        reporting the captured output (if any) prefixed by
        {!pp_cmd_status}. *)

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
        defaults to {!Env.current_assignments}[ ()], [cwd] to {!Dir.val-cwd}[
        ()], [stdin] to {!in_stdin}, [stdout] to {!out_stdout} and
        [stderr] to {!out_stderr}. *)

    val spawn_poll_status : pid -> (status option, string) result
    (** [spawn_poll_status pid] tries to collect the exit status of
        command spawn [pid]. If [block] is [false], [Ok None] is immediately
        returned if [pid] has not terinated yet. *)

    val spawn_wait_status : pid -> (status, string) result
    (** [spawn_wait_status] blocks and waits for [pid]'s termination status to
        become available. *)

    val kill : pid -> int -> (unit, string) result
    (** [kill pid signal] sends signal [signal] to the process [pid].

        {b Windows.} Only the {!Sys.sigkill} signal is emulated. *)

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
      ?env:Env.assignments -> ?cwd:Fpath.t ->
      ?argv0:string -> Cmd.t -> ('a, string) result
    (** [execv ~env ~cwd cmd] executes the realpath pointed by
        {!Cmd.tool}[ cmd] as a new process in environment with [cmd] as
        the {!Sys.argv} of this process. The function only returns in
        case of error. [env] defaults to
        {!B0_std.Os.Env.current_assignments}[ ()], [cwd] to
        {!B0_std.Os.Dir.val-cwd}[ ()]. If [argv0] is specified
        it is used instead of [cmd]'s tool for Sys.argv.(0). *)

    type t = Cmd.t
    (** {!Exit} needs that alias to refer to {!B0_std.Cmd.t}. *)
  end

  (** Program exit.

      There are two ways for a program to exit: either by returning an
      exit code or by calling {!B0_std.Os.Cmd.execv}. The type here
      allows to represent these two ways of exiting. *)
  module Exit : sig

    (** {1:program_exits Program exits} *)

    type code = int
    (** The type for exit codes. *)

    type execv
    (** The type for execv calls. *)

    type t =
    | Code : code -> t (** [exit] with code. *)
    | Execv : execv -> t (** exit with [execv] *)
    (** The type for specifying program exits.  *)

    val get_code : t -> code
    (** [get_code e] is the exit code of [e]. Raises [Invalid_argument] if
        [e] is {!Exec}. *)

    val exit : ?on_error:t -> t -> 'a
    (** [exit ~on_error e] exits according to [e]:
        {ul
        {- If [e] is [Code c], {!Stdlib.exit}[ c] is called and the
           function never returns.}
        {- If [e] is [Execv execv], [execv] is called. This can only
           return with an [Error _]. In that case the error
           is logged and [exit] is called again with [on_error]
           (and the default [on_error])}}
        [on_error] defaults to {!some_error}. Except if an asynchronous
        exception is raised this function never returns. *)

    (** {1:exit_with_codes Exiting with codes}

        {b Note.} The constants here match those established by
        [Cmdliner] with another one useful in cli tools. But we don't
        want a [Cmdliner] dependency on it here. *)

    val code : code -> t
    (** [code c] is [Code c]. *)

    val ok : t
    (** [ok] is [Code 0]. *)

    val no_such_name : t
    (** [no_such_name] is [Code 122], it indicates a named entity was
        not found. *)

    val some_error : t
    (** [some_error] is [Code 123], it indicates an indiscriminate
        error reported on stdout. *)

    val cli_error : t
    (** [cli_error] is [Code 124], it indicates a command line parsing error. *)

    val internal_error : t
    (** [internal_error] is [Code 125], it indicates an unexpected internal
        error (bug). *)

    (** {1:exit_results Exit with [results]} *)

    val of_result : (unit, string) result -> t
    (** [of_result v] exits with {!ok} if [v] is [Ok ()] and logs the
        Error and exits with {!some_error} if [v] is [Error _]. *)

    val of_result' : (t, string) result -> t
    (** [of_result v] exits with [e] if [v] is [Ok e] and logs the
        error and exits with {!some_error} if [v] is [Error _]. *)

    (** {1:exit_with_execv Exit by [execv]} *)

    val execv :
      ?env:Env.assignments -> ?cwd:Fpath.t -> ?argv0:string -> Cmd.t -> t
    (** [exec ?env ?cwd ?argv0 cmd] is an [Exec _]. That has a call to
        {!Os.Cmd.execv} with the corresponding arguments. *)

    val execv_env : execv -> Env.assignments option
    (** [execv_env exec] is the environment of [exec]. *)

    val execv_cwd : execv -> Fpath.t option
    (** [execv_env exec] is the environment of [exec]. *)

    val execv_argv0 : execv -> string option
    (** [execv_env exec] is the environment of [exec]. *)

    val execv_cmd : execv -> Cmd.t
    (** [execv_env exec] is the command of [exec]. *)

    (** {1:sigexit Signal exit hooks} *)

    val on_sigint :  hook:(unit -> unit) -> (unit -> 'a) -> 'a
    (** [on_sigint ~hook f] calls [f ()] and returns its value. If
        [SIGINT] is signalled during that time [hook] is called
        followed by [exit 130] – that is the exit code a [SIGINT]
        would produce.

        [on_sigint] replaces an existing signal handler for
        {!Sys.sigint} during time of the function call. It is restored
        when the function returns.

        {b Note.} Since {!Stdlib.exit} is called {!Stdlib.at_exit}
        functions are called if a [SIGINT] occurs during the call to
        [f]. This is not the case on an unhandled [SIGINT]. *)
  end


  (** {1:sleeping_and_timing Sleeping and timing} *)

  val sleep : Mtime.Span.t -> Mtime.Span.t
  (** [sleep dur] sleeps for duration [dur] and returns the duration
      slept. The latter may be smaller than [dur] if the call was
      interrupted by a signal. This becomes imprecise if [dur] is
      greater than ~104 days. *)

  val relax : unit -> unit
  (** [relax] sleeps for a very small duration. Can be used
      for relaxed busy waiting. *)

  (** CPU time and information. *)
  module Cpu : sig

    val logical_count : unit -> int
    (** [logical_count ()] is the number of logical CPUs available
        on the running machine. *)

    (** Measuring CPU user and system time. *)
    module Time : sig

      (** {1:cpu_span CPU time spans} *)

      (** CPU time spans. *)
      module Span : sig

        type t
        (** The type for CPU execution time spans. *)

        val make :
          utime:Mtime.Span.t -> stime:Mtime.Span.t ->
          children_utime:Mtime.Span.t -> children_stime:Mtime.Span.t -> t
        (** [make ~utime ~stime ~children_utime ~children_stime] is a cpu
            span with the given fields. See accessors for
            semantics. *)

        val zero : t
        (** [zero] is zero CPU times. *)

        val utime : t -> Mtime.Span.t
        (** [utime cpu] is the user time of [cpu]. *)

        val stime : t -> Mtime.Span.t
        (** [stime cpu] is the system time of [cpu]. *)

        val children_utime : t -> Mtime.Span.t
        (** [children_utime cpu] is the user time for children processes
            of [cpu]. *)

        val children_stime : t -> Mtime.Span.t
        (** [children_stime cpu] is the system time for children processes
            of [cpu]. *)
      end
      (** {1:counter CPU time counters} *)

      type counter
      (** The type for CPU time counters. *)

      val counter : unit -> counter
      (** [counter ()] is a counter counting from now on. *)

      val count : counter -> Span.t
      (** [count c] are CPU times since [c] was created. *)
    end
  end

  (** Monotonic time clock and sleep.

      See {!B0_std.Mtime} for a discussion about monotonic time. *)
  module Mtime : sig

    (** {1:monotonic_clock Monotonic clock} *)

    val now : unit -> Mtime.t
    (** [now ()] is the current system-relative monotonic timestamp. Its
        absolute value is meaningless. *)

    val elapsed : unit -> Mtime.Span.t
    (** [elapsed ()] is the monotonic time span elapsed since the
        beginning of the program. *)

    (** {1:monotonic_counters Monotonic wall-clock time counters} *)

    type counter
    (** The type for monotonic wall-clock time counters. *)

    val counter : unit -> counter
    (** [counter ()] is a counter counting from now on. *)

    val count : counter -> Mtime.Span.t
    (** [count c] is the monotonic time span elapsed since [c] was created. *)

    (** {1:err Error handling}

        The functions {!elapsed}, {!now}, {!val-counter},
        raise [Sys_error] whenever they can't determine the
        current time or that it doesn't fit in [Mtime]'s range. Usually
        this exception should only be catched at the toplevel of your
        program to log it and abort the program. It indicates a serious
        error condition in the system.

        {1:platform_support Platform support}

        {ul
        {- Platforms with a POSIX clock (includes Linux) use
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]}
        with CLOCK_MONOTONIC.}
        {- Darwin uses
        {{:https://developer.apple.com/library/mac/qa/qa1398/_index.html}[mach_absolute_time]}.}
        {- Windows uses
        {{:https://msdn.microsoft.com/en-us/library/windows/desktop/aa373083%28v=vs.85%29.aspx}Performance counters}.}} *)
  end

  val exn_don't_catch : exn -> bool
  (** [exn_don't_cath exn] is [true] iff [exn] is [Stack_overflow],
      [Out_of_memory] or [Sys.Break]. *)
end

(** Program log.

    Support for program logging. Not to be used by build logic.

    The module is modelled after [Logs] logging, see
    {{!Logs.basics}this quick introduction}. It can be made
    to log on a [Logs] source, see {{!Log.logger}here}. *)
module Log : sig

  (** {1:levels Reporting levels} *)

  type level = Quiet | Stdout | Stderr | Error | Warning | Info | Debug (** *)
  (** The type for reporting levels. They are meant to be used
      as follows:
      {ul
      {- [Quiet] doesn't report anything.}
      {- [Stdout] can be used for the standard output of an application.
         Using this instead of [stdout] directly allows the output to be
         silenced by [Quiet] which may be desirable, or not.}
      {- [Stderr] can be used for the standard error of an application.
         Using this instead of [stderr] directly
         allows the output to be silenced by [Quiet] which may be
         desirable, or not.}
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

  val stdout : 'a log
  (** [stdout] is [msg Stdout]. *)

  val stderr : 'a log
  (** [stderr] is [msg Stderr]. *)

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

  (** {2:time Logging time} *)

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
  (** [spawn_tracer level] is a {{!B0_std.Os.Cmd.tracing}spawn tracer}
      that logs with level [level]. If [level] is {!Log.Quiet} this is
      {!B0_std.Os.Cmd.spawn_tracer_nop}. *)

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


(** Future values.

    A future is an undetermined value that becomes determined at an an
    arbitrary point in the future. The future acts as a placeholder
    for the value while it is undetermined. *)
module Fut : sig

  (** {1:fut Future values} *)

  type 'a t
  (** The type for futures with values of type ['a]. *)

  val make : unit -> 'a t * ('a -> unit)
  (** [make ()] is [(f, set)] with [f] the future value and [set]
      the function to [set] it. The latter can be called only once,
      [Invalid_argument] is raised otherwise. *)

  val await : 'a t -> ('a -> unit) -> unit
  (** [await f k] waits for [f] to be determined and continues with [k v]
      with [v] the value of the future. If the future never determines
      [k] is not invoked. [k] must not raise. *)

  val value : 'a t -> 'a option
  (** [value f] is [f]'s value, if any. *)

  val sync : 'a t -> 'a
  (** [sync f] waits for [f] to determine. {b Warning.} This is relaxed busy
      waiting. *)

  val return : 'a -> 'a t
  (** [return v] is a future that determines [v]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn f] is [return (fn v)] with [v] the value of [f]. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind f fn] is the future [fn v] with [v] the value of [f]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair f0 f1] determines with the value of [f0] and [f1]. *)

  val of_list : 'a t list -> 'a list t
  (** [of_list fs] determines with the values of all [fs], in the same order. *)

  (** Future syntax. *)
  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [let*] is {!bind}. *)

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    (** [and*] is {!pair}. *)
  end
end

(** Blocking values.

    {b Note.} In direct style the {!Fut.t} values would go away.
    For now be bundled lazy blocking values in the same structure. *)
module Bval : sig

  type 'a setter
  (** The type for setting blocking value. *)

  type 'a t
  (** The type for immutable blocking values. *)

  val make : unit -> 'a t * 'a setter
  (** [make ()] is a blocking value and a setter to set it. *)

  val of_val : 'a -> 'a t
  (** [of_val v] is a (non-)blocking value holding [v]. *)

  val of_lazy_fun : (unit -> 'a) -> 'a t
  (** [of_lazy_fun f] is a blocking value that runs [f]
      iff {!get} or {!poll} is called on the value.

      {b XXX.} Something should be said about the context in
      which f runs.  *)

  val of_setter : 'a setter -> 'a t
  (** [of_setter s] is the blocking value of [s]. *)

  val is_lazy : 'a t -> bool
  (** [is_lazy bv] is [true] iff [bv] is a lazily triggered value. *)

  (** {1:setting Setting} *)

  val set : 'a setter -> 'a -> unit
  (** [set s v] sets the blocking value [of_setter s] to value [v].
      Raises [Invalid_argument] if [set] is already set. *)

  val try_set : 'a setter -> 'a -> bool
  (** [try_set s v] is [true] if [iv] was set to [v] and [false]
      if [iv] was already set. *)

  (** {1:getting Getting} *)

  val get : 'a t -> 'a Fut.t
  (** [get bv] is the value of [bv]. In direct style,
      this should be a blocking call. *)

  val poll : 'a t -> 'a option
  (** [poll bv] is [None] if [get bv] would block
      and [Some _] if it does not block. If [bv] was created
      with {!of_lazy_fun}, this ensure the computation gets triggered. *)

  val stir : 'a t -> unit
  (** [stir bv] is [ignore (poll v)]. Useful if you know [bv] will
      be needed later and may be a {!of_lazy_fun}. *)

  (** {1:formatting Formatting} *)

  val pp : 'a Fmt.t -> 'a t Fmt.t
  (** [pp] formats blocking values. Does not block if the value is not
      set in which case "<pending>" formatted. *)
end
