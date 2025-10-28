(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Operating system interaction.

    @canonical B0_std.Os *)

(** {1:file_system File system} *)

(** File system path operations.

    These functions operate on files and directories
    equally. Specific function operating on either kind of path are
    in the {!File} and {!Dir} modules. *)
module Path : sig

  (** {1:existence Existence} *)

  val exists : B0__fpath.t -> (bool, string) result
  (** [exists p] is [Ok true] if [p] exists in the file system
      and [Ok false] otherwise. Symbolic links are followed.

      See also {!File.exists}, {!Dir.exists}. *)

  val must_exist : B0__fpath.t -> (unit, string) result
  (** [must_exist p] is [Ok ()] if [p] exists in the file system
      and an error otherwise. Symbolic links are followed.

      See also {!File.must_exist}, {!Dir.must_exist}. *)

  (** {1:renaming Deleting and renaming} *)

  val delete : recurse:bool -> B0__fpath.t -> (bool, string) result
  (** [delete ~recurse p] deletes path [p] from the file system. If:
      {ul
      {- [p] is a file, the file is deleted.}
      {- [p] is an empty directory, the directory is deleted.}
      {- [p] is a non-empty directory and [recurse] is [true], the directory
         and all its contents is deleted.}
      {- [p] is a symbolic link, the link is deleted, not the linked object.}
      {- [p] is a dangling symbolic link, the link is deleted.}
      {- Otherwise the function errors.}}
      The result is:
      {ul
      {- [Ok true], if [p] existed and was deleted.}
      {- [Ok false], if the path [p] did not exist on the file system.}
      {- [Error _ ] in case of error. In particular if [p] is a non-empty
         directory and [recurse] is [false].}}
      See also {!File.delete}, {!Dir.delete} and {!Dir.delete_contents}. *)

  val rename :
    force:bool -> make_path:bool -> B0__fpath.t -> dst:B0__fpath.t ->
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

  val realpath : B0__fpath.t -> (B0__fpath.t, string) result
  (** [realpath p] expands all symbolic links and resolves all
      references to [.] and [..] segments. The function errors if
      [p] does not exist. *)

  (** {1:copy Copying} *)

  val copy :
    ?rel:bool -> ?atomic:bool -> ?follow_symlinks:bool ->
    ?prune:(Unix.stats -> string -> B0__fpath.t -> bool) -> make_path:bool ->
    recurse:bool -> B0__fpath.t -> dst:B0__fpath.t -> (unit, string) result
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

  val get_mode : B0__fpath.t -> (int, string) result
  (** [get_mode p] is the file mode of [p]. Symbolic links are followed. *)

  val set_mode : B0__fpath.t -> int -> (unit, string) result
  (** [set_mode file p] sets the file mode of [file] to [p]. Symbolic
      links are followed. *)

  val stat : B0__fpath.t -> (Unix.stats, string) result
  (** [stat p] is [p]'s file information. Symbolic links are followed. *)

  val is_mount_point : B0__fpath.t -> (bool, string) result
  (** [is_mount_point p] is [true] if [p] looks like a mount point. The
      criterion is if [p] and [p/..]'s {!stat} have a differing
      {!Unix.stat.std_dev} field. *)

  (** {1:symlinks Symbolic links}

      For hard links see {!File.hard_links}. *)

  val symlink :
    force:bool -> make_path:bool -> src:B0__fpath.t -> B0__fpath.t ->
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

  val symlink_link : B0__fpath.t -> (B0__fpath.t, string) result
  (** [symlink_link p] is [Ok l] if [p] is a symbolic link to [l]. *)

  val symlink_stat : B0__fpath.t -> (Unix.stats, string) result
  (** [symlink_stat p] is like {!stat} but if [p] is a symlink returns
      information about the link itself. If [p] is not a symlink then
      this is {!stat}. *)

  (** {1:tmppaths Temporary paths} *)

  type tmp_name = (string -> string, unit, string) format
  (** The type for temporary file name patterns. The string format
      is replaced by random hexadecimal ASCII characters. *)

  val tmp :
    ?make_path:bool -> ?dir:B0__fpath.t -> ?name:tmp_name -> unit ->
    (B0__fpath.t, string) result
  (** [tmp ~make_path ~dir name ()] is a file system path in [dir] that
      did not exist when the name was devised. It may exist once the function
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

  val exists : B0__fpath.t -> (bool, string) result
  (** [exists file] is [Ok true] if [file] is a regular file in the
      file system and [Ok false] otherwise. Symbolic links are
      followed.

      See also {!Path.exists}, {!Dir.exists}. *)

  val must_exist : B0__fpath.t -> (unit, string) result
  (** [must_exist file] is [Ok ()] if [file] is a regular file in
      the file system and an error otherwise. Symbolic links are
      followed.

      See also {!Path.must_exist}, {!Dir.must_exist}. *)

  val is_executable : B0__fpath.t -> bool
  (** [is_executable file] is [true] iff [file] exists and is executable. *)

  (** {1:delete_truncate Deleting and truncating} *)

  val delete : B0__fpath.t -> (bool, string) result
  (** [delete file] deletes file [file] from the file system. If
      {ul
      {- [file] is a file, the file is deleted.}
      {- [file] is a symbolic link to a file, the link is deleted,
         not the linked file.}
      {- [file] is a dangling symbolic link, the link is deleted.}
      {- Otherwise the function errors.}}
      The result is:
      {ul
      {- [Ok true], if the path [file] existed and was deleted.}
      {- [Ok false], if the path [file] did not exist on the file system.}
      {- [Error _] in case of error. In particular if [file] is a directory.}}
      See also {!Path.delete}, {!Dir.delete}. *)

  val truncate : B0__fpath.t -> int -> (unit, string) result
  (** [trunacte file size] truncates [file] to [size]. *)

  (** {1:hard_links Hard links}

      For symbolic links see {!Path.symlinks}. *)

  val link :
    force:bool -> make_path:bool -> src:B0__fpath.t -> B0__fpath.t ->
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
    B0__fpath.t -> (Unix.file_descr -> 'b) -> ('b, string) result
  (** [read_with_ic file f] opens [file] as a file descriptor [fdi]
      and returns [Ok (f ic)]. If [file] is {!Fpath.dash}, [ic] is
      {!stdin}.  After the function returns (normally or via an
      exception raised by [f]), [ic] is ensured to be closed, except
      if it is {!stdin}. The function errors if opening [file]
      fails. Errors have the form [Fmt.str "%s: %s" file err]. *)

  val read_with_ic : B0__fpath.t -> (in_channel -> 'b) -> ('b, string) result
  (** [read_with_ic file f] is exactly like {!read_with_fd} but
      opens an OCaml input channel in binary mode. *)

  val read : B0__fpath.t -> (string, string) result
  (** [read file] is [file]'s content as a string. If [file] is
      {!Fpath.dash} the contents of {!stdin} is read. {b Warning.} The
      signature of this function limits files to be at most
      {!Sys.max_string_length} in size. On 32-bit platforms this is
      {b only around [16MB]}. Errors have the form
      [Fmt.str "%s: %s" file err]. *)

  (** {1:writes Writing and copying} *)

  val write_with_fd :
    ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
    B0__fpath.t -> (Unix.file_descr -> ('a, 'b) result) ->
    (('a, 'b) result, string) result
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
    ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
    B0__fpath.t -> (out_channel -> ('a, 'b) result) ->
    (('a, 'b) result, string) result
  (** [write_with_oc ~atomic ~mode ~force ~make_path file f] operates like
      {!write_with_fd} but opens an OCaml channel in binary mode. *)

  val write :
    ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
    B0__fpath.t -> string -> (unit, string) result
  (** [write ~atomic ~mode ~force ~make_path file s] operates like
      {!write_with_fd} but directly writes [s] to [file]. *)

  val copy :
    ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
    B0__fpath.t -> dst:B0__fpath.t -> (unit, string) result
  (** [copy ~atomic ~mode ~force ~path ~make_path src ~dst:file]
      operates like {!write_with_fd} but directly writes the content
      of [src] (or {!stdin} if [src] is {!Fpath.dash}) to [file].
      [mode] defaults to the permissions of [src] if available and
      [0o644] otherwise. *)

  val copy_to_dir :
    ?atomic:bool -> ?mode:int -> force:bool -> make_path:bool ->
    ?src_root:B0__fpath.t -> B0__fpath.t -> dir:B0__fpath.t ->
    (unit, string) result
 (** [copy ~force ~make_path src ~dir] is
     [copy ~force ~make_path src ~dst] with [dst] equal to
     {!Fpath.reroot}[ ~src_root ~dst_root:dir src] and [src_root]
     defaulting to {!Fpath.parent}[ src]. *)

  (** {1:tmpfiles Temporary files}

      See also {{!B0_std.Os.Path.tmppaths}temporary paths}. *)

  val with_tmp_fd :
    ?flags:Unix.open_flag list -> ?mode:int -> ?make_path:bool ->
    ?dir:B0__fpath.t -> ?name:Path.tmp_name ->
    (B0__fpath.t -> Unix.file_descr -> 'b) -> ('b, string) result
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
    ?dir:B0__fpath.t -> ?name:Path.tmp_name -> unit ->
    (B0__fpath.t * Unix.file_descr, string) result
  (** [open_tmp_fd] is like {!with_tmp_fd} except it is the client's
      duty to close the file descriptor and delete the file (if the
      file is not deleted it will be when the program exits). *)

  val with_tmp_oc :
    ?flags:Unix.open_flag list -> ?mode:int -> ?make_path:bool ->
    ?dir:B0__fpath.t -> ?name:Path.tmp_name ->
    (B0__fpath.t -> out_channel -> 'b) -> ('b, string) result
  (** [with_tmp_oc] is like {!with_tmp_fd} but uses an OCaml output channel
      instead of a file decriptor. *)
end

(** Directory operations.

    This module operates on directories, most functions error if
    they are applied to other file kinds. *)
module Dir : sig

  (** {1:existence Existence} *)

  val exists : B0__fpath.t -> (bool, string) result
  (** [exists dir] is [Ok true] if [dir] is a directory in the file system
      and [Ok false] otherwise. Symbolic links are followed. *)

  val must_exist : B0__fpath.t -> (unit, string) result
  (** [must_exist dir] is [Ok ()] if [dir] is a directory in the file system
      and an error otherwise. Symbolic links are followed. *)

  (** {1:create Creating} *)

  val create :
    ?mode:int -> make_path:bool -> B0__fpath.t -> (bool, string) result
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

  (** {1:delete Deleting} *)

  val delete : recurse:bool -> B0__fpath.t -> (bool, string) result
   (** [delete ~recurse dir] delete the directory [dir] from the file
       system. If:
      {ul
      {- [dir] is an empty directory, the directory is deleted.}
      {- [dir] is a non-empty directory and [recurse] is [true], the directory
         and all its contents is deleted.}
      {- [dir] is a symbolic link to a directory, the link is deleted, not
        the linked directory.}
      {- [dir] is a dangling symbolic link, the link is deleted.}
      {- Otherwise the function errors.}}
      The result is:
      {ul
      {- [Ok true], if [dir] existed and was deleted.}
      {- [Ok false], if the path [dir] did not exist on the file system.}
      {- [Error _ ] in case of error. In particular if [dir] is a non-empty
         directory and [recurse] is [false] or if [dir] is a file or a symlink
         to a file.}}
       See also {!Dir.delete_contents}, {!Path.delete}, {!File.delete}. *)

  val delete_contents : B0__fpath.t -> (bool, string) result
  (** [delete_contents ~dotfiles dir] deletes the contents of the existing
      directory [dir]. If:
      {ul
      {- [dir] is a directory, all its content is deleted.}
      {- [dir] is a symbolic link to a directory, all the contents
         of the linked directory is deleted.}
      {- Otherwise the function errors}}
      The result is:
      {ul
      {- [Ok true], if [dir] was non empty.}
      {- [Ok false], if [dir] was empty.}
      {- [Error _] in case of error. In particular if [dir] does not
         exist or points to a file.}}
      See also {!Dir.delete}, {!Path.delete}, {!File.delete}. *)

  (** {1:content Contents} *)

  val fold :
    ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
    ?prune_dir:(Unix.stats -> string -> B0__fpath.t -> 'a -> bool) ->
    recurse:bool -> (Unix.stats -> string -> B0__fpath.t -> 'a -> 'a) ->
    B0__fpath.t -> 'a -> ('a, string) result
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
    ?prune_dir:(Unix.stats -> string -> B0__fpath.t -> 'a -> bool) ->
    recurse:bool -> (Unix.stats -> string -> B0__fpath.t -> 'a -> 'a) ->
    B0__fpath.t -> 'a -> ('a, string) result
  (** [fold_files] is like {!fold} but [f] is only applied to
      non-directory files. *)

  val fold_dirs :
    ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
    ?prune_dir:(Unix.stats -> string -> B0__fpath.t -> 'a -> bool) ->
    recurse:bool -> (Unix.stats -> string -> B0__fpath.t -> 'a -> 'a) ->
    B0__fpath.t -> 'a -> ('a, string) result
  (** [fold_dirs] is like {!fold} but [f] is only applied
      to directory files. *)

  val prune_denied : (Unix.stats -> string -> B0__fpath.t -> 'a -> bool)
  (** [prune_denied] is a [prune] function for {!fold}s to
      skip directories for which the user has no [R_OK] and [X_OK]
      permissions. *)

  val path_list :
    Unix.stats -> string -> B0__fpath.t -> B0__fpath.t list ->
    B0__fpath.t list
  (** [path_list] is a {{!fold}folding} function to get a (reverse w.r.t.
      list of paths). Paths in the result that correspond to directories
      satisfy {!Fpath.is_syntactic_dir}. *)

  val contents :
    ?kind:[`All | `Dirs | `Files ] ->
    ?rel:bool -> ?dotfiles:bool -> ?follow_symlinks:bool ->
    ?prune_dir:(Unix.stats -> string ->
                B0__fpath.t -> B0__fpath.t list -> bool) ->
    recurse:bool -> B0__fpath.t -> (B0__fpath.t list, string) result
  (** [contents] uses {!path_list} with:
      {ul
      {- {!fold} if [kind] is [`All] (default)}
      {- {!fold_files} if [kind] is [`Files].}
      {- {!fold_dirs} if [kind] is [`Dirs]}} *)

  (** {1:copy Copying} *)

  val copy :
    ?rel:bool -> ?atomic:bool -> ?follow_symlinks:bool ->
    ?prune:(Unix.stats -> string -> B0__fpath.t -> bool) -> make_path:bool ->
    recurse:bool -> B0__fpath.t -> dst:B0__fpath.t -> (unit, string) result
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

  val cwd : unit -> (B0__fpath.t, string) result
  (** [cwd ()] is the current working directory. The resulting path
      is guaranteed to be absolute. *)

  val set_cwd : B0__fpath.t -> (unit, string) result
  (** [set_cwd dir] sets the current working directory to [dir]. *)

  val with_cwd : B0__fpath.t -> (unit -> 'a) -> ('a, string) result
  (** [with_cwd dir f] is [f ()] with the current working directory
      bound to [dir]. After the function returns the current working
      directory is back to its initial value. *)

  (** {1:tmp_default Default temporary directory} *)

  val default_tmp : unit -> B0__fpath.t
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

  val set_default_tmp : B0__fpath.t -> unit
  (** [set_default_tmp p] sets the value returned by {!default_tmp} to
      [p]. *)

  (** {1:tmpdirs Temporary directories}

      See also {{!B0_std.Os.Path.tmppaths}temporary paths}. *)

  val with_tmp :
    ?mode:int -> ?make_path:bool -> ?dir:B0__fpath.t -> ?name:Path.tmp_name ->
    (B0__fpath.t -> 'a) -> ('a, string) result
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
    ?mode:int -> ?make_path:bool -> ?dir:B0__fpath.t -> ?name:Path.tmp_name ->
    unit -> (B0__fpath.t, string) result
  (** [tmp] is like {!with_tmp} except the directory and its content
      is only deleted at the end of program execution if the client
      doesn't do it before. *)

  (** {1:base Base directories}

      The directories returned by these functions are not guaranteed
      to exist. *)

  val user : unit -> (B0__fpath.t, string) result
  (** [user ()] is the home directory of the user executing the
      process. Determined by consulting [passwd] database with the
      user id of the [SUDO_UID] env var or of the process. If this
      fails falls back to parse a path from the [HOME] environment
      variables. On Windows no special fallback is implemented. *)

  val config : unit -> (B0__fpath.t, string) result
  (** [config ()] is the directory used to store user-specific program
      configurations. This is in order:
      {ol
      {- If set the value of [XDG_CONFIG_HOME].}
      {- If set and on Windows® the value of [APPDATA].}
      {- If [user ()] is [Ok home], [Fpath.(home / ".config")].}} *)

  val data : unit -> (B0__fpath.t, string) result
  (** [data ()] is the directory used to store user-specific program
      data. This is in order:
      {ol
      {- If set the value of [XDG_DATA_HOME].}
      {- If set and on Windows® the value of [APPDATA].}
      {- If [user ()] is [Ok home], [Fpath.(home / ".local" / "share")].}} *)

  val cache : unit -> (B0__fpath.t, string) result
  (** [cache ()] is the directory used to store user-specific
      non-essential data. This is in order:
      {ol
      {- If set the value of [XDG_CACHE_HOME].}
      {- If set and on Windows® the value of [%TEMP%]}
      {- If [user ()] is [Ok home], [Fpath.(home / ".cache")]}} *)

  val runtime : unit -> (B0__fpath.t, string) result
  (** [runtime ()] is the directory used to store user-specific runtime
      files. This is in order:
      {ol
      {- If set the value of [XDG_RUNTIME_DIR].}
      {- The value of {!default_tmp}.}} *)

  val state : unit -> (B0__fpath.t, string) result
  (** [state ()] is the directory used to store user-specific state data
      files. This is in order:
      {ol
      {- If set the value of [XDG_STATE_DIR].}
      {- If [user ()] is [Ok home], [Fpath.(home / ".local" / "state")]}} *)
end

(** {1:fd File descriptors and sockets} *)

(** File descriptors operations. *)
module Fd : sig

  val unix_buffer_size : int
  (** [unix_buffer_size] is the value of the OCaml runtime
      system buffer size for I/O operations.

      {b Available} in 5.4 as {!Sys.io_buffer_size}. *)

  val close_noerr : Unix.file_descr -> unit
  (** [close_noerr fd] uses {!Unix.close} on [fd], retries on [EINTR]
      and silently catches any error it may raise. Typically used with
      {!Fun.protect}. *)

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

  (** {1:endpoints Endpoint}

      {b TODO.} We'd like to have this in {!B0_std.Net.Endpoint} but
      we need to restructure the sources. *)

  val endpoint_wait_connectable :
    ?socket_type:Unix.socket_type -> timeout:B0__mtime.Span.t ->
    B0__net.Endpoint.t ->
    ([`Ready | `Timeout], string) result
  (** [endpoint_wait_connectable ~timeout ep st] blocks until [ep] becomes
      connectable
      or duration [timeout] elapses.

      [socket_type] defines the kind of socket connection, it defaults to
      {!Unix.SOCK_STREAM}. *)

  val endpoint_wait_connectable' :
    ?socket_type:Unix.socket_type -> timeout:B0__mtime.Span.t ->
    B0__net.Endpoint.t -> (unit, string) result
    (** [wait_connectable'] is like {!wait_connectable} but errors with a
        message on timeout. *)

  (** {1:socket_endpoint Sockets} *)

  val for_endpoint :
    ?nonblock:bool -> B0__net.Endpoint.t -> Unix.socket_type ->
    (Unix.file_descr * bool * Unix.sockaddr option, string) result
  (** [for_endpoint ?nonblock e st] is [Ok (fd, close, addr)] with:
      {ul
      {- [fd], a file descriptor for the socket. If [c] is [`Fd fd]
         this is [fd] untouched except for setting or clearing [nonblock].
         Otherwise [fd] is a new file descriptor set according to
         [nonblock] and has
         {{!Unix.set_close_on_exec}close on exec} set to [true]. The socket
         is not connected, use either {!connect} or {!bind} on it. Alternatively
         directly use {!connect_endpoint}.}
      {- [close] is [true] if the caller is in charge of closing it. This
         is [false] iff [c] is [`Fd _].}
      {- [addr], the socket peer address for the endpoint, if any.}}

      [nonblock] defaults to [false]. See also {!connect_endpoint}. *)

  (** {1:connect Connecting} *)

  val connect_endpoint :
    ?nonblock:bool -> B0__net.Endpoint.t -> Unix.socket_type ->
    (Unix.file_descr * bool * Unix.sockaddr, string) result
  (** [connect_endpoint ep st] is [Ok (fd, close, addr)] with:
      {ul
      {- [fd], a file descriptor for the socket {{!connect}connected} to the
          endpoint. If [c] is [`Fd fd] this is [fd] untouched except for
          setting or clearing [nonblock], it also checks that the [fd] is
          connected and errors otherwise.}
      {- [close] is [true] if the caller is in charge of closing it. This
         is [false] iff [ep] is [`Fd _].}
      {- [addr] the socket peer address for the endpoint.}}

      See also {!with_connected_endpoint}. *)

  val with_connected_endpoint :
    ?nonblock:bool -> B0__net.Endpoint.t -> Unix.socket_type ->
    (Unix.file_descr -> Unix.sockaddr -> 'a) -> ('a, string) result
  (** [with_connected_endpoint ep st f] uses {!connect_endpoint} and
      calls [f] with the resulting file descriptor and peer address
      ensuring, even if [f] raises, that:
      {ul
      {- If needed (see {!connect_endpoint}) the file descriptor ressource is
         closed after [f] returns.}
      {- The {!Sys.sigpipe} signal is ignored during the call to [f].}} *)

  val connect : Unix.file_descr -> Unix.sockaddr -> (unit, string) result
  (** [connect fd addr] associates the peer [addr] to the file descriptor
      [fd]. Writes on [fd] send data to [addr] and reads on [fd] receive
      data from [addr]. See also {!connect_endpoint}. *)

  (** {1:listening Listening} *)

  val listen_endpoint :
    ?nonblock:bool -> ?backlog:int -> B0__net.Endpoint.t ->
    Unix.socket_type -> (Unix.file_descr * bool * Unix.sockaddr, string) result
  (** [listen_endpoint ep st] is [Ok (fd, close, addr)] with:
      {ul
      {- [fd], a file descriptor for the socket listening on the
          endpoint. If [ep] is [`Fd fd] this is [fd] untouched except for
          setting or clearing [nonblock], it also checks that the [fd] is
          {{!bind}bound} and errors otherwise.}
      {- [close] is [true] if the caller is in charge of closing it. This
         is [false] iff [ep] is [`Fd _].}
      {- [addr] the listening address for the endpoint.}}

      If [st] is [SOCK_STREAM] listen is called on the resulting [fd]
      with [backlog] (see {!listen} for default.

      See also {!with_listening_endpoint}. *)

  val with_listening_endpoint :
    ?nonblock:bool -> ?backlog:int -> B0__net.Endpoint.t ->
    Unix.socket_type -> (Unix.file_descr -> Unix.sockaddr -> 'a) ->
    ('a, string) result
  (** [with_listening_endpoint ep st f] uses {!listen_endpoint} and
      calls [f] with the resulting file descriptor and listening address
      ensuring, even if [f] raises that:
      {ul
      {- If needed (see {!listen_endpoint}) the file descriptor ressource is
         closed after [f] returns.}
      {- The {!Sys.sigpipe} signal is ignored during the call to [f].}} *)

  val accept :
    cloexec:bool -> Unix.file_descr ->
    (Unix.file_descr * Unix.sockaddr, string) result
  (** [accept ~cloexec fd] calls {!Unix.accept}. *)

  val bind : Unix.file_descr -> Unix.sockaddr -> (unit, string) result
  (** [bind fd addr] binds the [fd] to the address [addr]. *)

  val listen : ?backlog:int -> Unix.file_descr ->  (unit, string) result
  (** [listen ~backlog fd] indicates that [fd] can be used to accept
      incoming connections on the address it is {{!bind}bound} to.

      [backlog] is the maximum length for the queue of pending
      incoming connections before they start to be rejected if they
      are not accepted. It defaults to [128] (FIXME get a hand on
      SOMAXCONN). *)
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
  (** [var' ~empty_is_none parse name] is like {!val-var} but the value
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

  val pp : t B0__fmt.t
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
      {{!B0_std.String.cut}cuts} them at the leftmost ['='] character and
      adds the resulting pair to [init] (defaults to {!empty}). If
      the same variable is bound more than once, the last one takes
      over. *)

  val to_assignments : t -> assignments
  (** [to_assignments env] is [env]'s bindings as assignments. *)

  val pp_assignments : assignments B0__fmt.t
  (** [pp] formats assignments for inspection. *)
end

(** Executing commands. *)
module Cmd : sig

  (** {1:tool_search Tool search}  *)

  val path_search :
    ?win_exe:bool -> ?path:B0__fpath.t list -> unit -> B0__cmd.tool_search
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

  val find : ?search:B0__cmd.tool_search -> B0__cmd.t -> B0__cmd.t option
  (** [find ~search cmd] is [cmd] with its {!B0_std.Cmd.val-tool}
      resolved to the executable file for the tool specified by [cmd]
      using [search] (defaults to [path_search ()]) or [Ok None] if
      the tool cannot be found. *)

  val find_first :
    ?search:B0__cmd.tool_search -> B0__cmd.t list -> B0__cmd.t option
  (** [find_first ?search cmds] is [List.find_map (find ?search) cmds]. *)

  val get : ?search:B0__cmd.tool_search -> B0__cmd.t ->
    (B0__cmd.t, string) result
  (** [get] is like {!find} except but return an error message if [Ok None]
      is returned. *)

  val get_first :
    ?search:B0__cmd.tool_search -> B0__cmd.t list ->
    (B0__cmd.t, string) result
  (** [get_first_tool cmds] is the first command of [cmds] that can be found
      with {!find} or an error if none is found. *)

  (** {1:statuses Process completion statuses} *)

  type status = [ `Exited of int | `Signaled of int ]
  (** The type for process exit statuses. *)

  val pp_status : status B0__fmt.t
  (** [pp_status] is a formatter for process exit statuses of the form:
      {ul
      {- ["exited %d"] for [`Exited _] values}
      {- ["signaled %s"] for [`Signaled _] value}} *)

  val pp_cmd_status : (B0__cmd.t * status) B0__fmt.t
  (** [pp_cmd_status] is a formatter for command process exit statuses
      of the form: ["cmd [%a]: %a"]. *)

  (** {1:stdis Process standard inputs} *)

  type stdi
  (** The type for representing the standard input of a process. *)

  val in_string : string -> stdi
  (** [in_string s] is a standard input that reads the string [s]. *)

  val in_file : B0__fpath.t -> stdi
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

  val out_file :
    ?mode:int -> force:bool -> make_path:bool -> B0__fpath.t -> stdo
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
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
    ?stderr:stdo -> B0__cmd.t -> (status, string) result
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
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?stdin:stdi ->
    ?stderr:[`Stdo of stdo | `Out] -> trim:bool -> B0__cmd.t ->
    (status * string, string) result
  (** [run_status_out] is like {!run_status} except [stdout] is read
      from the process to a string. The string is {!String.trim}ed
      if [trim] is [true] (default). If [stderr] is [`Out] the
      process' [stderr] is redirected to [stdout] and thus read back
      in the string aswell. *)

  val run :
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?stdin:stdi ->
    ?stdout:stdo -> ?stderr:stdo -> B0__cmd.t -> (unit, string) result
  (** [run] is {!run_status} with non-[`Exited 0] statuses turned
      into errors via {!pp_cmd_status}. *)

  val run_out :
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?stdin:stdi ->
    ?stderr:[`Stdo of stdo | `Out] -> trim:bool -> B0__cmd.t ->
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

  val pid_cmd : pid -> B0__cmd.t
  (** [pid_cmd pid] is the command that was given to launch [pid]. *)

  val pid_to_int : pid -> int
  (** [pid_to_int pid] is the system identifier for process
      identifier [pid]. *)

  val spawn :
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
    ?stderr:stdo -> B0__cmd.t -> (pid, string) result
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

  val spawn_wait : pid -> (unit, string) result
  (** [spawn_wait] is like {!spawn_wait_status} but with non-[`Exited 0]
      statuses turned into errors with {!pp_cmd_status} *)

  val kill : pid -> int -> (unit, string) result
  (** [kill pid signal] sends signal [signal] to the process [pid].

      {b Windows.} Only the {!Sys.sigkill} signal is emulated. *)

  (** {2:tracing Tracing} *)

  type spawn_tracer =
    pid option -> Env.assignments option -> cwd:B0__fpath.t option ->
    B0__cmd.t -> unit
  (** The type for spawn tracers. Called with each blocking and
      non-blocking spawned command aswell as {!execv}. The function
      is given the process identifier of the spawn (or [None] in
      case of {!execv}), the environment if different from the
      program's one, the current working directory if different from
      the program's one and the actual command. *)

  val spawn_tracer_nop : spawn_tracer
  (** [spawn_tracer_nop] is a spawn tracer that does nothing. *)

  val spawn_tracer_log : B0__log.level -> spawn_tracer
  (** [spawn_tracer_log level] is a spawn tracer
      that logs with level [level]. If [level] is {!Log.Quiet} this is
      {!spawn_tracer_nop}. *)

  val spawn_tracer : unit -> spawn_tracer
  (** [tracer ()] is the current spawn tracer. Initially this is
      [spawn_tracer_log Log.Debug]. *)

  val set_spawn_tracer : spawn_tracer -> unit
  (** [set_tracer t] sets the current spawn tracer to [t]. *)

  (** {1:exec Executing files}

      {b Windows.} On Windows a program executing an [execv*] function
      yields back control to the terminal as soon as the child starts
      (vs. ends on POSIX). This entails all sorts of unwanted
      behaviours. To workaround this, the following function executes,
      on Windows, the file as a spawned child process which is waited
      on for completion via [waitpid(2)]. Once the child process has
      terminated the calling process is immediately [exit]ed with the
      status of the child. *)

  val execv :
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?argv0:string ->
    B0__cmd.t -> ('a, string) result
  (** [execv ~env ~cwd cmd] executes the realpath pointed by
      {!B0_std.Cmd.val-tool}[ cmd] as a new process in environment with [cmd] as
      the {!Sys.argv} of this process. The function only returns in
      case of error. [env] defaults to
      {!B0_std.Os.Env.current_assignments}[ ()], [cwd] to
      {!B0_std.Os.Dir.val-cwd}[ ()]. If [argv0] is specified
      it is used instead of [cmd]'s tool for Sys.argv.(0). *)

  type t = B0__cmd.t
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
      [e] is {!Execv}. *)

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
    ?env:Env.assignments -> ?cwd:B0__fpath.t -> ?argv0:string ->
    B0__cmd.t -> t
  (** [exec ?env ?cwd ?argv0 cmd] is an [Exec _]. That has a call to
      {!Os.Cmd.execv} with the corresponding arguments. *)

  val execv_env : execv -> Env.assignments option
  (** [execv_env exec] is the environment of [exec]. *)

  val execv_cwd : execv -> B0__fpath.t option
  (** [execv_env exec] is the environment of [exec]. *)

  val execv_argv0 : execv -> string option
  (** [execv_env exec] is the environment of [exec]. *)

  val execv_cmd : execv -> B0__cmd.t
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

val sleep : B0__mtime.Span.t -> B0__mtime.Span.t
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
        utime:B0__mtime.Span.t -> stime:B0__mtime.Span.t ->
        children_utime:B0__mtime.Span.t ->
        children_stime:B0__mtime.Span.t -> t
      (** [make ~utime ~stime ~children_utime ~children_stime] is a cpu
          span with the given fields. See accessors for
          semantics. *)

      val zero : t
      (** [zero] is zero CPU times. *)

      val utime : t -> B0__mtime.Span.t
      (** [utime cpu] is the user time of [cpu]. *)

      val stime : t -> B0__mtime.Span.t
      (** [stime cpu] is the system time of [cpu]. *)

      val children_utime : t -> B0__mtime.Span.t
      (** [children_utime cpu] is the user time for children processes
          of [cpu]. *)

      val children_stime : t -> B0__mtime.Span.t
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

  val now : unit -> B0__mtime.t
  (** [now ()] is the current system-relative monotonic timestamp. Its
      absolute value is meaningless. *)

  val elapsed : unit -> B0__mtime.Span.t
  (** [elapsed ()] is the monotonic time span elapsed since the
      beginning of the program. *)

  (** {1:monotonic_counters Monotonic wall-clock time counters} *)

  type counter
  (** The type for monotonic wall-clock time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting from now on. *)

  val count : counter -> B0__mtime.Span.t
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

(** {1:info Name, version and architecture} *)

(** OS names. *)
module Name : sig
  type id = string
  (** The type for OS identifiers. Unless you create them
      yourself these strings are normalized: non-empty and lowercase
      ASCII. *)

  type t =
  | Bsd of id
  | Darwin of id
  | Linux of id
  | Windows of id
  | Other of id (** *)
  (** The type for OS names.

      Names are sorted into families. The datum of each family has the
      concrete OS identifier.

      {b Warning.} Minor versions of the library may add new family
      enumerants or attach a family to an identifier previously
      classified as [Other] (moving between families {e should not} happen,
      except to fix the odd bug). As such:
      {ul
      {- Pattern matching on [Other "…"] constants is not recommended.
         If you need to select such an identifier start by pattern matching
         on {!id} before dropping to pattern matching on this type.}
      {- Unless you want your code to be informed by the introduction
         of a new family, end your pattern match with a catch all
         branch [_] rather than [Other _].}} *)

  val id : t -> id
  (** [id n] is the identifier of [n]. *)

  val of_string : ?family:t -> string -> t
  (** [of_string s] dermines an OS from [s] normalized by ASCII
      lowercasing.  Unrecognized OS identifiers end up as [Other] with
      the normalized [s]. Strings printed by {!pp} are guaranteed to
      parse (with the family as the identifier).

      Identifiers returned by {!Os.name} and classified into a proper
      family may be classified as [Other _] by this function, as the
      contextual information provided by [uname(2)] is not available.
      However if [family] is provided, [s] is simply normalized and the
      family of [family] is used in the result. *)

  (** {1:family Family constants}

      These constants can be used as family representatives. *)

  val bsd : t
  (** [bsd] is [Bsd "bsd"]. *)

  val darwin : t
  (** [darwin] is [Darwin "darwin"]. *)

  val linux : t
  (** [linux] is [Linux "linux"]. *)

  val windows : t
  (** [windows] is [Windows "windows"]. *)

  val unknown : t
  (** [unknown] is [Other "unknown"]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal] asserts equality by family, except for [Other id] values
      which are each their own distinct family. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:fmt Formatting} *)

  val pp : t B0__fmt.t
  (** [pp] formats families for inspection. The concrete
      identifier is not printed. *)

  val pp_id : t B0__fmt.t
  (** [pp_id ppf n] formats the {!id} of [n]. *)
end

val name : ?id_like:bool -> unit -> Name.t
(** [name ()] is the name of the operating system running the process.
    If [id_like] is [true] returns a possible parent name instead (e.g.
    ["debian"] instead of "ubuntu"), defaults to [false]. This is
    determined, along with {!version} as follows:
    {ul
    {- On POSIX environments it depends on the lowercased [sysname] field
       returned by
       {{:https://pubs.opengroup.org/onlinepubs/009604599/basedefs/sys/utsname.h.html}[uname(2)]}:
       {ul
       {- ["linux"], the file
          {{:https://www.freedesktop.org/software/systemd/man/latest/os-release.html}/etc/os-release}
          is consulted. The lowercased [ID] or first element of [ID_LIKE]
          if [id_like] is [true] determines [id] and [Name.Linux id]
          is returned. The field [VERSION_ID] is used to determine {!version}.
          If the file can't be found [id] is [sysname] and {!version}
          ["unknown"].}
       {- ["freebsd"], behaves like the Linux case but [Bsd id] is returned.}
       {- ["darwin"], the file
          [/System/Library/CoreServices/SystemVersion.plist] is consulted.
          The lowercased [ProductName] key determines [id] and [Name.Darwin id]
          is returned. The [ProductVersion] key determines {!version}. If
          the file can't be found [id] is [sysname] and {!version}
          ["unknown"].}
        {- ["netbsd"] and ["openbsd"] then [Name.Bsd sysname] is returned
           and {!version} is ["unknown"].}
        {- Starts with ["cygwin_nt"], then [Name.Windows "cygwin"] and
           {!version} is determined like windows (see below).}
        {- Otherwise it returns [Other sysname] and {!version} is ["unknown"]}}}
    {- On Windows this returns [Windows "windows"] and {!version} uses the
       the [caml_win32_*]
       variables of the OCaml runtime system to format a version number}
    {- Otherwise it uses [Name.Other "unknown"] and {!version} is
       ["unknown"]}} *)

val version : unit -> string
(** [version ()] is a version string for the operating system. The format
    and determination depends on {!name}, read there. If no version
    can be determined this is ["unknown"]. *)

(** OS architectures. *)
module Arch : sig

  type id = string
  (** The type for architecture identifiers. Unless you create them
      yourself these strings are normalized: non-empty, lowercase ASCII, with
      original ['-'] characters mapped to ['_']. *)

  type t =
  | Arm32 of id
  | Arm64 of id
  | Ppc32 of id
  | Ppc64 of id
  | Riscv32 of id
  | Riscv64 of id
  | X86_32 of id
  | X86_64 of id
  | Other of id (** *)
  (** The type for OS machine architectures.

      Architectures are sorted into families. The datum of each
      family has the concrete architecture identifier.

      {b Warning.} Minor versions of the library may add new family
      enumerants or attach a family to an identifier previously
      classified as [Other] (moving between families {e should not} happen,
      except to fix the odd bug). As such:
      {ul
      {- Pattern matching on [Other "…"] constants is not recommended.
         If you need to select such an identifier start by pattern matching
         on {!id} before dropping to pattern matching on this type.}
      {- Unless you want your code to be informed by the introduction
         of a new family, end your pattern match with a catch all
         branch [_] rather than [Other _].}} *)

  val of_string : ?family:t -> string -> t
  (** [of_string s] is an architecture determined by [s] normalized by
      ASCII lowercasing it and mapping ['-'] to ['_'].  Unrecognized
      architectures end up as [Other] with the normalized [s]. Strings
      printed by {!pp} are guaranteed to parse (with the family as the
      identifier).

      If [family] is provided, [s] is simply normalized and the family
      of [family] is used in the result. *)

  val id : t -> id
  (** [id arch] is the identifier of [arch]. *)

  val bits : t -> int option
  (** [bits arch] determines the bitness of [arch] usually [Some 32]
      or [Some 64] or [None] if unknown. *)

  (** {1:constants Family constants}

      These constants can be used as family representatives. *)

  val arm32 : t
  (** [arm32] is [Arm32 "arm32"]. *)

  val arm64 : t
  (** [arm64] is [Arm64 "arm64"]. *)

  val ppc32 : t
  (** [ppc32] is [Ppc32 "ppc32"]. *)

  val ppc64 : t
  (** [ppc64] is [Ppc64 "ppc64"]. *)

  val riscv32 : t
  (** [riscv32] is [Riscv32 "riscv32"]. *)

  val riscv64 : t
  (** [riscv64] is [Riscv64 "riscv64"]. *)

  val x86_32 : t
  (** [x86_32] is [X86_32 "x86_32"]. *)

  val x86_64 : t
  (** [x86_64] is [X86_32 "x86_64"]. *)

  val unknown : t
  (** [unknown] is [Other "unknown"]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal] asserts equality by family, except for [Other id] values
      which are each their own distinct family. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:fmt Formatting} *)

  val pp : t B0__fmt.t
  (** [pp] formats architecture families for inspection. The concrete
      identifier is not printed. *)

  val pp_id : t B0__fmt.t
  (** [pp_id ppf arch] formats the {!id} of [arch]. *)

  val pp_bits : t B0__fmt.t
  (** [pp_bits ppf arch] formats the integer {!bits} of [arch] or ["<unknown>"]
      if [None]. *)
end

val arch : unit -> Arch.t
(** [arch ()] is the architecture of the operating system running the
    process (it may differ from your CPU). It is determined by calling
    {!Arch.of_string} with a string obtained as follows.
    {ul
    {- On POSIX environments it uses the [machine] field returned
       by {{:https://pubs.opengroup.org/onlinepubs/009604599/basedefs/sys/utsname.h.html}[uname(2)]}.}
    {- On Windows it uses the {{:https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getnativesysteminfo}[GetNativeSystemInfo]}
       function and munges the [wProcessorArchitecture] field into a string.}
    {- Otherwise it uses ["unknown"].}} *)

(** {1:bazaar Bazaar} *)

val exn_don't_catch : exn -> bool
(** [exn_don't_cath exn] is [true] iff [exn] is [Stack_overflow],
    [Out_of_memory] or [Sys.Break].

    FIXME find a place for that. *)
