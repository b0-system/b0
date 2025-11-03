(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File indexes.

    {b TODO.} Maybe move that back to brzo. At the b0 level we are trying
    something with {!B0_srcs}. *)

open B0_std

(** {1:indexes File indexes} *)

type t
(** The type for file indexes. *)

val empty : t
(** [empty] is an empty file index. *)

val of_dirs :
  ?prune_dir:(Unix.stats -> string -> Fpath.t -> bool) ->
  dotfiles:bool -> follow_symlinks:bool ->
  Fpath.t list -> (t, string) result
(** [of_dirs dirs] returns a file index for the files in [dirs] whose
    prefixes may be reduced see {!root_root_dirs}. See
    {!B0_std.Os.Dir.fold} for the semantics of optional arguments. *)

(** {1:root Root directories} *)

val root_dirs : t -> Fpath.t list
(** [root_dirs i] are the directories that were indexed as given in
    [of_dirs]. *)

val root_root_dirs : t -> Fpath.t list
(** [root_root_dirs] is {!B0_std.Fpath.drop_prefixed} and
    {!B0_std.Fpath.distinct} applied to {!root_dirs}. *)

(** {1:dirs Directories} *)

val dirs : t -> Fpath.Set.t
(** [dirs] is the set of directories in the index, without the root
    directories. *)

val find_dirname : t -> string -> Fpath.t list
(** [find_dirname i n] are the directories with basename [n] in [i]
    or the empty list if there is no such element. *)

val dir_files : t -> Fpath.t -> Fpath.t list
(** [dir_files i d] are the files in directory [d]. If [d] is not
    a root directory or a member of {!val:dirs} this is the empty list. *)

val dir_dirs : t -> Fpath.t -> Fpath.t list
(** [dir_dirs i d] are the directories in directory [d] of [i]. If [d] is not
    a root directory or a member of {!val:dirs} this is the empty list. *)

(** {1:files Files} *)

val files : t -> Fpath.Set.t
(** [files] is the set of files in the index. *)

val find_filename : t -> string -> Fpath.t list
(** [find_filename i n] are the files with basename [n] in [i] or the
    empty list if there is no such element. *)
