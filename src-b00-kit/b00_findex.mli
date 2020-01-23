(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** File indexes. *)

open B00_std

(** {1:indexes File indexes} *)

type t
(** The type for file indexes. *)

val empty : t
(** [empty] is an empty file index. *)

val of_dirs :
  ?dotfiles:bool -> ?follow_symlinks:bool ->
  ?prune:(Unix.stats -> string -> Fpath.t -> bool) ->
  Fpath.t list -> (t, string) result
(** [of_dirs dirs] returns a file index for the files in [dirs] whose
    prefixes may be reduced see {!root_root_dir}. See
    {!B00_std.Os.Dir.fold} for the semantics of optional arguments. *)

(** {1:root Root directories} *)

val root_dirs : t -> Fpath.t list
(** [root_dirs i] are the directories that were indexed as given in
    [of_dirs]. *)

val root_root_dirs : t -> Fpath.t list
(** [root_root_dirs] is {!B00_std.Fpath.drop_prefixed} and
    {!B00_std.Fpath.uniquify} applied to {!root_dirs}. *)

(** {1:dirs Directories} *)

val dirs : t -> Fpath.Set.t
(** [dirs] is the set of directories in the index, without the root
    directories. *)

val find_dirname : t -> string -> Fpath.t list
(** [find_dirname i n] are the directories with basename [n] in [i]
    or the empty list if there is no such element. *)

val dir_files : t -> Fpath.t -> Fpath.t list
(** [dir_files i d] are the files in directory [d]. If [d] is not
    a root directory or a member of {!dirs} this is the empty list. *)

val dir_dirs : t -> Fpath.t -> Fpath.t list
(** [dir_dirs i d] are the directories in directory [d] of [i]. If [d] is not
    a root directory or a member of {!dirs} this is the empty list. *)

(** {1:files Files} *)

val files : t -> Fpath.Set.t
(** [files] is the set of files in the index. *)

val find_filename : t -> string -> Fpath.t list
(** [find_filename i n] are the files with basename [n] in [i] or the
    empty list if there is no such element. *)

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
