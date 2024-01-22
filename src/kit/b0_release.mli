(*---------------------------------------------------------------------------
   Copyright (c) 2021 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Source software release helpers. *)

open B0_std

(** {1:metadata Metadata} *)

val tag : bool B0_meta.key
(** [tag] tags releasable entities. *)

(** {1:vcs_repo VCS repos and release versions} *)

val vcs_repo_of_pack :
  B0_pack.t -> (B0_std.Fpath.t * B0_vcs_repo.t, string) result
(** [vcs_repo_of_pack pack] is [Ok (scope_dir, repo)] if [pack]
    has a VCS controlled scope directory. *)

val vcs_repo_version_of_pack :
  ?commit_ish:B0_vcs_repo.commit_ish -> B0_pack.t -> (string, string) result
(** [vcs_repo_version_of_pack p] looks for a VCS in the scope directory of [p]
    and gets its {{!B0_vcs_repo.latest_tag}latest annotated tag} reachable from
    [commit_ish] (defaults to ["HEAD"]) and drops an initial ['v'] or
    ['V'].

    {b TODO.} add a meta key to prevent v drop. *)

(** {1:src Source archives} *)

(** {2:name Extension and basename } *)

val src_archive_ext : Fpath.ext B0_meta.key
(** [archive_ext] is the file extension of the source release's archive.
    Defaults to [".tbz"], this is used for creating archives so
    do not steer away from the extensions mentioned in {!B0_tar.compress}. *)

val src_archive_name : string B0_meta.key
(** [archive_name] is the basename of the source release's archive. *)

val src_archive_name_of_pack : B0_pack.t -> string
(** [src_archive_name_of_meta m] is the value or
    {!src_archive_name} or the pack's basename if absent, unless
    this is [default] in which case the basename of the pack's scope
    is taken. *)

(** {2:url URL} *)

val src_archive_url : string B0_meta.key
(** [src_archive_url] is an URL pattern that specifies a source
    release on the WWW. The following variables are to be subsituted
    [%‌%ARCHIVE_NAME%‌%], [%‌%ARCHIVE_EXT%‌%], [%‌%VERSION%‌%],
    [%‌%VERSION_NUM%‌%], see {!src_archive_url_of_pack} for more
    details. *)

val src_archive_url_of_pack :
  version:string -> B0_pack.t -> (string, string) result
(** [src_url_of_meta ~version p] derives a source URL for the pack [p].
    This looks up {!src_archive_url} [p] and substitute the variables
    as follows:
    {ul
    {- [%‌%ARCHIVE_NAME%‌%] with the value of {!src_archive_name_of_pack}.}
    {- [%‌%ARCHIVE_EXT%‌%] with the value of {!src_archive_ext}.}
    {- [%‌%VERSION%‌%] with the value of [version]}
    {- [%‌%VERSION_NUM%‌%] with the value of [version] with an initial [v]
       or [V] chopped.}}

    If {!src_archive_url} is absent, let [%%HOMEPAGE%%] be the value of
    {!B0_meta.homepage} and [%%REPO%%] be the value of
    {!B0_meta.repo} without the final [.git] and initial [git+] Then:
    {ul
    {- If the hostname of {!B0_meta.homepage} is github the following
       URL pattern is used:
       {v
%‌%REPO%‌%/releases/download/%‌%VERSION%‌%/\
%‌%ARCHIVE_NAME%‌%-%‌%VERSION_NUM%‌%‌%‌%ARCHIVE_EXT%‌%
       v}}
    {- Otherwise the following URL pattern is used:
      {v
%‌%HOMEPAGE%‌%/releases/\
%‌%ARCHIVE_NAME%‌%-%‌%VERSION_NUM%‌%‌%‌%ARCHIVE_EXT%‌%
       v}}} *)

(** {2:archive_creation Archive file creation} *)

val src_archive_for_pack :
  repo:B0_vcs_repo.t ->
  checkout_dir:Fpath.t ->
  keep_checkout_dir:bool -> commit_ish:B0_vcs_repo.commit_ish ->
  B0_pack.t -> (string * Fpath.t * string, string) result
(** [for_pack pack] is a directory basename and corresponding uncompresed
    tar archive for pack [pack]. The first string indicates the version
    string.

    [checkout_dir] indicates the directory in which the VCS is
    checkout, and release massaging occurs. {b WARNING.} this
    directory is deleted before starting. If [keep_checkout_dir] is
    [false] it is destroyed before the function returns. *)

(** {1:changes Changes file} *)

val changes_file : Fpath.t B0_meta.key
(** [changes_file] is a metadata key to specify a changes file
    value. Relative paths are relative to the scope directory of the
    definition. Defaults to [Fpath.v CHANGES.md]. *)

val changes_file_of_pack : B0_pack.t -> (Fpath.t option, string) result
(** [find_changes_file_of_pack p] looks for the {!changes_file} of [p] in
    the scope directory of [p] and returns its absolute path if it
    exists. *)

val get_changes_file_of_pack : B0_pack.t -> (Fpath.t, string) result
(** [get_changes_file_of_pack pack] looks for the {!changes_file} of [pack]
    and makes sure it exists or errors otherwise. *)

val changes_latest_of_file :
  Fpath.t -> ((string * string) option, string) result
(** [changes_latest_of_file f] extracts the latest release notes as the
    {{!B0_std.String.commonmark_first_section}first markdown section} of
    file [f]. *)

val changes_latest_version_of_title : string -> string option
(** [changes_latest_version title] extracts the first token of [title]
    (typically extracted with {!changes_latest_of_file}) as a version
    tag. *)

(** {1:action Action} *)

val action : B0_action.t
(** [.release] action *)
