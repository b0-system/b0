(*---------------------------------------------------------------------------
   Copyright (c) 2021 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 software release helpers. *)

open B00_std

(** {1:release_meta Release metadata} *)

(** Metadata keys for releases.  *)
module Meta : sig

  val src_archive_name : string B0_meta.key
  (** [archive_name] is the basename of the source release's archive. *)

  val src_archive_ext : Fpath.ext B0_meta.key
  (** [archive_ext] is the file extension of the source release's archive. *)

  val src_archive_url : string B0_meta.key
  (** [src_archive_url] is an URL pattern that specifies a source release on
      the WWW. The following variables are to be subsituted [%‌%ARCHIVE_NAME%‌%],
      [%‌%ARCHIVE_EXT%‌%], [%‌%VERSION%‌%], [%‌%VERSION_NUM%‌%],
      see {!src_archive_url_of_pack} for more details. *)
end

(** {1:version Versions} *)

val version_of_pack :
  ?commit_ish:B00_vcs.commit_ish -> B0_pack.t -> (string, string) result
(** [version_of_pack p] looks for a VCS in the scope directory of [p]
    and gets its {{!B00_vcs.latest_tag}latest annotated tag} reachable from
    [commit_ish] (defaults to ["HEAD"]) and drops an initial ['v'] or
    ['V']. {b TODO.} add a meta key to prevent v drop. *)

(** {1:src Source archives} *)

val src_archive_name_of_pack : B0_pack.t -> string
(** [src_archive_name_of_meta m] is the value or {!Meta.src_archive_name}
    or the pack's basename if absent, unless this is "default" in which
    case the basename of the pack's scope is taken. *)


val src_archive_ext_of_pack : B0_pack.t -> string
(** [src_archive_ext_of_pack p] is the value of {!Meta.src_archive_ext}
    of [p] or [".tbz"]. *)

val src_archive_url_of_pack :
  version:string -> B0_pack.t -> (string, string) result
(** [src_url_of_meta ~version p] derives a source URL for the pack [p].
    This looks up {!Meta.src_archive_url} [p] and substitute the variables
    as follows:
    {ul
    {- [%‌%ARCHIVE_NAME%‌%] with the value of {!src_archive_name_of_pack}.}
    {- [%‌%ARCHIVE_EXT%‌%] with the value of {!src_archive_name_of_ext}.}
    {- [%‌%VERSION%‌%] with the value of [version]}
    {- [%‌%VERSION_NUM%‌%] with the value of [version] with an initial [v]
       or [V] chopped.}}

    If {!Meta.src_archive_url} is absent, let [%%HOMEPAGE%%] be the value of
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

(** {1:changes Change logs} *)

val changes_file_of_pack : B0_pack.t -> (Fpath.t option, string) result
(**   [changes_file_of_pack p] looks for a [CHANGES.md] file located
      in the scope directory of [p]. *)

val changes_latest_of_file :
  Fpath.t -> ((string * string) option, string) result
(** [changes_latest_of_file f] extracts the latest release notes as the
    {{!B00_cmark.first_section}first markdown section} of file [f]. *)

(** {1:cmdlets Cmdlets} *)

(** [.release.*] Cmdlets *)
module Cmdlet : sig
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The b0 programmers

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
