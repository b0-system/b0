(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Select source files.

    {b FIXME.} This will need a few more design rounds. Here
    are a few things:
    {ul
    {- Review w.r.t. [b0].}
    {- We likely want combinators represenging {!type-sel}s and
    producing {!t} and ways union [t]s (for `Fut users).}
    {- The [`Fut] case should return a {!t}.}
    {- Support for watermaking should likely occur here.}}

    This module provides a type to select source files for build units
    in B0 files. To support generated source files, selections can
    depend on the build.

    In a nutshell the declaration:
{[
let srcs =
  Fpath.[ `Dir (v "src-exe"); `Dir_rec (v "src"); `X (v "src/not.ml");
          `X (v "src/not")]
]}
    instructs to:
    {ul
    {- Select all the files in {e directory} [src-exe].}
    {- Select all the files in the {e file hierarchy} rooted at
    directory [src].}
    {- Remove from the files found in directories the files whose paths
       segments are prefixed by [src/not.ml] and [src/not].}}

    Relative file paths are expressed relative to the build unit's
    {{!B0_build.Unit.scope_dir}scope directory}.

    The prefix relation for exclusions respects path segments
    boundaries. In the example any file whose path matches
    [src/not.ml], [src/not.ml/*], [src/not] or [src/not/*] is
    excluded from the selection. But for example [src/not.c] is not.

    The relative order of directory selections and exclusions doesn't
    matter, the semantics is to select all the files via [`Dir] and
    [`Dir_rec] and then apply the exclusion [`X] on the resulting
    set. Exclusions affect only directory selections, not file [`File]
    and future [`Future] {{!type-sel}selections}.

    When a directory is selected via [`Dir] or [`Dir_rec], all its files
    are, modulo exclusions.  It is expected that build units
    themselves filter the final result by file extension or additional
    mechanisms. Consult the documentation of build units for more
    information. *)

open B00_std

(** {1:sel Source selection} *)

type sel =
[ `Dir of Fpath.t
| `Dir_rec of Fpath.t
| `X of Fpath.t
| `File of Fpath.t
| `Fut of B0_build.t -> Fpath.Set.t Fut.t ]
(** The type for file selectors.
    {ul
    {- [`File f] unconditionaly selects the file [f]. [f] must exist and be
       a file.}
    {- [`Dir d] selects the files of directory [d] modulo [`X] exclusions.
       [d] must exist and be a directory. dotfile paths are ignored.}
    {- [`Dir_rec d] selects the files of the file {e hierarchy} rooted
       at [d] modulo [`X] exclusions. [d] must exist and be a directory.
       dotfile paths are ignored.}
    {- [`X x] removes from directory selections any file whose path segments
       are prefixed by [x], respecting segment boundaries. A potential trailing
       directory separators in [x] is removed.}
    {- [`Fut f] uses the given future during the build to determine
       a set of files unconditionally added to the selection.
       FIXME this s not the right interface, see {!root_of_file},
       maybe we should return a {!t} itself and merge the results}}

    Except for [`Fut], any relative path is made absolute to the
    current build unit with {!B0_build.Unit.root_dir}. *)

type sels = sel list
(** The type for source selection. *)

type t
(** The type for source selection results. *)

val select : B0_build.t -> sels -> t Fut.t
(** [select b sels] selects in [b] the sources specified by [sels].

    {b Important.} All files in the map that were selected via
    [`File], [`D] and [`D_rec] are automatically
    {{!B00.Memo.file_ready}made ready} in [b]. For those selected via
    [`Fut] readyness determination is left to the invoked funtion.

    {b FIXME.} Provide ordering guarantes and avoid non-det from the
    fs. *)

val by_ext : t -> B00_fexts.map
(** [by_ext s] are the selected files mapped by their file extension
    (not {{!B00_std.Fpath.file_exts}multiple file extension}).  Each
    file is guaranteed to appear only once in the map and is absolute. *)

(*
val root_for_file : t -> Fpath.t -> Fpath.t
(** [root_for_file s f] is an absolute root directory for a file
    [f] selected by [s]. If [f] was selected by:
    {ul
    {- [`File p] then this is [Fpath.parent p].}
    {- [`Dir d] or [`Dir_rec d] this is [d].}
    {- [`Fut fs] then this is like the parent of file.
       {b FIXME} We should also be able to specify root dirs here.}}

    In case a file is selected by multiple {!sel} the longer root
    prefix wins.

    Raises [Invalid_argument] if [f] was not selected by [s]. *)
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
