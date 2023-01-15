(*---------------------------------------------------------------------------
   Copyright (c) 2022 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 expectation tests.

    An expectation test runs a program or sequence of programs in a
    given environment with given inputs and checks designated outputs
    against expected outcomes.

    B0 expectation tests use your VCS to integrate and correct
    tests.

    See the {{!page-TODO.expect}TODO}. *)

open B0_std

type t
(** The type for expectation test runners. It represents a context
    to run a set of expctation tests. *)

(** Expectation test outcomes.

    An outcome counts as one test unit. You may sometimes
    want to merge multiple outcomes into one. *)
module Outcome : sig

  type t
  (** The type for expectation tests outcomes. *)

  type status = [ `Corrected | `Expected | `New | `Unexpected | `Unknown ]
  (** The type for outcome status *)

  val status : t -> status
  (** [status o] is the outcome status of [o]. *)

  val merge : t list -> t
  (** [merge o] merges the outcomes [o] in a single outcome (for counting). *)
end

val make :
  ?vcs:B00_vcs.t -> ?prefix:Fpath.t ->
  B0_cmdlet.Env.t -> base:Fpath.t -> (t, string) result
(** [make env ~base] is an expectation test runner in environment [env].

    {ul
    {- [vcs] is the VCS to use by default looked up with {!B00_vcs.t}
       in {!B0_cmdlet.Env.scope_dir}}
    {- [prefix]  [prefix] is a prefix relative to which relative paths are
       interpreted and absolute path suffixes highlighed defaults (maybe
       relativiized in the future) to {!B0_cmdlet.Env.scope_dir}.}
    {- [base] is a base directory in which the expectations are located
       relative to [prefix].}}

    {b Note.} It's important to have a well defined [base] in which
    expectations are VCS controlled so so that users can get easily
    get summaries and diffs via their VCS without the noise of
    surrounding changes. *)

val prefix : t -> Fpath.t
(** [prefix exp] is the prefix of [exp]. See {!make}. *)

val base : t -> Fpath.t
(** [base exp] is the absolute base path of [exp]. See {!make}. *)

val base_files : t -> recurse:bool -> (Fpath.t list, string) result
(** [base_files exp ~recurse] are the files in [base exp] and sub
    directories if [recurse] is [true]. *)

val dur : t -> Mtime.span
(** [dur exp] is the monotonic duration since {!make}. *)

(** {1:prims Primitives} *)

val file_outcome : t -> Fpath.t -> (Outcome.t, string) result
(** [file_outcome exp f] is the expectation test outcome for file [f]
    in [exp]. *)

(** {2:show Showing results} *)

val log_outcome : t -> Fpath.t -> Outcome.t -> unit
(** [log_outcome f o] logs the outcome [o] of file [f]. At some point
    we likely want to add [f] to [o]. This should be done as soon as
    possible. *)

val log_results : t -> Outcome.t list -> Os.Exit.t
(** [log_results t os] logs the result [os]. [dir] is the path used
    to print VCS command for the summary or diff. *)

(** {1:derived Derived expectations}

    Note these do log outcomes. *)

val stdout : t ->
  ?env:Os.Env.assignments -> ?cwd:Fpath.t -> stdout:Fpath.t -> Cmd.t ->
  (Outcome.t, string) result

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The b0 programmers

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
