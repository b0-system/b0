(*---------------------------------------------------------------------------
   Copyright (c) 2022 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Expectation tests.

    Expectation tests run computations and write outputs on
    {e expectation files} tracked by your VCS. The VCS can then be used to
    track and correct expectations.

    See the {{!page-todo.b0_expect}TODO}. *)

open B0_std

(** {1:contexts Contexts} *)

type t
(** The type for expectation contexts. An expectation context
    orchestrates and gathers the result of expectation tests. *)

val action_func : base:Fpath.t -> (t -> unit) -> B0_action.func
(** [action_func ~base f] is an action function calling [f ctx] and
    handling test reporting and aborting. [f] should simply perform
    computations, add {{!expectations}expectations} to [ctx] and,
    possibly, {{!aborting}aborting}.

    [ctx] checks expectations files stored in the [base] hierarchy. A
    relative [base] is made absolute with the scope directory of [env].

    A few things are setup via the command line arguments; run the
    cmdlet with [--help] to find out the details. *)

(** {1:expectations Expectations}

    {b Warning.} These functions may raise {!Abort}. *)

val file : ?diff:bool -> t -> Fpath.t -> unit
(** [file ctx file] adds [file] to the checked expectations files of [ctx].
    A relative [file] is made absolute with {!base}.

    If [diff] is [true] (default) a diff is shown on a new or
    unexpected [file] whenever {!log_diffs} is [true]. If [false] no
    diff is ever shown for [file].

    Raises [Invalid_argument] if [file] was already added to [ctx]. *)

val stdout :
  ?diff:bool -> t -> ?env:Os.Env.assignments -> ?cwd:Fpath.t ->
  ?stdout:Fpath.t -> Cmd.t -> unit
(** [stdout ctx cmd] synchronously runs [cmd] with
    {!B0_std.Os.Cmd.val-run} and standard output redirected [stdout]
    and adds [stdout] to the checked expectations files of [ctx]. A relative
    [stdout] is made absolute with {!base}.

    [stdout] defaults to [tool.stdout] with [tool] the basename of
    [cmd]'s tool. If you run the same program more than once you have
    to devise different names for each run.

    For [diff] see corresponding argument of {!file}. *)

(** {1:aborting Aborting}

    This can be used to abort test runs when unexpected and
    non-recoverable errors are hit. *)

exception Abort of string
(** The exception for non recoverable errors. May be raised by any function
    acting on contexts of this module. *)

val abort : string -> 'a
(** [error msg] is [raise (Error string)]. *)

val abortf : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [abortf fmt …] raises {!Abort} with a message formatted according
    to [fmt].*)

val result_to_abort : ('a, string) result -> 'a
(** [result_to_abort r] raises {!Abort} if [r] is [Error e] and
    returns [v] if [r] is [Ok v]. *)

val abort_to_result : (unit -> 'a) -> ('a, string) result
(** [abort_to_result f] calls [f ()] and catches {!Abort} to turn them
    into an [Error _] result. *)

(** {1:lowlevel Low-level interface} *)

(** {2:low_contexts Contexts} *)

(** Expectation outcomes.

    Represents the result of an expectation test. *)
module Outcome : sig

  (** {1:outcomes Outcomes} *)

  type status =
  [ `Corrected (** The outcome has changed but has been corrected. *)
  | `Expected (** The outcome is expected. *)
  | `New (** The expecation test is new. *)
  | `Unexpected (** The outcome is unexpected. *)
  | `Unknown (** The outcome is unknown. *) ]
  (** The type for outcome statuses. *)

  type test =
  | File of { file : Fpath.t; diff : bool }
  (** The type for kinds of tests. *)

  type t
  (** The type for test outcomes. *)

  val status : t -> status
  (** [status o] is the status of [o]. *)

  val test : t -> test
  (** [test o] is the test of [o]. *)
end

val make :
  ?vcs_repo:B0_vcs_repo.t -> ?log_absolute:bool -> ?log_diffs:bool ->
  B0_env.t -> base:Fpath.t -> t
(** [make env ~base] is a test context in environment [env] with:

    {ul
    {- [base] is the directory in which expectations are stored.
       Relative expectation files are intepreted relative to [base].
       A relative [base] is relative to the scope directory of [env].
       {b Important.} [base] should be a directory dedicated to
       expectations so that users can get easily get summaries and
       diffs via their VCS without the noise of surrounding changes.}
    {- [log_diff], if [true] (default) logs diffs on unexpected and new
       outcomes.}
    {- [log_absolute], if [false] (default) paths are logged relative
       to the cwd of [env]. If [true] all paths are made absolute.}
    {- [repo] is the VCS repository to use. By default looked up with
       {!B0_vcs_repo.find} in {!B0_env.scope_dir}.}}

    Raises {!Abort} if VCS detection fails. *)

val base : t -> Fpath.t
(** [base ctx] is the absolute base path of [ctx]. A relative expectation
    file is made absolute with this value. *)

val base_files : ?rel:bool -> t -> recurse:bool -> Fpath.t list
(** [base_files ctx ~recurse] are the absolute or relative (if [rel]
    is [true], defaults to [false]) file paths in [base ctx] and
    sub directories if [recurse] is [true]. *)

val dur : t -> Mtime.Span.t
(** [dur ctx] is the monotonic duration since [ctx] was created. *)

val env : t -> B0_env.t
(** [env ctx] is the environment of [ctx]. *)

val get_unit_exe_file_cmd : t -> B0_unit.t -> Cmd.t
(**  [get_unit_exe ctx u] is the executable of unit [u]. Raises
     {!Abort} in case of error. *)

val log_absolute : t -> bool
(** [log_absolute ctx] is [true] if absolute paths are logged. If [false]
    path are made relative with the cwd of {!env}. *)

val log_diffs : t -> bool
(** [log_diffs ctx] is [true] if unexpected and new outcomes log diffs. *)

val vcs_repo : t -> B0_vcs_repo.t
(** [vcs ctx] is the VCS used for operating the expectation files. *)

val outcomes : t -> Outcome.t list
(** [outcomes ctx] are the outcomes added with {!add_outcome} to [ctx],
    in reverse order of addition. *)

(** {2:prims Primitives} *)

val outcome_of_file : ?diff:bool -> t -> Fpath.t -> Outcome.t
(** [outcome_of_file ?diff ctx file] tests the expectation of file
    [file] in context [ctx]. A relative [file] is made absolute with
    {!base}.  [diff] indicates whether the diff of unexpected or new
    outcomes should be shown for [file] (defaults to [true]). *)

val add_outcome : t -> Outcome.t -> unit
(** [add_outcome ctx o] adds and logs outcome [o] in [ctx]. *)

val finish : t -> Os.Exit.t
(** [finish ctx] logs a summary of expectations and returns [0]
    if everything was expected or [1] otherwise. *)
