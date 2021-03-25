(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Cmdlets.

    Cmdlets are used to define custom software life-cycle procedures.
    Examples range from invoking linters on your sources to perform
    post build checks or actions.

    See the {{!page-cmdlet_manual}cmdlet manual} for a short introduction.

    Cmdlets run in the same environment in which [b0] is invoked (XXX
    lift that restriction ?). *)

open B00_std

(** {1:cmdlets Cmdlets} *)

type t
(** The type for cmdlets. *)

(** Cmdlet execution environments. *)
module Env : sig

  type cmdlet = t
  (** See {!B0_cmdlet.t}. *)

  type t
  (** The type for cmdlet execution environments. *)

  val v :
    cwd:Fpath.t -> scope_dir:Fpath.t -> root_dir:Fpath.t -> b0_dir:Fpath.t ->
    cmdlet:cmdlet -> t
  (** [v ~cwd ~scope_dir ~root_dir ~cmdlet] is an execution context
      with given parameters. See corresponding accessors for semantics. *)

  val cwd : t -> Fpath.t
  (** [cwd c] is the absolute path to the current working directory. *)

  val scope_dir : t -> Fpath.t
  (** [scope_dir c] is the absolute path to the directory of the B0 file
      in which the cmdlet is defined or the {{!root_dir}root
      directory} if the cmdlet is defined the global scope. *)

  val root_dir : t -> Fpath.t
  (** [root_dir c] is the {{!page-manual.root_dir}root directory}. *)

  val b0_dir : t -> Fpath.t
  (** [b0_dir c] is the {{!page-manual.root_dir}b0 directory}. *)

  val scratch_dir : t -> Fpath.t
  (** [scratch_dir c] is a shared scratch directory for cmdlets in [b0_dir].
      The directory must be created it may not exist, it's content may
      be destroyed at any time and cmdlets are in charge of inventing
      a naming scheme to avoid collisions. *)

  val cmdlet : t -> cmdlet
  (** [cmdlet e] is the executing cmdlet. *)
end

type cmd = Env.t -> Cmd.t -> Os.Exit.t
(** The type for cmdlet implementations. A function that given an execution
    context and command line arguments for the cmdlet should eventually
    exit in some way. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> cmd -> t
(** [v n ~doc ~meta cmd] is a cmdlet named [n] implemented by [cmd]
    with doc string [doc] and metadata [meta]. *)

val cmd : t -> cmd
(** [cmd c] is the command of the cmdlet. *)

(** {1:shortcuts Shortcuts} *)

val exit_of_result : (unit, string) result -> Os.Exit.t
(** [exit_of_result v] exits with {!B00_cli.Exit.ok} if [v] is [Ok ()] and
    logs the Error and exits with {!B00_cli.Exit.some_error} if [v]
    is [Error _]. *)

val in_scope_dir : Env.t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(Env.root_dir env // p)]). *)

val in_root_dir : Env.t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(Env.root_dir env // p)]). *)

val in_scratch_dir : Env.t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(Env.scratch_dir env // p)]). *)

(** {1:script Script execution} *)

val exec : ?env:Os.Env.assignments -> ?cwd:Fpath.t -> Fpath.t -> cmd
(** [exec exe env cmd] executes the file [exe] with arguments [cmd].
    The {{!Env.scope_dir}scope directory} is used as the default [cwd]
    and to resolve relative [exe] paths. *)

(** {1:cli Command line interaction}

    Use {!B00_cli} and {!B0_cli} to parse cmdlet arguments and
    {!B00_cli.Exit} for exit codes. Given a suitable {!Cmdliner} term
    this function can be used to implement the cmdlet's command. *)

val eval :
  ?man_xrefs:Cmdliner.Manpage.xref list -> ?man:Cmdliner.Manpage.block list ->
  ?envs:Cmdliner.Term.env_info list -> ?exits:Cmdliner.Term.exit_info list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  Env.t -> Cmd.t -> Os.Exit.t Cmdliner.Term.t -> Os.Exit.t
(** [eval e cmd t] defines a cmdlet command by evaluating the cmdliner
    term [t] with arguments [cmd]. The menagerie of optional
    parameters define a {!Cmdliner.Term.info} value for the term, see
    the docs there. By default [doc] is derived from the cmdlet's doc string
    and [exits] is {!B00_cli.Exit.infos}. *)


(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t

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
