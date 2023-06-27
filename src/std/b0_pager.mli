(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Pager interaction. *)

open B0_std

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val pager : string
  (** [pager] is [PAGER] *)

  val less : string
  (** [less] is [LESS]. *)

  val term : string
  (** [term] is [TERM]. *)

  val infos : Cmdliner.Cmd.Env.info list
  (** [infos] describe the [PAGER] and [TERM] environment variable for
      cmdliner. *)
end


(** {1:paging Paging} *)

val find :
  ?win_exe:bool -> ?search:Fpath.t list -> don't:bool -> unit ->
  (Cmd.t option, string) result
(** [find ~search ~don't] finds a suitable pager. This is (in order):
    {ol
    {- [Ok None] if [don't] is [true] or if the [TERM] environment variable
       is [dumb] or undefined.}
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [PAGER] environment variable that can be found via
       [Os.Cmd.find_tool ?win_exe ?search].}
    {- [Ok (Some pager)] if either [less] or [more] can be found
       (in that order) via [Os.Cmd.find_tool ?win_exe ?search].}
    {- [Ok None] otherwise.}} *)

val page_stdout : Cmd.t option -> (unit, string) result
(** [page_stdout pager] setups the program so that if [pager] is
    [Some cmd], the standard output of the program is redirected to
    a spawn of [cmd] with the following twists:
    {ul
    {- If the environment variable [LESS] is undefined in the
       current environment it is set to [LESS=FRX] for the spawn of
       [cmd].}
    {- A {!Stdlib.at_exit} function is installed that flushes
       {!B0_std.Fmt.stdout} and {!Stdlib.stdout}, closes [Unix.stdout] and
       waits upon the [cmd] spawn termination.}}
    If [pager] is [None] this function has no effect. *)

val page_files : Cmd.t option -> Fpath.t list -> (unit, string) result
(** [page_files pager fs] uses [pager] to page the files [fs]. If [pager] is:
    {ul
    {- [None], each of the [fs] files is output on stdout in order
       separated by a file separator character (U+001C).}
    {- [Some cmd], [cmd] is run with [fs] as arguments. If [fs]
       is empty does nothing.}} *)

(** {1:cli Cli interaction} *)

val don't : ?docs:string -> unit -> bool Cmdliner.Term.t
(** [don't ~docs ()] is a [--no-pager] command line option to
    unconditionally request not use a pager. [docs] is the manual
    section where the option is documented, defaults to
    {!Cmdliner.Manpage.s_common_options}. *)
