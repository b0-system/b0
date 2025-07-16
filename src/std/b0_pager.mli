(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Pager interaction. *)

open B0_std

(** {1:paging Paging} *)

type t
(** The type for pagers. *)

val does_page : t -> bool
(** [does_page p] is [true] iff [p] actually pages. *)

val find :
  ?search:Cmd.tool_search -> ?cmd:Cmd.t -> no_pager:bool -> unit ->
  (t, string) result
(** [find ~search ~don't] finds a suitable pager.
    This is (in order):
    {ol
    {- [Ok p] if [no_pager] is [true] or if the [TERM] environment variable
       is [dumb] or undefined and [cmd] is unspecified. In this case
       [does_page p] is [false].}
    {- [Ok p] or [Error _] if [cmd] is specified and according to whether
      [Os.Cmd.get ?search cmd] succeeds.}
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [PAGER] environment variable that can be found via
       [Os.Cmd.find ?search].}
    {- [Ok (Some pager)] if either [less] or [more] can be found
       (in that order) via [Os.Cmd.find ?search].}
    {- [Ok None] otherwise.}} *)

val page_stdout : t -> (unit, string) result
(** [page_stdout p] setups the program so that if [does_page p] is [true],
    the standard output of the program is redirected to a spawn of
    of the pager with the following twists:
    {ul
    {- If the environment variable [LESS] is undefined in the
       current environment it is set to [LESS=FRX] for the spawn of
       [cmd].}
    {- A {!Stdlib.at_exit} function is installed that flushes
       {!B0_std.Fmt.stdout} and {!Stdlib.stdout}, closes [Unix.stdout] and
       waits upon the [cmd] spawn termination.}}
    If [does_page p] is [false] this function has no effect. *)

val page_files : t -> Fpath.t list -> (unit, string) result
(** [page_files p fs] uses [pager] to page the files [fs]. If [does_page p]
    is:
    {ul
    {- [false], each of the [fs] files is output on stdout, in order,
       separated by a file separator character (U+001C).}
    {- [true], the pager's command is with [fs] as arguments, unless [fs]
       is empty in which case this is a nop.}} *)

(** {1:cli Cli interaction} *)

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
      cmdliner. This is used by {!B0_pager.don't}. *)
end

val no_pager : ?docs:string -> unit -> bool Cmdliner.Term.t
(** [no_pager ~docs ()] is a [--no-pager] command line option to
    unconditionally request not to use a pager. [docs] is the manual
    section where the option is documented, defaults to
    {!Cmdliner.Manpage.s_common_options}. *)
