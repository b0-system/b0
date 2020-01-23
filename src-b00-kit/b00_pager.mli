(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Pager interaction. *)

open B00_std
open Cmdliner

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val pager : string
  (** [pager] is [PAGER] *)

  val less : string
  (** [less] is [LESS]. *)

  val term : string
  (** [term] is [TERM]. *)
end

val envs : unit -> Term.env_info list
(** [envs ()] describe the [PAGER] and [TERM] environment variable for
    cmdliner. *)

(** {1:paging Paging} *)

val find :
  ?search:Fpath.t list -> don't:bool -> unit -> (Cmd.t option, string) result
(** [find ~search ~don't] finds a suitable pager. This is (in order):
    {ol
    {- [Ok None] if [don't] is [true] or if the [TERM] environment variable
       is [dumb] or undefined.}
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [PAGER] environment variable that can be found via
       [Os.Cmd.find ?search].}
    {- [Ok (Some pager)] if either [less] or [more] can be found
       (in that order) via [Os.Cmd.find ?search].}
    {- [Ok None] otherwise.}} *)

val page_stdout : Cmd.t option -> (unit, string) result
(** [page_stdout pager] setups the program so that if [pager] is
    [Some cmd], the standard output of the program is redirected to
    a spawn of [cmd] with the following twists:
    {ul
    {- If the environment variable [LESS] is undefined in the
       current environment it is set to [LESS=FRX] for the spawn of
       [cmd].}
    {- A {!Pervasive.at_exit} function is installed that flushes
       {!Fmt.stdout} and {!stdout}, closes [Unix.stdout] and waits
       upon the [cmd] spawn termination.}}
    If [pager] is [None] this function has no effect. *)

val page_files : Cmd.t option -> Fpath.t list -> (unit, string) result
(** [page_files pager fs] uses [pager] to page the files [fs]. If [pager] is:
    {ul
    {- [None], each of the [fs] files is output on stdout in order
       separated by a file separator character (U+001C).}
    {- [Some cmd], [cmd] is run with [fs] as arguments. If [fs]
       is empty does nothing.}} *)

(** {1:cli Cli interaction} *)

val don't : ?docs:string -> unit -> bool Term.t
(** [don't ~docs ()] is a [--no-pager] command line option to
    unconditionally request not use a pager. [docs] is the manual
    section where the option is documented. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
