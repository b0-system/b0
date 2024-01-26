(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Editor interaction.  *)

open B0_std

(** {1:edit Editing} *)

type t
(** The type for editors. *)

val find : ?search:Cmd.tool_search -> ?cmd:Cmd.t -> unit -> (t, string) result
(** [find ?search ?cmd ()] finds a suitable editor. This is (in order):
    {ul
    {- [Ok e] or [Error _] if [cmd] is specified and according to whether
       [OS.Cmd.get ?search] succeeds}
    {- [Ok e] if [pager] is a tool invocation parsed from the
       [VISUAL] environment variable that can be found via
       [Os.Cmd.find ?search].}
    {- [Ok e] if [pager] is a tool invocation parsed from the
       [EDITOR] environment variable that can be found via
       [Os.Cmd.find ?search]}
    {- [Ok e] if [Cmd.tool "nano"] can be found via
       [Os.Cmd.find ?search].}
    {- [Error _] otherwise.}} *)

val edit_files : t -> Fpath.t list -> (Os.Cmd.status, string) result
(** [edit_files editor fs] uses [editor] to edit the files [fs]. If [editor] is:
    {ul
    {- [None], an error message is returned mentioning that no editor
         was found.}
    {- [Some editor] invokes the command with files [fs] and returns the
       exit status of the program.}} *)

(** {1:cli Cli interaction} *)

(** Environment variables. *)
module Env : sig
  val visual : string
  (** [visual] is [VISUAL]. *)

  val editor : string
  (** [editor] is [EDITOR]. *)

  val infos : Cmdliner.Cmd.Env.info list
  (** [infos] describes the [VISUAL] and [EDITOR] environment variables. *)
end
