(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Editor interaction.  *)

open B0_std

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val visual : string
  (** [visual] is [VISUAL]. *)

  val editor : string
  (** [editor] is [EDITOR]. *)
end

val envs : unit -> Cmdliner.Cmd.Env.info list
(** [envs ()] describe the [VISUAL] and [EDITOR] environment variables. *)

(** {1:edit Editing} *)

val find :
  ?win_exe:bool -> ?search:Fpath.t list -> unit -> (Cmd.t option, string) result
(** [find ?win_exe ?search ()] finds a suitable editor. This is (in order):
    {ol
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [VISUAL] environment variable that can be found via
       [Os.Cmd.find ?search].}
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [EDITOR] environment variable that can be found via
       [Os.Cmd.find ?win_exe ?search].}
    {- [Ok (Some pager)] if [nano] can be found via [Os.Cmd.find ?win_exe
    ?search].}
    {- [Ok None] otherwise.}} *)

val edit_files :
  Cmd.t option -> Fpath.t list -> (Os.Cmd.status, string) result
(** [edit-files editor fs] uses [editor] to edit the files [fs]. If [editor] is:
    {ul
    {- [None], an error message is returned mentioning that no editor
         was found.}
    {- [Some editor] invokes the command with files [fs] and returns the
       exit status of the program.}} *)
