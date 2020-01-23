(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Editor interaction.  *)

open B00_std
open Cmdliner

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val visual : string
  (** [visual] is [VISUAL]. *)

  val editor : string
  (** [editor] is [EDITOR]. *)
end

val envs : unit -> Term.env_info list
(** [envs ()] describe the [VISUAL] and [EDITOR] environment variables. *)

(** {1:edit Editing} *)

val find :
  ?search:Fpath.t list -> unit -> (Cmd.t option, string) result
(** [find ?search ()] finds a suitable editor. This is (in order):
    {ol
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [VISUAL] environment variable that can be found via
       [Os.Cmd.find ?search].}
    {- [Ok (Some pager)] if [pager] is a tool invocation parsed from the
       [EDITOR] environment variable that can be found via
       [Os.Cmd.find ?search].}
    {- [Ok (Some pager)] if [nano] can be found via [Os.Cmd.find ?search].}
    {- [Ok None] otherwise.}} *)

val edit_files :
  Cmd.t option -> Fpath.t list -> (Os.Cmd.status, string) result
(** [edit-files editor fs] uses [editor] to edit the files [fs]. If [editor] is:
    {ul
    {- [None], an error message is returned mentioning that no editor
         was found.}
    {- [Some editor] invokes the command with files [fs] and returns the
       exit status of the program.}} *)

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
