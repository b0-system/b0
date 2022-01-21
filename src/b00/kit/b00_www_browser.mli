(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Web browser interaction.

    [Browser] shows URIs in the user's browsers. Up to severe platform
    and browser application limitations it tries to limit the creation
    of new tabs, reloading existing one which have the same URI or
    are, if requested, prefixed by the URI. *)

open B00_std

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val browser : string
  (** [browser] is [BROWSER].*)
end

(** {1:show Show URIs} *)

type t
(** The type for specifying a browser. *)

val find :
  ?search:Fpath.t list -> browser:Cmd.t option -> unit ->
  (t option, string) result
(** [find ~search ~browser] tries to find a browser in a rather
    complex and platform dependent way. *)

val show :
  background:bool -> prefix:bool -> t option -> string ->
  (unit, string) result
(** [show ~background ~prefix browser uri] shows URI using browser
    [browser] (if [None] an error message is returned mentioning
    that no browser was found. If [background] is [true] tries to
    keep the browser application in the background, if [false]
    brings it in user focus.

    The function tries to limit the creation of new tabs using the
    following strategy:
    {ul
    {- Repeat from the frontmost browser window to the backmost one until
       a tab to reload is found:
       {ol
       {- If the window's current tab's URI is [uri] (or is prefixed by [uri]
          when  [prefix] is [true]), reload this tab.}
       {- If the window has one or more tab whose URI is [uri] (or is prefixed
          by [uri] when [prefix] is [true]), pick the left most one, make it
         current in the window and reload it.}}}
    {- If no tab was found, get the frontmost window. If the current tab
       has no URI, use that tab with [uri] otherwise create a new tab
       with [uri] and make it current for the window.}} *)

(** {1:cli Cli interaction} *)

val browser :
  ?docs:string -> ?opts:string list -> unit -> Cmd.t option Cmdliner.Term.t
(** [browser] is an option and [BROWSER] environment variable to use
    with the [browser] argument of {!find}. [opts] are the cli options
    and default to [["browser"]]. *)

val prefix :
  ?docs:string -> ?opts:string list -> unit -> bool Cmdliner.Term.t
(** [prefix] is option to use the with [prefix] argument of
    {!val-show}. [opts] are the cli options and default to
    [["prefix"]]. *)

val background :
  ?docs:string -> ?opts:string list -> unit -> bool Cmdliner.Term.t
(** [background] is an option to use with the [background] argument of
    [!show]. [opts] are the cli options and default to
    [["background"]] *)

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
