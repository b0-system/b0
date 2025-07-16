(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Web browser interaction.

    [B0_web_browser] opens and reloads URLs in the user's browsers. Up
    to severe platform and browser application limitations it tries to
    limit the creation of new tabs, reloading existing one which have
    the same URL or are, if requested, prefixed by the URL. *)

open B0_std

(** {1:env Environment variables} *)

(** Environment variables. *)
module Env : sig
  val browser : string
  (** [browser] is [BROWSER].*)
end

(** {1:show Open and reload URLs} *)

type t
(** The type for browsers. *)

val find :
  ?search:Cmd.tool_search -> ?cmd:Cmd.t -> unit -> (t, string) result
(** [find ~search ~cmd] tries to find a browser in a rather
    complex and platform dependent way. *)

val show :
  background:bool -> prefix:bool -> t -> string -> (unit, string) result
(** [show ~background ~prefix browser url] shows [url] using browser
    [browser]. If [background] is [true] tries to
    keep the browser application in the background, if [false]
    brings it in user focus.

    The function tries to limit the creation of new tabs using the
    following strategy:
    {ul
    {- Repeat from the frontmost browser window to the backmost one until
       a tab to reload is found:
       {ol
       {- If the window's current tab's URL is [url] (or is prefixed by [url]
          when  [prefix] is [true]), reload this tab.}
       {- If the window has one or more tab whose URL is [url] (or is prefixed
          by [url] when [prefix] is [true]), pick the left most one, make it
         current in the window and reload it.}}}
    {- If no tab was found, get the frontmost window. If the current tab
       has no URI, use that tab with [url] otherwise create a new tab
       with [url] and make it current for the window.}} *)

(** {1:cli Cli interaction} *)

val browser :
  ?docs:string -> ?opts:string list -> unit -> Cmd.t option Cmdliner.Term.t
(** [browser] is an option and [BROWSER] environment variable to use
    with the [browser] argument of {!find}. [opts] are the cli options
    and default to [["b"; "browser"]]. *)

val prefix : ?docs:string -> default:bool -> unit -> bool Cmdliner.Term.t
(** [prefix] are options to use the with [prefix] argument of
    {!val-show}. This defines these options:
    {ul
    {- [--prefix] to specify [true] and if [default] is [false]
       [-p] aswell.}
    {- [--exact] to specify [false].}}
    The default value of the option is [default]. *)

val background :
  ?docs:string -> ?opts:string list -> unit -> bool Cmdliner.Term.t
(** [background] is an option to use with the [background] argument of
    [!show]. [opts] are the cli options and default to
    [["g"; "background"]] *)

val man_best_effort_reload : Cmdliner.Manpage.block list
(** [man_best_effort_reload] is a manual fragment explaining best-effort
    reloading. *)
