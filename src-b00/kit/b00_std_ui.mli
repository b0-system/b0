(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [B00_std] setup and cli fragments.  *)

open B00_std
open Cmdliner

(** {1:setup Setup}

    Configure {{!B00_std.Fmt.set_tty_styling_cap}colored output} and
    {{!B00_std.Log.set_level}log verbosity} and the
    {!B00_std.Os.Cmd.spawn_tracer}. *)

val get_tty_cap : Tty.cap option option -> Tty.cap
(** [get_tty_cap cap] determines [cap] with {!Tty.cap} and
    {!Tty.of_fd} on {!Unix.stdout} if [cap] is [None] or [Some
    None]. *)

val get_log_level : Log.level option -> Log.level
(** [get_log_level level] determines [level] with {!Log.Warning} if
    [level] is [None]. *)

val setup : Tty.cap -> Log.level -> log_spawns:Log.level -> unit
(** [setup tty_cap log_level ~log_spawns] sets:
    {ul
      {- {!B00_std.Fmt.set_tty_styling_cap} with [tty_cap].}
      {- {!B00_std.Log.set_level} with [log_level].}
      {- {!B00_std.Os.Cmd.set_spawn_tracer} with
        {!B00_std.Log.spawn_tracer}[ log_spawns]
         iff [level >= log_spawn].}}
      {b Warning.} If [level < log_spawn] but {!Log.level} is
      increased after this call, the spawns won't be traced (most cli
      programs do not change after the initial setup). Do your own
      setup if that is a problem for you. *)

(** {1:conv Cli argument converters} *)

val fpath : Fpath.t Arg.conv
(** [fpath] is a converter for file paths. No existence checks are
        performed on the path. *)

val cmd : Cmd.t Arg.conv
(** [cmd] is a converter for commands. *)

(** {1:cli Cli arguments} *)

val tty_cap_of_string : string -> (Tty.cap option, string) result
(** [tty_cap_of_string v] parses:
    {ul
    {- [""], ["auto"] into [None]}
    {- ["always"] into [Some `Ansi]}
    {- ["never"] into [Some `None]}} *)

val tty_cap :
  ?docs:string -> ?env:Cmdliner.Arg.env -> unit ->
  Tty.cap option option Term.t
(** [tty_cap ~docs ~env ()] is a cli interface for specifiying a TTY
    capability with a [--color] option. [docs] is where
    the options are documented. [env], if provided, is an
    environment variable to set the value (use something like
    ["MYPROGRAM_COLOR"]). [None] is returned if the value is not set
    on the cli or via the env var. *)

val log_level :
  ?none:Log.level -> ?docs:string -> ?env:Cmdliner.Arg.env -> unit ->
  Log.level option Term.t
(** [log_level ~none ~docs ~env ()] is a cli interface for
      specifiying a logging level with various options. [docs] is
      where the options are documented. [env], if provided, is an
      environment variable to set the value (use something like
      ["MYPROGRAM_VERBOSITY"]). [none] is used to document the level
      when the log level is unspecified (defaults to
      [Log.Warning]). [None] is returned if the value is not set on
      the cli or via the env var. *)

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
