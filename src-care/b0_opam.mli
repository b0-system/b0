(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0 support for opam.

    This module provides variant schemes to compile projects
    against a given opam switch as well as opam file support.

    {b TODO}
    {ul
    {- Now that we have {!B0.Pkg} implement autodep functionality.}} *)

open B0

(** {1 Variant schemes} *)

type pkg = string
(** The type for opam packages. This is either a package name or a
    package name with a version as you would define on the cli of
    [opam install]. *)

val variant_scheme :
  ?loc:Def.loc -> ?doc:string -> ?preset:Conf.Preset.t -> ?autodep:bool ->
  ?pkgs:pkg list -> ?build_switch:string -> string -> Variant.Scheme.t
(** [variant_scheme name] is a variant scheme that compiles in an
    environment setup by [opam env --switch name]. If [build_switch]
    is provided [name] is used for the host aim environment and [build_os]
    for the build aim environment. *)

(** {1 Low-level functions} *)

type cmd
(** A value representing the [opam] command.

    {b TODO} Maybe this should represent [opam] acting on
    a switch given at {!get}. *)

val get : unit -> cmd result
(** [get ()] looks up an opam binary in the build's program PATH or
    in the environment variable [B0_OPAM] if defined. *)

val cmd : cmd -> Cmd.t
(** [cmd opam] is opam's command. *)

val env : cmd -> switch:string -> OS.Env.t result
(** [env opam ~switch] is the environment for the opam switch [switch]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
