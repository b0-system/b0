(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** [jbuilder] compatiblity.

    This module reads [jbuild] files and expose them as
    {!B0_ocaml.Unit} build units. For the time being you need to
    write a [B0.ml] file and simply invoke {!import} to load your files.

    {b References.} The jbuilder
    {{:https://jbuilder.readthedocs.io}documentation}.

    {b TODO} As it stands this is a proof of concept, a larger set of
    fields and stanzas should be supported but for this to happen a
    real world {!B0_ocaml} needs to exist. Also automatic jbuild file
    lookup could be improved possibly even eschewing the need to write
    a [B0.ml] file at all. *)

open B0

(** {1:jbuild Import jbuild files} *)

val import : unit -> unit
(** [import ()] lookup for jbuild files in the directory and direct
    subdirectories of the [B0.ml] description file that invokes it and
    represents their stanzas as build units. *)

val import_file : Fpath.t -> unit
(** [import_file f] represents the stanzas in [f] as build units.  If
    [f] is relative it is expressed relative to the directory of [B0.ml]
    description files that invokes it. *)

(** {1:low Low-level functions} *)

type t
(** The type for jbuild file stanzas *)

val of_file : ?log:Log.level -> Fpath.t -> t result
(** [of_file f] reads a jbuild file from [f]. [log] is used to report
    parse errors, for now it defaults to [Some Log.Debug]. *)

val to_units : ?log:Log.level -> t -> unit
(** [add_units j] translate the [library] and [executable[s]] stanzas
    as {!B0_ocaml} build units.

    [log] is used to report errors, for now it defaults to [Some Log.Debug]. *)

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
