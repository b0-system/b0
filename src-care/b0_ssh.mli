(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0 support for SSH and rsync.

    This module provides indirect variant schemes to run builds
    through SSH. This gives you a cheap and easier (but somehow slow)
    form of cross-compilation.

    The SSH remote need a copy the build program (e.g. [b0]) with the
    same version in the current PATH. *)

open B0

(** {1:scheme Variant schemes} *)

type host = string
(** The type for SSH hosts. *)

val variant_scheme :
  ?loc:Def.loc -> ?doc:string -> ?name:string ->
  ?excludes:Fpath.t list -> ?root:Fpath.t -> host ->
  Variant.Scheme.t -> Variant.Scheme.t
(** [variant_scheme host s] is a variant scheme that compiles the
    project using scheme [s] on the SSH host [host]. The source tree
    is mirrored on the remote machine inside the [root] (default to
    [.b0_builds]) directory. The [excludes] paths which should be
    relative to the build root are excluded.  [name] defaults to
    ["ssh-$S"] with $S the name of [s].

    {b FIXME.} [excludes] should be relative to definition point. *)

(** {1 Low-level functions} *)

val get_ssh : unit -> Cmd.t result
(** [get_ssh ()] looks up an ssh binary in the build's program PATH or
    in the environment variable [B0_SSH] if defined. *)

val get_rsync : unit -> Cmd.t result
(** [get_rsync ()] looks up an rsync binary in the build's program PATH or
    in the environment variable [B0_RSYNC] if defined. *)

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
