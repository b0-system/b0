(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0 support for docker.

    Indirect variant schemes for variants that build and run in
    {{:https://www.docker.com/}docker} containers.

    Docker variant schemes allow to build and test your software on
    alternate platforms and environments. It also provides you a cheap
    and easier (albeit slow) form of cross-compilation.

    The image of the container used for building needs a copy of the
    build program (e.g. [b0]) with the same version in the current
    PATH. *)

open B0

(** {1 Variant schemes} *)

type image = string
(** The type for specifying docker images. *)

type image_build
(** The type for specifying how to build a docker image. *)

val variant_scheme :
  ?loc:Def.loc -> ?doc:string -> ?image_build:image_build -> image ->
  Variant.Scheme.t -> Variant.Scheme.t
(** [variant_scheme image s] is a variant scheme that compiles the
    project using scheme [s] in a docker container image [image] which
    is assumed to exist. If it does but [image_build] is specified,
    the image will first be created using these instructions. *)

(** {1 Low-level functions} *)

type cmd
(** A value representing the [docker] command. *)

val get : unit -> cmd result
(** [get ()] looks up a docker binary in the build's program PATH or
    in the environment variable [B0_DOCKER] if defined. *)

val cmd : cmd -> Cmd.t
(** [cmd d] is [d]'s docker command. *)

(** {2:images Images} *)

val image_build : ?opts:Cmd.t -> ?context:string -> Fpath.t -> image_build
(** [build ~opts ~context dockerfile] is an image named [name]
    build from [dockerfile] using [context] (defaults to [-]) with
    [opts] cli arguments added to the [docker build] command.

    {b FIXME.} Need a way to know the root for relative files (Basically
    B0.Def.Loc.get_sub_root) this need to be part of the API. Also
    context should not be a string. *)

val image_create : cmd -> image -> image_build -> unit result
(** [image_create d i b] creates (builds) image [i] with [b]. *)

val image_exists : cmd -> image -> bool result
(** [image_exists d img] is [true] iff [img] exists. *)

val image_delete : cmd -> image -> unit result
(** [image_delete d img] force deletes image [img]. *)

(** {2:containers Containers} *)

type container = string
(** The type for container names. *)

val container_exists : cmd -> container -> bool result
(** [container_exists d c] is [true] iff container [c] exists. *)

val container_create :
  cmd -> ?workdir:Fpath.t -> ?binds:(Fpath.t * Fpath.t) list ->
  image -> container -> unit result
(** [container_create d ~workdir ~binds i c] creates a container [c]
    in detached mode with image [i] runs. [binds] are host to
    container directory maps and [workdir] is the working directory of
    the container.

    FIXME we should ensure the volume map is the same. *)

val container_delete : cmd -> container -> unit result
(** [container_delete d c] force deletes container [c]. *)

val container_exec : cmd -> container -> Cmd.t -> OS.Cmd.status result
(** [container_exec d c cmd] runs [cmd] in container [c]. *)

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
