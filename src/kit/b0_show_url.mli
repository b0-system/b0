(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Action to show URLs after builds.

    This module provides the [.show-url] action which allows to reload
    URLs in your browser when you build static websites or web
    servers. *)

open B0_std

(** {1:keys Keys} *)

val path : Fpath.t B0_meta.key
(** [path] defines the default file of the build unit to show when
    [.show-url] is used to show a file. *)

val listen_args : (authority:string -> Cmd.t) B0_meta.key
(** [listen_args] defines the arguments to specify for how to make the
    server listen on [authority] for connections. These arguments
    are added at the end of the sevrver tool invocation or before a [--] token
    if there is one. *)

val timeout_s : int B0_meta.key
(** [timeout_s] defines the maximal number of seconds to wait for the
    server to be connectable before reloading the URL. Defaults to 1s. *)

(** {1:action [.show-url] action} *)

val action : B0_action.t
(** [action] is the [.show-url] action.

    See [b0 -- .show-url --help] for more information. *)
