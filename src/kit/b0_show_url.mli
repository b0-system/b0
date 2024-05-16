(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Action and unit execution to show URLs after builds.

    This module provides the [.show-url] action which allows to reload
    URLs in your browser when you build static websites or web
    servers. *)

open B0_std

(** {1:urls URLs} *)

type url =
[ `Url of Url.t (** The URL. *)
| `In of B0_env.dir * Fpath.t (** The path in given directory. *)
| `Fun of string * (B0_env.t -> B0_unit.t -> (Url.t, string) result) ]
(** The type for dermining the URL to show. *)

val url : url B0_meta.key
(** [url] defines the default URL to show when
    [.show-url] is used on a unit without specifying a path.*)

val get_url : B0_env.t -> B0_unit.t -> (Url.t, string) result
(** [get_url env u] performs the logic to get the {!val-url} for unit [u]
    in environment [env]. *)

(** {1:keys Server keys} *)

val listen_args : (authority:string -> Cmd.t) B0_meta.key
(** [listen_args] defines the arguments to specify for how to make the
    server listen on [authority] for connections. These arguments are
    added at the end of the server tool invocation or before a [--]
    token if there is one. *)

val timeout_s : int B0_meta.key
(** [timeout_s] defines the maximal number of seconds to wait for the
    server to be connectable before reloading the URL. Defaults to 1s. *)

(** {1:unit [.show-url] unit} *)

val unit : B0_unit.t
(** [unit] is the [.show-url] unit.

    See [b0 -- .show-url --help] for more information. *)

(** {1:unit_action Unit action} *)

val action : B0_unit.Action.t
(** [action] is a unit action that invokes the [show-url] tool on
    the unit's {!val-url}. *)
