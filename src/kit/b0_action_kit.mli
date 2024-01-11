(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tools for actions and unit executions. *)

open B0_std

(** {1:quick_url_fetching Quick URL fetching}

    See also {!B0_http}. *)

val fetch_url :
  ?env:Os.Env.assignments -> ?stderr:Os.Cmd.stdo -> ?args:Cmd.t ->
  ?progress:bool -> B0_env.t -> B0_http.Url.t -> Fpath.t ->
  (unit, string) result
(** [fetch_url env' url file] fetches [url] (redirections are
    followed) using [curl] looked up in [env'] and writes it to
    [file]. [progress] indicates whether progress should be reported
    on [stderr] (defaults to [true]), [args] are arguments added to
    the invocation, [env] and [stderr] are given to the corresponding
    {!Os.Cmd.run}. *)
