(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tools for actions and unit executions. *)

open B0_std

(** {1:quick_url_fetching Quick URL fetching}

    See also {!B0_http}. *)

val download_url :
  ?env:Os.Env.assignments -> ?stderr:Os.Cmd.stdo -> ?args:Cmd.t ->
  ?progress:bool -> B0_env.t -> ?mode:int -> force:bool -> make_path:bool ->
  Net.Url.t -> dst:Fpath.t -> (unit, string) result
(** [download_url env' url file] fetches the [url] (redirections are
    followed) using [curl] looked up in [env'] and the body to
    [file] with:
    {ul
    {- [progress] indicates whether progress should be reported
       on [stderr]. Defaults to [true].}
    {- [args] are arguments added to the invocation.}
    {- [env] and [stderr] are given to the corresponding
       {!B0_std.Os.Cmd.val-run}}}. *)
