(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [rsync] support. *)

open B0_std

type t
(** The type for rsync clients. *)

val get : ?search:Cmd.tool_search -> ?cmd:Cmd.t -> unit -> (t, string) result
(** [get ~search ~cmd ()] looks for the rsync command [cmd] (defaults to
    [Cmd.tool "rsync"]) in [search] (defaults to [Os.Cmd.get search]). *)

val copy :
  t -> ?opts:Cmd.t -> ?stats:bool -> ?progress:bool -> delete:bool ->
  ?src_host:string -> ?dst_host:string -> Fpath.t -> dst:Fpath.t ->
  (unit, string) result
(** [copy src ~dst] copies the contents of directory [src] to [dst]
    with [rsync]. As per [rsync] semantics, directoryness of [src] is
    important and [dst]'s one is not:

    {ul
    {- If [src] has a trailing directory separator, the contents
       [src/*] is copied to [dst/*].}
    {- If [src] has no trailing directory separator, the contents
       of [src/*] is copied to [dst/$(basename src)/*].}}

    [src_host] and [dst_host] specify the host for source
    and destination directory (e.g. ["myhost:"]). They default to [""].
    The rest of the arguments are:
    {ul
    {- If [delete] is [true], deletes files at destination that do not
       exist in [src] using the [--delete] flag. Defaults to [false].}
    {- If [stats] is [true] outputs statistics about the transfer using the
       [--stats] flag. Defaults to [false].}
    {- If [progress] is [true] (default) outputs progress about the transfer
       using [--info=progress2].}
    {- [opts] are other options for rsync. It defaults to [-azh] which
       means transfer in archive mode which preserves symlinks and
       file attributes and compression is enabled.}} *)
