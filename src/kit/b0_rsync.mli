(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [rsync] support. *)

open B0_std

val get_cmd :
  ?search:Fpath.t list -> ?cmd:Cmd.t -> unit -> (Cmd.t, string) result
(** [get_cmd ()] looks for [rsync] with {!B0_std.Os.Cmd.get}. *)

val copy :
  ?opts:Cmd.t -> ?stats:bool -> ?progress:bool -> delete:bool ->
  ?src_host:string -> src:Fpath.t ->
  ?dst_host:string -> Fpath.t -> (unit, string) result
(** [copy ~src dst] copies the contents of directory [src] to [dst]
    with [rsync]. As per [rsync] semantics, directoryness of [src] is
    important ([dst]'s one is not):
    {ul
    {- If [src] has a trailing directory separator, the contents
       [src/*] is copied to [dst/*].}
    {- If [src] has no trailing directory separator, the contents
       of [src/*] is copied to [dst/$(basename src)/*].}}

    [src_host] and [dst_host] specify the host for source
    and destination directory (e.g. ["myhost:"]). They default to [""].

    If [delete] is [true], deletes files at destination that do not
    exist in [src]. If [stats] is [true] (default to [false]) outputs
    statistics about the transfer. If [progress] is [true] (default)
    outputs progress about the transfer. [opts] defaults to [-azh],
    this means transfer in archive mode which preserves symlinks and
    file attributes and compression is enabled. *)
