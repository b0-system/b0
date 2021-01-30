(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [rsync] support. *)

open B00_std

val get_cmd :
  ?search:Fpath.t list -> ?cmd:Cmd.t -> unit -> (Cmd.t, string) result
(** [get_cmd ()] looks for [rsync] with {!Os.Cmd.get}. *)

val copy :
  ?opts:Cmd.t -> ?stats:bool -> delete:bool ->
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
    exist in [src]. If [stats] is [true] (default) outputs statistics
    about the transfer. [opts] defaults to [-azh], this means transfer
    in archive mode which preserves symlinks and file attributes and
    compression is enabled. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
