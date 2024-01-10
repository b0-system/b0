(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Higher-level memoizing operations. *)

open B0_std

val ready_and_copy_dir :
  ?rel:bool -> ?follow_symlinks:bool ->
  ?prune:(Unix.stats -> string -> Fpath.t -> bool) ->
  B0_memo.t -> recurse:bool -> src:Fpath.t -> Fpath.t -> unit
(** [ready_and_copy_dir m ~recurse ~src dst] is the moral equivalent
    of {!B0_std.Os.Dir.copy}. It makes ready and copies the contents
    of [src] to [dst] using the same conventions. *)
