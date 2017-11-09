(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build operation cache. See {!B0.Cache}. *)

open B0_result

(* Cache *)

type t
val create : dir:B0_fpath.t -> t result
val dir : t -> B0_fpath.t
val file_stamp : t -> B0_fpath.t -> B0_stamp.t option result
val file_stamps : t -> B0_stamp.t B0_fpath.Map.t
val file_stamp_dur : t -> B0_time.span
val exec : t -> B0_op.t -> bool
val add_op : t -> B0_op.t -> unit

val suspicious_files : t -> B0_fpath.t list result
val delete_unused_files : t -> unit result
val delete_files : t -> pct:int -> dir_byte_size:int option -> unit result

module Dir_stats : sig
  type t
  val file_count : t -> int
  val files_byte_size : t -> int
  val unused_file_count : t -> int
  val unused_files_byte_size : t -> int
  val pp : t B0_fmt.t
end

val dir_stats : t -> Dir_stats.t result

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
