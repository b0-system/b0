(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [B0_dir] directory structured access. *)

open B00_std

(** {1:build Builds} *)

val build_dir : b0_dir:Fpath.t -> variant:string -> Fpath.t
(** [build_dir ~b0_dir ~variant] is the designated directory
    for the build variant [variant] in [b0_dir]. *)

val shared_build_dir : build_dir:Fpath.t -> Fpath.t
(** [shared_build_dir ~build_dir] is the shared directory of [build_dir]
    obtained via {!build_dir}. *)

val store_dir : build_dir:Fpath.t -> Fpath.t
(** [store_dir ~build_dir] is the store directory of [build_dir] obtained
    via [!build_dir]. *)

val unit_build_dir : build_dir:Fpath.t -> name:string -> Fpath.t

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
