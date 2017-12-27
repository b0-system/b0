(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** File path metadata *)

(* This is defined here rather than in B0_fpath for bootstrap reasons:

   B0_hmap needs B0_def which needs B0_fpath. *)

module Meta : B0_hmap.S with type 'a Key.info = unit

module Meta_map : sig
  type t = Meta.t B0_fpath.map
  val empty : t
  val mem : B0_fpath.t -> 'a Meta.key -> t -> bool
  val add : B0_fpath.t -> 'a Meta.key -> 'a -> t -> t
  val rem : B0_fpath.t -> 'a Meta.key -> t -> t
  val find : B0_fpath.t -> 'a Meta.key -> t -> 'a option
  val get : B0_fpath.t -> 'a Meta.key -> t -> 'a
  val get_all : B0_fpath.t -> t -> Meta.t
end

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
