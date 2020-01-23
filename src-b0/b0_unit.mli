(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build units.

    A build unit gathers a set of related build operations under a
    name and associates metadata to it. It can be seen as a build
    scriptlet. Build units are the smallest unit of build in B0. *)

open B00_std

(** {1:units Units} *)

type build
(** The type for builds see {!B0_build} *)

type t
(** The type for build units. *)

type run = t -> build -> unit B00.Memo.fiber
(** The type definition unit build operations. *)

val nop : run
(** [nop] runs nothing. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> run -> t
(** [v n] is a build unit named [n] with build operations [run] and
    described by [doc]. *)

val run : t -> run
(** [run u] are the unit's build operations. *)

include B0_def.S with type t := t


(**/**)
module Build : sig
  type bunit = t
  type t = build
  val create :
    root_dir:Fpath.t -> b0_dir:Fpath.t -> B00.Memo.t ->
    locked:bool -> bunit list -> t

  val run : t -> (unit, string) result
  val memo : t -> B00.Memo.t
  val locked : t -> bool
  val units : t -> bunit list
  val require_unit : t -> bunit -> unit
  val current_unit : t -> bunit
  val unit_build_dir : t -> bunit -> Fpath.t
  val unit_root_dir : t -> bunit -> Fpath.t
end
(**/**)

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
