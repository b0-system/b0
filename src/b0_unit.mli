(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build units.

    A build unit is a named build procedure with metadata associated
    to it. Build units are the smallest unit of build in B0. *)

open B00_std

(** {1:proc Build procedures} *)

type build
(** The type for builds, see {!B0_build}. *)

type proc = build -> unit B00.Memo.fiber
(** The type for unit build procedures. *)

val nop : proc
(** [nop] does nothing. *)

(** {1:units Units} *)

type t
(** The type for build units. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> proc -> t
(** [v n ~doc ~meta] is a build unit named [n] with build operations
    [run] and described by [doc]. *)

val proc : t -> proc
(** [proc u] are the unit's build operations. *)

include B0_def.S with type t := t

(**/**)
module Build : sig
  type bunit = t
  type t = build

  val memo : t -> B00.Memo.t
  val store : t -> B00.Store.t
  val shared_build_dir : t -> Fpath.t

  module Unit : sig
    val current : t -> bunit
    val must : t -> Set.t
    val may : t -> Set.t
    val require : t -> bunit -> unit
    val build_dir : t -> bunit -> Fpath.t
    val root_dir : t -> bunit -> Fpath.t
  end

  val create :
    root_dir:Fpath.t -> b0_dir:Fpath.t -> B00.Memo.t -> may:Set.t ->
    must:Set.t -> t

  val run : t -> (unit, unit) result
  val current : t B00.Store.key
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
