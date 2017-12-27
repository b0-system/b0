(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build operation guards.

    Guards ensure that a given build operation is {e ready}, i.e. can
    be executed. This means that the files and build units it reads
    are up-to-date and ready to be read. *)

(** {1:handlers Build guard handlers} *)

type handler
(** The type for build guard handlers. *)

val handler : unit -> handler
(** [handler ()] is a guard handler. *)

val files_ready : handler -> B0_fpath.set
(** [files_ready h] is the set of files that are ready in [h]. *)

val units_ready : handler -> B0_unit.Idset.t
(** [units_ready h] is the set of build units that are ready in [h]. *)

val set_file_ready : handler -> B0_fpath.t -> unit
(** [set_file_ready h f] indicates to guards in [h] that [f] is ready. *)

val set_unit_ready : handler -> B0_unit.id -> unit
(** [set_unit_ready h u] indicates to guards in [h] that [u] is ready. *)

val add : handler -> B0_op.t -> unit
(** [add h op] guards [op] in [h] until it is {{!collect}collectable}. *)

val ready : handler -> B0_op.t option
(** [ready h] is an operation that is ready in [h] (if any).
    The operation is removed from [h]. *)

(** {1:guard Guards} *)

type t
(** The type for build operation guards. *)

val awaiting_files : t -> B0_fpath.set
(** [awaiting_files g] are [g]'s files that are waiting to be ready. *)

val awaiting_units : t -> B0_unit.Idset.t
(** [awaiting_units g] are [g]'s units that are waiting to be ready. *)

val op : t -> B0_op.t
(** [op g] is the build operation guarded by [g]. *)

val blocked : handler -> t list
(** [blocked h] is the list of guards that are blocked in [h]. *)

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
