(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build operation execution.

    This is simply an asynchronous event loop. There is no notion of
    synchronisation between operations, everything is executed in
    parallel. Synchronisation is handled before submission via
    {!B0_guard}operation guards}. *)

open B0_result

(** {1:handlers Operation execution handlers} *)

type handler
(** The type for operation execution handlers. *)

val handler :
  ?rand:Random.State.t -> max_spawn:int -> clock:B0_time.counter ->
  tmp_path:B0_fpath.t -> unit -> handler
(** [create ~max_spawn ~clock ~tmp_path] is an OS asynchronous
    interaction handler which supports up to [max_spawn] process
    spawns. [clock] is used to generate timestamps. [tmp_path] is a
    prefix for temporary files, it's directory name should
    exist. [rand] is the randomness used for internal queues it
    defaults to {!Random.State.make_self_init}. *)

val submit : handler -> B0_op.t -> unit
(** [submit h o] submits operation [o] to handler [h] (but not necessarily
    to the OS yet).

    @raise Invalid_argument if the operation was already submitted. *)

val collect : handler -> block:bool -> B0_op.t option
(** [collect h ~block] is an an operation (if any) handled by [h] that has
    completed (if any). If [block] is [true] and at least one incomplete
    operation exists in [h] it blocks until an operation completes; if no
    operation exists [None] is returned. *)

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
