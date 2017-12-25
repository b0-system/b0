(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Drivers.

    See {!B0_driver.Driver}. *)

open B0

(** {1 Driver setup} *)

type exec = [ `Driver | `Instance ]
type setup

val b0_dir : setup -> B0d_dir.t
val color : setup -> Tty.cap option
val verbosity : setup -> Log.level
val exec : setup -> exec

(** {1 Drivers} *)

type cmd = (setup -> int) Cmdliner.Term.t * Cmdliner.Term.info * exec
type t

val create : name:string -> version:string -> libs:string list -> cmd list -> t
val set : t -> unit
val driver_main : unit -> unit
val instance_main : unit -> unit

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
