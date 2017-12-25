(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Driver cli information. *)

open Cmdliner
open B0

(** {1:cli Driver cli information} *)

type t
(** The type for driver cli information. *)

val root : t -> Fpath.t option
(** [root i] is the (possibly relative) path to the directory that should
    be considered as the root directory. *)

val start_cwd : t -> Fpath.t
(** [start_cwd i] is the absolute path to initial current working directory. *)

val cwd : t -> Fpath.t
(** [cwd i] is the absolute path to the current working directory. It may
    differ from {!start_cwd} as it can be altered via the [-C] option. *)

val b0_dir : t -> Fpath.t option
(** [b0_dir i] is a (possibly relative) path to the b0 directory. *)

val color : t -> Tty.cap option
(** [color i] is the requested terminal capability. *)

val verbosity : t -> Log.level
(** [verbosity i] is the requested log verbosity. *)

val driver_dir : t -> Fpath.t option
(** [driver_dir i] is a (possibly relative) path to the driver directory. *)

val only : t -> bool
(** [only i] indicates that only the driver instance should be build. *)

val force : t -> bool
(** [force i] indicates that the driver instance should be rebuild
    unconditionally. *)

val trust : t -> bool
(** [trust i] inidcates the driver instance should be trusted to
    be valid. *)

val ocamlc : t -> string option
(** [ocamlc i] indicates an OCaml byte code compiler. *)

val ocamlopt : t -> string option
(** [ocamlopt i] indicates an OCaml native code compiler. *)

val compile_kind : t -> [ `Byte | `Native | `Auto ] option
(** [compile_kind i] indicates the kind of compilation to perform. *)

val compile : t -> string list
(** [compile i] are command line arguments to add to the compilation step. *)

val link : t -> string list
(** [link i] are command line arguments to add to the link step. *)

val setup : t result Term.t
(** [setup] is a term that gathers driver command line information
    and by side effect:
    {ul
    {- Sets {!Tty.set_styling_cap} to the value of {!color}.}
    {- Sets {!Log.set_level} to the value of {!verbosity}.}} *)

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
