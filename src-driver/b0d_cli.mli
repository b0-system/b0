(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** [Cmdliner] support for [B0]. *)

open Cmdliner
open B0

(** {1:bdirs Default dirs} *)

val default_b0_dir : Fpath.t
val default_cache_dir : Fpath.t
val default_driver_dir : Fpath.t

(** {1:exits Exits and error handling} *)

val exit_no_description : int
val exit_driver_setup_err : int
val exit_some_error : int
val driver_default_exits : Term.exit_info list
val handle_error : int result -> int

val no_description_found : unit -> int

(** {1:conv Argument converters} *)

val path_arg : Fpath.t Arg.conv

(** {1:copts Common arguments} *)

val root : Fpath.t option Term.t
val cwd : Fpath.t option Term.t
val b0_dir : Fpath.t option Term.t
val color : Tty.cap option Term.t
val verbosity : Log.level option Term.t
val variant : string option Term.t

(** {1:mcopts More common arguments} *)

val cache_dir : Fpath.t option Term.t
val cache_index : Fpath.t option Term.t

val variant_env : string
val variant : string option Cmdliner.Term.t
val variant_scheme_env : string
val variant_scheme : string option Cmdliner.Term.t

(** {1:out_fmt Output format arguments} *)

type out_fmt = [ `Normal | `Short | `Long ]
val out_fmt : [ `Normal | `Short | `Long ] Cmdliner.Term.t

(** {1:args Arguments} *)

val file_kind : [ `All | `Built | `Roots ] Term.t
val ctrl : Build.ctrl Term.t

(** {1:driver Driver setup arguments}

    Along with {{!copts}these arguments}, these are automatically added to
    driver commands. *)

val s_driver_opts : string
val driver_dir : Fpath.t option Term.t
val driver_only : bool Term.t
val driver_force : bool Term.t
val driver_trust : bool Term.t
val driver_ocamlc : string option Term.t
val driver_ocamlopt : string option Term.t
val driver_compile_kind : [`Byte | `Native | `Auto] option Term.t
val driver_compile : string list Term.t
val driver_link : string list Term.t

(** {1:man Man} *)

val common_man : Manpage.block

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
