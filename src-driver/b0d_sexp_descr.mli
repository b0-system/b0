(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0.b0 file parser. *)

open B0

(** {1:parser B0.b0 file parser} *)

val is_key : string -> bool
(** [is_key s] is [true] iff [s] is a valid [B0.b0] file field. *)

val version : int Sexp.key
val libs : string list Sexp.key
val drop_libs : String.set Sexp.key
val srcs :
  rel_to:Fpath.t -> b0_ml:Fpath.t option ->
  (Fpath.t * string list * string) list Sexp.key

val subs : [`Include of String.set | `Exclude of String.set ] Sexp.key
val compile_kind :
  src:Fpath.t ->
  [`Byte of Fpath.t list | `Native of Fpath.t list | `Auto ] Sexp.key

val b0_dir : rel_to:Fpath.t -> Fpath.t option Sexp.key
val driver_dir : rel_to:Fpath.t -> Fpath.t option Sexp.key

val compile : string list Sexp.key
val compile_byte : string list Sexp.key
val compile_native : string list Sexp.key
val link : string list Sexp.key
val link_byte : string list Sexp.key
val link_native : string list Sexp.key
val ocamlc : string option Sexp.key
val ocamlopt : string option Sexp.key

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
