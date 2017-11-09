(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0.b0 file parser. *)

open B0

(** {1:parser B0.b0 file parser} *)

val is_field : string -> bool
(** [is_field s] is [true] iff [s] is a valid [B0.b0] file field. *)

type 'a field = (Sexp.t * Sexp.loc) String.Map.t -> 'a
(** The type for field. Given an s-expression map as returned by
    {!Sexp.list_to_string_map} returns the field value.

    {b Warning.} Fields raise [Failure _] on error. *)

(** {2:fields Fields}

    See {!B0.b0b0} for details. *)

val version : int field
val libs : string list field
val drop_libs : String.set field
val srcs :
  rel_to:Fpath.t -> b0_ml:Fpath.t option ->
  (Fpath.t * string list * string) list field

val subs : [`Include of String.set | `Exclude of String.set ] field
val compile_kind :
  src:Fpath.t ->
  [`Byte of Fpath.t list | `Native of Fpath.t list | `Auto ] field

val b0_dir : rel_to:Fpath.t -> Fpath.t option field
val driver_dir : rel_to:Fpath.t -> Fpath.t option field

val compile : string list field
val compile_byte : string list field
val compile_native : string list field
val link : string list field
val link_byte : string list field
val link_native : string list field
val ocamlc : string option field
val ocamlopt : string option field

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
