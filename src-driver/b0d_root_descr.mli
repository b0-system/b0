(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Gather and compile B0 build description files.

    See {!B0.descriptions}. *)

open B0

(** {1:descr Build descriptions} *)

type compile_kind =
  [ `Byte of Fpath.t list
  | `Native of Fpath.t list
  | `Conflict of Fpath.t list * Fpath.t list (* byte * native *)
  | `Auto ]
(** The type for compile kind. The file paths are the description files
    that enforce the constraint. *)

type t
(** The type for root build descriptions. *)

val find : force:bool -> dir:Fpath.t -> t option result
(** [find ~force ~dir dir] is the root description gathered by starting from
    [dir] (if any). If [force] is [true] forces [dir] to be the root
    directory of the description.

    {b Warning}. This function currently only works with an absolute
    [dir], this is not checked by the function. *)

val file : t -> Fpath.t
(** [file d] is an absolute path to the root description file. *)

val dir : t -> Fpath.t
(** [dir d] is an absolute path to the root directory. *)

val b0_dir : t -> Fpath.t option
(** [b0_dir d] is an absolute path to the b0 directory (if any). *)

val driver_dir : t -> Fpath.t option
(** [driver_dir d] is an absolute path to the driver directory (if any). *)

val libs : t -> string list
(** [libs d] are libraries to use to compile the description. *)

val drop_libs : t -> String.set
(** [drop_libs d] are libraries to remove from the compilation description. *)

val srcs : t -> (Fpath.t * (Fpath.t * string list * string) list) list
(** [srcs d] are sources to add to the compilation. All file paths are
    absolute. *)

val compile_kind : t -> compile_kind
(** [compile_kind d] is the kind of compilation to perform. *)

val compile : t -> string list
(** [compile d] are cli arguments to add to the compilation step. *)

val compile_byte : t -> string list
(** [compile_byte d] are cli arguments to add to the byte code compilation
    step. *)

val compile_native : t -> string list
(** [compile_byte d] are cli arguments to add to the native code compilation
    step. *)

val link : t -> string list
(** [link d] are cli arguments to add to the link step. *)

val link_byte : t -> string list
(** [link_byte d] are cli arguments to add to the byte code link step. *)

val link_native : t -> string list
(** [link_native d] are cli arguments to add to the native code link step. *)

val ocamlc : t -> string option
(** [ocamlc d] is the byte code compiler to use. *)

val ocamlopt : t -> string option
(** [ocamlopt d] is the native code compiler to use. *)

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
