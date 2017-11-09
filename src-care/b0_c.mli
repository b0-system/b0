(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** B0 support for the C programming language. *)

open B0

(** {1:tools C compilers} *)

val toolchain : [ `Cc | `Mingw | `Msvc ] Conf.key
(** [toolchain] is the C toolchain used. This is automatically selected
    according to {!B0_care.OS.name}. *)

(** {1:exts C build artefacts file extensions}

    These extensions mainly depend on {!toolchain}. *)

val asm_ext : Fpath.ext Conf.key
(** [asm_ext] is the file extension for assembly files. Determined
    from {!toolchain}. *)

val obj_ext : Fpath.ext Conf.key
(** [ext_obj] is the file extension for C object files. Determined
    from {!toolchain}. *)

val lib_ext : Fpath.ext Conf.key
(** [lib_ext] is the file extension for C static libraries. Determined
    from {!toolchain}. *)

val dll_ext : Fpath.ext Conf.key
(** [dll_ext] is the file extension for C dynamic libraries. Determined
    from {!toolchain}. *)

(** {1:libs C libraries} *)

(*
val pkg_config : Fpath.t Conf.key
(** [pkg_config] is the [pkg-config] tool. *)

val lib_name : string Unit.Meta.key
(** [lib_name] is the name of C library built by the unit. *)

val lib : string -> string option
*)

(*
type lib_pc =
  { name : string;
    version : string;
    description : string;
    url : string;
    cflags : string list;
    lflags : string list;
    url : string;
    version : string;
    requires :
*)



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
