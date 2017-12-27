(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Command line tools.

    See {!B0_tool}. *)

(* Environment variables *)

type env_vars = string list
val tmp_vars : env_vars

(* Command line tools *)

type t

val v : ?internal:env_vars -> ?env_vars:env_vars -> string -> t
val of_file : ?internal:env_vars -> ?env_vars:env_vars -> B0_fpath.t -> t
val of_unit_file :
  B0_unit.t -> ?internal:env_vars -> ?env_vars:env_vars -> B0_fpath.t -> t

val name : t -> B0_fpath.t
val env_vars : t -> env_vars
val internal : t -> env_vars
val unit : t -> B0_unit.t option

val lookup_env : t -> B0_os.Env.t -> B0_os.Env.t * B0_os.Env.t (* all, ext *)

(* Configuration keys *)

val key :
  ?loc:B0_def.loc -> ?doc:string -> ?group:B0_conf.Group.t ->
  ?internal:env_vars -> ?env_vars:env_vars -> ?tools:B0_fpath.t list ->
  string -> t B0_conf.key


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
