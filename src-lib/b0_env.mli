(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build and run environments.

    See {!B0.Env}. *)

open B0_result

(* Build aims *)

type build_aim = [ `Build_os | `Host_os ]

(* Build tool lookup *)

type tool_lookup = B0_fpath.t list -> B0_fpath.t result
val pp_tool_alts : B0_fpath.t list B0_fmt.t
val env_tool_lookup : ?sep:string -> ?var:string -> B0_os.Env.t -> tool_lookup

(* Build environments *)

type t

val v :
  ?build_lookup:tool_lookup -> ?build_forced_env:B0_os.Env.t ->
  ?build_env:B0_os.Env.t -> ?host_lookup:tool_lookup ->
  ?host_forced_env:B0_os.Env.t -> B0_os.Env.t -> t

val env : t -> build_aim -> B0_os.Env.t
val forced_env : t -> build_aim -> B0_os.Env.t
val tool : t -> build_aim -> B0_fpath.t list -> B0_fpath.t result

(* Environment variables *)

val split_forced_env :
  ?force_prefix:string -> B0_os.Env.t -> B0_os.Env.t * B0_os.Env.t

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
