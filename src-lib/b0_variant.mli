(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build variants.

    See {!B0.Variant}. *)

open B0_result

module Scheme : sig
  type trigger = [ `Before | `After ]
  type direct
  type proxy
  type kind = [ `Direct of direct | `Proxy of proxy ]
  type t

  (* Direct Schemes *)

  val direct :
    ?preset:B0_conf.Preset.t ->
    ?setup:(B0_conf.t -> unit result) ->
    ?delete:(B0_conf.t -> unit result) ->
    ?env:(unit -> B0_env.t result) ->
    ?build:(trigger -> B0_build.t -> unit result) ->
    ?stage:(trigger -> B0_outcome.t -> unit result) -> unit -> direct

  val direct_preset : direct -> B0_conf.Preset.t option
  val direct_env : direct -> unit -> B0_env.t result

  (* Proxy schemes *)

  type proxy_conf

  val proxy_conf :
    root_dir:B0_fpath.t -> b0_dir:B0_fpath.t -> variant_dir:B0_fpath.t ->
    unit -> proxy_conf

  val proxy_conf_root_dir : proxy_conf -> B0_fpath.t
  val proxy_conf_b0_dir : proxy_conf -> B0_fpath.t
  val proxy_conf_variant_dir : proxy_conf -> B0_fpath.t

  val proxy :
    ?create:(proxy_conf -> unit result) ->
    ?setup:(proxy_conf -> unit result) ->
    ?delete:(proxy_conf -> unit result) ->
    run:(proxy_conf -> B0_cmd.t -> B0_os.Cmd.status result) -> t -> proxy

  val proxy_run : proxy -> proxy_conf -> B0_cmd.t -> B0_os.Cmd.status result
  val proxy_create : proxy -> proxy_conf -> unit result
  val proxy_delete : proxy -> proxy_conf -> unit result
  val proxy_setup : proxy -> proxy_conf -> unit result

  (* Schemes *)

  val v : ?loc:B0_def.loc -> ?doc:string -> string -> kind -> t
  val with_preset :
    ?loc:B0_def.loc -> ?doc:string -> t -> B0_conf.Preset.t option ->
    string -> t
  val kind : t -> kind
  val nop : t
  val wrap : t -> t -> string -> t
  include B0_def.S with type t := t
end

type t
(** The type for variants. *)

val create :
  ?preset:bool -> dir:B0_fpath.t -> string -> Scheme.t ->
  (t * B0_conf.Preset.def_error list) result

val reset : t -> unit result

val delete :
  ?force:bool -> t -> (unit, [`Scheme of R.msg | R.msg ]) Pervasives.result

val scheme : t -> Scheme.t
val path : t -> B0_fpath.t
val conf_path : t -> B0_fpath.t
val outcome_path : t -> B0_fpath.t
val cache_index_path : t -> B0_fpath.t
val build_path : t -> B0_fpath.t

val value_kind : string
val name : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val compare_by_name : t -> t -> int

type unknown_scheme = [ `Unknown_scheme of string * t ]
val of_unknown_scheme : unknown_scheme -> t
val pp_unknown_scheme : unknown_scheme B0_fmt.t

type load = (t, unknown_scheme) Pervasives.result
val of_load : load -> t

val exists : dir:B0_fpath.t -> string -> bool result
val find : dir:B0_fpath.t -> string -> load option result
val get : dir:B0_fpath.t -> string -> load result
val get_or_suggest :
  dir:B0_fpath.t -> string -> (load, string list) Pervasives.result result

val list : dir:B0_fpath.t -> load list result
val list_empty : dir:B0_fpath.t -> bool result

val pp_name_str : string B0_fmt.t
val pp_name : t B0_fmt.t
val pp_synopsis : t B0_fmt.t
val pp_info : t B0_fmt.t
val pp_info_ext : t B0_fmt.t -> t B0_fmt.t

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
