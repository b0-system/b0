(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build configuration.

    See {!B0.Conf} *)

open B0_result

type build_aim = [ `Build_os | `Host_os ]

(* Configuration *)

include B0_hmap.MAP

val load : B0_fpath.t -> (t * decode_error list) result
val save : t -> B0_fpath.t -> encode_error list result

(* Key value. *)

type 'a value
val const : 'a -> 'a value
val discover :
  ?store:bool -> (B0_env.t -> B0_env.build_aim -> 'a key -> t -> 'a result) ->
  'a value

val value : B0_env.t -> B0_env.build_aim -> 'a key -> t -> 'a value -> 'a result
val value_const : 'a value -> 'a option
val value_store : 'a value -> bool
val pp_value : 'a B0_fmt.t -> 'a value B0_fmt.t

(* Key groups. *)

module rec Group : sig
  type t
  val v : ?loc:B0_def.loc -> ?doc:string -> string -> t
  val none : t
  val keys : t -> Key.t list
  include B0_def.S with type t := t
end

(* Keys *)

and Key : sig
  include B0_hmap.KEY with type 'a typed = 'a key

  val v :
    ?loc:B0_def.loc -> ?doc:string -> ?group:Group.t -> string ->
    'a B0_conv.t -> default:'a value -> 'a typed

  val default : 'a typed -> 'a value
  val group : t -> Group.t
end

val key :
  ?loc:B0_def.loc -> ?doc:string -> ?group:Group.t -> string ->
  'a B0_conv.t -> default:'a value -> 'a key

val get_default :
  B0_env.t -> B0_env.build_aim -> 'a key -> t -> 'a result

val get_effective :
  B0_env.t -> B0_env.build_aim -> 'a key -> t ->
  ([`Env | `Default | `Conf] * 'a) result

val keys : t -> Key.t list

(* Presets *)

module Preset : sig

  (* Key value definition *)

  type conf = t
  val const : 'a -> 'a value
  val discover :
    ?store:bool ->
    (B0_env.t -> B0_env.build_aim -> 'a key -> t -> 'a result) -> 'a value

  type def

  val def_key : def -> Key.t
  val def_doc : def -> string

  type binding = B : 'a key * 'a value -> binding
  val def_binding : def -> binding

  (* Presets *)

  type t
  val v : ?loc:B0_def.loc -> ?doc:string -> string -> def list -> t

  include B0_def.S with type t := t

  val def : ?doc:string -> 'a key -> 'a value -> def

  val defs : t -> def list
  val find_def : string -> t -> def option
  val get_or_suggest_def : string -> t -> (def, string list) Pervasives.result
  val keys : t -> Key.t list

  type def_error = string * [`Msg of string]
  val add : B0_env.t -> B0_env.build_aim -> t -> conf -> conf * def_error list
  val rem : t -> conf -> conf
end

val of_preset :
  B0_env.t -> B0_env.build_aim -> Preset.t ->
  t * Preset.def_error list

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
