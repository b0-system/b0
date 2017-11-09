(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** [B0] named value definitions.

    See {!B0.Def}. *)

(* Definition locations *)

type loc

module Loc : sig
  type t = loc
  val none : t
  val b0 : t
  val lib : string -> t
  val file :  B0_fpath.t -> t
  val is_none : t -> bool
  val get_file : t -> B0_fpath.t
  val find_file : t -> B0_fpath.t option
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t B0_fmt.t
  val set_root : B0_fpath.t option -> unit
  val get_root : unit -> B0_fpath.t option
  val set_sub_root : B0_fpath.t option -> unit
  val get_sub_root : unit -> B0_fpath.t option
  val set_current : t -> unit
  val get_current : unit -> t
end

(* Definition names *)

module Name : sig
  val space : unit -> string
  val spaced : string -> string
  exception Panic
end

(* Named values *)

type t
type def = t
val nil : t

val name : t -> string
val loc : t -> loc
val doc : t -> string

module type DEFINED = sig
  type t
  val def_kind : string
  val def_get : t -> def
  val def_namespaced : bool
  val def_name_tty_color : B0_tty.color
  val def_pp_info : t B0_fmt.t
end

module type S = sig
  type t
  val value_kind : string
  val name : t -> string
  val loc : t -> loc
  val doc : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val compare_by_name : t -> t -> int
  val find : string -> t option
  val get : string -> t
  val get_or_suggest : string -> (t, string list) Pervasives.result
  val list : unit -> t list
  val pp_name_str : string B0_fmt.t
  val pp_name : t B0_fmt.t
  val pp_synopsis : t B0_fmt.t
  val pp_info : t B0_fmt.t
  val pp_info_ext : t B0_fmt.t -> t B0_fmt.t
end

module type S_DEF = sig
  include S
  val def : ?loc:loc -> ?doc:string -> string -> def
  val def_add : t -> unit
  val def_rem : t -> unit
end

module Make (V : DEFINED) : S_DEF with type t = V.t

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
