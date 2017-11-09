(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** File write cache. See {!B0.Cache}. *)

open B0_result

(* Cache keys *)

type key = B0_hash.t
val key_of_string : string -> key result
val key_to_string : key -> string
val pp_key : Format.formatter -> key -> unit

(* Cache elements *)

type elt

val elt :
  variant:string -> age:int -> op:B0_cmd.t -> key:key ->
  B0_fpath.t -> file_stamp:B0_stamp.t -> elt

val elt_variant : elt -> string
val elt_age : elt -> int
val elt_op : elt -> B0_cmd.t
val elt_key : elt -> key
val elt_file_path : elt -> B0_fpath.t
val elt_file_stamp : elt -> B0_stamp.t

(* Cache *)

type t

val empty : index_file:B0_fpath.t -> dir:B0_fpath.t -> t
val is_empty : t -> bool
val dir : t -> B0_fpath.t
val index_file : t -> B0_fpath.t

(* Persist *)

val exists : index_file:B0_fpath.t -> bool result
val load : index_file:B0_fpath.t -> dir:B0_fpath.t -> t result
val save : t -> unit result

(* Operations *)

val mem : t -> key -> bool
val add : t -> elt -> unit result
val rem : t -> key -> unit result
val find : t -> key -> elt option
val use : t -> key -> bool result

val verify :
  repair:bool -> t -> key ->
  [ `Ok | `Miss_index | `Miss_file | `Stamp_mismatch | `Unknown ] result

val foreign :
  ignore_keys:bool -> t -> ([`Key | `Other] * B0_fpath.t) list result

(* Traverse *)

val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (elt -> unit) -> t -> unit
val path_map : t -> elt B0_fpath.Map.t

(* Build operations *)

val set_dur_counter : t -> B0_mtime.counter -> unit (* FIXME *)
val file_stamp_dur : t -> B0_mtime.span

val exec : t -> B0_op.t -> bool
val add_op : t -> B0_op.t -> unit
val file_stamp : t -> B0_fpath.t -> B0_stamp.t
val file_stamps : t -> B0_stamp.t B0_fpath.Map.t

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
