(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build outcomes.

    See {!B0.Outcome} *)

open B0_result

type t = B0_build.outcome
val read : B0_fpath.t -> t result
val write : B0_fpath.t -> t -> unit result

val fpath_meta : t -> B0_fpath_meta.Meta_map.t

val root_files : t -> B0_fpath.set
val built_files : t -> B0_fpath.set

val conf : t -> B0_conf.t
val units : t -> B0_unit.marshalable list
val unit_names : t -> string list
val pp_stats : t B0_fmt.t

(* Operations *)

module Op : sig
  include module type of B0_op
end

val ops : t -> Op.t list

val unit_id_name_map : t -> string B0_unit.Idmap.t
val unit_id_set : string list -> t -> B0_unit.Idset.t
val ops_to_json : t -> Op.t list -> string

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
