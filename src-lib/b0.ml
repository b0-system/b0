(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Fmt = B0_fmt

module R = B0_result.R
type 'a result = ('a, [`Msg of string]) Pervasives.result
let ( >>= ) = B0_result.( >>= )
let ( >>| ) = B0_result.( >>| )

let strf = B0_string.strf
module String = B0_string
module Fpath = B0_fpath

module Codec = B0_codec (* FIXME remove that *)

module Cmd = B0_cmd
module Conv = B0_conv
module Tty = B0_tty
module Log = B0_log
module Def = B0_def
module Hmap = B0_hmap

module OS = B0_os
module Time = B0_time
module Hash = B0_hash
module Stamp = B0_stamp
module Cache = B0_cache
module Env = B0_env
module Conf = B0_conf

type build = B0_build.t
module Meta = B0_meta
module Tool = B0_tool
module Pkg = B0_pkg
module Unit = struct

  include B0_unit

  (* Unit build functions are indirectly associated to their unit
     here. This allows to cut an invasive recursive definition in
     build types. It also allows to serialize the basic Unit.t
     datatype without entering closure land. See B0_unit.mli for
     more details. *)

  let funcs : (build -> unit) Map.t ref = ref Map.empty

  let create ?loc ?src_root ?doc ?doc_outcome ?only_aim ?pkg ?meta name func =
    let u = create ?loc ?src_root ?doc ?doc_outcome ?only_aim ?pkg ?meta name in
    funcs := Map.add u func !funcs;
    u

  let func u = match Map.find u !funcs with
  | func -> func
  | exception Not_found -> assert false

end

module Outcome = B0_outcome

module Build = struct
  include B0_build

  let create ?prev_outcome cache ctrl env conf meta ~dir ~universe units =
    let pair u = u, Unit.func u in
    let universe = List.rev_map pair universe in
    let units = List.rev_map pair units in
    create ?prev_outcome cache ctrl env conf meta ~dir ~universe units
end

module Variant = B0_variant
module Sexp = B0_sexp
module Json = B0_json

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
