(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Metadata *)

(* Id map and sets *)

let uid =
  let id = ref (-1) in
  fun () -> incr id; !id

type id = int

module Id = struct
  type t = id
  let compare : int -> int -> int = Pervasives.compare
end

module Idset = struct
  include Set.Make (Id)
  let pp ?sep pp_elt = B0_fmt.iter ?sep iter pp_elt
end

module Idmap = struct
  include Map.Make (Id)
  let pp ?sep pp_binding = B0_fmt.iter_bindings ?sep iter pp_binding
end

(* Packages *)

type pkg =
  { def : B0_def.t;
    basename : string;
    uid : int;
    mutable meta : B0_meta.Pkg.t; }

module Pkg = struct
  type t = pkg
  let def_kind = "package"
  let def_get u = u.def
  let def_namespaced = true
  let def_name_tty_color = `Magenta
  let def_pp_info ppf u =
    if not (B0_meta.Pkg.is_empty u.meta)
    then (B0_fmt.cut ppf (); B0_meta.Pkg.pp ppf u.meta)

  let compare u0 u1 = (compare : int -> int -> int) u0.uid u1.uid
end

include B0_def.Make (Pkg)

let basename u = u.basename
let equal p0 p1 = p0.uid = p1.uid
let compare = Pkg.compare

let create ?loc ?doc ?(meta = B0_meta.Pkg.empty) n =
  let def = def ?loc ?doc n in
  let uid = uid () in
  let basename = n in
  let p = { def; basename; uid; meta } in
  def_add p; p

let id u = u.uid
let meta u = u.meta
let set_meta u m = u.meta <- m
let meta_mem k u = B0_meta.Pkg.mem k (meta u)
let meta_add k v u = set_meta u (B0_meta.Pkg.add k v (meta u))
let meta_find k u = B0_meta.Pkg.find k (meta u)
let meta_get k u = B0_meta.Pkg.get k (meta u)
let has_tag k u = match B0_meta.Pkg.find k (meta u) with
| None -> false
| Some b -> b

(* Unit map and sets *)

module Set = struct
  include Set.Make (Pkg)
  let pp ?sep pp_elt = B0_fmt.iter ?sep iter pp_elt
end

module Map = struct
  include Map.Make (Pkg)
  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  let of_list us = List.fold_left (fun m (k,v) -> add k v m) empty us
  let pp ?sep pp_binding = B0_fmt.iter_bindings ?sep iter pp_binding
end

type set = Set.t
type 'a map = 'a Map.t

type marshalable = string * (id * (string * string) list)
let to_marshalable u =
  let encs, _errs (* FIXME *) = B0_meta.Pkg.encode u.meta in
  (name u), ((id u), encs)

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
