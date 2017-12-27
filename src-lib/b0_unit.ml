(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Metadata *)

module Key_info = struct
  type 'a t = unit
  let key_kind = "unit metadata key"
  let key_namespaced = true
  let key_name_tty_color = `Default
  let pp _ = B0_fmt.nop
end

module Meta = B0_hmap.Make (Key_info) ()

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

(* Build units *)

type build_unit =
  { def : B0_def.t;
    doc_outcome : string;
    basename : string;
    only_aim : B0_conf.build_aim option;
    uid : int;
    pkg : B0_pkg.t option;
    src_root : B0_fpath.t;
    mutable meta : Meta.t; }

module Unit = struct
  type t = build_unit
  let def_kind = "build unit"
  let def_get u = u.def
  let def_namespaced = true
  let def_name_tty_color = `Green
  let def_pp_info ppf u =
    B0_fmt.cut ppf ();
    B0_fmt.(field "pkg" (option ~none:none_str B0_pkg.pp_name)) ppf u.pkg;
    B0_fmt.cut ppf ();
    B0_fmt.(field "meta" B0_fmt.(vbox @@ Meta.pp)) ppf u.meta;
    B0_fmt.cut ppf ();
    B0_fmt.(field "src_root" B0_fpath.pp) ppf u.src_root;
    ()

  let compare u0 u1 = (compare : int -> int -> int) u0.uid u1.uid
end

include B0_def.Make (Unit)

let basename u = u.basename
let doc_outcome u = u.doc_outcome
let only_aim u = u.only_aim
let equal u0 u1 = u0.uid = u1.uid
let compare = Unit.compare

let err_src_root_none =
  "src_root cannot be None; there is no current sub root set in Def."

let err_src_root_some src_root set =
  B0_string.strf "src_root cannot be %a; a sub root is set in Def (%a)."
    B0_fpath.pp src_root B0_fpath.pp set

let get_src_root = function
| None ->
    begin match B0_def.Loc.get_sub_root () with
    | None -> invalid_arg err_src_root_none
    | Some src_root -> src_root
    end
| Some src_root ->
    begin match B0_def.Loc.get_sub_root () with
    | Some set -> invalid_arg (err_src_root_some src_root set)
    | None -> src_root
    end

let create
    ?loc ?src_root ?doc ?(doc_outcome = "") ?only_aim ?pkg
    ?(meta = Meta.empty) n
  =
  let def = def ?loc ?doc n in
  let uid = uid () in
  let basename = n in
  let src_root = get_src_root src_root in
  let u = { def; basename; doc_outcome; only_aim; uid; pkg; src_root; meta } in
  def_add u; u

let nil =
  { def = B0_def.nil; basename = "nil"; doc_outcome = "nil";  only_aim = None;
    uid = -1; pkg = None; src_root = B0_os.File.null; meta = Meta.empty }

let id u = u.uid
let src_root u = u.src_root
let meta u = u.meta
let set_meta u m = u.meta <- m
let meta_mem k u = Meta.mem k (meta u)
let meta_add k v u = set_meta u (Meta.add k v (meta u))
let meta_find k u = Meta.find k (meta u)
let meta_get k u = Meta.get k (meta u)
let has_tag k u = match Meta.find k (meta u) with None -> false | Some b -> b
let pkg u = u.pkg

(* Unit map and sets *)

module Set = struct
  include Set.Make (Unit)
  let pp ?sep pp_elt = B0_fmt.iter ?sep iter pp_elt
end

module Map = struct
  include Map.Make (Unit)
  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  let of_list us = List.fold_left (fun m (k,v) -> add k v m) empty us
  let pp ?sep pp_binding = B0_fmt.iter_bindings ?sep iter pp_binding
end

type set = Set.t
type 'a map = 'a Map.t

type marshalable = string * (id * (string * string) list)
let to_marshalable u =
  let encs, _errs (* FIXME *) = Meta.encode u.meta in
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
