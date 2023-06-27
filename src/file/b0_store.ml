(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

module rec Key : sig
  type t = V : 'a typed -> t
  and 'a typed =
    { uid : int; tid : 'a Type.Id.t; mark : string;
      det : Store.t -> B0_memo.t -> 'a Fut.t; untyped : t; }
  val compare : t -> t -> int
end = struct
  type t = V : 'a typed -> t
  and 'a typed =
    { uid : int; tid : 'a Type.Id.t; mark : string;
      det : Store.t -> B0_memo.t -> 'a Fut.t; untyped : t;}
  let compare (V l0) (V l1) = (compare : int -> int -> int) l0.uid l1.uid
end
and Store : sig
  module Kmap : Map.S with type key = Key.t
  type binding = B : 'a Key.typed * 'a Fut.t -> binding
  type t = { memo : B0_memo.t;
             mutable map : binding Kmap.t; dir : Fpath.t }
end = struct
  module Kmap = Map.Make (Key)
  type binding = B : 'a Key.typed * 'a Fut.t -> binding
  type t = { memo : B0_memo.t;
             mutable map : binding Kmap.t; dir : Fpath.t }
end

type 'a key = 'a Key.typed
type binding = B : 'a key * 'a -> binding
type t = Store.t

let create memo ~dir bs =
  let add m (B (k, v)) =
    Store.Kmap.add k.untyped (Store.B (k, Fut.return v)) m
  in
  let map = List.fold_left add Store.Kmap.empty bs in
  { Store.memo; map; dir : Fpath.t; }

let memo s = s.Store.memo
let dir s = s.Store.dir

let key_uid = (* FIXME atomic *)  let id = ref (-1) in fun () -> incr id; !id
let key ?(mark = "") det =
  let uid = key_uid () and tid = Type.Id.make () in
  let rec k = { Key.uid; tid; mark; det; untyped }
  and untyped = Key.V k in k

let get : type a. t -> a key -> a Fut.t =
  fun s k -> match Store.Kmap.find_opt k.Key.untyped s.map with
  | None ->
      (* We don't use the key determination future directly because
         its determination may indirectly trigger new gets of the same
         key because the memo will be stired and possibly a [get] of
         this key will occur before we get to indicate in the map that
         the key is being determined. Using our own future here makes
         sure all further [get]s end up in the other branch. *)
      let fut, set = Fut.create () in
      s.map <- Store.Kmap.add k.Key.untyped (Store.B (k, fut)) s.map;
      let memo =
        (* XXX maybe it would be interesting to have a stack of marks
           for build understanding "key via m ; m; m;" *)
        B0_memo.with_mark s.memo k.Key.mark
      in
      Fut.await (k.Key.det s memo) set;
      fut
  | Some (Store.B (l', fut)) ->
      match Type.Id.provably_equal k.Key.tid l'.Key.tid with
      | Some Type.Equal -> fut | None -> assert false

let set s k v = match Store.Kmap.mem k.Key.untyped s.Store.map with
| true -> Fmt.invalid_arg "Key %s already set in store" k.Key.mark
| false ->
    let fut = Fut.return v in
    s.map <- Store.Kmap.add k.Key.untyped (Store.B (k, fut)) s.map
