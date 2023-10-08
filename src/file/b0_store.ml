(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

module M = Map.Make (Int)

type 'a key =
  { det : t -> B0_memo.t -> 'a Fut.t;
    id : 'a Type.Id.t;
    mark : string; }

and binding = B : 'a key * 'a Fut.t -> binding
and t =
  { memo : B0_memo.t;
    mutable map : binding M.t;
    dir : Fpath.t }

let[@inline] key_uid k = Type.Id.uid k.id
let key ?(mark = "") det = { det; id = Type.Id.make (); mark; }

let make memo ~dir bs =
  let add map (B (k, _) as b) = M.add (key_uid k) b map in
  let map = List.fold_left add M.empty bs in
  { memo; map; dir; }

let memo s = s.memo
let dir s = s.dir

let get : type a. t -> a key -> a Fut.t = fun s k ->
  let kid = key_uid k in
  match M.find_opt kid s.map with
  | None ->
      (* We don't use the key determination future directly because
         its determination may indirectly trigger new gets of the same
         key because the memo will be stired and possibly a [get] of
         this key will occur before we get to indicate in the map that
         the key is being determined. Using our own future here makes
         sure all further [get]s end up in the other branch. *)
      let fut, set = Fut.make () in
      s.map <- M.add kid (B (k, fut)) s.map;
      let memo =
        (* XXX maybe it would be interesting to have a stack of marks
           for build understanding "key via m ; m; m;" *)
        B0_memo.with_mark s.memo k.mark
      in
      Fut.await (k.det s memo) set;
      fut
  | Some (B (k', fut)) ->
      match Type.Id.provably_equal k.id k'.id with
      | Some Type.Equal -> fut
      | None -> assert false

let set s k v =
  if M.mem (key_uid k) s.map
  then Fmt.invalid_arg "Key %s already set in store" k.mark else
  let fut = Fut.return v in
  s.map <- M.add (key_uid k) (B (k, fut)) s.map
