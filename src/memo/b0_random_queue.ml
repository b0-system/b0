(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t =
  { rand : Random.State.t;
    mutable length : int;
    mutable slots : 'a option array }

let grow q =
  let slots' = Array.make (2 * q.length) None in
  Array.blit q.slots 0 slots' 0 q.length;
  q.slots <- slots'

let empty ?(rand = Random.State.make_self_init ()) () =
  { rand; length = 0; slots = Array.make 256 None }

let add q v =
  if q.length = Array.length q.slots then grow q;
  q.slots.(q.length) <- Some v;
  q.length <- q.length + 1;
  ()

let take q = match q.length with
| 0 -> None
| _ ->
    let i = Random.State.int q.rand q.length in
    let v = q.slots.(i) in
    let last = q.length - 1 in
    q.slots.(i) <- q.slots.(last);
    q.slots.(last) <- None;
    q.length <- last;
    v

let length q = q.length
