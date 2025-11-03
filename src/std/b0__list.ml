(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include List

let cons_if b v vs = if b then v :: vs else vs

let classify
    (type a) (type b)
    ?(cmp_elts : a -> a -> int = Stdlib.compare)
    ?(cmp_classes : b -> b -> int = Stdlib.compare)
    ~classes:(classes : (a -> b list)) els
  =
  let module Set = Set.Make (struct type t = a let compare = cmp_elts end) in
  let module Map = Map.Make (struct type t = b let compare = cmp_classes end) in
  let add_classes acc p =
    let add_class acc c = try Map.add c (Set.add p (Map.find c acc)) acc with
    | Not_found -> Map.add c (Set.singleton p) acc
    in
    List.fold_left add_class acc (classes p)
  in
  let classes = List.fold_left add_classes Map.empty els in
  List.rev (Map.fold (fun c els acc -> (c, Set.elements els) :: acc) classes [])

let distinct (type a) (compare : a -> a -> int) vs =
  let module Set = Set.Make (struct type t = a let compare = compare end) in
  let rec loop seen acc = function
  | [] -> List.rev acc
  | v :: vs ->
      if Set.mem v seen then loop seen acc vs else
      loop (Set.add v seen) (v :: acc) vs
  in
  loop Set.empty [] vs

let rec fold_stop_on_error f l acc = match l with
| [] -> Ok acc
| v :: vs ->
    match f v acc with
    | Ok acc -> fold_stop_on_error f vs acc
    | Error _ as e -> e

let rec iter_stop_on_error f = function
| [] -> Ok ()
| v :: vs ->
    match f v with
    | Error _ as e -> e
    | Ok v -> iter_stop_on_error f vs

let rec iter_iter_on_error ~error f =
  let rec loop ecount f = function
  | [] -> if ecount > 0 then Error ecount else Ok ()
  | v :: vs ->
      match f v with
      | Error _ as e -> error e; loop (ecount + 1) f vs
      | Ok () -> loop ecount f vs
  in
  loop 0 f
