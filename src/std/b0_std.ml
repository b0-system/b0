(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Cmd = B0__cmd
module Fmt = B0__fmt
module Fpath = B0__fpath
module Log = B0__log
module Mtime = B0__mtime
module Os = B0__os

(* Stdlib extensions *)

module Char = B0__char
module List = B0__list
module Result = B0__result
module String = B0__string
module Type = B0__type

(* Concurrency *)

module Fut = struct
  type 'a state = Det of 'a | Undet of { mutable awaits : ('a -> unit) list }
  type 'a t = 'a state ref

  let rec kontinue ks v =
    let todo = ref ks in
    while match !todo with [] -> false | _ -> true do
      match !todo with k :: ks -> todo := ks; k v | [] -> ()
    done

  let set f v = match !f with
  | Det _ -> invalid_arg "The future is already set"
  | Undet u -> f := Det v; kontinue u.awaits v

  let _make () = ref (Undet { awaits = [] })
  let make () = let f = _make () in f, set f
  let value f = match !f with Det v -> Some v | _ -> None
  let await f k = match !f with
  | Det v -> k v | Undet u -> u.awaits <- k :: u.awaits

  let rec sync f = match !f with
  | Det v -> v
  | Undet _ -> Os.relax (); sync f

  let return v = ref (Det v)

  let map fn f =
    let r = _make () in
    await f (fun v -> set r (fn v)); r

  let bind f fn =
    let r = _make () in
    await f (fun v -> await (fn v) (set r)); r

  let pair f0 f1 =
    let r = _make () in
    await f0 (fun v0 -> await f1 (fun v1 -> set r (v0, v1))); r

  let of_list fs = match fs with
  | [] -> return []
  | fs ->
      let r = _make () in
      let rec loop acc = function
      | [] -> set r (List.rev acc)
      | f :: fs -> await f (fun v -> loop (v :: acc) fs)
      in
      loop [] fs; r

  module Syntax = struct
    let ( let* ) = bind
    let ( and* ) = pair
  end
end

module Bval = struct
  let already_set () = invalid_arg "already set"

  type 'a t =
  | V of 'a
  | Lazy of 'a Fut.t * (unit -> unit)
  | Fut of ('a Fut.t * ('a -> unit))

  type 'a setter = 'a t
  let make () = let bv = Fut (Fut.make ()) in bv, bv
  let of_val v = V v
  let of_lazy_fun f =
    (* XXX stir should spawn a fiber. *)
    let value, set = Fut.make () in
    let run = ref true in
    let stir () = if !run then (run := true; set (f ())) else () in
    Lazy (value, stir)

  let of_setter = Fun.id
  let is_lazy = function Lazy _ -> true | _ -> false

  (* Setting *)

  let set s v = match s with
  | Fut (fut, set) -> set v
  | _ -> assert false

  let try_set s v = match s with
  | Fut (fut, set) ->
      (match Fut.value fut with None -> set v; true | Some _ -> false)
  | _ -> assert false

  let try_set' s f = match s with
  | Fut (fut, set) ->
      begin match Fut.value fut with
      | Some _ -> false
      | None ->
          let v = f () in
          match Fut.value fut with
          | Some _ -> false
          | None -> set v; true
      end
  | _ -> assert false


  (* Getting *)

  let get = function
  | V v -> Fut.return v
  | Lazy (fut, stir) -> stir (); fut
  | Fut (fut, _) -> fut

  let poll = function
  | V v -> Some v
  | Lazy (fut, stir) -> stir (); Fut.value fut
  | Fut (fut, _) -> Fut.value fut

  let stir = function Lazy (_, stir) -> stir () | _ -> ()

  (* Formatting *)

  let pp pp_v ppf = function
  | V v -> pp_v ppf v
  | Lazy (fut, _) | Fut (fut, _) ->
      match Fut.value fut with
      | None -> Fmt.string ppf "<pending>" | Some v -> pp_v ppf v
end
