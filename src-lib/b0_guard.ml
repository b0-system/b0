(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* A [guard] is a build operation with a set of events it is waiting
   on. An event is identified by a non-negative integer. This integer
   indexes in a table ([defs]) of event definitions. An event
   definition [def] defines the kind of event and has the list of
   guards that are waiting for it to occur. Whenever an event occurs
   its list of guards becomes empty forever. The mapping between event
   kinds to event identifier is maintained by the [handler] type. *)

module Ev = struct
  module T = struct
    type t = int
    let compare : int -> int -> int = Pervasives.compare
  end

  type t = T.t
  module Set = Set.Make (T)

  type guard = { op : B0_op.t; mutable awaiting : Set.t }
  type kind = Fpath_ready of B0_fpath.t | Unit_ready of B0_unit.id
  type def = { kind : kind; mutable guards : guard list }
  type defs = { mutable len : int; mutable ds : def array; }

  let nil_def = { kind = Unit_ready B0_unit.(id nil); guards = [] }
  let defs () = { len = 0; ds = Array.make 1024 nil_def; }
  let create defs kind =
    let grow a =
      let len = Array.length a in
      let a' = Array.make (len * 2) nil_def in
      Array.blit a 0 a' 0 len;
      a'
    in
    let ev = defs.len in
    defs.len <- defs.len + 1;
    if defs.len > Array.length defs.ds then (defs.ds <- grow defs.ds);
    defs.ds.(ev) <- { kind; guards = [] };
    ev

  let kind d ev = d.ds.(ev).kind
  let ready d ev = match d.ds.(ev).guards with [] -> true | _ -> false
  let set_ready d ev = d.ds.(ev).guards <- []
  let add_guard d g ev = d.ds.(ev).guards <- g :: d.ds.(ev).guards
  let guards d ev = d.ds.(ev).guards
  let all_guards d =
    let compare_guard g0 g1 = compare (B0_op.id g0.op) (B0_op.id g1.op) in
    let rec loop d acc i = match i < d.len with
    | false -> acc
    | true -> loop d (List.rev_append d.ds.(i).guards acc) (i + 1)
    in
    List.sort_uniq compare_guard (loop d [] 0)
end

(* Handlers *)

type handler =
  { evs : Ev.defs;
    mutable awaiting_file_evs : Ev.t B0_fpath.map;
    mutable awaiting_unit_evs : Ev.t B0_unit.Idmap.t;
    ready : B0_op.t Queue.t; }

let handler () =
  { evs = Ev.defs ();
    awaiting_file_evs = B0_fpath.Map.empty;
    awaiting_unit_evs = B0_unit.Idmap.empty;
    ready = Queue.create (); }

let add_ready h op = B0_op.set_status op B0_op.Ready; Queue.add op h.ready

let set_event_ready h ev =
  let guard_set_event_ready ev g =
    g.Ev.awaiting <- Ev.Set.remove ev g.Ev.awaiting;
    if Ev.Set.is_empty g.Ev.awaiting then add_ready h g.Ev.op else ()
  in
  List.iter (guard_set_event_ready ev) (Ev.guards h.evs ev);
  Ev.set_ready h.evs ev

let set_file_ready h f = match B0_fpath.Map.find f h.awaiting_file_evs with
| ev -> set_event_ready h ev
| exception Not_found ->
    let ev = Ev.create h.evs (Ev.Fpath_ready f) in
    h.awaiting_file_evs <- B0_fpath.Map.add f ev h.awaiting_file_evs

let set_unit_ready h u = match B0_unit.Idmap.find u h.awaiting_unit_evs with
| ev -> set_event_ready h ev
| exception Not_found ->
    let ev = Ev.create h.evs (Ev.Unit_ready u) in
    h.awaiting_unit_evs <- B0_unit.Idmap.add u ev h.awaiting_unit_evs

let get_awaiting_file_evs h fset =
  let add_file h f awaits = match B0_fpath.Map.find f h.awaiting_file_evs with
  | ev when Ev.ready h.evs ev -> awaits
  | ev -> Ev.Set.add ev awaits
  | exception Not_found -> (* Create one... *)
      let ev = Ev.create h.evs (Ev.Fpath_ready f) in
      h.awaiting_file_evs <- B0_fpath.Map.add f ev h.awaiting_file_evs;
      Ev.Set.add ev awaits
  in
  B0_fpath.Set.fold (add_file h) fset Ev.Set.empty

let get_awaiting_unit_evs h uset = (* Potentially creates events *)
  let add_unit h u awaits = match B0_unit.Idmap.find u h.awaiting_unit_evs with
  | ev when Ev.ready h.evs ev -> awaits
  | ev -> Ev.Set.add ev awaits
  | exception Not_found -> (* Create one... *)
      let ev = Ev.create h.evs (Ev.Unit_ready u) in
      h.awaiting_unit_evs <- B0_unit.Idmap.add u ev h.awaiting_unit_evs;
      Ev.Set.add ev awaits
  in
  B0_unit.Idset.fold (add_unit h) uset Ev.Set.empty

let add_op_awaiting_on h op awaiting = match Ev.Set.is_empty awaiting with
| true -> add_ready h op
| false ->
    let g = { Ev.op; awaiting } in
    Ev.Set.iter (Ev.add_guard h.evs g) awaiting

let add h op = match B0_op.kind op with
| B0_op.Sync s ->
    let awaiting = get_awaiting_unit_evs h (B0_op.sync_units s) in
    add_op_awaiting_on h op awaiting
| _ ->
    let awaiting = get_awaiting_file_evs h (B0_op.reads op) in
    add_op_awaiting_on h op awaiting

let ready h = match Queue.take h.ready with
| exception Queue.Empty -> None
| c -> Some c

(* Guards *)

type t = Ev.guard
let op g = g.Ev.op
let guards h = Ev.all_guards h.evs
let awaiting_files h g =
  let add ev fset = match Ev.kind h.evs ev with
  | Ev.Fpath_ready p -> B0_fpath.Set.add p fset
  | Ev.Unit_ready _ -> fset
  in
  Ev.Set.fold add g.Ev.awaiting B0_fpath.Set.empty

let awaiting_units h g =
  let add ev uset = match Ev.kind h.evs ev with
  | Ev.Unit_ready u -> B0_unit.Idset.add u uset
  | Ev.Fpath_ready _ -> uset
  in
  Ev.Set.fold add g.Ev.awaiting B0_unit.Idset.empty

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
