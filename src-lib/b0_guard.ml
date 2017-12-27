(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Guards *)

type t =
  { mutable awaiting_files : B0_fpath.set; (* files the operation awaits *)
    mutable awaiting_units : B0_unit.Idset.t;  (* units the operation awaits *)
    op : B0_op.t; }

let awaiting_files g = g.awaiting_files
let awaiting_units g = g.awaiting_units
let op g = g.op
let guard_ready g =
  B0_fpath.Set.is_empty g.awaiting_files &&
  B0_unit.Idset.is_empty g.awaiting_units

(* Handlers *)

type handler =
  { mutable guards_awaiting_files : t list B0_fpath.map;
    mutable guards_awaiting_units : t list B0_unit.Idmap.t;
    mutable files_ready : B0_fpath.set;
    mutable units_ready : B0_unit.Idset.t;
    ready : B0_op.t Queue.t; }

let handler () =
  { guards_awaiting_files = B0_fpath.Map.empty;
    guards_awaiting_units = B0_unit.Idmap.empty;
    files_ready = B0_fpath.Set.empty;
    units_ready = B0_unit.Idset.empty;
    ready = Queue.create (); }

(* Guarding build operations *)

let add_ready h op = B0_op.set_status op B0_op.Ready; Queue.add op h.ready
let ready h = match Queue.take h.ready with
| exception Queue.Empty -> None
| c -> Some c

let add_guard_awaiting_file h g f =
  let guards = match B0_fpath.Map.find f h.guards_awaiting_files with
  | exception Not_found -> [g]
  | gs -> g :: gs
  in
  h.guards_awaiting_files <- B0_fpath.Map.add f guards h.guards_awaiting_files;
  ()

let add_guard_awaiting_unit h g u =
  let guards = match B0_unit.Idmap.find u h.guards_awaiting_units with
  | exception Not_found -> [g]
  | gs -> g :: gs
  in
  h.guards_awaiting_units <- B0_unit.Idmap.add u guards h.guards_awaiting_units;
  ()

let add h op = match B0_op.kind op with
| B0_op.Sync s ->
    let sync_units = B0_op.sync_units s in
    let awaiting_units = B0_unit.Idset.diff sync_units h.units_ready in
    let g = { awaiting_files = B0_fpath.Set.empty; awaiting_units; op } in
    begin match guard_ready g with
    | true -> add_ready h g.op
    | false -> B0_unit.Idset.iter (add_guard_awaiting_unit h g) g.awaiting_units
    end
| _ ->
    let awaiting_files = B0_fpath.Set.diff (B0_op.reads op) h.files_ready in
    let g = { awaiting_files; awaiting_units = B0_unit.Idset.empty; op } in
    match guard_ready g with
    | true -> add_ready h g.op
    | false -> B0_fpath.Set.iter (add_guard_awaiting_file h g) g.awaiting_files

let blocked h =
  let add_guards _ gs acc = List.rev_append gs acc in
  let guards = [] in
  let guards = B0_fpath.Map.fold add_guards h.guards_awaiting_files guards in
  let guards = B0_unit.Idmap.fold add_guards h.guards_awaiting_units guards in
  let compare_guard g0 g1 = compare (B0_op.id g0.op) (B0_op.id g1.op) in
  List.sort_uniq compare_guard guards

(* Awaiting for files *)

let files_ready h = h.files_ready

let guard_set_file_ready h f g =
  g.awaiting_files <- B0_fpath.Set.remove f g.awaiting_files;
  if guard_ready g then add_ready h g.op else ()

let set_file_ready h f =
  (* FIXME strategy
  if B0_fpath.Set.mem f h.files_ready then
    B0_log.debug (fun m -> m "ALREADY READY %a" B0_fpath.pp f); *)
  h.files_ready <- B0_fpath.Set.add f h.files_ready;
  match B0_fpath.Map.find f h.guards_awaiting_files with
  | exception Not_found -> ()
  | gs ->
      h.guards_awaiting_files <- B0_fpath.Map.remove f h.guards_awaiting_files;
      List.iter (guard_set_file_ready h f) gs

(* Awaiting for units *)

let units_ready h = h.units_ready

let guard_set_unit_ready h u g =
  g.awaiting_units <- B0_unit.Idset.remove u g.awaiting_units;
  if guard_ready g then add_ready h g.op else ()

let set_unit_ready h u =
  h.units_ready <- B0_unit.Idset.add u h.units_ready;
  match B0_unit.Idmap.find u h.guards_awaiting_units with
  | exception Not_found -> ()
  | gs ->
      h.guards_awaiting_units <- B0_unit.Idmap.remove u h.guards_awaiting_units;
      List.iter (guard_set_unit_ready h u) gs

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
