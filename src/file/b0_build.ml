(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


open B0_std
open B0_std.Fut.Syntax

type b0_unit = B0_defs.b0_unit
type b0_unit_set = B0_defs.Unit.Set.t

include B0_defs.Build_def

let memo b = b.u.m

(* Units *)

let must_build b = b.b.must_build
let may_build b = b.b.may_build
let current b = match b.u.current with
| None -> invalid_arg "Build not running" | Some u -> u

let require_unit b u =
  if String.Map.mem (B0_defs.Unit.name u) b.b.requested then () else
  match B0_defs.Unit.Set.mem u b.b.may_build with
  | true ->
      b.b.requested <- String.Map.add (B0_defs.Unit.name u) u b.b.requested;
      Random_queue.add b.b.waiting u
  | false ->
      B0_memo.fail b.u.m
        "@[<v>Unit %a requested %a which is not part of the build.@,\
         Try with %a or add the unit %a to the build."
        B0_defs.Unit.pp_name (current b)
        B0_defs.Unit.pp_name u Fmt.code "--unlock"
        B0_defs.Unit.pp_name u

let require_units b us = List.iter (require_unit b) us
let current_meta b = B0_defs.Unit.meta (current b)

(* Directories *)

let unit_dir b u =
  B0_dir.unit_build_dir ~build_dir:b.b.build_dir ~name:(B0_defs.Unit.name u)

let unit_scope_dir b u = match B0_def.scope_dir (B0_defs.Unit.def u) with
| None -> b.b.root_dir
| Some dir -> dir

let current_dir b = unit_dir b (current b)
let scope_dir b = unit_scope_dir b (current b)
let shared_dir b = b.b.shared_dir

let in_unit_dir b u p = Fpath.(unit_dir b u // p)
let in_unit_scope_dir b u p = Fpath.(unit_scope_dir b u // p)
let in_current_dir b p = Fpath.(current_dir b // p)
let in_scope_dir b p = Fpath.(scope_dir b // p)
let in_shared_dir b p = Fpath.(b.b.shared_dir // p)

(* Store *)

let self =
  B0_store.key @@ fun _ ->
  failwith "B0_build.self was not set at store creation"

let store b = b.b.store
let get b k = B0_store.get b.b.store k

(* Create *)

let make ~root_dir ~b0_dir ~variant ~store m ~may_build ~must_build =
  let u = { current = None; m } in
  let build_dir = B0_dir.build_dir ~b0_dir ~variant in
  let shared_dir = B0_dir.shared_build_dir ~build_dir in
  let store = B0_store.make m ~dir:(B0_dir.store_dir ~build_dir) store in
  let may_build = B0_defs.Unit.Set.union may_build must_build in
  let add_requested u acc = String.Map.add (B0_defs.Unit.name u) u acc in
  let requested =
    B0_defs.Unit.Set.fold add_requested must_build String.Map.empty
  in
  let waiting =
    let q = Random_queue.empty () in
    B0_defs.Unit.Set.iter (Random_queue.add q) must_build; q
  in
  let b =
    { root_dir; b0_dir; build_dir; shared_dir; store; must_build;
      may_build; requested; waiting; }
  in
  let b = { u; b } in
  B0_store.set store self b;
  b

(* Run *)

let run_unit b unit =
  let m = B0_memo.with_mark b.u.m (B0_defs.Unit.name unit) in
  let u = { current = Some unit; m } in
  let b = { b with u } in
  B0_memo.run_proc m begin fun () ->
    let* () = B0_memo.mkdir b.u.m (unit_dir b unit) in
    (B0_defs.unit_build_proc unit) b
  end

let rec run_units b = match Random_queue.take b.b.waiting with
| Some u -> run_unit b u; run_units b
| None ->
    B0_memo.stir ~block:true b.u.m;
    if Random_queue.length b.b.waiting = 0 then () else run_units b

let log_file b = Fpath.(b.b.build_dir / "_log")
let write_log_file ~log_file m =
  Log.if_error ~use:() @@ B0_cli.Memo.Log.(write log_file (of_memo m))

let report_memo_errors ppf m = match B0_memo.status m with
| Ok _ as v -> v
| Error e ->
    let read_howto = Fmt.any "b0 log -r " in
    let write_howto = Fmt.any "b0 log -w " in
    B0_zero_conv.Op.pp_aggregate_error ~read_howto ~write_howto () ppf e;
    Error ()

let run b =
  let log_file =
    (* FIXME we likely want to surface that at the API level. Either
         at create or run *)
    log_file b
  in
  let hook () = write_log_file ~log_file b.u.m in
  Os.Exit.on_sigint ~hook @@ fun () ->
  begin
    B0_memo.run_proc b.u.m begin fun () ->
      let* () = B0_memo.delete b.u.m b.b.build_dir in
      let* () = B0_memo.mkdir b.u.m b.b.build_dir in
      let* () = B0_memo.mkdir b.u.m (B0_store.dir b.b.store) in
      run_units b; Fut.return ()
    end;
    B0_memo.stir ~block:true b.u.m;
    let ret = report_memo_errors Fmt.stderr b.u.m in
    Log.time (fun _ m -> m "deleting trash") begin fun () ->
      Log.if_error ~use:() (B0_memo.delete_trash ~block:false b.u.m)
    end;
    write_log_file ~log_file b.u.m;
    ret
  end

let did_build b =
  String.Map.fold (fun _ u acc -> B0_defs.Unit.Set.add u acc)
    b.b.requested B0_defs.Unit.Set.empty
