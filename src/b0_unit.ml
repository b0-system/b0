(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

(* A bit of annoying recursive definition. *)

module rec Build_def : sig
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B00.Memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_build_dir : Fpath.t;
      store : B00.Store.t;
      must_build : Unit.Set.t; may_build : Unit.Set.t;
      mutable requested : Unit.t String.Map.t;
      mutable waiting : Unit.t Rqueue.t; }
end = struct
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B00.Memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_build_dir : Fpath.t;
      store : B00.Store.t;
      must_build : Unit.Set.t; may_build : Unit.Set.t;
      mutable requested : Unit.t String.Map.t;
      mutable waiting : Unit.t Rqueue.t; }
end

and Unit_def : sig
  type proc = Build_def.t -> unit B00.Memo.fiber
  type t = { def : B0_def.t; proc : proc; }
  include B0_def.VALUE with type t := t
end = struct
  type proc = Build_def.t -> unit B00.Memo.fiber
  type t = { def : B0_def.t; proc : proc; }
  let def_kind = "unit"
  let def u = u.def
  let pp_name_str = Fmt.(code string)
end

and Unit : sig include B0_def.S with type t = Unit_def.t end
  = B0_def.Make (Unit_def)

(* Build procedures *)

type build = Build_def.t
type proc = Unit_def.proc
let nop _ k = k ()

(* Build units *)

include Unit

let pp_synopsis ppf v =
  let pp_tag ppf u =
    Fmt.string ppf
      (if B0_meta.mem B0_meta.exe (meta u) then "exe" else
       if B0_meta.mem B0_meta.lib (meta u) then "lib" else
       if B0_meta.mem B0_meta.doc (meta u) then "doc" else "   ")
  in
  Fmt.pf ppf "@[[%a] %a %a@]" pp_tag v pp_name v pp_doc v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (meta v)

let v ?doc ?meta n proc =
  let def = define ?doc ?meta n in
  let u = { Unit_def.def; proc } in add u; u

let proc u = u.Unit_def.proc

(* Builds *)

module Build = struct
  type bunit = Unit.t
  include Build_def

  let current =
    B00.Store.key @@ fun _ ->
      failwith
        "B0_build.current can't be determined is must be set at store creation"

  let build_dir ~b0_dir = Fpath.(b0_dir / "b" / "user")
  let shared_build_dir ~build_dir = Fpath.(build_dir / "_shared")
  let store_dir ~build_dir = Fpath.(build_dir / "_store")

  let create ~root_dir ~b0_dir m ~may_build ~must_build =
    let u = { current = None; m } in
    let build_dir = build_dir ~b0_dir in
    let shared_build_dir = shared_build_dir ~build_dir in
    let store = B00.Store.create m ~dir:(store_dir ~build_dir) [] in
    let may_build = Set.union may_build must_build in
    let add_requested u acc = String.Map.add (name u) u acc in
    let requested = Unit.Set.fold add_requested must_build String.Map.empty in
    let waiting =
      let q = Rqueue.empty () in Unit.Set.iter (Rqueue.add q) must_build; q
    in
    let b =
      { root_dir; b0_dir; build_dir; shared_build_dir; store; must_build;
        may_build; requested; waiting; }
    in
    let b = { u; b } in
    B00.Store.set store current b;
    b

  let memo b = b.u.m
  let store b = b.b.store
  let shared_build_dir b = b.b.shared_build_dir
  let get b k = B00.Store.get b.b.store k

  module Unit = struct
    let current b = match b.u.current with
    | None -> invalid_arg "Build not running" | Some u -> u

    let must_build b = b.b.must_build
    let may_build b = b.b.may_build

    let require b u =
      if String.Map.mem (name u) b.b.requested then () else
      match Unit.Set.mem u b.b.may_build with
      | true ->
          b.b.requested <- String.Map.add (name u) u b.b.requested;
          Rqueue.add b.b.waiting u
      | false ->
          B00.Memo.fail b.u.m
            "@[<v>Unit %a requested %a which is not part of the build.@,\
             Try with %a or add the unit %a to the build."
            pp_name (current b) pp_name u Fmt.(code string) "--unlock"
            pp_name u

    let build_dir b u = Fpath.(b.b.build_dir / name u)
    let root_dir b u = match B0_def.dir (def u) with
    | None -> b.b.root_dir
    | Some dir -> dir
  end

  let run_unit b unit =
    let m = B00.Memo.with_mark b.u.m (name unit) in
    let u = { current = Some unit; m } in
    let b = { b with u } in
    B00.Memo.mkdir b.u.m (Unit.build_dir b unit) @@ fun () ->
    (proc unit) b @@ fun () -> ()

  let rec run_units b = match Rqueue.take b.b.waiting with
  | Some u -> run_unit b u; run_units b
  | None ->
      B00.Memo.stir ~block:true b.u.m;
      if Rqueue.length b.b.waiting = 0 then () else run_units b

  let log_file b = Fpath.(b.b.build_dir / "_log")
  let write_log_file ~log_file m =
    Log.if_error ~use:() @@ B00_ui.Memo.Log.(write log_file (of_memo m))

  let run b =
    let log_file =
      (* FIXME we likely want to surface that at the API level. Either
         at create or run *)
      log_file b
    in
    let hook () = write_log_file ~log_file b.u.m in
    Os.Sig_exit.on_sigint ~hook @@ fun () ->
    begin
      B00.Memo.spawn_fiber b.u.m begin fun () ->
        B00.Memo.delete b.u.m b.b.build_dir @@ fun () ->
        B00.Memo.mkdir b.u.m b.b.build_dir @@ fun () ->
        B00.Memo.mkdir b.u.m (B00.Store.dir b.b.store) @@ fun () ->
        run_units b;
      end;
      B00.Memo.stir ~block:true b.u.m;
      let ret = match B00.Memo.status b.u.m with
      | Ok _ as v -> v
      | Error e ->
          let read_howto = Fmt.any "b0 log -r " in
          let write_howto = Fmt.any "b0 log -w " in
          B000_conv.Op.pp_aggregate_error
            ~read_howto ~write_howto () Fmt.stderr e;
          Error ()
      in
      Log.time (fun _ m -> m "deleting trash") begin fun () ->
        Log.if_error ~use:() (B00.Memo.delete_trash ~block:false b.u.m)
      end;
      write_log_file ~log_file b.u.m;
      ret
    end
end

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
