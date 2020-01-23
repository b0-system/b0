(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

type t = { def : B0_def.t; run : run; }
and run = t -> build -> unit B00.Memo.fiber
and build = { c : build_ctx; b : build_build }
and build_ctx = { current : t option (* fixme nil *) }
and build_build =
  { root_dir : Fpath.t;
    b0_dir : Fpath.t;
    m : B00.Memo.t;
    locked : bool;
    mutable units : t list; }

let nop _ _ k = k ()

module T = struct
  type nonrec t = t
  let def_kind = "unit"
  let def u = u.def
  let pp_name_str = Fmt.(code string)
end

include (B0_def.Make (T) : B0_def.S with type t := t)

let v ?doc ?meta n run =
  let def = define ?doc ?meta n in
  let u = { def; run } in add u; u

let run u = u.run

module Build = struct

  type bunit = t
  type t = build

  let create ~root_dir ~b0_dir m ~locked units =
    let c = { current = None } in
    let b = { root_dir; b0_dir; m; locked; units } in
    { c; b }

  let memo b = b.b.m
  let locked b = b.b.locked
  let units b = b.b.units
  let require_unit b u = failwith "TODO"

  let current_unit b = match b.c.current with
  | None -> invalid_arg "Build not running" | Some u -> u

  let root_build_dir b = Fpath.(b.b.b0_dir / "b" / "user")
  let unit_build_dir b u = Fpath.(b.b.b0_dir / "b" / "user" / name u)
  let unit_root_dir b u = match B0_def.file (def u) with
  | None -> b.b.root_dir
  | Some b0_file -> Fpath.parent b0_file

  let log_file b = Fpath.(root_build_dir b / ".log")

  let run_unit b u =
    let build_dir = unit_build_dir b u (* FixME add to context *) in
    B00.Memo.spawn_fiber b.b.m @@ fun () ->
    B00.Memo.delete b.b.m build_dir @@ fun () ->
    B00.Memo.mkdir b.b.m build_dir @@ fun () ->
    let c = { current = Some u } in
    let b = { b with c } in
    (run u) u b @@ fun () -> ()

  let write_log_file ~log_file m =
    Log.if_error ~use:() @@ B00_ui.Memo.Log.(write log_file (of_memo m))

  let run b =
    let log_file = log_file b in
    (* That shit should be streamlined: brzo, odig, b0caml all
       have similar setup/log/reporting bits. *)
    Os.Sig_exit.on_sigint
      ~hook:(fun () -> write_log_file ~log_file b.b.m) @@ fun () ->
    let rqueue = Rqueue.empty () in (* FIXME add to build *)
    List.iter (Rqueue.add rqueue) b.b.units;
    let rec loop rqueue = match Rqueue.take rqueue with
    | None -> ()
    | Some u -> run_unit b u; loop rqueue
    in
    loop rqueue;
    B00.Memo.stir ~block:true b.b.m;
    write_log_file ~log_file b.b.m;
    match B00.Memo.status b.b.m with
    | Ok _ as v -> v
    | Error e ->
        let read_howto = Fmt.any "b0 log -r " in
        let write_howto = Fmt.any "b0 log -w " in
        B000_conv.Op.pp_aggregate_error
          ~read_howto ~write_howto () Fmt.stderr e;
        Fmt.error "Build failure"
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
