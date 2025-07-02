(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* XXX at the moment we are not serializing Memo.t.ready_roots.
   This means we can't use the log with [B0_zero.Op.find_aggregate_error]
   we might want to change this but it seems log writing is already
   not so fast. *)

(* Logs *)

type t =
  { hash_fun : string;
    file_hashes : B0_hash.t Fpath.Map.t;
    hash_dur : Mtime.Span.t;
    total_dur : Mtime.Span.t;
    cpu_dur : Os.Cpu.Time.Span.t;
    jobs : int;
    ops : B0_zero.Op.t list; }

let of_memo m =
  let r = B0_memo.reviver m in
  let module H = (val (B0_zero.Reviver.hash_fun r)) in
  let file_hashes = B0_zero.Reviver.file_hashes r in
  let hash_dur = B0_zero.Reviver.file_hash_dur r in
  let total_dur = Os.Mtime.count (B0_memo.clock m) in
  let cpu_dur = Os.Cpu.Time.count (B0_memo.cpu_clock m) in
  let jobs = B0_zero.Exec.jobs (B0_memo.exec m) in
  let ops = B0_memo.ops m in
  { hash_fun = H.id; hash_dur; file_hashes; total_dur; cpu_dur; jobs; ops }

let hash_fun l = l.hash_fun
let file_hashes l = l.file_hashes
let hash_dur l = l.hash_dur
let total_dur l = l.total_dur
let cpu_dur l = l.cpu_dur
let jobs l = l.jobs
let ops l = l.ops

(* IO *)

let enc_file_hashes b hs =
  let enc_file_hash b f h =
    B0_bincode.enc_fpath b f; B0_bincode.enc_hash b h
  in
  let count = Fpath.Map.cardinal hs in
  B0_bincode.enc_int b count;
  Fpath.Map.iter (enc_file_hash b) hs

let dec_file_hashes s i =
  let rec loop acc count s i =
    if count = 0 then i, acc else
    let i, file = B0_bincode.dec_fpath s i in
    let i, hash = B0_bincode.dec_hash s i in
    loop (Fpath.Map.add file hash acc) (count - 1) s i
  in
  let i, count = B0_bincode.dec_int s i in
  loop Fpath.Map.empty count s i

let magic = "b\x00\x00\x00log"

let enc b l =
  B0_bincode.enc_magic magic b ();
  B0_bincode.enc_string b l.hash_fun;
  enc_file_hashes b l.file_hashes;
  B0_bincode.enc_mtime_span b l.hash_dur;
  B0_bincode.enc_mtime_span b l.total_dur;
  B0_bincode.enc_cpu_time_span b l.cpu_dur;
  B0_bincode.enc_int b l.jobs;
  B0_bincode.enc_list (B0_bincode.enc B0_zero_conv.Op.bincode) b l.ops

let dec s i =
  let i, () = B0_bincode.dec_magic magic s i in
  let i, hash_fun = B0_bincode.dec_string s i in
  let i, file_hashes = i, Fpath.Map.empty in
  let i, file_hashes = dec_file_hashes s i in
  let i, hash_dur = B0_bincode.dec_mtime_span s i in
  let i, total_dur = B0_bincode.dec_mtime_span s i in
  let i, cpu_dur = B0_bincode.dec_cpu_time_span s i in
  let i, jobs = B0_bincode.dec_int s i in
  let i, ops =
    B0_bincode.dec_list (B0_bincode.dec (B0_zero_conv.Op.bincode)) s i
  in
  i, { hash_fun; file_hashes; hash_dur; total_dur; cpu_dur; jobs; ops; }

let bincode = B0_bincode.make enc dec

let write file l =
  let data =
    Log.time (fun _ msg -> msg "generating log") @@ fun () ->
    let buf = Buffer.create (1024 * 1024) in
    B0_bincode.to_string ~buf bincode l
  in
  Log.time (fun _ msg -> msg "writing log") @@ fun () ->
  Os.File.write ~force:true ~make_path:true file data

let read file =
  Result.bind (Os.File.read file) @@ fun data ->
  B0_bincode.of_string ~file bincode data
