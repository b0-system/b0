(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

(* Output according to Trace Event Format.
   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview#heading=h.yr4qxyxotyw *)

let string_array_json a =
  let add seq v = B0_json.G.(el (string v) ++ seq) in
  B0_json.G.arr @@ Array.fold_left add B0_json.G.empty a

let span_us_json s =
  B0_json.G.int @@ Int64.(to_int @@ div (Time.to_uint64_ns s) 1000L)

let cmd_json c = B0_json.G.string @@ Cmd.to_string c
let path_json p = B0_json.G.string @@ Fpath.to_string p
let path_set_json pset =
  let add_path p seq = B0_json.G.(seq ++ el (path_json p)) in
  B0_json.G.arr @@ Fpath.Set.fold add_path pset B0_json.G.empty

let aim_json a =
  B0_json.G.string (match a with `Build_os -> "build OS" | `Host_os -> "host OS")

let duration_json o =
  span_us_json @@
  Time.abs_diff (Outcome.Op.exec_end_time o) (Outcome.Op.exec_start_time o)

let args_json op =
  let open B0_json.G in
  let kind_mems = match Outcome.Op.kind op with
  | Outcome.Op.Spawn s ->
      (* TODO add allowed_exits *)
      mem "cmd" (cmd_json @@ Outcome.Op.spawn_cmd s) ++
      mem "cwd" (path_json @@ Outcome.Op.spawn_cwd s) ++
      mem "env" (string_array_json @@ Outcome.Op.spawn_env s)
  | Outcome.Op.Mkdir m ->
      mem "dir" (path_json @@ Outcome.Op.mkdir_dir m)
  | Outcome.Op.Sync s ->
      mem "units" (B0_json.G.string "TODO")
  | _ -> B0_json.G.empty
  in
  obj @@ (* The order here is for the viewer. *)
  mem "kind" (string (Outcome.Op.kind_to_string @@ Outcome.Op.kind op)) ++
  mem "writes" (path_set_json (Outcome.Op.writes op)) ++
  mem "cached" (B0_json.G.bool (Outcome.Op.cached op)) ++
  kind_mems ++
  mem "reads" (path_set_json (Outcome.Op.reads op)) ++
  mem "aim" (aim_json (Outcome.Op.aim op))

let op_json id_name_map seq op =
  let id op = B0_json.G.string (string_of_int @@ Outcome.Op.id op) in
  let unit_name op =
    match Unit.Idmap.find (Outcome.Op.unit_id op) id_name_map with
    | name -> name
    | exception Not_found -> assert false
  in
  let cat op =
    B0_json.G.string @@
    Outcome.Op.kind_to_string @@ Outcome.Op.kind op
  in
  let open B0_json.G in
  seq ++
  begin
    el @@ obj @@
    mem "name" (id op) ++
    mem "id" (id op) ++
    mem "cat" (cat op) ++
    mem "ph" (string "X") ++
    mem "ts" (span_us_json (Outcome.Op.exec_start_time op)) ++
    mem "dur" (duration_json op) ++
    mem "pid" (string @@ unit_name op) ++
    mem "tid" (int 1) ++
    mem "args" (args_json op)
  end

let ops_json id_name_map ops =
  let seq = List.fold_left (op_json id_name_map) B0_json.G.empty ops in
  B0_json.G.arr seq

let ops_to_trace_event_format o ops = ops_json (Outcome.unit_id_name_map o) ops

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
