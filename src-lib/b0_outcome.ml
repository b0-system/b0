(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_build

type t = B0_build.outcome

module Op = B0_op

let ops o = o.o_ops

let codec = B0_codec.v ~id:"outcome"
let read p = B0_codec.read codec p
let write p o = B0_codec.write codec p o

let fpath_meta o =
  let decode l =
    let m, _errs (* FIXME *) = B0_fpath_meta.Meta.decode l in
    m
  in
  B0_fpath.Map.map decode o.o_fpath_meta

let conf o =
  let c, _errs (* FIXME *) =  B0_conf.decode o.o_conf in
  c

let built_files o = B0_fpath.Map.dom o.o_written_files
let root_files o = B0_fpath.Map.dom o.o_source_files

let units o = o.o_units
let unit_names o = List.rev_map (fun (n, _) -> n) o.o_units

(* pp stats *)

let spawn_counts o =
  let is_spawn o = match Op.kind o with Op.Spawn _ -> true | _ -> false in
  let ops = List.filter is_spawn o.o_ops in
  let cached, uncached = List.partition Op.cached ops in
  (List.length uncached, List.length cached)

let pp_timed_count ppf (c, t) =
  B0_fmt.pf ppf "@[%d in %a@]" c B0_time.pp_span t

let pp_cpu_times ppf (u, s) =
  B0_fmt.pf ppf "@[user: %a sys: %a@]"
    B0_time.pp_float_s u B0_time.pp_float_s s

let pp_previous pp_v ppf v = B0_tty.pp [`Faint] pp_v ppf v

let pp_cmp pp_v ppf (now, prev) = match prev with
| None -> pp_v ppf now
| Some prev ->
    let pp_last ppf prev = B0_fmt.pf ppf "prev. %a" pp_v prev in
    B0_fmt.pf ppf "%a %a" pp_v now (pp_previous pp_last) prev

let pp_age ppf o = B0_fmt.int ppf o.o_age
let pp_spawns ppf o =
  let spawns, cached = spawn_counts o in
  B0_fmt.pf ppf "%d (%d cached)" spawns cached

let pp_tool_stamps ppf s =
  pp_timed_count ppf (s.cmd_stamp_count, s.cmd_stamp_dur)

let pp_file_stamps ppf s =
  pp_timed_count ppf (s.file_stamp_count, s.file_stamp_dur)

let pp_x_time ppf (self, children) =
  B0_time.pp_float_s ppf self;
  B0_fmt.sp ppf ();
  B0_fmt.field "children" B0_time.pp_float_s ppf children

let pp_user_time ppf s =
  pp_x_time ppf B0_time.(cpu_utime_s @@ s.cpu_dur,
                          cpu_children_utime_s @@ s.cpu_dur)

let pp_system_time ppf s =
  pp_x_time ppf B0_time.(cpu_stime_s @@ s.cpu_dur,
                          cpu_children_stime_s @@ s.cpu_dur)

let pp_duration ppf (n, p) =
  let dur_ratio now last =
    truncate @@ B0_time.(to_ns now /. to_ns last) *. 100.
  in
  let pp_last ppf p =
    B0_fmt.pf ppf "%d%% of prev. %a" (dur_ratio n.total_dur p.total_dur)
      B0_time.pp_span p.total_dur
  in
  match p with
  | None -> B0_time.pp_span ppf n.total_dur
  | Some p ->
      B0_fmt.pf ppf "%a %a" B0_time.pp_span n.total_dur (pp_previous pp_last) p

let pp_stats ppf o =
  let cmp = match o.o_age with
  | 0 -> o.o_stats, None
  | _ -> o.o_stats, Some o.o_prev_stats
  in
  B0_fmt.pf ppf "@[<v>";
  B0_fmt.field "age" pp_age ppf o; B0_fmt.cut ppf ();
(*
  B0_fmt.field "tool stamps" (pp_cmp pp_tool_stamps) ppf cmp; B0_fmt.cut ppf ();
*)
  B0_fmt.field "file stamps" (pp_cmp pp_file_stamps) ppf cmp; B0_fmt.cut ppf ();
  B0_fmt.field "spawns" pp_spawns ppf o; B0_fmt.cut ppf ();
  B0_fmt.field "user time" (pp_cmp pp_user_time) ppf cmp; B0_fmt.cut ppf ();
  B0_fmt.field "system time" (pp_cmp pp_system_time) ppf cmp; B0_fmt.cut ppf ();
  B0_fmt.field "duration" pp_duration ppf cmp;
  B0_fmt.pf ppf "@]";
  ()

let unit_id_name_map o =
  let add_id acc (name, (id, _)) = B0_unit.Idmap.add id name acc in
  List.fold_left add_id B0_unit.Idmap.empty o.o_units

let unit_id_set names o =
  let add_id acc (u, (id, _)) = match List.mem u names with
  | true -> B0_unit.Idset.add id acc
  | false -> acc
  in
  List.fold_left add_id B0_unit.Idset.empty o.o_units

let select_ops idset o =
  let add_op acc op = match B0_unit.Idset.mem (B0_op.unit_id op) idset with
  | true -> op :: acc
  | false -> acc
  in
  List.fold_left add_op [] o.o_ops

(* Output according to Trace Event Format.
   https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview#heading=h.yr4qxyxotyw *)

let string_array_json a =
  let add seq v = B0_json.(el (str v) ++ seq) in
  B0_json.arr @@ Array.fold_left add B0_json.empty a

let span_us_json s =
  B0_json.int @@ Int64.(to_int @@ div (B0_time.to_uint64_ns s) 1000L)

let cmd_json c = B0_json.str @@ B0_cmd.to_string c
let path_json p = B0_json.str @@ B0_fpath.to_string p
let path_set_json pset =
  let add_path p seq = B0_json.(seq ++ el (path_json p)) in
  B0_json.arr @@ B0_fpath.Set.fold add_path pset B0_json.empty

let aim_json a =
  B0_json.str (match a with `Build_os -> "build OS" | `Host_os -> "host OS")

let duration_json o =
  let d = B0_time.abs_diff (B0_op.exec_end_time o) (B0_op.exec_start_time o) in
  span_us_json d

let args_json op =
  let open B0_json in
  let kind_mems = match B0_op.kind op with
  | B0_op.Spawn s ->
      mem "cmd" (cmd_json @@ B0_op.spawn_cmd s) ++
      mem "cwd" (path_json @@ B0_op.spawn_cwd s) ++
      mem "env" (string_array_json @@ B0_op.spawn_env s)
  | B0_op.Mkdir m ->
      mem "dir" (path_json @@ B0_op.mkdir_dir m)
  | B0_op.Sync s ->
      mem "units" (B0_json.str "TODO")
  | _ -> B0_json.empty
  in
  obj @@ (* The order here is for the viewer. *)
  mem "kind" (str (B0_op.kind_to_string @@ B0_op.kind op)) ++
  mem "writes" (path_set_json (B0_op.writes op)) ++
  mem "cached" (B0_json.bool (B0_op.cached op)) ++
  kind_mems ++
  mem "reads" (path_set_json (B0_op.reads op)) ++
  mem "aim" (aim_json (B0_op.aim op))

let op_json id_name_map seq op =
  let id op = B0_json.str (string_of_int @@ B0_op.id op) in
  let unit_name op =
    match B0_unit.Idmap.find (B0_op.unit_id op) id_name_map with
    | name -> name
    | exception Not_found -> assert false
  in
  let cat op = B0_json.str @@ B0_op.kind_to_string @@ B0_op.kind op in
  let open B0_json in
  seq ++
  begin
    el @@ obj @@
    mem "name" (id op) ++
    mem "id" (id op) ++
    mem "cat" (cat op) ++
    mem "ph" (str "X") ++
    mem "ts" (span_us_json (B0_op.exec_start_time op)) ++
    mem "dur" (duration_json op) ++
    mem "pid" (str @@ unit_name op) ++
    mem "tid" (int 1) ++
    mem "args" (args_json op)
  end

let ops_json id_name_map ops =
  let seq = List.fold_left (op_json id_name_map) B0_json.empty ops in
  B0_json.arr seq

let ops_to_json o ops =
  let idmap = unit_id_name_map o in
  B0_json.to_string (ops_json idmap ops)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
