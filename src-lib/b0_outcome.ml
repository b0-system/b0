(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_build

type stats =
  { tool_stamp_count : int; (* FIXME maybe remove *)
    tool_stamp_dur : B0_time.span;
    file_stamp_count : int; (* uncached *)
    file_stamp_dur : B0_time.span;
    cpu_dur : B0_time.cpu;
    total_dur : B0_time.span; }

type t =
  { age : int;
    stats : stats;
    prev_stats : stats option;
    file_info : file_info B0_fpath.Map.t;
    fpath_meta : (string * string) list B0_fpath.map;
    conf : (string * string) list;
    ops : B0_op.t list;
    units : B0_unit.marshalable list }

(* Build outcome *)

let stats_of_build b =
  let tool_stamp_count = 0 in
  let tool_stamp_dur = B0_time.zero in
  let file_stamp_count =
    B0_fpath.Map.cardinal @@ B0_cache.file_stamps (B0_build.cache b)
  in
  let file_stamp_dur = B0_cache.file_stamp_dur (B0_build.cache b) in
  let cpu_dur = B0_build.cpu_dur b in
  let total_dur = B0_build.total_dur b in
  { tool_stamp_count; tool_stamp_dur; file_stamp_count; file_stamp_dur;
    cpu_dur; total_dur }

let of_build ?prev b =
  if not (B0_build.finished b) then invalid_arg "build is not finished" else
  let outcome_fpath_meta b =
    let encode meta =
      let encs, _errs (* FIXME *) = B0_meta.Fpath.encode meta in
      encs
    in
    B0_fpath.Map.map encode (B0_build.fpath_meta b)
  in
  let outcome_conf b =
    let encs, _errs (* FIXME *) = B0_conf.encode (B0_build.conf_used b) in
    encs
  in
  let age = match prev with None -> 0 | Some p -> p.age + 1 in
  let stats = stats_of_build b in
  let prev_stats = match prev with None -> None | Some p -> Some p.stats in
  let file_info = B0_build.file_info b in
  let fpath_meta = outcome_fpath_meta b in
  let ops = B0_build.ops b in
  let units = (* FIXME we likely want the ones from b.b.ustate here
                   aswell as the status. *)
    List.rev_map B0_unit.to_marshalable (B0_build.units b)
  in
  let conf = outcome_conf b in
  { age; stats; prev_stats; file_info; fpath_meta; conf; ops; units }

let codec = B0_codec.v ~id:"outcome" (* FIXME remove *)
let read p = B0_codec.read codec p
let write p o = B0_codec.write codec p o

let fpath_meta o =
  let decode l =
    let m, _errs (* FIXME *) = B0_meta.Fpath.decode l in
    m
  in
  B0_fpath.Map.map decode o.fpath_meta

let conf o =
  let c, _errs (* FIXME *) =  B0_conf.decode o.conf in
  c

let units o = o.units
let unit_names o = List.rev_map (fun (n, _) -> n) o.units
let unit_id_name_map o =
  let add_id acc (name, (id, _)) = B0_unit.Idmap.add id name acc in
  List.fold_left add_id B0_unit.Idmap.empty o.units

let unit_id_set names o =
  let add_id acc (u, (id, _)) = match List.mem u names with
  | true -> B0_unit.Idset.add id acc
  | false -> acc
  in
  List.fold_left add_id B0_unit.Idset.empty o.units

let built_files o =
  let add_built f info acc = match info.B0_build.f_kind with
  | B0_build.Built -> B0_fpath.Set.add f acc
  | _ -> acc
  in
  B0_fpath.Map.fold add_built o.file_info B0_fpath.Set.empty

let root_files o =
  let add_root f info acc = match info.B0_build.f_kind with
  | B0_build.Root -> B0_fpath.Set.add f acc
  | _ -> acc
  in
  B0_fpath.Map.fold add_root o.file_info B0_fpath.Set.empty

(* Build operations *)

module Op = B0_op

let ops o = o.ops

(* Build statistics *)

let spawn_counts o =
  let is_spawn o = match Op.kind o with Op.Spawn _ -> true | _ -> false in
  let ops = List.filter is_spawn o.ops in
  let cached, uncached = List.partition Op.cached ops in
  (List.length uncached, List.length cached)

let pp_timed_count ppf (c, t) =
  B0_fmt.pf ppf "@[%d in %a@]" c B0_time.pp_span t

let pp_cpu_times ppf (u, s) =
  B0_fmt.pf ppf "@[user: %a sys: %a@]"
    B0_time.pp_float_s u B0_time.pp_float_s s

let pp_previous pp_v ppf v = B0_fmt.tty [`Faint] pp_v ppf v

let pp_cmp pp_v ppf (now, prev) = match prev with
| None -> pp_v ppf now
| Some prev ->
    let pp_last ppf prev = B0_fmt.pf ppf "prev. %a" pp_v prev in
    B0_fmt.pf ppf "%a %a" pp_v now (pp_previous pp_last) prev

let pp_age ppf o = B0_fmt.int ppf o.age
let pp_spawns ppf o =
  let spawns, cached = spawn_counts o in
  B0_fmt.pf ppf "%d (%d cached)" spawns cached

let pp_tool_stamps ppf s =
  pp_timed_count ppf (s.tool_stamp_count, s.tool_stamp_dur)

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
  let cmp = (o.stats, o.prev_stats) in
  B0_fmt.pf ppf "@[<v>";
  B0_fmt.field "age" pp_age ppf o; B0_fmt.cut ppf ();
(*  B0_fmt.field "tool stamps" (pp_cmp pp_tool_stamps) ppf cmp; B0_fmt.cut ppf (); *)
  B0_fmt.field "file stamps" (pp_cmp pp_file_stamps) ppf cmp; B0_fmt.cut ppf ();
  B0_fmt.field "spawns" pp_spawns ppf o; B0_fmt.cut ppf ();
  B0_fmt.field "user time" (pp_cmp pp_user_time) ppf cmp; B0_fmt.cut ppf ();
  B0_fmt.field "system time" (pp_cmp pp_system_time) ppf cmp; B0_fmt.cut ppf ();
  B0_fmt.field "duration" pp_duration ppf cmp;
  B0_fmt.pf ppf "@]";
  ()


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
