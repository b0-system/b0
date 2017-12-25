(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* TODO the quick refactoring from using Unit.t directly to using Unit.id
   must have introduced quite some mess. A review is needed at that
   ocasion use [uid] intead of [u]. *)

let strf = Format.asprintf

(* Global build stats *)

type stats =
  { mutable cpu_dur : B0_time.cpu;
    mutable cmd_stamp_count : int; (* uncached *)
    mutable cmd_stamp_dur : B0_time.span;
    mutable file_stamp_count : int; (* uncached *)
    mutable file_stamp_dur : B0_time.span;
    mutable total_dur : B0_time.span; }

let zero_stats () =
  { cpu_dur = B0_time.cpu_zero;
    cmd_stamp_count = 0; cmd_stamp_dur = B0_time.zero;
    file_stamp_count = 0; file_stamp_dur = B0_time.zero;
    total_dur = B0_time.zero; }

(* Outcome information *)

type written_file = (* Information about a written file in the build. *)
  { w_age : int; (* Age in which the path was last linked/written. *)
    w_file_stamp : B0_stamp.t; (* File stamp *)
    w_cmd_write_stamp : B0_hash.t; (* Cmd stamp (key in the cache). *) }

type source_file = (* Information about a source (non-written) file. *)
  B0_stamp.t

(* Build outcomes, persisted build info. *)

type outcome =
  { o_age : int;
    o_stats : stats;
    o_prev_stats : stats;
    o_written_files : written_file B0_fpath.map;
    o_source_files : source_file B0_fpath.map;
    o_fpath_meta : (string * string) list B0_fpath.map;
    o_conf : (string * string) list;
    o_ops : B0_op.t list;
    o_units : B0_unit.marshalable list }

let empty_outcome () =
  { o_age = 0; o_stats = zero_stats (); o_prev_stats = zero_stats ();
    o_written_files = B0_fpath.Map.empty; o_source_files = B0_fpath.Map.empty;
    o_fpath_meta = B0_fpath.Map.empty;
    o_conf = []; o_ops = []; o_units = []}

(* Build control *)

type ctrl = { c_tty_cap : B0_tty.cap; c_max_spawn : int }
let ctrl ?tty_cap:(c_tty_cap = B0_tty.None) ?max_spawn:(c_max_spawn = 4) () =
  { c_tty_cap; c_max_spawn }

(* Build *)

type unit_status = Active | Failed of unit B0_fmt.t | Aborted | Finished
type unit_state =
  { unit_unit : B0_unit.t;
    mutable unit_status : unit_status;
    mutable unit_activity : int; (* Unfinished ops + konts *) }

type build =
  { prev_outcome : outcome; (* Persistent build info *)
    dur_counter : B0_time.counter;
    cpu_counter : B0_time.cpu_counter;
    dir : B0_fpath.t; (* Build directory *)
    trash : B0_fpath.t ; (* Trash directory *)
    age : int;
    stats : stats;
    ctrl : ctrl;
    cache : B0_cache.t;
    mutable conf : B0_conf.t;
    env : B0_env.t;
    universe : build_unit list;
    units : build_unit list;
    mutable conf_last : B0_conf.t;
    mutable fpath_meta : B0_fpath_meta.Meta_map.t;
    mutable finished : bool;
    mutable written_files : written_file B0_fpath.map;
    mutable source_files : source_file B0_fpath.map;
    mutable ustate_host : unit_state B0_unit.Idmap.t;
    mutable ustate_build : unit_state B0_unit.Idmap.t;
    guards : B0_guard.handler;
    execs : B0_exec.handler;
    mutable konts : (B0_op.t -> unit) B0_op.Map.t;
    mutable ops : B0_op.t list; (* Kept for outcome *) }

and t =
  { unit : B0_unit.t; (* Current unit *)
    aim : B0_conf.build_aim; (* Current aim *)
    build_dir : B0_fpath.t; (* Current build dir *)
    b : build }

and build_unit = B0_unit.t * (t -> unit)

type cache = B0_cache.t
let dir b = b.b.dir
let trash b = b.b.trash

let _unit_build_dir b aim u =
  let suff = match aim with `Build_os -> "@b" | `Host_os -> "" in
  B0_fpath.(b.b.dir / (B0_unit.name u ^ suff))

let unit_build_dir b u = _unit_build_dir b b.aim u
let build_dir b = b.build_dir
let build_file b f = B0_fpath.(b.build_dir / f)
let stored_conf b = b.b.conf
let cache b = b.b.cache
let units b = List.map fst b.b.units
let src_dir b = B0_unit.src_root b.unit
let unit b = b.unit
let time_stamp b = B0_time.count b.b.dur_counter

let create ?prev_outcome cache ctrl env conf fpath_meta ~dir ~universe units =
  let dur_counter = B0_time.counter () in
  let cpu_counter = B0_time.cpu_counter () in
  let trash = B0_fpath.(dir / "trash") in
  let tmp = B0_fpath.(dir / "tmp") in
  B0_os.Dir.create ~path:true dir >>= fun _ ->
  B0_os.Dir.create ~path:false trash >>= fun _ ->
  B0_os.Dir.create ~path:false tmp >>= fun _ ->
  let tmp_path = B0_fpath.(tmp / "tmp") in
  let prev_outcome, age = match prev_outcome with
  | None -> empty_outcome (), 0 | Some o -> o, o.o_age + 1
  in
  let written_files = prev_outcome.o_written_files in
  let execs =
    B0_exec.handler ~max_spawn:ctrl.c_max_spawn () ~dur_counter ~tmp_path
  in
  let b =
    { prev_outcome; dur_counter; cpu_counter;
      dir; trash; age; stats = zero_stats (); ctrl; cache; conf; env;
      universe; units;
      fpath_meta; conf_last = B0_conf.empty; finished = false; written_files;
      source_files = B0_fpath.Map.empty;
      ustate_host = B0_unit.Idmap.empty;
      ustate_build = B0_unit.Idmap.empty;
      konts = B0_op.Map.empty; guards = B0_guard.handler (); execs; ops = [] }
  in
  Ok { unit = B0_unit.nil; build_dir = B0_os.File.null; aim = `Host_os; b }

(* Logging, FIXME make that a data type and add a logger function. Better
   for IDE interaction. *)

let pp_name ppf u = B0_tty.pp_str [] ppf (B0_unit.name u)
let pp_name_red ppf u = B0_tty.pp_str [`Fg `Red] ppf (B0_unit.name u)
let pp_aim ppf = function
| `Host_os -> ()
| `Build_os -> B0_fmt.pf ppf "[AIM:BUILD]"

let log_unit_start aim u =
  let build_style = [`Fg `Yellow] in
  B0_log.app (fun m -> m "[%a]%a %a" (B0_tty.pp_str build_style) "START"
                 pp_aim aim
                 pp_name u)

let log_unit_finish aim u =
  let build_style = [`Fg `Green] in
  let pp_outcome ppf u = match B0_unit.doc_outcome u with
  | "" -> ()
  | o -> B0_fmt.pf ppf "%s " o
  in
  B0_log.app (fun m -> m "[%a]%a %a%a" (B0_tty.pp_str build_style) "BUILT"
                 pp_aim aim
                 pp_outcome u pp_name u)

let log_unit_fail u msg =
  B0_log.app (fun m -> m "@[<v>[%a] %a:@,  @[%a@]@,@]"
                 (B0_tty.pp_str [`Fg `Red]) "FAIL "
                 pp_name_red u msg ())

let log_unit_abort u u_cause =
  let msg ppf u_cause = match u_cause with
  | None -> ()
  | Some u -> B0_fmt.pf ppf ": %a failed" pp_name u
  in
  B0_log.app (fun m -> m "[%a] %a%a"
                 (B0_tty.pp_str [`Fg `Red]) "ABORT"
                 pp_name u msg u_cause)

let log_finish b =
  let add_success _ ustate (u, tot) = match ustate.unit_status with
  | Finished -> (u + 1, tot + 1)
  | _ -> (u, tot + 1)
  in
  let acc = (0, 0) in
  let acc = B0_unit.Idmap.fold add_success b.b.ustate_host acc in
  let succ, tot = B0_unit.Idmap.fold add_success b.b.ustate_build acc in
  let style = if succ <> tot then [`Fg `Red] else [`Fg `Green] in
  let count = strf "%d/%d" succ tot in
  let d = b.b.stats.total_dur in
  B0_log.app (fun m -> m "[%a] units in %a"
                 (B0_tty.pp_str style) count B0_time.pp_span d);
  ()

(* Failing builds *)

exception Fail of unit B0_fmt.t

let fail msgf = raise (Fail (fun ppf () -> msgf (B0_fmt.pf ppf)))
let fail_on_error_msg = function
| Error (`Msg msg) -> fail (fun m -> m "%s" msg)
| Ok v -> v

let conf b k = match B0_conf.get_effective b.b.env b.aim k b.b.conf with
| Error _ as e  -> fail_on_error_msg e
| Ok (src, v) ->
    b.b.conf_last <- B0_conf.add k v b.b.conf_last;
    begin match src with
    | `Default when B0_conf.value_store (B0_conf.Key.default k) ->
        b.b.conf <- B0_conf.add k v b.b.conf
    | _ -> ()
    end;
    v

let find_path_meta b p k = B0_fpath_meta.Meta_map.find p k b.b.fpath_meta
let add_path_meta ?(force = false) b p k v =
  match B0_fpath.Map.mem p b.b.fpath_meta && not force with
  | true ->
      fail (fun m -> m "%a: file metadata redefinition for key %a"
               B0_fpath.pp p B0_fpath_meta.Meta.Key.pp_name
               (B0_fpath_meta.Meta.Key.of_typed k))
  | false ->
    (* Could check and warn if the path not either a source or in the current
       bdir *)
             b.b.fpath_meta <- B0_fpath_meta.Meta_map.add p k v b.b.fpath_meta

(* Unit state handling *)

let create_unit_state b aim u =
  let s = { unit_unit = u; unit_status = Active; unit_activity = 0 } in
  begin match aim with
  | `Host_os ->
      b.b.ustate_host <- B0_unit.Idmap.add (B0_unit.id u) s b.b.ustate_host;
  | `Build_os ->
      b.b.ustate_build <- B0_unit.Idmap.add (B0_unit.id u) s b.b.ustate_build;
  end;
  s

let find_unit_state b aim uid =
  let m = match aim with
  | `Host_os -> b.b.ustate_host
  | `Build_os -> b.b.ustate_build
  in
  B0_unit.Idmap.find uid m

let get_unit_state b aim uid =
  let m = match aim with
  | `Host_os -> b.b.ustate_host
  | `Build_os -> b.b.ustate_build
  in
  match B0_unit.Idmap.find uid m with
  | u -> u | exception Not_found -> assert false

let get_unit_status s = s.unit_status
let set_unit_status s st = s.unit_status <- st
let get_unit_activity s = s.unit_activity
let incr_unit_activity s = s.unit_activity <- s.unit_activity + 1
let decr_unit_activity s = s.unit_activity <- s.unit_activity - 1

let fail_unit b aim uid msg =
  let state = get_unit_state b aim uid in
  set_unit_status state (Failed msg);
  log_unit_fail state.unit_unit msg

let abort_unit b aim uid u_cause =
  let u_cause = match u_cause with
  | None -> None | Some uid -> Some ((get_unit_state b aim uid).unit_unit)
  in
  let state = get_unit_state b aim uid in
  set_unit_status state Aborted;
  log_unit_abort state.unit_unit u_cause

let handle_unit_failure b aim uid f x = try f x with
| Fail msg -> fail_unit b aim uid msg
| exn ->
    let bt = Printexc.get_raw_backtrace () in
    let msg ppf () =
      B0_fmt.pf ppf "@[<v>Unexpected failure:@, %a@]"
        B0_fmt.exn_backtrace (exn, bt)
    in
    fail_unit b aim uid msg

let try_finish_unit b aim uid =
  let ustate = get_unit_state b aim uid in
  match get_unit_activity ustate = 0 with
  | false -> ()
  | true ->
      if get_unit_status ustate = Active then set_unit_status ustate Finished;
      B0_guard.set_unit_ready b.b.guards uid;
      log_unit_finish aim ustate.unit_unit

let start_unit b aim (u, func) =
  let start build_dir b =
    let rec prepare_unit_build_dir b =
      try Unix.mkdir (B0_fpath.to_string build_dir) 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          fail_on_error_msg (B0_os.B0.trash_path build_dir ~in_dir:(trash b));
          prepare_unit_build_dir b
      | Unix.Unix_error (e, _, _) ->
          fail (fun m ->  m "Create unit build dir %a: %s"
                   B0_fpath.pp build_dir (Unix.error_message e))
    in
    prepare_unit_build_dir b; func b
  in
  let build_dir = _unit_build_dir b aim u in
  let b = { b with build_dir; aim; unit = u } in
  let ustate = create_unit_state b aim u in
  incr_unit_activity ustate;
  log_unit_start aim u;
  handle_unit_failure b aim (B0_unit.id u) (start build_dir) b;
  decr_unit_activity ustate; (* func has finished *)
  try_finish_unit b aim (B0_unit.id u);
  ()

let start b =
  let start (u, func as unit) =
    let aim = match B0_unit.only_aim u with None -> `Host_os | Some a -> a in
    start_unit b aim unit
  in
  List.iter start b.b.units

let ensure_build b u aim =
  let uid = (B0_unit.id u) in
  match find_unit_state b aim uid with
  | u -> ()
  | exception Not_found ->
      let find_unit (u, func) = B0_unit.id u = uid in
      match List.find find_unit b.b.units with
      | unit -> start_unit b aim unit
      | exception Not_found ->
          fail (fun m -> m "TODO unit to build (%a) is not in the build set"
            B0_unit.pp_name u)

(* Kontinuations *)

let add_kont b o k =
  incr_unit_activity (get_unit_state b (B0_op.aim o) (B0_op.unit_id o));
  b.b.konts <- B0_op.Map.add o k b.b.konts

let discard_kont b o =
  decr_unit_activity (get_unit_state b (B0_op.aim o) (B0_op.unit_id o));
  b.b.konts <- B0_op.Map.remove o b.b.konts

let exec_kont b o = match B0_op.Map.find o b.b.konts with
| exception Not_found -> ()
| k ->
    b.b.konts <- B0_op.Map.remove o b.b.konts;
    k o;
    decr_unit_activity (get_unit_state b (B0_op.aim o) (B0_op.unit_id o))

(* Handling operations *)

let add_file_write b w_cmd_write_stamp file w_file_stamp =
  let written_file = { w_age = b.b.age; w_file_stamp; w_cmd_write_stamp } in
  b.b.written_files <- B0_fpath.Map.add file written_file b.b.written_files

let register_write b file =
  let cmd_write_stamp = B0_hash.zero in
  let file_stamp = B0_cache.file_stamp b.b.cache file in
  add_file_write b cmd_write_stamp file file_stamp;
  B0_guard.set_file_ready b.b.guards file

let register_op_writes b o =
  try B0_fpath.Set.iter (register_write b) (B0_op.writes o) with
  | Sys_error err ->
      B0_log.app (fun m -> m
                     "FIXME Not written %s:@\n %a" err B0_op.pp o)

let finish_cached_op b o = (* FIXME make that more precise *)
  register_op_writes b o;
  exec_kont b o

let finish_op_kind b o =
  let finish_spawn b op s = match B0_op.spawn_result s with
  | Ok (_, `Exited c) ->
      let success = match B0_op.spawn_success_codes s with
      | None when c = 0 -> true
      | Some [] -> true
      | Some cs when List.mem c cs -> true
      | _ -> false
      in
      begin match success with
      | false -> fail (fun m -> m "%a" B0_op.pp_spawn_fail op)
      | true -> register_op_writes b o
      end
  | Ok (_, `Signaled c) ->
      fail (fun m -> m "%a" B0_op.pp_spawn_fail op)
  | Error (`Msg e) ->
      fail (fun m -> m "%s" e) (* TODO register non-writes ? *)
  in
  let finish_read b o r = match B0_op.read_result r with
  | Error (`Msg e) -> discard_kont b o; fail (fun m -> m "%s" e)
  | Ok _ -> exec_kont b o
  in
  let finish_copy_file b o c = match B0_op.copy_file_result c with
  | Error (`Msg e) -> fail (fun m -> m "%s" e)
  | Ok () -> register_write b (B0_op.copy_file_dst c)
  in
  let finish_mkdir b o m = match B0_op.mkdir_result m with
  | Error (`Msg e) -> discard_kont b o; fail (fun m -> m "%s" e)
  | Ok () -> exec_kont b o
  in
  let finish_sync b o s = exec_kont b o in
  match B0_op.cached o with
  | true -> finish_cached_op b o
  | false ->
      begin match B0_op.kind o with
      | B0_op.Spawn s -> finish_spawn b o s
      | B0_op.Read r -> finish_read b o r
      | B0_op.Copy_file c -> finish_copy_file b o c
      | B0_op.Mkdir m -> finish_mkdir b o m
      | B0_op.Sync u -> finish_sync b o u
      | B0_op.Write _ | B0_op.Delete _ -> (* not exposed yet *) assert false
      end;
      (* N.B. we don't hit that line if the operation fails *)
      B0_cache.add_op b.b.cache o

let finish_op b o =
  let uid = B0_op.unit_id o in
  let aim = B0_op.aim o in
  let ustate = get_unit_state b aim uid in
  match get_unit_status ustate with
  | Finished -> assert false
  | Aborted | Failed _ -> ()
  | Active ->
      decr_unit_activity ustate;
      let finish () =
        finish_op_kind b o; try_finish_unit b aim uid
      in
      handle_unit_failure b aim uid finish ()

let submit_op b o =
  let aim = B0_op.aim o in
  match get_unit_status (get_unit_state b aim @@ B0_op.unit_id o) with
  | Active ->
      begin match B0_cache.exec b.b.cache o with
      | true -> finish_op b o
      | false -> B0_exec.submit b.b.execs o
      end
  | Failed _ -> ()
  | Aborted | Finished -> assert false

let rec submit_ops b = match B0_guard.ready b.b.guards with
| Some o -> submit_op b o; submit_ops b
| None -> ()

let rec collect_ops ~finish b =
  match B0_exec.collect b.b.execs ~block:finish with
  | Some o -> finish_op b o; submit_ops b; collect_ops ~finish b
  | None ->
      match B0_guard.ready b.b.guards with
      | Some o -> submit_op b o; collect_ops ~finish b
      | None -> ()

(* Build operations *)

type 'a fiber = ('a -> unit) -> unit

let add_op b o =
  b.b.ops <- o :: b.b.ops;
  incr_unit_activity (get_unit_state b (B0_op.aim o) (B0_op.unit_id o));
  B0_guard.add b.b.guards o;
  submit_ops b;
  collect_ops ~finish:false b;
  ()

let ready b p =
  B0_guard.set_file_ready b.b.guards p;
  b.b.source_files <-
    B0_fpath.Map.add p B0_stamp.zero b.b.source_files (* FIXME *)

let src b src =
  let src = B0_fpath.(src_dir b // src) in
  ready b src; src

type stdo = [ `Ui | `File of B0_fpath.t | `Tee of B0_fpath.t ]

type run =
  { tool : B0_tool.t;
    args : B0_cmd.t;
    env : string array; }

let tool_run_env b tool =
  let env = B0_env.env b.b.env b.aim in
  let forced_env = B0_env.forced_env b.b.env b.aim in
  let tool_env, _ = B0_tool.lookup_env tool env in
  let env = B0_os.Env.override tool_env ~by:forced_env in
  Array.of_list @@ B0_os.Env.to_assignments env

let tool b tool args = { tool; args; env = tool_run_env b tool }
let conf_tool b k args = tool b (conf b k) args

let spawn_env b run env = match B0_string.Map.is_empty env with
| true -> run.env
| false ->
    let env = B0_env.env b.b.env b.aim in
    let forced_env = B0_env.forced_env b.b.env b.aim in
    let tool_env, _ = B0_tool.lookup_env run.tool env in
    let env = B0_os.Env.(override (override tool_env ~by:env) ~by:forced_env) in
    Array.of_list @@ B0_os.Env.to_assignments env

let spawn b
    ?(reads = []) ?(writes = []) ?success ?(env = B0_string.Map.empty) ?cwd
    ?stdin ?(stdout = `Ui) ?(stderr = `Ui) run
  =
  let time = time_stamp b in
  let cwd = match cwd with None -> b.b.dir | Some cwd -> cwd in
  let env = spawn_env b run env in
  let cmd, reads  = match B0_tool.unit run.tool with
  | Some u ->
      let udir = _unit_build_dir b `Build_os u in
      let cmd = B0_fpath.(udir // B0_tool.name run.tool) in
      ensure_build b u `Build_os;
      cmd, cmd :: reads
  | None ->
      (* FIXME do that when run is defined (? *)
      fail_on_error_msg @@
      B0_env.tool b.b.env b.aim [B0_tool.name run.tool],
      reads
  in
  let reads = B0_fpath.Set.of_list reads in
  let writes = B0_fpath.Set.of_list writes in
  let cmd = B0_cmd.(v (p cmd) %% run.args) in
  let u = b.unit in
  let o =
    B0_op.spawn u b.aim time ~reads ~writes ~success ~stdin ~stdout ~stderr
      ~cwd env cmd
  in
  add_op b o

let copy_file ?linenum b src dst =
  let time = time_stamp b in
  let o = B0_op.copy_file ?linenum b.unit b.aim time src dst in
  add_op b o

let read b f k =
  let time = time_stamp b in
  let o = B0_op.read b.unit b.aim time f in
  let k o = k (B0_op.get_read_data @@ B0_op.get_read o) in
  add_kont b o k;
  add_op b o

let write b ?reads f gen =
  (* We need a post guard cb to call gen. *) failwith "TODO"

let mkdir b d k =
  let time = time_stamp b in
  let o = B0_op.mkdir b.unit b.aim time d in
  let k o = k () in
  add_kont b o k;
  add_op b o

let await_units b us k =
  let time = time_stamp b in
  let add_id acc u = B0_unit.Idset.add (B0_unit.id u) acc in
  let ids = List.fold_left add_id B0_unit.Idset.empty us in
  let o = B0_op.sync b.unit b.aim time ids in
  let k o = k () in
  add_kont b o k;
  add_op b o

(* Finishing the build

   Most of the code deals with reporting errors if we still have
   guarded operations. For now the heuristic is to:

   1. Discard guards whose operations belong to a failed unit
      while collecting their writes in a set of writes that never
      happened.
   2. Abort the unit of the operations of guards whose reads intersect
      the writes that never happened or who where waiting on units
      that never finished. Operations of aborted guards are then added
      to writes that never happened. The process is applied until fix
      point.
   4. Try to find a read/wrte cycle in pairs of the remaining guards.
      If that is found fail the corresponding units and start over.

    FIXME deal with transitive cycles.
    FIXME try to find earliest problem in chain of ops
    FIXME this should be centered on units rather than
    iterating on the guards like we do at the moment: take an active
    unit and explore the cause by starting from its active ops.
    this will also find transitive cycles. *)

let add_never_written o maybe_u never =
  let add_write w map = B0_fpath.Map.add w maybe_u map in
  B0_fpath.Set.fold add_write (B0_op.writes o) never

let resolve_known_failed_unit_writes b never gs =
  (* Lookup operations that belong to a failed unit and collect their
     writes into a map. *)
  let rec loop unr never = function
  | [] -> never, unr
  | g :: gs ->
      let o = B0_guard.op g in
      let uid = B0_op.unit_id o in
      let aim = B0_op.aim o in
      match get_unit_status (get_unit_state b aim uid) with
      | Finished | Aborted -> assert false
      | Active -> loop (g :: unr) never gs
      | Failed _ -> loop unr (add_never_written o (Some uid) never) gs
  in
  loop [] never gs

let abort_awaiting_on_failures b never gs =
  (* Lookup operations that were waiting on failed writes or units
     and abort their unit and recursively. *)
  let rec loop res_count (never : B0_unit.id option B0_fpath.map) unr = function
  | [] -> if res_count = 0 then never, unr else loop 0 never [] unr
  | g :: gs ->
      let o = B0_guard.op g in
      let uid = B0_op.unit_id o in
      let aim = B0_op.aim o in
      match get_unit_status (get_unit_state b aim uid) with
      | Finished | Failed _ -> assert false
      | Aborted -> loop (res_count + 1) (add_never_written o None never) unr gs
      | Active ->
          let awaits = B0_guard.awaiting_files g in
          match B0_fpath.Set.is_empty awaits with
          | true ->
              let units = B0_guard.awaiting_units g in
              let never_read uid acc =
                match get_unit_status (get_unit_state b aim uid) with
                | Finished | Active -> acc
                | Aborted ->
                    begin match acc with
                    | None | Some None -> Some None
                    | Some _  (* more informative *) as u -> u
                    end
                | Failed _ -> Some (Some uid)
              in
              begin match B0_unit.Idset.fold never_read units None with
              | None -> loop res_count never (g :: unr) gs
              | Some maybe_u ->
                  abort_unit b aim uid maybe_u;
                  loop (res_count + 1) (add_never_written o None never) unr gs
              end
          | false ->
              let never_read p acc = match B0_fpath.Map.find p never with
              | exception Not_found -> acc
              | maybe_u ->
                  match acc with
                  | None -> Some maybe_u
                  | Some None -> Some maybe_u (* may be more informative *)
                  | Some _ -> acc
              in
              match B0_fpath.Set.fold never_read awaits None with
              | None -> loop res_count never (g :: unr) gs
              | Some maybe_u ->
                  abort_unit b aim uid maybe_u;
                  loop (res_count + 1) (add_never_written o None never) unr gs
  in
  loop 0 never [] gs

let pp_op_cycle b o r co ppf () =
  let u = (get_unit_state b (B0_op.aim o) (B0_op.unit_id o)).unit_unit in
  B0_fmt.pf ppf "@[<v>cyclic with %a@,reads %a:@,%a@]"
    pp_name u B0_fpath.pp r B0_op.pp o

let pp_unit_cycle u cu ppf () =
  B0_fmt.pf ppf "@[<v>cycle awaiting on %a@]" pp_name cu

let fail_cycles b never gs = (* O (n^2) *)
  let rec loop count never unr = function
  | [] -> count, never, unr
  | g :: gs ->
      let op = B0_guard.op g in
      let state = get_unit_state b (B0_op.aim op) (B0_op.unit_id op) in
      match get_unit_status state with
      | Finished | Aborted -> assert false
      | Failed _ -> loop (count + 1) never unr gs
      | Active ->
          let rec find_cycle g0 = function
          | [] -> loop count never (g :: unr) gs
          | g1 :: cands ->
              let o0, o1 = B0_guard.op g0, B0_guard.op g1 in
              let u0, u1 = B0_op.unit_id o0, B0_op.unit_id o1 in
              let aim0, aim1 = B0_op.aim o0, B0_op.aim o1 in
              match B0_op.cycle o0 o1 with
              | Some (r0, r1) ->
                  fail_unit b aim0 u0 (pp_op_cycle b o0 r0 o1);
                  fail_unit b aim1 u1  (pp_op_cycle b o1 r1 o0);
                  let never = add_never_written o0 (Some u0) never in
                  let never = add_never_written o1 (Some u1) never in
                  loop (count + 1) never unr gs
              | None ->
                  let o0_needs = B0_guard.awaiting_units g0 in
                  let o1_needs = B0_guard.awaiting_units g1 in
                  match B0_unit.Idset.mem u1 o0_needs,
                        B0_unit.Idset.mem u1 o1_needs
                  with
                  | true, true ->
                      let unit_of_id a id = (get_unit_state b a id).unit_unit in
                      let uu0, uu1 = unit_of_id aim0 u0, unit_of_id aim1 u1 in
                      fail_unit b aim0 u0 (pp_unit_cycle uu0 uu1);
                      fail_unit b aim1 u1 (pp_unit_cycle uu1 uu0);
                      let never = add_never_written o0 (Some u0) never in
                      let never = add_never_written o1 (Some u1) never in
                      loop (count + 1) never unr gs
                  | _, _ ->
                      find_cycle g0 cands
          in
          find_cycle g gs
  in
  loop 0 never [] gs

let pp_never u files ppf () =
  B0_fmt.pf ppf "@[<v>Never got ready (may be due to other failures):@,%a@]"
    (B0_fpath.Set.pp ~sep:B0_fmt.cut B0_fpath.pp) files

let pp_never_units b aim uid us ppf () =
  let pp_name ppf id = pp_name ppf (get_unit_state b aim uid).unit_unit in
  B0_fmt.pf ppf "@[<v>Never got ready (may be due to other failures):@,%a@]"
    (B0_unit.Idset.pp ~sep:B0_fmt.sp pp_name) us

let never_became_ready b never gs =
  let rec loop never unr = function
  | [] -> never, unr
  | g :: gs ->
      let op = B0_guard.op g in
      let uid = B0_op.unit_id op in
      let aim = B0_op.aim op in
      match get_unit_status (get_unit_state b aim uid) with
      | Finished | Aborted -> assert false
      | Failed _ -> loop never unr gs
      | Active ->
          let files = B0_guard.awaiting_files g in
          match B0_fpath.Set.is_empty files with
          | false ->
              fail_unit b aim uid (pp_never uid files);
              loop never unr gs
          | true ->
              let units = B0_guard.awaiting_units g in
              fail_unit b aim uid (pp_never_units b aim uid units);
              loop never unr gs
  in
  loop never [] gs

let resolve_blocked b =
  match B0_guard.blocked b.b.guards with
  | [] ->
      B0_log.debug (fun m -> m "No guards blocked.")
  | gs ->
      B0_log.debug (fun m -> m "Resolving blocked guards");
      let rec resolve never = function
      | [] -> ()
      | gs ->
          let never, gs = resolve_known_failed_unit_writes b never gs in
          let never, gs = abort_awaiting_on_failures b never gs in
          let count, never, gs = fail_cycles b never gs in
          let never, gs = match count with
          | 0 -> never_became_ready b never gs
          | n -> never, gs
          in
          resolve never gs
      in
      ignore (resolve B0_fpath.Map.empty gs);
      ()

let finish b =
  collect_ops ~finish:true b;
  ignore ((B0_os.B0.rm_rf (trash b) |> B0_log.on_error_msg ~use:(fun _ -> 1)));
  resolve_blocked b;
  b.b.stats.cpu_dur <- B0_time.cpu_count b.b.cpu_counter;
  b.b.stats.total_dur <- B0_time.count b.b.dur_counter;
  b.b.finished <- true;
  log_finish b;
  Ok ()

(* Build outcome *)

let outcome b =
  let outcome_fpath_meta b =
    let encode meta =
      let encs, _errs (* FIXME *) = B0_fpath_meta.Meta.encode meta in
      encs
    in
    B0_fpath.Map.map encode b.b.fpath_meta
  in
  let outcome_conf b =
    let encs, _errs (* FIXME *) = B0_conf.encode b.b.conf_last in
    encs
  in
  if not b.b.finished then invalid_arg "build is not finished" else
  let o_units = (* FIXME we likely the ones from b.b.ustate here
                   aswell as the status. *)
    List.rev_map (fun (u, _) -> B0_unit.to_marshalable u) b.b.units
  in
  b.b.stats.file_stamp_dur <- B0_cache.file_stamp_dur b.b.cache;
  b.b.stats.file_stamp_count <-
    B0_fpath.Map.cardinal @@ B0_cache.file_stamps b.b.cache;
  { o_age = b.b.age; o_stats = b.b.stats;
    o_prev_stats = b.b.prev_outcome.o_stats;
    o_written_files = b.b.written_files;
    o_source_files = b.b.source_files;
    o_fpath_meta = outcome_fpath_meta b;
    o_conf = outcome_conf b; o_ops = b.b.ops; o_units }

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
