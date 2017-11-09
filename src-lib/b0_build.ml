(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* TODO the quick refactoring from using Unit.t directly to using Unit.id
   must have introduced quite some mess. A review is needed at that
   ocasion use [uid] intead of [u]. *)

let strf = Format.asprintf

type file_kind = Root | Built
type file_info = { f_kind : file_kind; }

(* Build control *)

type ctrl = { c_tty_cap : B0_tty.cap; c_max_spawn : int }
let ctrl ?tty_cap:(c_tty_cap = B0_tty.None) ?max_spawn:(c_max_spawn = 4) () =
  { c_tty_cap; c_max_spawn }

(* Build *)

type build =
  { clock : B0_time.counter; mutable total_dur : B0_time.span;
    clock_cpu : B0_time.cpu_counter; mutable cpu_dur : B0_time.cpu;
    dir : B0_fpath.t; (* Build directory *)
    trash : B0_fpath.t ; (* Trash directory *)
    ctrl : ctrl;
    cache : B0_cache.t;
    env : B0_env.t;
    universe : build_unit list;
    units : build_unit list;
    mutable conf : B0_conf.t;
    mutable conf_last : B0_conf.t;
    mutable finished : bool;
    mutable fpath_meta : B0_meta.Fpath.Map.t;
    mutable file_info : file_info B0_fpath.Map.t;
    mutable ustate_host : B0_unit.build_state B0_unit.Idmap.t;
    mutable ustate_build : B0_unit.build_state B0_unit.Idmap.t;
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

let env b = b.b.env
let dir b = b.b.dir
let trash b = b.b.trash

let cpu_dur b = match b.b.finished with
| true -> b.b.cpu_dur
| false -> B0_time.cpu_count b.b.clock_cpu

let total_dur b = match b.b.finished with
| true -> b.b.total_dur
| false -> B0_time.count b.b.clock

let finished b = b.b.finished
let fpath_meta b = b.b.fpath_meta
let file_info b = b.b.file_info
let ops b = b.b.ops

let _unit_build_dir b aim u =
  let suff = match aim with `Build_os -> "@b" | `Host_os -> "" in
  B0_fpath.(b.b.dir / (B0_unit.name u ^ suff))

let unit_build_dir b u = _unit_build_dir b b.aim u
let build_dir b = b.build_dir
let build_file b f = B0_fpath.(b.build_dir / f)
let stored_conf b = b.b.conf
let conf_used b = b.b.conf_last (* FIXME that name is dodgy. *)
let cache b = b.b.cache
let units b = List.map fst b.b.units
let src_dir b = B0_unit.src_root b.unit
let unit b = b.unit
let timestamp b = B0_time.count b.b.clock

let create cache ctrl env conf fpath_meta ~dir ~universe units =
  let clock = B0_time.counter () in
  let clock_cpu = B0_time.cpu_counter () in
  let trash = B0_fpath.(dir / "trash") in
  let tmp = B0_fpath.(dir / "tmp") in
  B0_os.Dir.create ~path:true dir >>= fun _ ->
  B0_os.Dir.create ~path:false trash >>= fun _ ->
  B0_os.Dir.create ~path:false tmp >>= fun _ ->
  let tmp_path = B0_fpath.(tmp / "tmp") in
  let execs =
    B0_exec.handler ~max_spawn:ctrl.c_max_spawn () ~clock ~tmp_path
  in
  let b =
    { clock;
      total_dur = B0_time.zero;
      clock_cpu;
      cpu_dur = B0_time.cpu_zero;
      dir; trash; ctrl; cache; conf; env;
      universe; units;
      conf_last = B0_conf.empty; finished = false;
      fpath_meta;
      file_info = B0_fpath.Map.empty;
      ustate_host = B0_unit.Idmap.empty;
      ustate_build = B0_unit.Idmap.empty;
      konts = B0_op.Map.empty; guards = B0_guard.handler (); execs; ops = [] }
  in
  Ok { unit = B0_unit.nil; build_dir = B0_os.File.null; aim = `Host_os; b }

(* Logging, FIXME make that a data type and add a logger function. Better
   for IDE interaction. *)

let pp_name ppf u = B0_fmt.tty_str [] ppf (B0_unit.name u)
let pp_name_red ppf u = B0_fmt.tty_str [`Fg `Red] ppf (B0_unit.name u)
let pp_aim ppf = function
| `Host_os -> ()
| `Build_os -> B0_fmt.pf ppf "[AIM:BUILD]"

let log_unit_start aim u =
  let build_style = [`Fg `Yellow] in
  B0_log.app (fun m -> m "[%a]%a %a" (B0_fmt.tty_str build_style) "START"
                 pp_aim aim pp_name u)

let log_unit_finish aim u =
  let build_style = [`Fg `Green] in
  let pp_outcome ppf u = match B0_unit.doc_outcome u with
  | "" -> ()
  | o -> B0_fmt.pf ppf "%s " o
  in
  B0_log.app (fun m -> m "[%a]%a %a%a" (B0_fmt.tty_str build_style) "BUILT"
                 pp_aim aim pp_outcome u pp_name u)

let log_unit_fail u msg =
  B0_log.app (fun m -> m "@[<v>[%a] %a:@,  @[%a@]@,@]"
                 (B0_fmt.tty_str [`Fg `Red]) "FAIL " pp_name_red u msg ())

let log_unit_abort u u_cause =
  let msg ppf u_cause = match u_cause with
  | None -> ()
  | Some u -> B0_fmt.pf ppf ": %a failed" pp_name u
  in
  B0_log.app (fun m -> m "[%a] %a%a"
                 (B0_fmt.tty_str [`Fg `Red]) "ABORT" pp_name u msg u_cause)

let log_finish b =
  let add_success _ ustate (u, tot) =
    match B0_unit.build_state_status ustate with
    | B0_unit.Finished -> (u + 1, tot + 1)
    | _ -> (u, tot + 1)
  in
  let acc = (0, 0) in
  let acc = B0_unit.Idmap.fold add_success b.b.ustate_host acc in
  let succ, tot = B0_unit.Idmap.fold add_success b.b.ustate_build acc in
  let style = if succ <> tot then [`Fg `Red] else [`Fg `Green] in
  let count = strf "%d/%d" succ tot in
  let d = b.b.total_dur in
  B0_log.app (fun m -> m "[%a] units in %a"
                 (B0_fmt.tty_str style) count B0_time.pp_span d);
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

let find_path_meta b p k = B0_meta.Fpath.Map.find p k b.b.fpath_meta
let add_path_meta ?(force = false) b p k v =
  match B0_fpath.Map.mem p b.b.fpath_meta && not force with
  | true ->
      fail (fun m -> m "%a: file metadata redefinition for key %a"
               B0_fpath.pp p B0_meta.Fpath.Key.pp_name
               (B0_meta.Fpath.Key.of_typed k))
  | false ->
    (* Could check and warn if the path not either a source or in the current
       bdir *)
      b.b.fpath_meta <- B0_meta.Fpath.Map.add p k v b.b.fpath_meta

(* Unit state handling *)

let create_unit_state b aim u =
  let s = B0_unit.build_state_create u in
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

let get_op_unit_state b o =
  let uid = B0_op.unit_id o in
  let aim = B0_op.aim o in
  get_unit_state b aim uid

let fail_unit b aim uid msg =
  let state = get_unit_state b aim uid in
  B0_unit.build_state_set_status state (B0_unit.Failed msg);
  log_unit_fail (B0_unit.build_state_unit state) msg

let abort_unit b aim uid u_cause =
  let u_cause = match u_cause with
  | None -> None
  | Some uid -> Some (B0_unit.build_state_unit (get_unit_state b aim uid))
  in
  let state = get_unit_state b aim uid in
  B0_unit.build_state_set_status state B0_unit.Aborted;
  log_unit_abort (B0_unit.build_state_unit state) u_cause

let fail_unit_on_exn b aim uid f x = try f x with
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
  match B0_unit.build_state_activity ustate = 0 with
  | false -> ()
  | true ->
      if B0_unit.build_state_status ustate = B0_unit.Active
      then B0_unit.build_state_set_status ustate B0_unit.Finished;
      B0_guard.set_unit_ready b.b.guards uid;
      log_unit_finish aim (B0_unit.build_state_unit ustate)

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
  B0_unit.build_state_incr_activity ustate;
  log_unit_start aim u;
  fail_unit_on_exn b aim (B0_unit.id u) (start build_dir) b;
  B0_unit.build_state_decr_activity ustate; (* func returned *)
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

let add_op_kont b o k =
  B0_unit.build_state_incr_activity (get_op_unit_state b o);
  b.b.konts <- B0_op.Map.add o k b.b.konts

let op_kont ~discard b o = match discard with
| true ->
    B0_unit.build_state_decr_activity (get_op_unit_state b o);
    b.b.konts <- B0_op.Map.remove o b.b.konts
| false ->
    match B0_op.Map.find o b.b.konts with
    | exception Not_found -> ()
    | k ->
        b.b.konts <- B0_op.Map.remove o b.b.konts;
        k o;
        B0_unit.build_state_decr_activity (get_op_unit_state b o)

(* Handling operations *)

let add_file_write b w_cmd_write_stamp file w_file_stamp =
  let file_info = { f_kind = Built } in
  b.b.file_info <- B0_fpath.Map.add file file_info b.b.file_info

let register_write b file =
  let cmd_write_stamp = B0_hash.zero in
  let file_stamp = B0_cache.file_stamp b.b.cache file in
  add_file_write b cmd_write_stamp file file_stamp;
  B0_guard.set_file_ready b.b.guards file

let register_op_writes b o =
  try B0_fpath.Set.iter (register_write b) (B0_op.writes o) with
  | Sys_error err ->
      (* FIXME THIS actually never happens *)
      B0_log.app (fun m -> m "FIXME Not written %s:@\n %a" err B0_op.pp o)

let finish_cached_op ~failed_unit b o = (* FIXME make that more precise *)
  register_op_writes b o;
  op_kont ~discard:failed_unit b o

let log_spawn_ui b o =
  let build_style = [`Fg `Blue] in
  let pp_cmd_tool ppf cmd =
    B0_fmt.tty_str [`Bold] ppf (B0_cmd.get_line_tool cmd)
  in
  let unit = B0_unit.build_state_unit (get_op_unit_state b o) in
  let aim = B0_op.aim o in
  let s = match B0_op.kind o with B0_op.Spawn s -> s | _ -> assert false in
  match B0_op.spawn_stdo_ui s with
  | `None -> ()
  | `Stdo _ as ui  ->
      B0_log.app (fun m -> m "@[<v1>@[[%a]%a %a [spawn:%d] %a:@]@,%a@]"
                     (B0_fmt.tty_str build_style) "NOTE "
                     pp_aim aim pp_name unit
                     (B0_op.id o)
                     pp_cmd_tool (B0_op.spawn_cmd s)
                     B0_op.pp_spawn_stdo_ui ui)
  | `Tmp_file _ -> assert false

let finish_op_kind ~failed_unit b o =
  let finish_spawn b op s = match B0_op.spawn_result s with
  | Ok (_, `Exited c) ->
      let success = match B0_op.spawn_allowed_exits s with
      | None when c = 0 -> true
      | Some [] -> true
      | Some cs when List.mem c cs -> true
      | _ -> false
      in
      begin match success with
      | false ->
          (* FIXME unit aim ? *)
          fail (fun m -> m "%a" B0_op.pp_spawn_fail op)
      | true ->
          if B0_op.spawn_has_stdo_ui s then log_spawn_ui b o;
          register_op_writes b o
      end
  | Ok (_, `Signaled c) -> fail (fun m -> m "%a" B0_op.pp_spawn_fail op)
  | Error (`Msg e) -> fail (fun m -> m "%s" e) (* TODO register non-writes ? *)
  in
  let finish_copy_file b o c = match B0_op.copy_file_result c with
  | Error (`Msg e) -> fail (fun m -> m "%s" e)
  | Ok () -> register_write b (B0_op.copy_file_dst c)
  in
  match B0_op.cached o with
  | true -> finish_cached_op ~failed_unit b o
  | false ->
      try
        begin match B0_op.kind o with
        | B0_op.Spawn s -> finish_spawn b o s
        | B0_op.Read r -> ignore (fail_on_error_msg @@ B0_op.read_result r)
        | B0_op.Copy_file c -> finish_copy_file b o c
        | B0_op.Mkdir m -> fail_on_error_msg @@ B0_op.mkdir_result m
        | B0_op.Sync s -> ()
        | B0_op.Write _ | B0_op.Delete _ -> (* not exposed yet *) assert false
        end;
        op_kont ~discard:false b o;
        B0_cache.add_op b.b.cache o
      with
      | Fail _ as e ->
          op_kont ~discard:true b o;
          if not failed_unit then raise e

let finish_op b o =
  let aim = B0_op.aim o in
  let uid = B0_op.unit_id o in
  let ustate = get_unit_state b aim uid in
  B0_unit.build_state_decr_activity ustate;
  fail_unit_on_exn b aim uid begin fun () ->
    match B0_unit.build_state_status ustate with
    | B0_unit.Active ->
        finish_op_kind ~failed_unit:false b o; try_finish_unit b aim uid
    | B0_unit.Failed _ ->
        finish_op_kind ~failed_unit:true b o (* Still keep the work done *)
    | B0_unit.Finished | B0_unit.Aborted -> assert false
  end ()

let submit_op b o =
  match B0_unit.build_state_status (get_op_unit_state b o)
  with
  | B0_unit.Active ->
      begin match B0_cache.exec b.b.cache o with
      | true -> finish_op b o
      | false -> B0_exec.submit b.b.execs o
      end
  | B0_unit.Failed _ -> B0_op.(set_status o Aborted)
  | B0_unit.Aborted | B0_unit.Finished -> assert false

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

(* Build operations

   Status transitions. At creation time the operation is unconditionally
   [Guarded] and added to the guard handled. If:

   1. It becomes [Ready] (i.e. poped from the guard handler). If its
      unit:

      a) Has failed. The operation immediately becomes [Aborted].
      b) Hasn't failed. The cache is consulted, it the operation is
         cached then it becomes [Cached]. Otherwise it is added
         to the execution handler, in which case it eventually becomes
         [Executed] once it gets out of the execution handler. Note that
         this last state doesn't mean that the operation succeded.

   2. It never becomes [Ready], failure analysis eventually
      sets it to [Aborted]. *)

type 'a fiber = ('a -> unit) -> unit

let add_op b o =
  b.b.ops <- o :: b.b.ops;
  B0_unit.build_state_incr_activity (get_op_unit_state b o);
  B0_guard.add b.b.guards o;
  submit_ops b;
  collect_ops ~finish:false b;
  ()

let ready b p =
  B0_guard.set_file_ready b.b.guards p;
  let file_info = { f_kind = Root } in
  b.b.file_info <- B0_fpath.Map.add p file_info b.b.file_info

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

let spawn_env b run spawn_env = match B0_string.Map.is_empty spawn_env with
| true -> run.env
| false ->
    let env = B0_env.env b.b.env b.aim in
    let forced_env = B0_env.forced_env b.b.env b.aim in
    let tool_env, _ = B0_tool.lookup_env run.tool env in
    let env =
      B0_os.Env.(override (override tool_env ~by:spawn_env) ~by:forced_env)
    in
    Array.of_list @@ B0_os.Env.to_assignments env

let spawn b
    ?(reads = []) ?(writes = []) ?exits ?(env = B0_string.Map.empty) ?cwd
    ?stdin ?(stdout = `Ui) ?(stderr = `Ui) run
  =
  let time = timestamp b in
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
    B0_op.spawn
      u b.aim time ~reads ~writes ~exits ~stdin ~stdout ~stderr ~cwd env cmd
  in
  add_op b o

let copy_file ?linenum b src dst =
  let time = timestamp b in
  let o = B0_op.copy_file ?linenum b.unit b.aim time src dst in
  add_op b o

let read b f k =
  let time = timestamp b in
  let o = B0_op.read b.unit b.aim time f in
  let k o = k (B0_op.get_read_data @@ B0_op.get_read o) in
  add_op_kont b o k;
  add_op b o

let write b ?reads f gen =
  (* We need a post guard cb to call gen. *) failwith "TODO"

let mkdir b d k =
  let time = timestamp b in
  let o = B0_op.mkdir b.unit b.aim time d in
  let k o = k () in
  add_op_kont b o k;
  add_op b o

let await_units b us k =
  let time = timestamp b in
  let add_id acc u = B0_unit.Idset.add (B0_unit.id u) acc in
  let ids = List.fold_left add_id B0_unit.Idset.empty us in
  let o = B0_op.sync b.unit b.aim time ids in
  let k o = k () in
  add_op_kont b o k;
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
      match B0_unit.build_state_status (get_unit_state b aim uid) with
      | B0_unit.Finished | B0_unit.Aborted -> assert false
      | B0_unit.Active -> loop (g :: unr) never gs
      | B0_unit.Failed _ -> loop unr (add_never_written o (Some uid) never) gs
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
      match B0_unit.build_state_status (get_unit_state b aim uid) with
      | B0_unit.Finished | B0_unit.Failed _ -> assert false
      | B0_unit.Aborted ->
          loop (res_count + 1) (add_never_written o None never) unr gs
      | B0_unit.Active ->
          let awaits = B0_guard.awaiting_files b.b.guards g in
          match B0_fpath.Set.is_empty awaits with
          | true ->
              let units = B0_guard.awaiting_units b.b.guards g in
              let never_read uid acc =
                match B0_unit.build_state_status (get_unit_state b aim uid) with
                | B0_unit.Finished | B0_unit.Active -> acc
                | B0_unit.Aborted ->
                    begin match acc with
                    | None | Some None -> Some None
                    | Some _  (* more informative *) as u -> u
                    end
                | B0_unit.Failed _ -> Some (Some uid)
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
  let u = B0_unit.build_state_unit
      (get_unit_state b (B0_op.aim o) (B0_op.unit_id o))
  in
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
      match B0_unit.build_state_status state with
      | B0_unit.Finished | B0_unit.Aborted -> assert false
      | B0_unit.Failed _ -> loop (count + 1) never unr gs
      | B0_unit.Active ->
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
                  let o0_needs = B0_guard.awaiting_units b.b.guards g0 in
                  let o1_needs = B0_guard.awaiting_units b.b.guards g1 in
                  match B0_unit.Idset.mem u1 o0_needs,
                        B0_unit.Idset.mem u1 o1_needs
                  with
                  | true, true ->
                      let unit_of_id a id =
                        B0_unit.build_state_unit (get_unit_state b a id)
                      in
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
  B0_fmt.pf ppf
    "@[<v>File(s) never got ready (may be due to other failures):@,%a@]"
    (B0_fpath.Set.pp ~sep:B0_fmt.cut B0_fpath.pp) files

let pp_never_units b aim uid us ppf () =
  let pp_name ppf id =
    let u = B0_unit.build_state_unit (get_unit_state b aim uid) in
    pp_name ppf u
  in
  B0_fmt.pf ppf
    "@[<v>Unit(s) never got ready (may be due to other failures):@,%a@]"
    (B0_unit.Idset.pp ~sep:B0_fmt.sp pp_name) us

let never_became_ready b never gs =
  let rec loop never unr = function
  | [] -> never, unr
  | g :: gs ->
      let op = B0_guard.op g in
      let uid = B0_op.unit_id op in
      let aim = B0_op.aim op in
      match B0_unit.build_state_status (get_unit_state b aim uid) with
      | B0_unit.Finished | B0_unit.Aborted -> assert false
      | B0_unit.Failed _ -> loop never unr gs
      | B0_unit.Active ->
          let files = B0_guard.awaiting_files b.b.guards g in
          match B0_fpath.Set.is_empty files with
          | false ->
              fail_unit b aim uid (pp_never uid files);
              loop never unr gs
          | true ->
              let units = B0_guard.awaiting_units b.b.guards g in
              fail_unit b aim uid (pp_never_units b aim uid units);
              loop never unr gs
  in
  loop never [] gs

let resolve_blocked b = match B0_guard.guards b.b.guards with
| [] ->
    B0_log.debug (fun m -> m "No guards blocked.")
| gs ->
    let abort g = B0_op.(set_status (B0_guard.op g) Aborted) in
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
    List.iter abort gs;
    ignore (resolve B0_fpath.Map.empty gs);
    ()

let finish b =
  collect_ops ~finish:true b;
  ignore ((B0_os.B0.rm_rf (trash b) |> B0_log.on_error_msg ~use:(fun _ -> 1)));
  resolve_blocked b;
  b.b.cpu_dur <- B0_time.cpu_count b.b.clock_cpu;
  b.b.total_dur <- B0_time.count b.b.clock;
  b.b.finished <- true;
  log_finish b;
  Ok ()

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
