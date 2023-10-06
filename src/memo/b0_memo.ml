(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_zero

module Tool = struct
  type env_vars = string list
  let tmp_vars = ["TMPDIR"; "TEMP"; "TMP"]

  type response_file =
    { to_file : Cmd.t -> string;
      cli : Fpath.t -> Cmd.t; }

  let response_file_of to_file cli = { to_file; cli }
  let args0 =
    let to_file cmd = String.concat "\x00" (Cmd.to_list cmd) ^ "\x00" in
    let cli f = Cmd.(arg "-args0" %% path f) in
    { to_file; cli }

  type t =
    { name : Fpath.t;
      vars : env_vars;
      unstamped_vars : env_vars;
      response_file : response_file option; }

  let make ?response_file ?(unstamped_vars = tmp_vars) ?(vars = []) name =
    { name; vars; unstamped_vars; response_file }

  let by_name ?response_file ?unstamped_vars ?vars name =
    match Fpath.is_seg name with
    | false -> Fmt.invalid_arg "%S: tool is not a path segment" name
    | true -> make ?unstamped_vars ?vars (Fpath.v name)

  let name t = t.name
  let vars t = t.vars
  let unstamped_vars t = t.unstamped_vars
  let response_file t = t.response_file
  let read_env ~forced_env_vars t env =
    let add_var acc var = match String.Map.find_opt var env with
    | Some v -> String.Map.add var v acc
    | None -> acc
    in
    let stamped = List.fold_left add_var String.Map.empty t.vars in
    let stamped = List.fold_left add_var stamped forced_env_vars in
    let all = List.fold_left add_var stamped t.unstamped_vars in
    all, stamped
end

type feedback = [ `Op_complete of Op.t ]
type t = { c : ctx; m : memo }
and ctx = { mark : Op.mark }
and tool_lookup = t -> Cmd.tool -> (Fpath.t, string) result Fut.t
and memo =
  { clock : Os.Mtime.counter;
    cpu_clock : Os.Cpu.Time.counter;
    feedback : feedback -> unit;
    cwd : Fpath.t;
    win_exe : bool;
    tool_lookup : tool_lookup;
    env : Os.Env.t;
    forced_env_vars : Tool.env_vars;
    guard : Guard.t;
    reviver : Reviver.t;
    exec : Exec.t;
    mutable has_failures : bool;
    mutable op_id : int;
    mutable ops : Op.t list;
    mutable ready_roots : Fpath.Set.t; }

(* Properties *)

let clock m = m.m.clock
let cpu_clock m = m.m.cpu_clock
let env m = m.m.env
let exec m = m.m.exec
let forced_env_vars m = m.m.forced_env_vars
let guard m = m.m.guard
let has_failures m = m.m.has_failures
let ops m = m.m.ops
let reviver m = m.m.reviver
let tool_lookup m = m.m.tool_lookup
let trash m = Exec.trash m.m.exec
let win_exe m = m.m.win_exe

let with_feedback m feedback = { m with m = { m.m with feedback }}

(* Tool lookup *)

let file_ready m p =
  (* XXX Maybe we should really test for file existence here and notify
     a failure if it doesn't exist. But also maybe we should
     introduce a stat cache and propagate it everywhere in B0_zero.

     XXX maybe it would be better to have this as a build op but then
     we might not be that interested in repeated file_ready, e.g.
     done by multiple resolvers. Fundamentally ready_roots had
     to be added for correct "never became ready" error reports. *)
  Guard.set_file_ready m.m.guard p;
  m.m.ready_roots <- Fpath.Set.add p m.m.ready_roots

let cache_lookup lookup =
  let cache = Hashtbl.create 91 in
  fun m tool -> match Hashtbl.find cache tool with
  | p -> p
  | exception Not_found ->
      let p = lookup m tool in Hashtbl.add cache tool p; p

let tool_lookup_of_os_env ?sep ?(var = "PATH") env =
  let search_path = match String.Map.find var env with
  | exception Not_found -> "" | s -> s
  in
  match Fpath.list_of_search_path ?sep search_path with
  | Error _ as e -> fun _ _ -> Fut.return e
  | Ok search ->
      fun m t -> match Os.Cmd.get_tool ~win_exe:m.m.win_exe ~search t with
      | Error _ as e -> Fut.return e
      | Ok file as tool -> file_ready m file; Fut.return tool

let make_zero
    ?clock ?cpu_clock:cc ~feedback ~cwd ?(win_exe = Sys.win32)
    ?tool_lookup ?env ?(forced_env_vars = []) guard reviver exec
  =
  let* env = match env with None -> Os.Env.current () | Some env -> Ok env in
  let clock = match clock with None -> Os.Mtime.counter () | Some c -> c in
  let cpu_clock = match cc with
  | None -> Os.Cpu.Time.counter () | Some c -> c
  in
  let tool_lookup = cache_lookup @@ match tool_lookup with
  | None -> tool_lookup_of_os_env env
  | Some l -> l
  in
  let op_id = 0 and ops = [] in
  let m =
    { clock; cpu_clock; feedback; cwd; win_exe; tool_lookup; env;
      forced_env_vars; guard; reviver; exec; has_failures = false; op_id; ops;
      ready_roots = Fpath.Set.empty }
  in
  Ok { c = { mark = "" }; m }

let make
    ?(hash_fun = (module Hash.Xxh3_64 : Hash.T)) ?win_exe ?tool_lookup
    ?env ?forced_env_vars ?cwd
    ?(jobs = Os.Cpu.logical_count ()) ?feedback ~cache_dir ~trash_dir ()
  =
  let feedback = match feedback with | Some f -> f | None -> fun _ -> () in
  let fb_exec = (feedback :> Exec.feedback -> unit) in
  let fb_memo = (feedback :> feedback -> unit) in
  let clock = Os.Mtime.counter () in
  let* cwd = match cwd with None -> Os.Dir.cwd () | Some cwd -> Ok cwd in
  let* cache = File_cache.make cache_dir in
  let guard = Guard.make () in
  let reviver = Reviver.make clock hash_fun cache in
  let trash = Trash.make trash_dir in
  let exec = Exec.make ~clock ~feedback:fb_exec ~trash ~jobs () in
  make_zero ~clock ~feedback:fb_memo ~cwd ?win_exe ?tool_lookup ?env
    ?forced_env_vars guard reviver exec

(* Activity marks *)

let with_mark m mark = { c = { mark }; m = m.m }
let mark m = m.c.mark

(* Low-level operations *)

let add_op m o = m.m.ops <- o :: m.m.ops; Guard.add m.m.guard o
let delete_trash ~block m = Trash.delete ~block (Exec.trash m.m.exec)
let hash_file m f = Reviver.hash_file m.m.reviver f
let hash_string m s = Reviver.hash_string m.m.reviver s
let new_op_id m = let id = m.m.op_id in m.m.op_id <- id + 1; id
let status m =
  B0_zero.Op.find_aggregate_error ~ready_roots:m.m.ready_roots (ops m)

let timestamp m = Os.Mtime.count m.m.clock

(* Feedback *)

let notify_op m ?k kind msg =
  let k = match k with None -> None | Some k -> Some (fun o -> k ()) in
  let id = new_op_id m and created = timestamp m in
  let o = Op.Notify.make_op ~id ~mark:m.c.mark ~created ?k kind msg in
  add_op m o

let notify_reviver_error m kind o e =
  notify_op m kind (Fmt.str "@[cache error: op %d: %s@]" (Op.id o) e)

(* Procedures *)

exception Fail

let fail m fmt =
  let k msg = notify_op m `Fail msg; raise Fail in
  Fmt.kstr k fmt

let fail_if_error m = function Ok v -> v | Error e -> fail m "%s" e

module Env = struct
  let find ~empty_is_none var m = match String.Map.find_opt var m.m.env with
  | None -> None
  | Some "" when empty_is_none -> None
  | Some _ as v -> v

  let find' ~empty_is_none parse var m = match find ~empty_is_none var m with
  | None -> None
  | Some v ->
      match parse v with
      | Ok v -> Some v
      | Error e -> fail m "parsing %a: %s" Fmt.(code string) var e

  let mem var m = String.Map.mem var m.m.env
end

let invoke_k m ~pp_kind k v = try k v with
| Stack_overflow as e -> raise e
| Out_of_memory as e -> raise e
| Sys.Break as e -> raise e
| Fail -> ()
| e ->
    let bt = Printexc.get_raw_backtrace () in
    let err =
      Fmt.str "@[<v>%a raised unexpectedly:@,%a@]"
        pp_kind v Fmt.exn_backtrace (e, bt)
    in
    notify_op m `Fail err

let run_proc m k =
  let k = fun () -> ignore (k ()) in
  let pp_kind = Fmt.any "Procedure" in
  invoke_k m ~pp_kind k ()

let continue_op m o =
  let pp_kind ppf o = Fmt.pf ppf "Continuation of operation %d" (Op.id o) in
  List.iter (Guard.set_file_ready m.m.guard) (Op.writes o);
  m.m.feedback (`Op_complete o);
  invoke_k m ~pp_kind Op.invoke_k o

let discontinue_op m o =
  (* This is may not be entirely clear from the code :-( but any
     failed op means this function eventually gets called, hereby
     giving [has_failure] its appropriate semantics. *)
  m.m.has_failures <- true;
  List.iter (Guard.set_file_never m.m.guard) (Op.writes o);
  Op.discard_k o; m.m.feedback (`Op_complete o)

let finish_op m o = match Op.status o with
| Op.Done ->
    if Op.revived o then continue_op m o else
    begin match Hash.equal (Op.hash o) Hash.nil with
    | true ->
        begin match Op.did_not_write o with
        | [] -> continue_op m o
        | miss ->
            Op.set_status o (Op.Failed (Op.Missing_writes miss));
            discontinue_op m o
        end
    | false ->
        match Reviver.record m.m.reviver o with
        | Ok true -> continue_op m o
        | Ok false ->
            let miss = Op.did_not_write o in
            Op.set_status o (Op.Failed (Op.Missing_writes miss));
            discontinue_op m o
        | Error e ->
            begin match Op.did_not_write o with
            | [] -> notify_reviver_error m `Warn o e; continue_op m o
            | miss ->
                  Op.set_status o (Op.Failed (Op.Missing_writes miss));
                  discontinue_op m o
            end
    end
| Op.Aborted | Op.Failed _ -> discontinue_op m o
| Op.Waiting -> assert false

let submit_op m o = match Op.status o with
| Op.Aborted -> finish_op m o
| Op.Waiting ->
    begin match Reviver.hash_op m.m.reviver o with
    | Error e ->
        (* XXX Add Op.Failed_hash failure instead of abusing Op.Exec ? *)
        let status = match Op.cannot_read o with
        | [] -> Op.Failed (Op.Exec (Some (Fmt.str "op hash error: %s" e)))
        | reads -> Op.Failed (Op.Missing_reads reads)
        in
        Op.set_status o status;
        discontinue_op m o
    | Ok hash ->
        Op.set_hash o hash;
        begin match Reviver.revive m.m.reviver o with
        | Ok false -> Exec.schedule m.m.exec o
        | Ok true -> finish_op m o
        | Error e ->
            (* It's not really interesting to warn here. Also entails
               that cache format changes trips out people a bit less. *)
            notify_reviver_error m `Info o e; Exec.schedule m.m.exec o
        end
    end
| Op.Done | Op.Failed _ -> assert false

(* XXX we may blow stack continuations can add which stirs.
   XXX futures make it even worse. *)

let rec stir ~block m = match Guard.allowed m.m.guard with
| Some o -> submit_op m o; stir ~block m
| None ->
    match Exec.collect m.m.exec ~block with
    | Some o -> finish_op m o; stir ~block m
    | None -> ()

let add_op_and_stir m o = add_op m o; stir ~block:false m

(* Notifications *)

type notify_kind = B0_zero.Op.Notify.kind
let notify ?k m kind fmt = Fmt.kstr (notify_op m ?k kind) fmt
let notify_if_error m kind ~use = function
| Ok v -> v | Error e -> notify_op m kind e; use

(* Files *)

let read m file =
  let id = new_op_id m and created = timestamp m in
  let r, set = Fut.make () in
  let k o =
    let r = Op.Read.get o in
    let data = Op.Read.data r in
    Op.Read.discard_data r; set data
  in
  let o = Op.Read.make_op ~id ~mark:m.c.mark ~created ~k file in
  add_op_and_stir m o; r

let wait_files m files =
  let id = new_op_id m and created = timestamp m in
  let r, set = Fut.make () in
  let k o = set () in
  let o = Op.Wait_files.make_op ~id ~mark:m.c.mark ~created ~k files in
  add_op_and_stir m o; r

let write m ?(stamp = "") ?(reads = []) ?(mode = 0o644) write d =
  let id = new_op_id m and mark = m.c.mark and created = timestamp m in
  let o = Op.Write.make_op ~id ~mark ~created ~stamp ~reads ~mode ~write d in
  add_op_and_stir m o

let copy m ?(mode = 0o644) ?linenum ~src dst =
  let id = new_op_id m and mark = m.c.mark and created = timestamp m in
  let o = Op.Copy.make_op ~id ~mark ~created ~mode ~linenum ~src dst in
  add_op_and_stir m o

let mkdir m ?(mode = 0o755) dir =
  let id = new_op_id m and mark = m.c.mark and created = timestamp m in
  let r, set = Fut.make () in
  let k o = set () in
  let o = Op.Mkdir.make_op ~id ~mark ~created ~k ~mode dir in
  add_op_and_stir m o; r

let delete m p =
  let id = new_op_id m and mark = m.c.mark and created = timestamp m in
  let r, set = Fut.make () in
  let k o = set () in
  let o = Op.Delete.make_op ~id ~mark ~created ~k p in
  add_op_and_stir m o;
  r

(* FIXME That whole lookup approach stills seems quite involved.
     Try to simplify. *)

type _tool =
  { tool : Tool.t;
    tool_file : Fpath.t;
    tool_env : Os.Env.assignments;
    tool_stamped_env : Os.Env.assignments; }

type tool =
| Miss of Tool.t * string
| Tool of _tool

type cmd = { cmd_tool : tool Fut.t; cmd_args : Cmd.t }

let tool_env m t =
  let forced_env_vars = m.m.forced_env_vars in
  Tool.read_env ~forced_env_vars t m.m.env

let spawn_env m cmd_tool = function
| None -> cmd_tool.tool_env, cmd_tool.tool_stamped_env
| Some spawn_env ->
    let forced_env_vars = m.m.forced_env_vars in
    let env = Os.Env.override m.m.env ~by:spawn_env in
    let tool_env, stamped = Tool.read_env ~forced_env_vars cmd_tool.tool env in
    Os.Env.to_assignments tool_env, Os.Env.to_assignments stamped

let tool m tool =
  let cmd_tool =
    let name, is_path =
      let name = Fpath.to_string (Tool.name tool) in
      let name =
        let suffix = ".exe" in
        if not m.m.win_exe || String.ends_with ~suffix name then name else
        name ^ suffix
      in
      Fpath.v name, String.contains name Fpath.dir_sep_char
    in
    let tool_file = match is_path with
    | true -> Fut.return (Ok name)
    | false -> m.m.tool_lookup m name
    in
    Fut.bind tool_file @@ function
    | Error e -> Fut.return (Miss (tool, e))
    | Ok tool_file ->
        let tool_env, tool_stamped_env = tool_env m tool in
        let tool_env = Os.Env.to_assignments tool_env in
        let tool_stamped_env = Os.Env.to_assignments tool_stamped_env in
        Fut.return @@
        Tool { tool; tool_file; tool_env; tool_stamped_env }
  in
  fun cmd_args -> { cmd_tool; cmd_args }

let tool_opt m t = (* FIXME this does not do the same business as tool *)
  Fut.bind (m.m.tool_lookup m (Tool.name t)) @@ function
  | Error e (* FIXME distinguish no lookup from errors *) ->
      Fut.return None
  | Ok _ -> Fut.return (Some (tool m t))

let _spawn
    m ?(stamp = "") ?(reads = []) ?(writes = []) ?writes_manifest_root ?env
    ?cwd ?stdin ?(stdout = `Ui) ?(stderr = `Ui) ?(success_exits = [0])
    ?post_exec ?k cmd
  =
  Fut.await cmd.cmd_tool @@ function
  | Miss (tool, e) -> fail m "%s" e
  | Tool tool ->
      let id = new_op_id m and created = timestamp m in
      let reads =
        (* XXX should we do this at a lower level ? *)
        tool.tool_file :: reads
      in
      let env, stamped_env = spawn_env m tool env in
      let cwd = match cwd with None -> m.m.cwd | Some d -> d in
      let k = match k with
      | None -> fun o -> ()
      | Some k ->
          fun o -> match Op.Spawn.exit (Op.Spawn.get o) with
          | Some (`Exited code) -> k code
          | _ -> assert false
      in
      let o =
        Op.Spawn.make_op
          ~id ~mark:m.c.mark ~created ~reads ~writes ?writes_manifest_root
          ?post_exec ~k ~stamp ~env ~stamped_env ~cwd ~stdin ~stdout ~stderr
          ~success_exits tool.tool_file cmd.cmd_args
      in
      add_op_and_stir m o

let spawn
    m ?stamp ?reads ?writes ?env ?cwd ?stdin ?stdout ?stderr ?success_exits
    ?post_exec ?k cmd
  =
  _spawn m ?stamp ?reads ?writes ?env ?cwd ?stdin ?stdout ?stderr
    ?success_exits ?post_exec ?k cmd

let spawn'
    m ?stamp ?reads ~writes_root ?writes ?env ?cwd ?stdin ?stdout ?stderr
    ?success_exits ?k cmd
  =
  let writes = match writes with
  | Some writes -> writes
  | None ->
      fun o ->
        let dotfiles = true and recurse = true in
        fail_if_error m
          Os.Dir.(fold_files ~dotfiles ~recurse path_list writes_root [])
  in
  let post_exec o =
    if B0_zero.Op.revived o || B0_zero.Op.status o <> B0_zero.Op.Done
    then ()
    else Op.set_writes o (writes o)
  in
  _spawn m ?stamp ?reads ~writes_manifest_root:writes_root ?env ?cwd ?stdin
    ?stdout ?stderr ?success_exits ~post_exec ?k cmd
