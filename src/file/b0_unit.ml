(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "unit"

open B0_std

(* A bit of annoying recursive definition. *)

module rec Build_def : sig
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B0_memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_dir : Fpath.t;
      store : B0_store.t;
      must_build : Unit.Set.t;
      may_build : Unit.Set.t;
      mutable requested : Unit.t String.Map.t;
      mutable waiting : Unit.t Random_queue.t; }
end = struct
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B0_memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_dir : Fpath.t;
      store : B0_store.t;
      must_build : Unit.Set.t;
      may_build : Unit.Set.t;
      mutable requested : Unit.t String.Map.t;
      mutable waiting : Unit.t Random_queue.t; }
end

and Unit_def : sig
  type build_proc = Build_def.t -> unit Fut.t
  and t = { def : B0_def.t; build_proc : build_proc; }
  include B0_def.VALUE with type t := t
end = struct
  type build_proc = Build_def.t -> unit Fut.t
  and t = { def : B0_def.t; build_proc : build_proc; }
  let def_kind = "unit"
  let def u = u.def
  let pp_name_str = Fmt.code
end

and Unit : sig include B0_def.S with type t = Unit_def.t end
  = B0_def.Make (Unit_def)

(* Build procedures *)

type b0_build = Build_def.t
type build_proc = Unit_def.build_proc
let build_nop b = Fut.return ()

(* Build units *)

include Unit

(* We also maintain a tool name index for built tools. *)

let tool_name =
  let doc = "Executable tool name without platform specific extension" in
  B0_meta.Key.make "tool-name" ~doc ~pp_value:Fmt.string

let tool_name_index : t list String.Map.t ref = ref String.Map.empty

let add_tool_name u = match find_meta tool_name u with
| None -> ()
| Some n -> tool_name_index := String.Map.add_to_list n u !tool_name_index

let make ?doc ?meta n build_proc =
  let def = define ?doc ?meta n in
  let u = { Unit_def.def; build_proc } in add u; add_tool_name u; u

let build_proc u = u.Unit_def.build_proc

let pp_synopsis ppf v =
  let pp_tag ppf u =
    let base = [`Bg `White; `Fg `Black; `Bold] in
    let tag, style =
      (if has_tag B0_meta.exe u then
         (if has_tag B0_meta.test u && has_tag B0_meta.run u
          then " T ", [`Bg `Green; `Fg `Black; `Bold]
          else " E ", base)
       else
       if has_tag B0_meta.lib u
       then " L ", [`Bg `Yellow; `Fg `Black; `Bold] else
       if has_tag B0_meta.doc u
       then " D ", [`Bg `Cyan; `Fg `Black; `Bold] else
       " U ", base)
    in
    Fmt.tty style ppf tag;
  in
  Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (meta v)

(* Builds *)

module Build = struct
  open B0_std.Fut.Syntax

  include Build_def

  let memo b = b.u.m

  (* Units *)

  let must_build b = b.b.must_build
  let may_build b = b.b.may_build
  let current b = match b.u.current with
  | None -> invalid_arg "Build not running" | Some u -> u

  let require_unit b u =
    if String.Map.mem (name u) b.b.requested then () else
    match Unit.Set.mem u b.b.may_build with
    | true ->
        b.b.requested <- String.Map.add (name u) u b.b.requested;
        Random_queue.add b.b.waiting u
    | false ->
        B0_memo.fail b.u.m
          "@[<v>Unit %a requested %a which is not part of the build.@,\
           Try with %a or add the unit %a to the build."
          pp_name (current b) pp_name u Fmt.code "--unlock"
          pp_name u

  let require_units b us = List.iter (require_unit b) us
  let current_meta b = meta (current b)

  (* Directories *)

  let unit_dir b u =
    B0_dir.unit_build_dir ~build_dir:b.b.build_dir ~name:(name u)

  let unit_scope_dir b u = match B0_def.scope_dir (def u) with
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
    let may_build = Set.union may_build must_build in
    let add_requested u acc = String.Map.add (name u) u acc in
    let requested = Unit.Set.fold add_requested must_build String.Map.empty in
    let waiting =
      let q = Random_queue.empty () in
      Unit.Set.iter (Random_queue.add q) must_build; q
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
    let m = B0_memo.with_mark b.u.m (name unit) in
    let u = { current = Some unit; m } in
    let b = { b with u } in
    B0_memo.run_proc m begin fun () ->
      let* () = B0_memo.mkdir b.u.m (unit_dir b unit) in
      (build_proc unit) b
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
    String.Map.fold (fun _ u acc -> Set.add u acc) b.b.requested Set.empty
end

type b0_unit = t
type b0_env =
  { b0_dir : Fpath.t;
    build : Build.t;
    built_tools : b0_unit String.Map.t Lazy.t;
    cwd : Fpath.t;
    root_dir : Fpath.t;
    scope_dir : Fpath.t;
    build_env : Os.Env.t;
    driver_env : Os.Env.t; }

(* Executing *)

let exe_file =
  let doc = "Absolute file path to a built executable." in
  let pp_value = Fmt.any "<built value>" in
  B0_meta.Key.make "exe-file" ~doc ~pp_value

let outcomes =
  let doc = "Unit build outcomes." in
  let pp_value = Bval.pp (Fmt.vbox (Fmt.list Fpath.pp)) in
  B0_meta.Key.make "outcomes" ~doc ~pp_value

let is_public u = match find_meta B0_meta.public u with
| None -> false | Some b -> b

let get_or_suggest_tool ~keep n =
  (* XXX I don't 'understand how this current_is_root () can work. *)
  let is_root = B0_scope.current_is_root () in
  let keep = if is_root then keep else fun u -> in_current_scope u && keep u in
  let us = Option.value ~default:[] (String.Map.find_opt n !tool_name_index) in
  match List.filter keep us with
  | _ :: _ as us -> Ok us
  | [] ->
      let add_sugg acc u = if keep u then u :: acc else acc in
      let add_suggs k us acc =
        if String.edit_distance k n > 2 then acc else
        List.fold_left add_sugg acc us
      in
      Error (List.rev (String.Map.fold add_suggs !tool_name_index []))

let tool_is_user_accessible u = is_public u || in_root_scope u

let tool_name_map units =
  let warn_dup_tool use ign n =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a defined both by unit %a and %a.@,\
       Ignoring definition in unit %a.@]"
      Fmt.code n pp_name use pp_name ign pp_name ign
  in
  let warn_no_exe_file u n =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a defined by unit %a does not specify a@,\
       B0_meta.exe_file key. It will not be used in the build (if needed).@]"
      Fmt.code n pp_name u
  in
  let add_unit u acc =
    if not (tool_is_user_accessible u) then acc else
    match B0_meta.find tool_name (meta u) with
    | None -> acc
    | Some t ->
        match String.Map.find_opt t acc with
        | Some u' -> warn_dup_tool u u' t; acc
        | None ->
            if B0_meta.mem exe_file (meta u)
            then String.Map.add t u acc
            else (warn_no_exe_file u t; acc)
  in
  Set.fold add_unit units String.Map.empty

module Env = struct
  type t = b0_env
  let make ~b0_dir ~build ~cwd ~root_dir ~scope_dir ~driver_env =
    let build_env = B0_memo.env (Build.memo build) in
    let built_tools = lazy begin
      (* FIXME it would be nice to have a check that the build
         finished here. *)
      tool_name_map (Build.did_build build)
    end
    in
    { b0_dir; build; built_tools; cwd; root_dir; scope_dir; build_env;
      driver_env; }

  (* Directories. *)

  let b0_dir env = env.b0_dir
  let cwd env = env.cwd
  let root_dir env = env.root_dir
  let scope_dir env = env.scope_dir
  let scratch_dir env = B0_dir.scratch_dir ~b0_dir:env.b0_dir
  let unit_dir env u = Build.unit_dir env.build u

  let in_root_dir env p = Fpath.(root_dir env // p)
  let in_scope_dir env p = Fpath.(scope_dir env // p)
  let in_scratch_dir env p = Fpath.(scratch_dir env // p)
  let in_unit_dir env u p = Fpath.(unit_dir env u // p)
  let build env = env.build

  type dir = [`Cwd | `Root_dir | `Scope_dir | `Unit_dir ]

  let pp_dir ppf = function
  | `Cwd -> Fmt.string ppf "current working directory"
  | `Scope_dir -> Fmt.string ppf "scope directory"
  | `Unit_dir -> Fmt.string ppf "unit directory"
  | `Root_dir -> Fmt.string ppf "root directory"

  let dir env = function
  | `Cwd -> cwd env
  | `Root_dir -> root_dir env
  | `Scope_dir -> scope_dir env
  | `Unit_dir -> invalid_arg "Cannot lookup `Unit_dir"

  let in_dir env d p = Fpath.(dir env d // p)

  (* Process environments. *)

  type env = [ `Build_env | `Driver_env ]
  let build_env env = env.build_env
  let driver_env env = env.driver_env
  let env env = function
  | `Build_env -> env.build_env
  | `Driver_env -> env.driver_env

  let pp_env ppf = function
  | `Build_env -> Fmt.string ppf "build environment"
  | `Driver_env -> Fmt.string ppf "b0 invocation environment"

  (* Tool lookup *)

  let get_cmd ?(skip_build = false) env cmd =
    if skip_build then Os.Cmd.get cmd else
    let tool_map = Lazy.force env.built_tools in
    match Cmd.find_tool cmd with
    | None -> Fmt.error "No tool to lookup: the command is empty"
    | Some tool ->
        match String.Map.find_opt (Fpath.to_string tool) tool_map with
        | None -> Os.Cmd.get cmd
        | Some u ->
            Result.map (fun v -> Cmd.path (Fut.sync v)) (get_meta exe_file u)

  let unit_exe_file env u =
    if Set.mem u (Build.did_build env.build)
    then Result.map Fut.sync (get_meta exe_file u) else
    Fmt.error "Cannot get executable of unit %a: it did not build." pp_name u

  let unit_exe_file_cmd env u = Result.map Cmd.path (unit_exe_file env u)
end

module Exec = struct
  type env =
  [ Env.env
  | `Override of Env.env * Os.Env.t
  | `Env of Os.Env.t
  | `Fun of string * (b0_env -> b0_unit -> (Os.Env.t, string) result) ]

  let pp_env ppf = function
  | #Env.env as env -> Env.pp_env ppf env
  | `Override (#Env.env as env, by) ->
      Fmt.pf ppf "@[<v>%a overriden by:@,%a@]" Env.pp_env env Os.Env.pp by
  | `Env env -> Os.Env.pp ppf env
  | `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

  let env =
    let doc = "Environment for an execution." in
    let default = `Build_env in
    B0_meta.Key.make "exec-env" ~default ~doc ~pp_value:pp_env

  let get_env b0_env u = match find_or_default_meta env u with
  | #Env.env as env -> Ok (Env.env b0_env env)
  | `Override (env, by) -> Ok (Os.Env.override (Env.env b0_env env) ~by)
  | `Env env -> Ok env
  | `Fun (_doc, f) -> f b0_env u

  type cwd =
  [ Env.dir
  | `In of Env.dir * Fpath.t
  | `Fun of string * (b0_env -> b0_unit -> (Fpath.t, string) result) ]

  let pp_cwd ppf = function
  | #Env.dir as dir -> Env.pp_dir ppf dir
  | `In (dir, p) -> Fmt.pf ppf "%a in %a" Fpath.pp p Env.pp_dir dir
  | `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

  let cwd =
    let doc = "Current working directory for an execution." in
    let default = `Cwd in
    B0_meta.Key.make "exec-cwd" ~default ~doc ~pp_value:pp_cwd

  let get_cwd b0_env u = match find_or_default_meta cwd u with
  | #Env.dir as dir -> Ok (Env.dir b0_env dir)
  | `In (#Env.dir as dir, p) -> Ok (Env.in_dir b0_env dir p)
  | `Fun (_doc, f) -> f b0_env u

  type t =
  [ `Unit_exe
  | `Cmd of string * (b0_env -> b0_unit -> args:Cmd.t -> (Cmd.t, string) result)
  | `Fun of
      string *
      (b0_env -> ?env:Os.Env.assignments -> ?cwd:Fpath.t ->
       b0_unit -> args:Cmd.t -> (Os.Exit.t, string) result) ]

  let pp ppf = function
  | `Unit_exe -> Fmt.pf ppf "file in %a" B0_meta.Key.pp_name exe_file
  | `Cmd (doc, _) -> Fmt.pf ppf "<fun> %s" doc
  | `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

  let key : t B0_meta.key =
    let doc = "Unit execution." in
    let default = `Unit_exe in
    B0_meta.Key.make "exec" ~default ~doc ~pp_value:pp

  open Result.Syntax

  let run_exe_file exe_file b0_env u ~args =
    let* env = Result.map Os.Env.to_assignments (get_env b0_env u) in
    let* cwd = get_cwd b0_env u in
    let exe_file = Fut.sync exe_file in
    let cmd = Cmd.(path exe_file %% args) in
    Ok (Os.Exit.exec ~env ~cwd cmd)

  let run_fun f b0_env u ~args =
    let* env = Result.map Os.Env.to_assignments (get_env b0_env u) in
    let* cwd = get_cwd b0_env u in
    f b0_env ?env:(Some env) ?cwd:(Some cwd) u ~args

  let run_cmd cmd b0_env u ~args =
    let* env = Result.map Os.Env.to_assignments (get_env b0_env u) in
    let* cwd = get_cwd b0_env u in
    let* cmd = cmd b0_env u ~args in
    Ok (Os.Exit.exec ~env ~cwd cmd)

  let find u = match find_or_default_meta key u with
  | `Cmd (_, cmd) -> Some (run_cmd cmd)
  | `Fun (_, f) -> Some (run_fun f)
  | `Unit_exe ->
      match find_meta exe_file u with
      | None -> None
      | Some exe_file -> Some (run_exe_file exe_file)
end

let () = B0_scope.close ()
