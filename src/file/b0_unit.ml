(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Fut.Syntax

(* A bit of annoying recursive definition. *)

module rec Build_def : sig
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : Unit.t option; m : B0_memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_build_dir : Fpath.t;
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
      shared_build_dir : Fpath.t;
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
  let pp_name_str = Fmt.(code string)
end

and Unit : sig include B0_def.S with type t = Unit_def.t end
  = B0_def.Make (Unit_def)

(* Build procedures *)

type build = Build_def.t
type build_proc = Unit_def.build_proc
let build_nop b = Fut.return ()

(* Build units *)

include Unit

(* We also maintain a tool name index for built tools. *)

let () = B0_scope.open_lib ~module':__MODULE__ "unit"

let tool_name =
  let doc = "Executable tool name without platform specific extension" in
  B0_meta.Key.make "tool-name" ~doc ~pp_value:Fmt.string

let tool_name_index = ref String.Map.empty

let add_tool_name u = match find_meta tool_name u with
| None -> ()
| Some n -> tool_name_index := String.Map.add_to_list n u !tool_name_index

let make ?doc ?meta n build_proc =
  let def = define ?doc ?meta n in
  let u = { Unit_def.def; build_proc } in add u; add_tool_name u; u

let build_proc u = u.Unit_def.build_proc

let pp_synopsis ppf v =
  let pp_tag ppf u =
    let tag, style =
      (if has_tag B0_meta.exe u then "u", [`Fg `Green] else
       if has_tag B0_meta.lib u then "u", [`Fg `Magenta] else
       if has_tag B0_meta.doc u then "u", [`Fg `Yellow] else
       "u", [])
    in
    Fmt.tty' style ppf "[";
    Fmt.string ppf tag;
    Fmt.tty' style ppf "]";
  in
  Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (meta v)

(* Builds *)

module Build = struct
  include Build_def

  let memo b = b.u.m

  (* Units *)

  let must_build b = b.b.must_build
  let may_build b = b.b.may_build
  let current b = match b.u.current with
  | None -> invalid_arg "Build not running" | Some u -> u

  let require b u =
    if String.Map.mem (name u) b.b.requested then () else
    match Unit.Set.mem u b.b.may_build with
    | true ->
        b.b.requested <- String.Map.add (name u) u b.b.requested;
        Random_queue.add b.b.waiting u
    | false ->
        B0_memo.fail b.u.m
          "@[<v>Unit %a requested %a which is not part of the build.@,\
           Try with %a or add the unit %a to the build."
          pp_name (current b) pp_name u Fmt.(code string) "--unlock"
          pp_name u

  let current_meta b = meta (current b)

  (* Directories *)

  let scope_dir b u = match B0_def.scope_dir (def u) with
  | None -> b.b.root_dir
  | Some dir -> dir

  let current_scope_dir b = scope_dir b (current b)
  let build_dir b u =
    B0_dir.unit_build_dir ~build_dir:b.b.build_dir ~name:(name u)

  let current_build_dir b = build_dir b (current b)
  let shared_build_dir b = b.b.shared_build_dir

  let in_build_dir b p = Fpath.(build_dir b (current b) // p)
  let in_shared_build_dir b p = Fpath.(b.b.shared_build_dir // p)
  let in_scope_dir b p = Fpath.(scope_dir b (current b) // p)

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
    let shared_build_dir = B0_dir.shared_build_dir ~build_dir in
    let store = B0_store.make m ~dir:(B0_dir.store_dir ~build_dir) store in
    let may_build = Set.union may_build must_build in
    let add_requested u acc = String.Map.add (name u) u acc in
    let requested = Unit.Set.fold add_requested must_build String.Map.empty in
    let waiting =
      let q = Random_queue.empty () in
      Unit.Set.iter (Random_queue.add q) must_build; q
    in
    let b =
      { root_dir; b0_dir; build_dir; shared_build_dir; store; must_build;
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
      let* () = B0_memo.mkdir b.u.m (build_dir b unit) in
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

type env =
  { b0_dir : Fpath.t;
    build : Build.t;
    cwd : Fpath.t;
    root_dir : Fpath.t;
    scope_dir : Fpath.t }

(* Executing *)

type exec_cwd =
[ `Build_dir
| `Cwd
| `Custom_dir of string * (build -> t -> Fpath.t Fut.t)
| `In of [ `Build_dir | `Root_dir | `Scope_dir ] * Fpath.t
| `Root_dir
| `Scope_dir ]

let pp_exec_cwd ppf = function
| `Build_dir -> Fmt.string ppf "Unit build directory"
| `Cwd -> Fmt.string ppf "User current working directory"
| `Custom_dir (doc, _) -> Fmt.string ppf doc
| `In (`Build_dir, p) -> Fmt.pf ppf "%a in unit build directory" Fpath.pp p
| `In (`Scope_dir, p) -> Fmt.pf ppf "%a in scope directory" Fpath.pp p
| `In (`Root_dir, p) -> Fmt.pf ppf "%a in root directory" Fpath.pp p
| `Scope_dir -> Fmt.string ppf "Scope directory"
| `Root_dir -> Fmt.string ppf "Root directory"

let exec_cwd =
  let doc = "Process current working directory for an execution." in
  B0_meta.Key.make "exec-cwd" ~doc ~pp_value:pp_exec_cwd

let get_exec_cwd build u = match find_meta exec_cwd u with
| None | Some `Cwd -> Fut.return None
| Some `Build_dir -> Fut.return (Some (Build.build_dir build u))
| Some (`In (`Build_dir, p)) ->
    Fut.return (Some Fpath.(Build.build_dir build u // p))
| Some `Root_dir -> failwith "TODO"
| Some (`In (`Root_dir, p)) -> failwith "TODO"
| Some `Scope_dir -> Fut.return (Some (Build.scope_dir build u))
| Some (`In (`Scope_dir, p)) ->
    Fut.return (Some Fpath.(Build.scope_dir build u // p))
| Some (`Custom_dir (_doc, f)) -> Fut.map Option.some (f build u)

type exec_env =
[ `Build_env
| `Build_env_override of Os.Env.t
| `Custom_env of string * (build -> t -> Os.Env.t Fut.t)
| `Env of Os.Env.t ]

let pp_exec_env ppf = function
| `Build_env -> Fmt.string ppf "Build environment"
| `Build_env_override env ->
    Fmt.pf ppf "@[<v>Build environment override with:@,%a@]"
      Fmt.(list string) (Os.Env.to_assignments env)
| `Custom_env (doc, _) -> Fmt.string ppf doc
| `Env env ->
    Fmt.pf ppf "@[<v>%a@]" Fmt.(list string) (Os.Env.to_assignments env)

let exec_env =
  let doc = "Process environment for an execution." in
  B0_meta.Key.make "exec-env" ~doc ~pp_value:pp_exec_env

let get_exec_env build u = match find_meta exec_env u with
| None | Some `Build_env -> Fut.return None
| Some `Build_env_override env ->
    (* FIXME proper environment lookup *)
    let e = Os.Env.current () |> Log.if_error ~use:Os.Env.empty in
    Fut.return (Some (Os.Env.override e ~by:env))
| Some (`Custom_env (_doc, f)) -> Fut.map Option.some (f build u)
| Some (`Env env) -> Fut.return (Some env)

let exe_file =
  let doc = "Absolute file path to a built executable." in
  let pp_value = Fmt.any "<built value>" in
  B0_meta.Key.make "exe-file" ~doc ~pp_value

let run_exe_file exe_file build u ~args =
  let* exe_file = exe_file in
  let* env = get_exec_env build u in
  let* cwd = get_exec_cwd build u in
  let env = Option.map Os.Env.to_assignments env in
  let cmd = Cmd.(path exe_file (* exe_name ? *) %% args) in
  Fut.return (Os.Exit.exec ?env ?cwd exe_file cmd)

let exec =
  let doc = "Unit execution function." in
  let pp_value ppf (doc, _) = Fmt.string ppf doc in
  B0_meta.Key.make "exec" ~doc ~pp_value

let run_exec exec build u ~args =
  let* cwd = get_exec_cwd build u in
  let* env = get_exec_env build u in
  exec build ?env ?cwd u ~args

let find_exec u = match find_meta exec u with
| Some (_, exec) -> Some (run_exec exec)
| None ->
    match find_meta exe_file u with
    | None -> None
    | Some exe_file -> Some (run_exe_file exe_file)

let is_public u = match find_meta B0_meta.public u with
| None -> false | Some b -> b

let get_or_suggest_tool ~keep n =
  (* XXX I don't understand how this current_is_root () can work. *)
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
      Fmt.code' n pp_name use pp_name ign pp_name ign
  in
  let warn_no_exe_file u n =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a defined by unit %a does not specify a@,\
       B0_meta.exe_file key. It will not be used in the build (if needed).@]"
      Fmt.code' n pp_name u
  in
  let add_unit u acc =
    if not (is_public u || in_root_scope u) then acc else
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

let () = B0_scope.close ()

module Env = struct
  type t = env
  let make ~b0_dir ~build ~cwd ~root_dir ~scope_dir =
    { b0_dir; build; cwd; root_dir; scope_dir; }

  let b0_dir env = env.b0_dir
  let cwd env = env.cwd
  let root_dir env = env.root_dir
  let scope_dir env = env.scope_dir
  let scratch_dir env = B0_dir.scratch_dir ~b0_dir:env.b0_dir
  let unit_dir env u = Build.build_dir env.build u

  let in_root_dir env p = Fpath.(root_dir env // p)
  let in_scope_dir env p = Fpath.(scope_dir env // p)
  let in_scratch_dir env p = Fpath.(scratch_dir env // p)
  let in_unit_dir env u p = Fpath.(unit_dir env u // p)

  let build env = env.build

  (* TODO lookup the builds *)

  let get_tool ?(no_build = false) env cmd = Os.Cmd.get_tool cmd
  let get_cmd ?(no_build = false) env cmd = Os.Cmd.get cmd

  let unit_file_exe env u =
    if Set.mem u (Build.did_build env.build)
    then Result.map Fut.sync (get_meta exe_file u) else
    Fmt.error "Cannot get executable of unit %a: it did not build."
      pp_name u

  let unit_cmd env u = Result.map Cmd.path (unit_file_exe env u)
end
