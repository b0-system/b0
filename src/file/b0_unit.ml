(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "unit"

open B0_std

(* Build procedures *)

type build_proc = B0_defs.build_proc
let build_nop b = Fut.return ()

(* Build units *)

include B0_defs.Unit

type b0_unit = t

let exe_file = B0_defs.exe_file
let tool_name = B0_defs.tool_name

let make = B0_defs.unit_make
let build_proc = B0_defs.unit_build_proc

(* Executing *)

let outcomes =
  let doc = "Unit build outcomes." in
  let pp_value = Bval.pp (Fmt.vbox (Fmt.list Fpath.pp)) in
  B0_meta.Key.make "outcomes" ~doc ~pp_value

let is_public = B0_defs.unit_is_public

let get_or_suggest_tool ~keep n =
  (* XXX I don't 'understand how this current_is_root () can work. *)
  let is_root = B0_scope.current_is_root () in
  let keep = if is_root then keep else fun u -> in_current_scope u && keep u in
  let us =
    Option.value ~default:[] (String.Map.find_opt n !B0_defs.tool_name_index)
  in
  match List.filter keep us with
  | _ :: _ as us -> Ok us
  | [] ->
      let add_sugg acc u = if keep u then u :: acc else acc in
      let add_suggs k us acc =
        if String.edit_distance k n > 2 then acc else
        List.fold_left add_sugg acc us
      in
      Error (List.rev (String.Map.fold add_suggs !B0_defs.tool_name_index []))

let tool_is_user_accessible = B0_defs.tool_is_user_accessible

(* We need this here because of formatting *)

type action =
  [ `Unit_exe
  | `Cmd of
      string * (B0_env.t -> b0_unit -> args:Cmd.t -> (Cmd.t, string) result)
  | `Fun of
      string *
      (B0_env.t -> b0_unit -> args:Cmd.t -> (Os.Exit.t, string) result) ]

let pp ppf = function
| `Unit_exe -> Fmt.pf ppf "file in %a" B0_meta.Key.pp_name exe_file
| `Cmd (doc, _) -> Fmt.pf ppf "<fun> %s" doc
| `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

let action_key : action B0_meta.key =
  (* We need this here because of formatting *)
  let doc = "Unit execution." in
  let default = `Unit_exe in
  B0_meta.Key.make "action" ~default ~doc ~pp_value:pp

(* Formatting build units *)

let pp_tag ppf u =
  let base = [`Fg `Black; `Bold] in
  let wbg = `Bg `White in
  let is_test u = has_tag B0_meta.test u in
  let is_run_test u = has_tag B0_meta.test u && has_tag B0_meta.run u in
  let is_long u = has_tag B0_meta.long u in
  let is_public u = has_tag B0_meta.public u in
  let is_exe u = has_tag B0_meta.exe u in
  let is_lib u = has_tag B0_meta.lib u in
  let is_doc u = has_tag B0_meta.doc u in
  let is_action u = mem_meta action_key u in
  let tag, style =
    if is_run_test u
    then " T ", (if is_long u then wbg else `Bg `Green) :: base else
    if is_exe u
    then
      (if is_test u then " T " else " E "),
      (if is_public u then `Fg `Red else `Fg `Black) :: [`Bold; wbg] else
    if is_lib u
    then " L ", (if is_public u then `Bg `Yellow else wbg) :: base else
    if is_doc u
    then " D ", (`Bg `Cyan :: base) else
    ((if is_action u then " A " else " U "), (wbg :: base))
  in
  Fmt.tty style ppf tag

let pp_synopsis ppf v = Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (meta v)

module Action = struct

  type b0_unit = t

  type env =
  [ B0_env.env
  | `Override of B0_env.env * Os.Env.t
  | `Env of Os.Env.t
  | `Fun of string * (B0_env.t -> b0_unit -> (Os.Env.t, string) result) ]

  let pp_env ppf = function
  | #B0_env.env as env -> B0_env.pp_env ppf env
  | `Override (#B0_env.env as env, by) ->
      Fmt.pf ppf "@[<v>%a overriden by:@,%a@]" B0_env.pp_env env Os.Env.pp by
  | `Env env -> Os.Env.pp ppf env
  | `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

  let env =
    let doc = "Environment for an execution." in
    let default = `Build_env in
    B0_meta.Key.make "action-env" ~default ~doc ~pp_value:pp_env

  let get_env b0_env u = match find_or_default_meta env u with
  | #B0_env.env as env -> Ok (B0_env.env b0_env env)
  | `Override (env, by) -> Ok (Os.Env.override (B0_env.env b0_env env) ~by)
  | `Env env -> Ok env
  | `Fun (_doc, f) -> f b0_env u

  type cwd =
  [ B0_env.dir
  | `In of B0_env.dir * Fpath.t
  | `Fun of string * (B0_env.t -> b0_unit -> (Fpath.t, string) result) ]

  let pp_cwd ppf = function
  | #B0_env.dir as dir -> B0_env.pp_dir ppf dir
  | `In (dir, p) -> Fmt.pf ppf "%a in %a" Fpath.pp p B0_env.pp_dir dir
  | `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

  let cwd =
    let doc = "Current working directory for an execution." in
    let default = `Cwd in
    B0_meta.Key.make "action-cwd" ~default ~doc ~pp_value:pp_cwd

  let get_cwd b0_env u = match find_or_default_meta cwd u with
  | #B0_env.dir as dir -> Ok (B0_env.dir b0_env dir)
  | `In (#B0_env.dir as dir, p) -> Ok (B0_env.in_dir b0_env dir p)
  | `Fun (_doc, f) -> f b0_env u

  type t = action

  let key = action_key

  let units : b0_unit list B0_meta.key =
    (* XXX Initially the idea was not to have a notion of explicit
       dependency between units. But now that we have this for action
       perhaps this should be moved to B0_unit. Or not, we could keep
       that notion only for executions and not builds but why ? *)
    let doc = "Units needed for action" in
    let pp_value = Fmt.list ~sep:Fmt.sp pp_name in
    B0_meta.Key.make "action-units" ~default:[] ~pp_value ~doc

  let dyn_units : (args:Cmd.t -> b0_unit list) B0_meta.key =
    let doc = "Dyn units needed for action (hack)" in
    let pp_value = Fmt.any "<func>" in
    let default ~args:_ = [] in
    B0_meta.Key.make "action-dyn-units" ~default ~pp_value ~doc

  let store : B0_store.binding list B0_meta.key =
    let doc = "Store imposed by the action." in
    let pp_value = Fmt.any "<store bindings>" in
    B0_meta.Key.make "action-store" ~default:[] ~pp_value ~doc

  let packs : B0_pack.t list B0_meta.key =
    let doc = "Packs needed for action" in
    let pp_value = Fmt.list ~sep:Fmt.sp B0_pack.pp_name in
    B0_meta.Key.make "action-packs" ~default:[] ~pp_value ~doc

  open Result.Syntax

  let run_exe_file exe_file b0_env u ~args =
    let* env = Result.map Os.Env.to_assignments (get_env b0_env u) in
    let* cwd = get_cwd b0_env u in
    let exe_file = Fut.sync exe_file in
    let cmd = Cmd.(path exe_file %% args) in
    Ok (Os.Exit.execv ~env ~cwd cmd)

  let run_fun f b0_env u ~args = f b0_env u ~args

  let run_cmd cmd b0_env u ~args =
    let* env = Result.map Os.Env.to_assignments (get_env b0_env u) in
    let* cwd = get_cwd b0_env u in
    let* cmd = cmd b0_env u ~args in
    Ok (Os.Exit.execv ~env ~cwd cmd)

  let find u = match find_or_default_meta key u with
  | `Cmd (_, cmd) -> Some (run_cmd cmd)
  | `Fun (_, f) -> Some (run_fun f)
  | `Unit_exe ->
      match find_meta exe_file u with
      | None -> None
      | Some exe_file -> Some (run_exe_file exe_file)

  let exit_some_error e =
  Log.err (fun m -> m "@[%a@]" Fmt.lines e); Os.Exit.some_error

  type func = B0_env.t -> b0_unit -> args:Cmd.t -> Os.Exit.t

  (* Script and tool execution *)

  let exec_file ?env:e ?cwd cmd env t ~args =
    let scope_dir = B0_env.scope_dir env in
    let cwd = Option.value ~default:scope_dir cwd in
    match Cmd.get_tool cmd with
    | Error e -> exit_some_error e
    | Ok file ->
        let file = Fpath.(scope_dir // file) in
        Os.Exit.execv ?env:e ~cwd Cmd.(set_tool file cmd %% args)

  let exec_cmd ?env:e ?cwd env cmd = match B0_env.get_cmd env cmd with
  | Error e -> exit_some_error e
  | Ok cmd ->
      let scope_dir = B0_env.scope_dir env in
      let cwd = Option.value ~default:scope_dir cwd in
      Os.Exit.execv ?env:e ~cwd cmd
end


(* Actions *)



let of_action
    ?store:st ?packs:ps ?units:us ?dyn_units:dus ?doc
    ?(meta = B0_meta.empty) name (f : Action.func)
  =
  let f e u ~args = Ok (f e u ~args) in
  let meta =
    meta
    |> B0_meta.add Action.key (`Fun ("action", f))
    |> B0_meta.add_some Action.units us
    |> B0_meta.add_some Action.dyn_units dus
    |> B0_meta.add_some Action.packs ps
    |> B0_meta.add_some Action.store st
  in
  make ?doc ~meta name build_nop

let of_action' ?store (* ?packs *) ?units ?dyn_units ?doc ?meta name func =
  let func action env ~args = Os.Exit.of_result (func action env ~args) in
  of_action ?store (* ?packs *) ?units ?dyn_units ?doc ?meta name func

(* Command line interaction. *)

let of_cmdliner_cmd ?store ?packs ?units ?dyn_units ?doc ?meta name cmd =
  let func env action ~args =
    let argv = Array.of_list (name :: Cmd.to_list args) in
    let cmd = cmd action env in
    B0_cli.Exit.of_eval_result (Cmdliner.Cmd.eval_value ~argv cmd)
  in
  of_action ?store ?packs ?units ?dyn_units ?doc ?meta name func

let eval_cmdliner_term
    ?man_xrefs ?man ?envs ?exits ?sdocs ?docs ?doc:d ?version
    action env term ~args
  =
  let name = name action in
  let doc = Option.value ~default:(doc action) d in
  let exits = Option.value ~default:B0_cli.Exit.infos exits in
  let info =
    Cmdliner.Cmd.info
      ?man_xrefs ?man ?envs ~exits ?sdocs ?docs ?version name ~doc
  in
  let argv = Array.of_list (name :: Cmd.to_list args) in
  let cmd = Cmdliner.Cmd.v info term in
  B0_cli.Exit.of_eval_result (Cmdliner.Cmd.eval_value ~argv cmd)

let tool_name_map = B0_defs.tool_name_map

let () = B0_scope.close ()
