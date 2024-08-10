(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "unit"

open B0_std

(* Build procedures *)

type build_proc = B0_defs.build_proc
let build_nop b = Fut.return ()

(* Units *)

include B0_defs.Unit

let make = B0_defs.unit_make
let build_proc = B0_defs.unit_build_proc

(* Built executable *)

let exe_file = B0_defs.exe_file
let tool_name = B0_defs.tool_name

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

(* We need this here because of pp_tag *)

type action_func = B0_env.t -> t -> args:Cmd.t -> (Os.Exit.t, string) result
type action = [ `Unit_exe | `Fun of string * action_func ]

let pp ppf = function
| `Unit_exe -> Fmt.pf ppf "file in %a" B0_meta.Key.pp_name exe_file
| `Fun (doc, _) -> Fmt.pf ppf "<fun> %s" doc

let action_key : action B0_meta.key =
  let doc = "Unit action." in
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
  let is_sample u = has_tag B0_meta.sample u in
  let is_action u = mem_meta action_key u in
  let tag, style =
    if is_run_test u
    then " T ", (if is_long u then wbg else `Bg `Green) :: base else
    if is_exe u
    then
      (if is_test u then " T " else (if is_sample u then " S " else " E ")),
      (if is_public u then `Fg `Red else `Fg `Black) :: [`Bold; wbg] else
    if is_lib u
    then " L ", (if is_public u then `Bg `Yellow else wbg) :: base else
    if is_doc u
    then " D ", (`Bg `Cyan :: base) else
    ((if is_action u then " A " else " U "), (wbg :: base))
  in
  Fmt.st style ppf tag

let pp_synopsis ppf v = Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (meta v)

module Action = struct
  type b0_unit = t

  (* Environment *)

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

  (* Cwd *)

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

  (* Action *)

  type func = action_func
  type t = action

  let func ?(doc = "undocumented") func = `Fun (doc, func)
  let scope_exec ?env:e ?cwd cmd env _ ~args =
    let open Result.Syntax in
    let scope_dir = B0_env.scope_dir env in
    let cwd = Option.value ~default:scope_dir cwd in
    let* file = Cmd.get_tool cmd in
    let file = Fpath.(scope_dir // file) in
    Ok (Os.Exit.execv ?env:e ~cwd Cmd.(set_tool file cmd %% args))

  let of_cmdliner_term
      ?man_xrefs ?man ?envs ?exits ?sdocs ?docs ?doc:d ?version
      termf env u ~args
    =
    let open Cmdliner in
    let name = name u in
    let doc = Option.value ~default:(doc u) d in
    let exits = Option.value ~default:B0_std_cli.Exit.infos exits in
    let info =
      Cmd.info ?man_xrefs ?man ?envs ~exits ?sdocs ?docs ?version name ~doc
    in
    let argv = Array.of_list (name :: B0_std.Cmd.to_list args) in
    let cmd = Cmdliner.Cmd.v info (termf env u) in
    (* FIXME use Cmd.eval_value' *)
    Ok (B0_std_cli.Exit.of_eval_result (Cmd.eval_value ~argv cmd))

  (* Metadata *)

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

  let run_exe_file exe_file b0_env u ~args =
    let open Result.Syntax in
    let* env = Result.map Os.Env.to_assignments (get_env b0_env u) in
    let* cwd = get_cwd b0_env u in
    let exe_file = Fut.sync exe_file in
    let cmd = Cmd.(path exe_file %% args) in
    Ok (Os.Exit.execv ~env ~cwd cmd)

  (* Running *)

  let err_miss_exe_file u =
    Fmt.error "Unit %a not actionable, no executable file found (no %a key)"
      pp_name u B0_meta.Key.pp_name exe_file

  let run' exit_rc run b0_env u ~args action =
    let open Result.Syntax in
    let* env = get_env b0_env u in
    let env = Os.Env.to_assignments env in
    let* cwd = get_cwd b0_env u in
    match action with
    | `Unit_exe ->
        begin match find_meta exe_file u with
        | None -> err_miss_exe_file u
        | Some exe_file ->
            let exe_file = Fut.sync exe_file in
            run ~env ~cwd ~argv0:None Cmd.(path exe_file %% args)
        end
    | `Fun (_, cmd) ->
        let* exit = cmd b0_env u ~args in
        begin match exit with
        | Os.Exit.Code rc -> Ok (exit_rc rc)
        | Os.Exit.Execv execv ->
            let env = Option.value ~default:env (Os.Exit.execv_env execv) in
            let cwd = Option.value ~default:cwd (Os.Exit.execv_cwd execv) in
            let argv0 = Os.Exit.execv_argv0 execv in
            let cmd = Os.Exit.execv_cmd execv in
            let* cmd = B0_env.get_cmd b0_env cmd in
            run ~env ~cwd ~argv0 cmd
        end

  let run b0_env u ~args action =
    let exit_rc rc = `Exited rc in
    let run ~env ~cwd ~argv0:_ cmd = Os.Cmd.run_status ~env ~cwd cmd in
    run' exit_rc run b0_env u ~args action

  let exit b0_env u ~args action =
    let exit_rc rc = Os.Exit.code rc in
    let run ~env ~cwd ~argv0 cmd = Ok (Os.Exit.execv ~env ~cwd ?argv0 cmd) in
    run' exit_rc run b0_env u ~args action
end

let is_actionable u = match find_or_default_meta Action.key u with
| `Unit_exe -> mem_meta exe_file u | `Fun _ -> true

(* Actions *)

let of_action'
    ?store:st ?packs:ps ?units:us ?dyn_units:dus ?doc
    ?(meta = B0_meta.empty) name (f : Action.func)
  =
  let meta =
    meta
    |> B0_meta.add Action.key (Action.func ?doc f)
    |> B0_meta.add_some Action.units us
    |> B0_meta.add_some Action.dyn_units dus
    |> B0_meta.add_some Action.packs ps
    |> B0_meta.add_some Action.store st
  in
  make ?doc ~meta name build_nop

let of_action ?store ?packs ?units ?dyn_units ?doc ?meta name func =
  let func action env ~args =
    Result.map (Fun.const Os.Exit.ok) (func action env ~args)
  in
  of_action' ?store ?packs ?units ?dyn_units ?doc ?meta name func

(* Command line interaction. *)

let of_cmdliner_cmd ?store ?packs ?units ?dyn_units ?doc ?meta name cmd =
  let func env u ~args =
    let argv = Array.of_list (name :: Cmd.to_list args) in
    let cmd = cmd env u in
    Ok (B0_std_cli.Exit.of_eval_result (Cmdliner.Cmd.eval_value ~argv cmd))
  in
  of_action' ?store ?packs ?units ?dyn_units ?doc ?meta name func

let tool_name_map = B0_defs.tool_name_map

let () = B0_scope.close ()
