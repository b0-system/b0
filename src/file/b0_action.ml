(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let exit_some_error e =
  Log.err (fun m -> m "@[%a@]" Fmt.lines e); B0_cli.Exit.some_error

let exit_of_result = function
| Ok _ -> B0_cli.Exit.ok | Error e -> exit_some_error e

let exit_of_result' = function Ok e -> e | Error e -> exit_some_error e

(* Actions *)

include B0_unit

type func = t -> B0_env.t -> args:Cmd.t -> Os.Exit.t

let () = B0_scope.open_lib ~module':__MODULE__ "meta"

let func : func B0_meta.key =
  let doc = "Action function" and pp_value = Fmt.any "<func>" in
  B0_meta.Key.make "action-func" ~pp_value ~doc

let units : B0_unit.t list B0_meta.key =
  let doc = "Units needed for action" in
  let pp_value = Fmt.list ~sep:Fmt.sp B0_unit.pp_name in
  B0_meta.Key.make "action-units" ~default:[] ~pp_value ~doc

let dyn_units : (args:Cmd.t -> B0_unit.t list) B0_meta.key =
  let doc = "Dyn units needed for action (hack)" in
  let pp_value = Fmt.any "<func>" in
  let default ~args:_ = [] in
  B0_meta.Key.make "action-dyn-units" ~default ~pp_value ~doc

let packs : B0_pack.t list B0_meta.key =
  let doc = "Packs needed for action" in
  let pp_value = Fmt.list ~sep:Fmt.sp B0_pack.pp_name in
  B0_meta.Key.make "action-packs" ~default:[] ~pp_value ~doc

let store : B0_store.binding list B0_meta.key =
  let doc = "Store imposed by the action." in
  let pp_value = Fmt.any "<store bindings>" in
  B0_meta.Key.make "action-store" ~default:[] ~pp_value ~doc

let () = B0_scope.close ()

let is_action u = B0_meta.mem func (B0_unit.meta u)

let make
    ?store:st ?packs:ps ?units:us ?dyn_units:dus ?doc
    ?(meta = B0_meta.empty) name f
  =
  let meta =
    meta
    |> B0_meta.add func f
    |> B0_meta.add_some units us
    |> B0_meta.add_some dyn_units dus
    |> B0_meta.add_some packs ps
    |> B0_meta.add_some store st
  in
  B0_unit.make ?doc ~meta name B0_unit.build_nop

let make' ?store ?packs ?units ?dyn_units ?doc ?meta name func =
  let func action env ~args = exit_of_result (func action env ~args) in
  make ?store ?packs ?units ?dyn_units ?doc ?meta name func

let func' action = B0_meta.find func (B0_unit.meta action)
let func action = B0_meta.get func (B0_unit.meta action)
let units action = B0_unit.find_or_default_meta units action
let dyn_units action = B0_unit.find_or_default_meta dyn_units action
let packs action = B0_unit.find_or_default_meta packs action
let store action = B0_unit.find_or_default_meta store action

let pp_synopsis ppf v =
  let pp_tag ppf v =
    let style = [`Bg `White; `Fg `Black; `Bold] in
    Fmt.tty style ppf " A ";
  in
  Fmt.pf ppf "@[%a %a@]" pp_tag v B0_unit.pp_synopsis v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m
  in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (B0_unit.meta v)

(* Script and tool execution *)

let exec_file ?env:e ?cwd cmd t env ~args =
  let scope_dir = B0_env.scope_dir env in
  let cwd = Option.value ~default:scope_dir cwd in
  match Cmd.get_tool cmd with
  | Error e -> exit_some_error e
  | Ok file ->
      let file = Fpath.(scope_dir // file) in
      Os.Exit.exec ?env:e ~cwd Cmd.(set_tool file cmd %% args)

let exec_cmd ?env:e ?cwd env cmd = match B0_env.get_cmd env cmd with
| Error e -> exit_some_error e
| Ok cmd ->
    let scope_dir = B0_env.scope_dir env in
    let cwd = Option.value ~default:scope_dir cwd in
    Os.Exit.exec ?env:e ~cwd cmd

(* Command line interaction. *)

let of_cmdliner_cmd ?store ?packs ?units ?dyn_units ?doc ?meta name cmd =
  let func action env ~args =
    let argv = Array.of_list (name :: Cmd.to_list args) in
    let cmd = cmd action env in
    B0_cli.Exit.of_eval_result (Cmdliner.Cmd.eval_value ~argv cmd)
  in
  make ?store ?packs ?units ?dyn_units ?doc ?meta name func

let eval_cmdliner_term
    ?man_xrefs ?man ?envs ?exits ?sdocs ?docs ?doc:d ?version
    action env term ~args
  =
  let name = B0_unit.name action in
  let doc = Option.value ~default:(B0_unit.doc action) d in
  let exits = Option.value ~default:B0_cli.Exit.infos exits in
  let info =
    Cmdliner.Cmd.info
      ?man_xrefs ?man ?envs ~exits ?sdocs ?docs ?version name ~doc
  in
  let argv = Array.of_list (name :: Cmd.to_list args) in
  let cmd = Cmdliner.Cmd.v info term in
  B0_cli.Exit.of_eval_result (Cmdliner.Cmd.eval_value ~argv cmd)
