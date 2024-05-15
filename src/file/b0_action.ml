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

type t = B0_unit.t

type func = B0_env.t -> t -> args:Cmd.t -> Os.Exit.t

let () = B0_scope.open_lib ~module':__MODULE__ "meta"

let packs : B0_pack.t list B0_meta.key =
  let doc = "Packs needed for action" in
  let pp_value = Fmt.list ~sep:Fmt.sp B0_pack.pp_name in
  B0_meta.Key.make "action-packs" ~default:[] ~pp_value ~doc

let () = B0_scope.close ()

let make
    ?store:st ?packs:ps ?units:us ?dyn_units:dus ?doc
    ?(meta = B0_meta.empty) name (f : func)
  =
  let f e u ~args = Ok (f e u ~args) in
  let meta =
    meta
    |> B0_meta.add B0_unit.Action.key (`Fun ("action", f))
    |> B0_meta.add_some B0_unit.Action.units us
    |> B0_meta.add_some B0_unit.Action.dyn_units dus
    |> B0_meta.add_some packs ps
    |> B0_meta.add_some B0_unit.Action.store st
  in
  B0_unit.make ?doc ~meta name B0_unit.build_nop

let make' ?store (* ?packs *) ?units ?dyn_units ?doc ?meta name func =
  let func action env ~args = exit_of_result (func action env ~args) in
  make ?store (* ?packs *) ?units ?dyn_units ?doc ?meta name func

let packs action = B0_unit.find_or_default_meta packs action

(* Script and tool execution *)

let exec_file ?env:e ?cwd cmd env t ~args =
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
  let func env action ~args =
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
