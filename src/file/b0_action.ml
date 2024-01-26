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

type func = t -> B0_env.t -> args:Cmd.t -> Os.Exit.t
and t =
  { def : B0_def.t;
    func : func;
    units : B0_unit.t list;
    dyn_units : args:Cmd.t -> B0_unit.t list;
    packs : B0_pack.t list;
    store : B0_store.binding list; }

module T = struct
  type nonrec t = t
  let def_kind = "action"
  let def p = p.def
  let pp_name_str = Fmt.(code string)
end

include (B0_def.Make (T) : B0_def.S with type t := t)

let make
    ?(store = []) ?(packs = []) ?(units = []) ?(dyn_units = fun ~args:_ -> [])
    ?doc ?meta name func
  =
  let def = define ?doc ?meta name in
  let p = { def; func; units; dyn_units; packs; store } in
  add p; p

let make' ?store ?packs ?units ?dyn_units ?doc ?meta name func =
  let func action env ~args = exit_of_result (func action env ~args) in
  make ?store ?packs ?units ?dyn_units ?doc ?meta name func

let func action = action.func
let units action = action.units
let dyn_units action = action.dyn_units
let packs action = action.packs
let store action = action.store

let pp_synopsis ppf v =
  let pp_tag ppf v =
    let style = [`Fg `Green] in
    Fmt.tty' style ppf "[";
    Fmt.string ppf "a";
    Fmt.tty' style ppf "]";
  in
  Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v

let pp ppf v =
  let pp_non_empty ppf m = match B0_meta.is_empty m with
  | true -> () | false -> Fmt.pf ppf "@, @[%a@]" B0_meta.pp m
  in
  Fmt.pf ppf "@[<v>%a%a@]" pp_synopsis v pp_non_empty (meta v)

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
