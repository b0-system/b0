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
    packs : B0_pack.t list;
    store : B0_store.binding list; }

module T = struct
  type nonrec t = t
  let def_kind = "action"
  let def p = p.def
  let pp_name_str = Fmt.(code string)
end

include (B0_def.Make (T) : B0_def.S with type t := t)

let make ?(store = []) ?(packs = []) ?(units = []) ?doc ?meta name func =
  let def = define ?doc ?meta name in
  let p = { def; func; units; packs; store } in
  add p; p

let make' ?store ?packs ?units ?doc ?meta name func =
  let func action env ~args = exit_of_result (func action env ~args) in
  make ?store ?packs ?units ?doc ?meta name func

let func action = action.func
let units action = action.units
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

let exec_file ?env:e ?cwd env file ~args =
  let scope_dir = B0_env.scope_dir env in
  let cwd = Option.value ~default:scope_dir cwd in
  let file = Fpath.(scope_dir // file) in
  Os.Exit.exec ?env:e ~cwd file Cmd.(path file %% args)

let exec_file' ?env:e ?cwd file =
  fun _ env ~args -> exec_file ?env:e ?cwd env file ~args

let exec_tool ?env:e ?cwd env tool ~args = match Os.Cmd.get_tool tool with
| Error e -> exit_some_error e
| Ok exe ->
    let scope_dir = B0_env.scope_dir env in
    let cwd = Option.value ~default:scope_dir cwd in
    Os.Exit.exec ?env:e ~cwd exe Cmd.(path exe %% args)

(* Command line interaction. *)

let of_cmdliner_cmd ?store ?packs ?units ?doc ?meta name cmd =
  let func action env ~args =
    let argv = Array.of_list (name :: Cmd.to_list args) in
    let cmd = cmd action env in
    B0_cli.Exit.of_eval_result (Cmdliner.Cmd.eval_value ~argv cmd)
  in
  make ?store ?packs ?units ?doc ?meta name func

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
