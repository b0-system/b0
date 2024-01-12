(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let err_nothing () = match B0_unit.list () with
| _ :: _ -> "Nothing to build with this invocation."
| [] ->
    Fmt.str "No units are defined in the %a file"
      Fmt.code' B0_driver.Conf.b0_file_name

(* XXX more needs to go the library here *)

(* Explaining what gets into the build *)

let green = Fmt.tty' [`Fg `Green]
let red = Fmt.tty' [`Fg `Red]

let log_explain_lock ~is_locked ~lock ~locked_packs =
  let option_reason pre opt ppf = function
  | None -> () | Some _ -> Fmt.pf ppf "%s option %a" pre Fmt.(code string) opt
  in
  let packs_reason lock ppf = function
  | [] -> ()
  | p :: rest as ps ->
      Fmt.pf ppf "%s pack%s %a"
        (match lock with Some true -> " and" | _ -> "")
        (if rest = [] then "" else "s")
        (Fmt.and_enum B0_pack.pp_name) ps
  in
  match is_locked with
  | true ->
      Log.app @@ fun m ->
      m "Build %a by%a%a." red "locked"
        (option_reason "" "--lock") lock (packs_reason lock) locked_packs
  | false ->
      Log.app @@ fun m ->
      m "Build %a%a" green "unlocked" (option_reason " by" "--unlock") lock

let log_units color ~kind us =
  Log.app @@ fun m ->
  m "@[<v1>%a build:@,@[<v>%a@]@]"
    color kind Fmt.(list B0_unit.pp_synopsis) (B0_unit.Set.elements us)

let show_what ~lock ~is_locked ~locked_packs ~must_build ~may_build c =
  Log.if_error' ~use:B0_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  if B0_unit.Set.is_empty must_build
  then (Log.app (fun m -> m "%s" (err_nothing ())); Ok B0_cli.Exit.ok)
  else begin
    log_explain_lock ~is_locked ~lock ~locked_packs;
    log_units red ~kind:"Must" must_build;
    if not is_locked then begin
      let may_build = B0_unit.Set.diff may_build must_build in
      if not (B0_unit.Set.is_empty may_build)
      then log_units green ~kind:"May" may_build
    end;
    Ok B0_cli.Exit.ok
  end

(* Finding what to build *)

let unit_set_of ~units ~packs =
  let pack_units = List.concat_map B0_pack.units packs in
  B0_unit.Set.of_list (List.rev_append units pack_units)

let get_excluded_units ~x_units ~x_packs =
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false x_units in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:false x_packs in
  Ok (unit_set_of ~units ~packs)

let get_must_units_and_locked_packs ~exec ~exec_units ~exec_packs ~units ~packs
  =
  let* units, packs = match exec, units, packs with
  | None, [], [] -> (* argumentless b0 invocation *)
      begin match B0_pack.find "default" with
      | None -> Ok (B0_unit.list (), [])
      | Some t -> Ok ([], [t])
      end
  | _ ->
      let* units = B0_unit.get_list_or_hint ~all_if_empty:false units in
      let* packs = B0_pack.get_list_or_hint ~all_if_empty:false packs in
      Ok (List.rev_append exec_units units, List.rev_append exec_packs packs)
  in
  let locked_packs = List.filter B0_pack.locked packs in
  Ok (unit_set_of ~units ~packs, locked_packs)

let is_locked ~lock ~locked_packs = match lock, locked_packs with
| Some false, _ -> false
| None, [] -> false
| _, _ -> true

let get_may_must ~is_locked ~units ~x_units =
  let must = B0_unit.Set.diff units x_units in
  let may =
    if is_locked then must else
    let all = B0_unit.Set.of_list (B0_unit.list ()) in
    B0_unit.Set.diff all x_units
  in
  may, must

let check_tool_ambiguities tool_name us =
  let warn_multi_defs tool_name u us =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a defined in multiple units: %a.@,Using unit %a.@]"
      Fmt.code' tool_name Fmt.(list ~sep:comma B0_unit.pp_name) (u :: us)
      B0_unit.pp_name u
  in
  let warn_has_unit_name tool_name u u' =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a of unit %a also matches unit name %a@,\
       Running the tool, use %a %a to execute the unit.@]"
      Fmt.code' tool_name B0_unit.pp_name u B0_unit.pp_name u'
      Fmt.code' "b0 unit exec" B0_unit.pp_name u'
  in
  let warn_has_action_name tool_name u a =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a of unit %a also matches action name %a@,\
       Running the tool, use %a %a to execute the action.@]"
      Fmt.code' tool_name B0_unit.pp_name u B0_action.pp_name a
      Fmt.code' "b0 action exec" B0_action.pp_name a
  in
  let u = match us with
  | [u] -> u | u :: us -> warn_multi_defs tool_name u us; u
  | [] -> assert false
  in
  let () = match B0_unit.find tool_name with
  | None -> ()
  | Some u' when B0_unit.equal u u' ->
      (* XXX Surprising stuff from an end user point of view could
         be happening if the unit is executable (vs simply have
         an executable file. Maybe we should make B0_unit.exec
         and B0_unit.exe_file or warn again ? *) ()
  | Some u' -> warn_has_unit_name tool_name u u'
  in
  let () = match B0_action.find tool_name with
  | None -> () | Some a -> warn_has_action_name tool_name u a
  in
  u

let find_store_and_execution args = function
| None -> Ok ([], None, [], [])
| Some name ->
    let keep = B0_unit.tool_is_user_accessible in
    match B0_unit.get_or_suggest_tool ~keep name with
    | Ok us ->
        let u = check_tool_ambiguities name us in
        Ok ([], Some (`Unit u), [u], [])
    | Error tool_suggs ->
        let u = B0_unit.get_or_suggest name in
        let action = B0_action.get_or_suggest name in
        match u, action with
        | Ok u, Ok action ->
            (* XXX we should maybe disallow declaring an action with the same
               name as a tool or unit in a given scope. *)
            Fmt.error "Both a tool and action are called %a"
              (Fmt.code Fmt.string) name
        | Ok u, Error _ ->
            Ok ([], Some (`Unit u), [u], [])
        | Error _, Ok a ->
            let store = B0_action.store a in
            let units = B0_action.units a in
            let dyn_units = B0_action.dyn_units a ~args:(Cmd.list args) in
            let units = dyn_units @ units in
            Ok (store, Some (`Action a), units, B0_action.packs a)
        | Error us, Error cs ->
            let tname u = Option.get (B0_unit.find_meta B0_unit.tool_name u) in
            let ts = List.rev_map tname tool_suggs in
            let us = List.rev_map B0_unit.name us in
            let cs = List.rev_map B0_action.name cs in
            let set = String.Set.of_list (List.concat [ts; us; cs]) in
            let suggs = String.Set.elements set in
            let hint = Fmt.did_you_mean in
            let nothing_to ppf v =
              Fmt.pf ppf "Nothing to execute for %a." Fmt.code' v
            in
            let pp ppf (v, hints) = match hints with
            | [] -> nothing_to ppf v
            | hints -> Fmt.pf ppf "%a@ %a" nothing_to v (hint Fmt.code') hints
            in
            (* XXX show how to list available actions if there's no typo *)
            Fmt.error "@[%a@]" pp (name, suggs)

let memo c ~may_build ~must_build =
  (* Look for tools in the build first. XXX cross *)
  let tool_lookup ~may_build ~must_build ~env =
    let lookup = B0_memo.tool_lookup_of_os_env env in
    let units = B0_unit.Set.union may_build must_build in
    let tool_map = B0_unit.tool_name_map units  in
    (* We first look into the build and then in [m]'s environment. *)
    fun m t -> match String.Map.find_opt (Fpath.to_string t) tool_map with
    | None -> lookup m t
    | Some u ->
        (* FIXME, not there yet we need to require that to build !
           we need to push the whole [memo c] thing into B0_build so that
           we can access the store to get B0_build.t *)
        Fut.map Result.ok (B0_meta.get B0_unit.exe_file (B0_unit.meta u))
  in
  let hash_fun = B0_driver.Conf.hash_fun c in
  let cwd = B0_driver.Conf.cwd c in
  let cache_dir = B0_driver.Conf.cache_dir c in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let trash_dir = Fpath.(b0_dir / B0_cli.Memo.trash_dir_name) in
  let jobs = B0_driver.Conf.jobs c in
  let feedback =
    let op_howto ppf o = Fmt.pf ppf "b0 log --id %d" (B0_zero.Op.id o) in
    let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
    B0_cli.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
      Fmt.stderr
  in
  let* env = Os.Env.current () in
  let tool_lookup = tool_lookup ~may_build ~must_build ~env in
  B0_memo.make
    ~hash_fun ~cwd ~tool_lookup ~env ~cache_dir ~trash_dir ~jobs ~feedback ()

(* Executing the action or the unit *)

let executor_env build def c =
  let cwd = B0_driver.Conf.cwd c in
  let root_dir = Fpath.parent @@ Option.get @@ B0_driver.Conf.b0_file c in
  let scope_dir = Option.value (B0_def.scope_dir def) ~default:root_dir in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let driver_env = B0_driver.Conf.env c in
  B0_env.make ~cwd ~scope_dir ~root_dir ~b0_dir ~build ~driver_env

let action_executor show_path build action c = match show_path with
| false ->
    let func = B0_action.func action in
    let env = executor_env build (B0_action.def action) c in
    let exec ~args = Ok (func action env ~args) in
    Ok (Some exec)
| true -> (* We could show the path to the b0 driver, bof. *)
    Fmt.error "%a has no path, it is an action not an executable unit"
      B0_action.pp_name action

let unit_executor show_path build u c =
  let warn_noexec u =
    Log.warn @@ fun m ->
    m "Unit %a not executable: ignoring execution." B0_unit.pp_name u
  in
  let show p ~args =
    (* N.B. it seems there's no way of quoting to make the shell notation $()
       work if args or p have spaces !? *)
    Log.app (fun m -> m "%a" Cmd.pp Cmd.(path p %% args));
    Ok B0_cli.Exit.ok
  in
  match B0_unit.Exec.find u with
  | None -> warn_noexec u; Ok None
  | Some exec ->
      match show_path with
      | false ->
          let env = executor_env build (B0_unit.def u) c in
          Ok (Some (fun ~args -> exec env u ~args))
      | true ->
          match B0_unit.find_meta B0_unit.exe_file u with
          | Some path -> Ok (Some (fun ~args -> show (Fut.sync path) ~args))
          | None ->
              Fmt.error "No executable outcome path found in executable unit %a"
                B0_unit.pp_name u

let error_executor_needs ~exec_needs ~must_build exec =
  let diff = B0_unit.Set.diff exec_needs must_build in
  let pp_exec ppf = function
  | `Action a -> Fmt.pf ppf "action %a" B0_action.pp_name a
  | `Unit u -> Fmt.pf ppf "unit %a" B0_unit.pp_name u
  in
  Fmt.error "@[Cannot execute %a: %a will not build, see %a@]"
    pp_exec exec
    (Fmt.iter B0_unit.Set.iter ~sep:Fmt.comma B0_unit.pp_name) diff
    Fmt.code' "--what"

(* Build command *)

let build units x_units packs x_packs what lock show_path action args c =
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* store, exec, exec_units, exec_packs =
    find_store_and_execution args action
  in
  let* x_units = get_excluded_units ~x_units ~x_packs in
  let* units, locked_packs =
    get_must_units_and_locked_packs ~exec ~exec_units ~exec_packs ~units ~packs
  in
  let is_locked = is_locked ~lock ~locked_packs in
  let may_build, must_build = get_may_must ~is_locked ~units ~x_units in
  if what
  then show_what ~lock ~is_locked ~locked_packs ~must_build ~may_build c else
  Log.if_error' ~use:B0_driver.Exit.build_error @@
  let* m = memo c ~may_build ~must_build in
  let build =
    let variant = "user" in
    let b0_file = Option.get (B0_driver.Conf.b0_file c) in
    let root_dir = Fpath.parent b0_file in
    let b0_dir = B0_driver.Conf.b0_dir c in
    B0_build.make ~root_dir ~b0_dir ~variant ~store m ~may_build ~must_build
  in
  let exec_needs = unit_set_of ~units:exec_units ~packs:exec_packs in
  let* executor = match exec with
  | Some exec when not (B0_unit.Set.subset exec_needs must_build) ->
      error_executor_needs ~exec_needs ~must_build exec
  | Some (`Action a) -> action_executor show_path build a c
  | Some (`Unit u) -> unit_executor show_path build u c
  | None when B0_unit.Set.is_empty must_build -> Fmt.error "%s" (err_nothing ())
  | None -> Ok None
  in
  match B0_build.run build with
  | Error () -> Ok B0_driver.Exit.build_error
  | Ok () ->
      match executor with
      | None -> Ok B0_cli.Exit.ok
      | Some executor -> executor ~args:(Cmd.list args)

(* Command line interface *)

open Cmdliner

let units = B0_cli.units ~doc:"Build unit $(docv). Repeatable." ()
let x_units =
  B0_cli.x_units ()
    ~doc:"Exclude unit $(docv) from the build. Takes over inclusion."

let packs = B0_cli.packs ~doc:"Build pack $(docv). Repeteable." ()
let x_packs =
  B0_cli.x_packs ()
    ~doc:"Exclude units in pack $(docv) from the build. Takes over inclusion."

let what =
  let doc = "Do not run the build, show units that must and may build." in
  Arg.(value & flag & info ["what"] ~doc)

let lock =
  let lock =
    let doc = "Lock the build to units and packs specified on the cli." in
    Some true, Arg.info ["lock"] ~doc
  in
  let unlock =
    let doc = "Unlock a build that contains a locked pack." in
    Some false, Arg.info ["unlock"] ~doc
  in
  Arg.(value & vflag None [lock; unlock])

let show_path =
  let doc = "Rather than perform action print invocation on $(b,stdout). For \
             simple actions this prints the path to the build executable and \
             is useful if you want to time it without timing the build."
  in
  Arg.(value & flag & info ["path"] ~doc)

let action =
  let doc = "Action to run. Specify it after a $(b,--) otherwise \
             it gets taken for a $(mname) command when $(b,b0) is used \
             without a command."
  in
  Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"ACTION")

let args =
  let doc = "Arguments given as is to the action." in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let term =
  Term.(const build $ units $ x_units $ packs $ x_packs $ what $ lock $
        show_path $ action $ args)

let cmd =
  let doc = "Build and run actions (default command)" in
  let synopsis =
    `P "$(iname) \
        [$(b,-u) $(i,UNIT)]…  [$(b,-p) $(i,PACK)]… [$(i,OPTION)]… \
        $(b,--) [$(i,ACTION)] [$(i,ARG)]…";
  in
  let descr =
    [ `P "The $(iname) command builds and runs actions, executable units \
          or the tool they define.";
      `P "To build a unit use the $(b,-u) option. To build all the units of \
          a pack use the $(b,-p) option.";
      `P "If an action is specified, its required units and packs are \
          added to the build like $(b,-u) and $(b,-p) options do. If an \
          executable unit is specified, it is added like $(b,-u) does.";
      `P "If no unit or pack is specified on the command line and no \
          action or executable unit is specified all units build unless \
          a pack named $(b,default) exists in the root scope in which \
          case $(b,-p default) is implied.";
      `P "Build procedures may dynamically require the build of units \
          unspecified on the command line. To prevent a unit from building \
          use the $(b,-x) and $(b,-X) options. These options take over \
          unit inclusions specified with $(b,-u) and $(b,-p) options.";
      `P "If you want to make sure only the exact units you specified are \
          in the build, use the $(b,--lock) option to lock the build. \
          If you request a pack that has the $(b,B0_meta.locked) tag, \
          the build locks automatically unless $(b,--unlock) is specified.";
      `P "If you add the $(b,--what) option, the build doesn't run but what \
          must and may build is shown.";
      `P "More background information is available in the manuals, \
          see $(b,odig doc b0)."; ]
  in
  let descr = `Blocks descr in
  B0_tool_std.Cli.subcmd_with_b0_file "build" ~doc ~descr ~synopsis term
