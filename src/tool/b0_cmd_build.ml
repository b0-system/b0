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
      Fmt.code B0_driver.Conf.b0_file_name

(* Explaining what gets into the build *)

let green = Fmt.st [`Fg `Green]
let red = Fmt.st [`Fg `Red]

let log_explain_lock ~is_locked ~lock ~locked_packs =
  let option_reason pre opt ppf = function
  | None -> () | Some _ -> Fmt.pf ppf "%s option %a" pre Fmt.code opt
  in
  let packs_reason lock ppf = function
  | [] -> ()
  | p :: rest as ps ->
      Fmt.pf ppf "%s pack%s %a"
        (match lock with Some true -> " and" | _ -> "")
        (if rest = [] then "" else "s")
        (Fmt.and_enum B0_pack.pp_name) ps
  in
  let locked_packs = B0_pack.Set.elements locked_packs in
  match is_locked with
  | true ->
      Log.stdout @@ fun m ->
      m "Build %a by%a%a." red "locked"
        (option_reason "" "--lock") lock (packs_reason lock) locked_packs
  | false ->
      Log.stdout @@ fun m ->
      m "Build %a%a" green "unlocked" (option_reason " by" "--unlock") lock

let log_units color ~kind us =
  Log.stdout @@ fun m ->
  m "@[<v>%a build:@,@[<v>%a@]@]"
    color kind Fmt.(list B0_unit.pp_synopsis) (B0_unit.Set.elements us)

let output_what ~lock ~is_locked ~locked_packs ~must_build ~may_build conf =
  Log.if_error' ~use:Os.Exit.some_error @@
  let no_pager = B0_driver.Conf.no_pager conf in
  let* pager = B0_pager.find ~no_pager () in
  let* () = B0_pager.page_stdout pager in
  if B0_unit.Set.is_empty must_build
  then (Log.stdout (fun m -> m "%s" (err_nothing ())); Ok Os.Exit.ok)
  else begin
    log_explain_lock ~is_locked ~lock ~locked_packs;
    log_units red ~kind:"Must" must_build;
    if not is_locked then begin
      let may_build = B0_unit.Set.diff may_build must_build in
      if not (B0_unit.Set.is_empty may_build)
      then log_units green ~kind:"May" may_build
    end;
    Ok Os.Exit.ok
  end

(* Finding what to build *)

let unit_set_of ~units ~packs =
  let add_pack p acc = List.rev_append (B0_pack.units p) acc in
  let pack_units = B0_pack.Set.fold add_pack packs [] in
  B0_unit.Set.of_list (List.rev_append units pack_units)

let get_default_build () = match B0_pack.find "default" with
| None -> B0_unit.list (), []
| Some t -> [], [t]

let get_must_units_and_locked_packs ~is_action ~units ~packs ~args () =
  let store, units, packs =
    let store, units, action_packs =
      let add_unit (store, us, ps) u =
        if not (is_action u) then (store, u :: us, ps) else
        let st = B0_unit.find_or_default_meta B0_unit.Action.store u in
        let units = B0_unit.find_or_default_meta B0_unit.Action.units u in
        let dyn_units =
          (B0_unit.find_or_default_meta
             B0_unit.Action.dyn_units u) ~args:(Cmd.list args)
        in
        let us = u :: List.rev_append ( List.rev_append dyn_units units) us in
        let packs = B0_unit.find_or_default_meta B0_unit.Action.packs u in
          List.rev_append st store, us, List.rev_append packs ps
      in
      List.fold_left add_unit ([], [], []) units
    in
    store, units, List.rev_append action_packs packs
  in
  let packs = B0_pack.Set.of_list packs in
  let locked_packs = B0_pack.Set.filter B0_pack.locked packs in
  store, unit_set_of ~units ~packs, locked_packs

let is_locked ~lock ~locked_packs = match lock with
| Some lock -> lock | None -> not (B0_pack.Set.is_empty locked_packs)

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
      Fmt.code tool_name Fmt.(list ~sep:comma B0_unit.pp_name) (u :: us)
      B0_unit.pp_name u
  in
  let warn_has_unit_name tool_name u u' =
    Log.warn @@ fun m ->
    m "@[<v>Tool %a of unit %a also matches unit name %a@,\
       Running the tool, use %a %a to execute the unit.@]"
      Fmt.code tool_name B0_unit.pp_name u B0_unit.pp_name u'
      Fmt.code "b0 unit action" B0_unit.pp_name u'
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
  u

let find_action_unit = function
| None -> Ok None
| Some name ->
  let keep = B0_unit.tool_is_user_accessible in
  match B0_unit.get_or_suggest_tool ~keep name with
  | Ok us ->
      let u = check_tool_ambiguities name us in
      Ok (Some u)
  | Error tool_suggs ->
      match B0_unit.get_or_suggest name with
      | Ok u -> Ok (Some u)
      | Error us ->
          let tname u = Option.get (B0_unit.find_meta B0_unit.tool_name u) in
          let ts = List.rev_map tname tool_suggs in
          let us = List.rev_map B0_unit.name us in
          let set = String.Set.of_list (List.concat [ts; us]) in
          let suggs = String.Set.elements set in
          let hint = Fmt.did_you_mean in
          let nothing_to ppf v =
            Fmt.pf ppf "Nothing to run for %a." Fmt.code v
          in
          let pp ppf (v, hints) = match hints with
          | [] -> nothing_to ppf v
          | hints -> Fmt.pf ppf "%a@ %a" nothing_to v (hint Fmt.code) hints
          in
          (* XXX instruct how to list available actions if there's no typo *)
          Fmt.error "@[%a@]" pp (name, suggs)

let memo conf ~may_build ~must_build =
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
  let hash_fun = B0_driver.Conf.hash_fun conf in
  let cwd = B0_driver.Conf.cwd conf in
  let cache_dir = B0_driver.Conf.cache_dir conf in
  let b0_dir = B0_driver.Conf.b0_dir conf in
  let trash_dir = Fpath.(b0_dir / B0_memo_cli.trash_dirname) in
  let jobs = B0_driver.Conf.jobs conf in
  let feedback =
    let op_howto ppf o = Fmt.pf ppf "b0 log --id %d" (B0_zero.Op.id o) in
    let output_op_level = Log.Info and output_ui_level = Log.Error in
    let level = Log.level () in
    B0_memo_cli.pp_leveled_feedback
      ~op_howto ~output_op_level ~output_ui_level ~level Fmt.stderr
  in
  let* env = Os.Env.current () in
  let tool_lookup = tool_lookup ~may_build ~must_build ~env in
  B0_memo.make
    ~hash_fun ~cwd ~tool_lookup ~env ~cache_dir ~trash_dir ~jobs ~feedback ()

let make_build conf ~store ~may_build ~must_build =
  let* m = memo conf ~may_build ~must_build in
  let build =
    let variant = "user" in
    let b0_file = Option.get (B0_driver.Conf.b0_file conf) in
    let root_dir = Fpath.parent b0_file in
    let b0_dir = B0_driver.Conf.b0_dir conf in
    B0_build.make ~root_dir ~b0_dir ~variant ~store m ~may_build ~must_build
  in
  Ok build

(* Executing the action or the unit *)

let warn_noexec u =
  Log.warn @@ fun m ->
  m "@[Unit %a not actionable, execution ignored@ (no@ %a@ or@ %a@ key).@]"
    B0_unit.pp_name u B0_meta.Key.pp_name B0_unit.Action.key B0_meta.Key.pp_name
    B0_unit.exe_file

let error_no_path action_unit =
  Fmt.error "%a: No path to executable file found (no %a key)"
    B0_unit.pp_name action_unit B0_meta.Key.pp_name B0_unit.exe_file

let do_output_path action_unit ~args =
  let* path = B0_unit.get_meta B0_unit.exe_file action_unit in
  let p = Fut.sync path in
  (* Is there a way of quoting to make the shell notation $() work if args
     or p have spaces ? *)
  Log.stdout (fun m -> m "%a" Cmd.pp Cmd.(path p %% args));
  Ok Os.Exit.ok

let env_for_unit c build u =
  let cwd = B0_driver.Conf.cwd c in
  let root_dir = Fpath.parent @@ Option.get @@ B0_driver.Conf.b0_file c in
  let scope_dir = Option.value (B0_unit.scope_dir u) ~default:root_dir in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let driver_env = B0_driver.Conf.env c in
  B0_env.make ~cwd ~scope_dir ~root_dir ~b0_dir ~build ~driver_env

let do_action_exit c build action_unit ~args =
  (* Note if we want to run the action asap we should not exit, but
     B0_unit.Action.run *)
  let env = env_for_unit c build action_unit in
  let a = B0_unit.(find_or_default_meta Action.key action_unit) in
  B0_unit.Action.exit env action_unit ~args a

(* Build command *)

let build
    ~units ~x_units ~packs ~x_packs ~what ~lock ~output_path ~build_only ~action
    ~args conf
  =
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* action_unit = find_action_unit action in
  let is_action = match action_unit with
  | None -> Fun.const false
  | Some u -> B0_unit.equal u
  in
  let* x_units = B0_cli.get_excluded_units ~x_units ~x_packs in
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false units in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:false packs in
  let units, packs = match action_unit with
  | None when units = [] && packs = [] -> get_default_build ()
  | None -> units, packs
  | Some u -> u :: units, packs
  in
  let store, units, locked_packs =
    get_must_units_and_locked_packs ~is_action ~units ~packs ~args ()
  in
  Log.if_error' ~use:B0_driver.Exit.build_error @@
  let is_locked = is_locked ~lock ~locked_packs in
  let may_build, must_build = get_may_must ~is_locked ~units ~x_units in
  if what
  then output_what ~lock ~is_locked ~locked_packs ~must_build ~may_build conf
  else
  (* Assert or warn a few things before starting the build. *)
  let* action_unit = match action_unit with
  | None when B0_unit.Set.is_empty must_build -> Fmt.error "%s" (err_nothing ())
  | Some u when not (B0_unit.mem_meta B0_unit.exe_file u) && output_path ->
      error_no_path u
  | Some u when not (B0_unit.is_actionable u) -> warn_noexec u; Ok None
  | o -> Ok o
  in
  let* build = make_build conf ~store ~may_build ~must_build in
  let args = Cmd.of_list Fun.id args in
  match B0_build.run build with
  | Error () -> Ok B0_driver.Exit.build_error
  | Ok () ->
      match action_unit with
      | None -> Ok Os.Exit.ok
      | Some action_unit when output_path -> do_output_path action_unit ~args
      | Some action_unit when build_only -> Ok Os.Exit.ok
      | Some action_unit -> do_action_exit conf build action_unit ~args

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let what =
  let doc = "Do not run the build, output units that must and may build." in
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

let output_path =
  let doc =
    "Rather than perform action output invocation on $(b,stdout). For simple \
     actions this prints the path to the build executable and is useful if \
     you want to time it without timing the build."
  in
  Arg.(value & flag & info ["path"] ~doc)

let build_only =
  let doc = "Build only. Do not execute the action." in
  Arg.(value & flag & info ["b"; "build"] ~doc)

let action_arg =
  let docv = "ACTION" in
  let complete pre =
    let keep u =
      if not (B0_unit.is_actionable u) then None else
      let name = match B0_unit.find_meta B0_unit.tool_name u with
      | Some name when String.starts_with ~prefix:pre name -> Some name
      | _ ->
          match B0_unit.name u with
          | name when String.starts_with ~prefix:pre name -> Some name
          | _ -> None
      in
      Option.map (fun n -> n, B0_unit.doc u) name
    in
    List.filter_map keep (B0_unit.list ())
  in
  let completion = Arg.Completion.make ~complete () in
  Arg.Conv.of_conv Arg.string ~completion ~docv ()

let action =
  let doc = "Action or tool to run. Specify it after a $(b,--) otherwise \
             it gets taken for a $(tool) command when $(b,b0) is used \
             without a command."
  in
  Arg.(value & pos 0 (some action_arg) None & info [] ~doc)

let args =
  let doc = "Arguments given as is to the action." in
  let aargs =
    let completion = Arg.Completion.make ~restart:true () in
    Arg.Conv.of_conv ~docv:"ARG" ~completion Arg.string ()
  in
  Arg.(value & pos_right 0 aargs [] & info [] ~doc)

let term =
  let+ units = B0_cli.build_units and+ x_units = B0_cli.build_x_units
  and+ packs = B0_cli.build_packs and+ x_packs = B0_cli.build_x_packs
  and+ what and+ lock and+ output_path and+ build_only and+ action and+ args in
  build ~units ~x_units ~packs ~x_packs ~what ~lock ~output_path ~build_only
    ~action ~args

let cmd =
  let doc = "Build and run actions (default command)" in
  let synopsis =
    `P "$(cmd) \
        [$(b,-u) $(i,UNIT)]…  [$(b,-p) $(i,PACK)]… [$(i,OPTION)]… \
        $(b,--) [$(i,ACTION)] [$(i,ARG)]…";
  in
  let descr = `Blocks [
      `P "The $(cmd) command builds units and runs actions, or \
          the tools they define.";
      `P "To build a unit use the $(b,-u) option. To build all the units of \
          a pack use the $(b,-p) option.";
      `P "If an action or tool is specified, its required units and packs are \
          added to the build like $(b,-u) and $(b,-p) options do and the unit \
          which defines it is added like $(b,-u) does.";
      `P "If no unit or pack is specified on the command line and no \
          action or tool is specified all units build unless a pack \
          named $(b,default) exists in the root scope in which case \
          $(b,-p default) is implied.";
      `P "Build procedures may dynamically require the build of units \
          unspecified on the command line. To prevent a unit from building \
          use the $(b,-x) and $(b,-X) options. These options take over \
          unit inclusions specified with $(b,-u) and $(b,-p) options.";
      `P "If you want to make sure only the exact units you specified are \
          in the build, use the $(b,--lock) option to lock the build. \
          If you request a pack that has the $(b,B0_meta.locked) tag, \
          the build locks automatically unless $(b,--unlock) is specified.";
      `P "If you add the $(b,--what) option, the build doesn't run but what \
          must and may build is output.";
      `P "More background information is available in the manuals, \
          see $(b,odig doc b0)."; ]
  in
  B0_tool_cli.cmd_with_b0_file "build" ~doc ~descr ~synopsis term
