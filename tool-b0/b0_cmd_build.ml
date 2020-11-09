(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

(* Running the build *)

let warn_dup_tool u u' n =
  Log.warn @@ fun m ->
  m "@[<v>Tool %a defined both by unit %a and %a.@,\
          Using the former in the build (if needed)."
    Fmt.(code string) n B0_unit.pp_name u B0_unit.pp_name u'

let warn_no_exe_file u n =
  Log.warn @@ fun m ->
  m "[@<v>Tool %a defined by unit %a does not specify a@,\
     B0_meta.exe_file key. It will not be used in the build (if needed)."
    Fmt.(code string) n B0_unit.pp_name u

(* Look for tools in the build first. XXX cross *)

let tool_lookup ~may_build ~must_build ~env =
  let tool_name_map =
    let add_unit u acc =
      (* FIXME we likely want some kind of scoped based visibility control.
         Add B0_meta.install or B0_meta.tool or B0_meta.public. *)
      match B0_meta.find B0_meta.exe_name (B0_unit.meta u) with
      | None -> acc
      | Some t ->
          match String.Map.find_opt t acc with
          | Some u' -> warn_dup_tool u u' t; acc
          | None ->
              if B0_meta.mem B0_meta.exe_file (B0_unit.meta u)
              then (warn_no_exe_file u t; acc)
              else String.Map.add t u acc
    in
    String.Map.empty
    |> B0_unit.Set.fold add_unit may_build
    |> B0_unit.Set.fold add_unit must_build
  in
  let lookup = B00.Memo.tool_lookup_of_os_env env in
  (* We first look into the build and then in [m]'s environment. *)
  fun m t -> match String.Map.find_opt (Fpath.to_string t) tool_name_map with
  | None -> lookup m t
  | Some u ->
      Fut.map Result.ok (B0_meta.get B0_meta.exe_file (B0_unit.meta u))

let memo c ~may_build ~must_build =
  let hash_fun = B0_driver.Conf.hash_fun c in
  let cwd = B0_driver.Conf.cwd c in
  let cache_dir = B0_driver.Conf.cache_dir c in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let trash_dir = Fpath.(b0_dir / B00_cli.Memo.trash_dir_name) in
  let jobs = B0_driver.Conf.jobs c in
  let feedback =
    let op_howto ppf o = Fmt.pf ppf "b0 log --id %d" (B000.Op.id o) in
    let show_op = Log.Info and show_ui = Log.Error and level = Log.level () in
    B00_cli.Memo.pp_leveled_feedback ~op_howto ~show_op ~show_ui ~level
      Fmt.stderr
  in
  let* env = Os.Env.current () in
  B00.Memo.memo ~hash_fun ~cwd ~cache_dir ~trash_dir ~jobs ~feedback ()

let units_of ~units ~packs =
  let pack_units = List.concat_map B0_pack.units packs in
  B0_unit.Set.of_list (List.rev_append units pack_units)

let get_excluded_units ~x_units ~x_packs =
  let* units = B0_unit.get_list_or_hint x_units in
  let* packs = B0_pack.get_list_or_hint x_packs in
  Ok (units_of ~units ~packs)

let get_must_units_and_locked_packs ~units ~packs =
  let* units, packs = match units, packs with
  | [], [] ->
      begin match B0_pack.find "default" with
      | None -> Ok (B0_unit.list (), [])
      | Some t -> Ok ([], [t])
      end
  | _ ->
      let* units = B0_unit.get_list_or_hint units in
      let* packs = B0_pack.get_list_or_hint packs in
      Ok (units, packs)
  in
  let locked_packs = List.filter B0_pack.locked packs in
  Ok (units_of ~units ~packs, locked_packs)

let is_locked ~lock ~locked_packs = match lock, locked_packs with
| Some false, _ -> false
| None, [] -> false
| _, _ -> true

let get_may_must ~locked ~units ~x_units =
  let must = B0_unit.Set.diff units x_units in
  let may =
    if locked then must else
    let all = B0_unit.Set.of_list (B0_unit.list ()) in
    B0_unit.Set.diff all x_units
  in
  may, must

let find_outcome_action ~must_build (* not empty *) action args =
  let warn_args () = Log.warn @@ fun m ->
    m "No outcome action specified: ignoring arguments (see option %a)."
      Fmt.(code string) "-a"
  in
  let warn_noact u = Log.warn @@ fun m ->
    m  "No outcome action for %a: ignoring arguments." B0_unit.pp_name u
  in
  let warn_disable u = Log.warn @@ fun m ->
    m "Outcome action ignored: unit %a must not build, see %a."
      B0_unit.pp_name u Fmt.(code string) "--what"
  in
  match action with
  | None -> (if args <> [] then warn_args ()); Ok None
  | Some a ->
      let* u = B0_unit.get_or_hint a in
      match B0_unit.action u with
      | None -> warn_noact u; Ok None
      | Some act when B0_unit.Set.mem u must_build -> Ok (Some (act, u))
      | Some _ -> warn_disable u; Ok None

let build_run lock ~units ~packs ~x_units ~x_packs action args c =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* x_units = get_excluded_units ~x_units ~x_packs in
  let* units, locked_packs = get_must_units_and_locked_packs ~units ~packs in
  let locked = is_locked ~lock ~locked_packs in
  let may_build, must_build = get_may_must ~locked ~units ~x_units in
  match B0_unit.Set.is_empty must_build with
  | true -> Log.err (fun m -> m "Empty build!"); Ok B0_driver.Exit.build_error
  | false ->
      let b0_file = Option.get (B0_driver.Conf.b0_file c) in
      let root_dir = Fpath.parent b0_file in
      let b0_dir = B0_driver.Conf.b0_dir c in
      Log.if_error' ~use:B0_driver.Exit.build_error @@
      let* m = memo c ~may_build ~must_build in
      let variant = "user" in
      let build =
        B0_build.create ~root_dir ~b0_dir ~variant  m ~may_build ~must_build
      in
      let* action = find_outcome_action ~must_build action args in
      match B0_build.run build with
      | Error () -> Ok B0_driver.Exit.build_error
      | Ok () ->
          match action with
          | None -> Ok B00_cli.Exit.ok
          | Some (action, u) -> Ok (Fut.sync (action build u ~args))

(* Explaining what gets into the build *)

let green = Fmt.(tty_string [`Fg `Green])
let red = Fmt.(tty_string [`Fg `Red])
let log_explain_lock ~locked ~lock ~locked_packs =
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
  match locked with
  | true ->
      Log.app (fun m ->
          m "Build %a by%a%a."
            red "locked"
            (option_reason "" "--lock") lock
            (packs_reason lock) locked_packs);
  | false ->
      Log.app (fun m ->
          m "Build %a%a" green "unlocked" (option_reason " by" "--unlock") lock)

let log_units color ~kind us =
  Log.app (fun m ->
      m "@[<v1>%a build:@,@[<v>%a@]@]"
        color kind Fmt.(list B0_unit.pp_synopsis) (B0_unit.Set.elements us))

let build_what lock ~units ~packs ~x_units ~x_packs c =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* x_units = get_excluded_units ~x_units ~x_packs in
  let* units, locked_packs = get_must_units_and_locked_packs ~units ~packs in
  let locked = is_locked ~lock ~locked_packs in
  let may_build, must_build = get_may_must ~locked ~units ~x_units in
  Log.if_error' ~use:B00_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B00_pager.find ~don't () in
  let* () = B00_pager.page_stdout pager in
  match B0_unit.Set.is_empty must_build with
  | true -> Log.app (fun m -> m "Empty build."); Ok B00_cli.Exit.ok
  | false ->
      log_explain_lock ~locked ~lock ~locked_packs;
      log_units red ~kind:"Must" must_build;
      if not locked then begin
        let may_build = B0_unit.Set.diff may_build must_build in
        if not (B0_unit.Set.is_empty may_build)
        then log_units green ~kind:"May" may_build
      end;
      Ok B00_cli.Exit.ok

(* Build command *)

let build what lock units packs x_units x_packs action args c =
  let units = match action with None -> units | Some a -> a :: units in
  if what
  then build_what lock ~units ~packs ~x_units ~x_packs c
  else build_run  lock ~units ~packs ~x_units ~x_packs action args c

(* Command line interface *)

open Cmdliner

let units = B0_cli.Arg.units ~doc:"Build unit $(docv). Repeatable." ()
let packs = B0_cli.Arg.packs ~doc:"Build pack $(docv). Repeteable." ()

let x_units =
  B0_cli.Arg.x_units ()
    ~doc:"Exclude unit $(docv) from the build. Takes over inclusion."

let x_packs =
  B0_cli.Arg.x_packs ()
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

let action =
  let doc = "Build and perform outcome unit action of unit $(docv)." in
  Arg.(value & opt (some string) None & info ["a"; "action"] ~doc ~docv:"UNIT")

let args =
  let doc = "Arguments given as is to the outcome action. \
             Start with $(b,--) otherwise options get interpreted by $(mname)."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"ARG")

let cmd =
  let doc = "Build (default)" in
  let sdocs = Manpage.s_common_options in
  let exits = B0_driver.Exit.infos in
  let envs = B00_pager.envs () in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(mname) $(tname) \
        [$(b,-u) $(i,UNIT) | $(b,-p) $(i,PACK) | $(i,OPTION)]... \
        [$(b,--) ARG...]";
    `S Manpage.s_description;
    `P "The $(tname) command runs your builds.";
    `P "To build a unit use the $(b,-u) option. To build all the units of \
        a pack use the $(b,-p) option. If no unit or pack is specified on \
        the command line all units build unless a pack named $(b,default) \
        exists in the root scope in which case $(b,-p default) is implied.";
    `P "Build procedures may dynamically require the build of units \
        unspecified on the command line. To prevent a unit from building \
        use the $(b,-x) and $(b,-X) options. These options take over \
        unit inclusions specified with $(b,-u) and $(b,-p) options.";
    `P "If you want to make sure only the exact units you specified are \
        in the build, use the $(b,--lock) option to lock the build. \
        If you request a pack that has the $(b,B0_meta.locked) tag, \
        the build locks automatically unless $(b,--unlock) is specified.";
    `P "If you add the $(b,--what) option, the build doesn't run but what must
        and may build is shown.";
    `P "More background information is available in the manuals, \
        see $(b,odig doc b0).";
    B0_b0.Cli.man_see_manual; ]
  in
  let build_cmd =
    Term.(const build $ what $ lock $ units $ packs $ x_units $ x_packs $
          action $ args)
  in
  B0_driver.with_b0_file ~driver:B0_b0.driver build_cmd,
  Term.info "build" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
