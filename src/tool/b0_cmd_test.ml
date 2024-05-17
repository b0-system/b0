(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_testing

let exit_test_error = Os.Exit.Code 1
let exit_no_tests = Os.Exit.Code 2

let pp_test_tags ppf () =
  Fmt.pf ppf "%a and %a" Fmt.code "test" Fmt.code "run"

let pp_no_tests ppf () =
  Fmt.pf ppf "No test found in the build (no unit with tags %a)" pp_test_tags ()

let pp_run_tests ppf tests =
  if B0_unit.Set.is_empty tests then pp_no_tests ppf () else
  Fmt.pf ppf "@[<v>These %a will run:@,%a@]"
    Fmt.(tty [`Fg `Green]) "tests"
    Fmt.(list B0_unit.pp_synopsis) (B0_unit.Set.elements tests)

let pp_fail ppf (u, st) =
  Fmt.pf ppf "@[%a %a %a@]"
    Fmt.code "b0 test -u" B0_unit.pp_name u (Fmt.option Os.Cmd.pp_status) st

let pp_report ppf (total, dur, fails) = match fails with
| [] ->
    Fmt.pf ppf "@[%a The build %a all tests in %a (%a with build)@]"
      Test_fmt.pp_pass () Test_fmt.pp_passed () Test_fmt.pp_dur dur
      Mtime.Span.pp total
| fails ->
    let count = List.length fails in
    Fmt.pf ppf "%a @[<v>The build %a %a %s in %a:@,%a@]"
      Test_fmt.pp_fail () Test_fmt.pp_failed ()
      Test_fmt.pp_count count
      (if count <= 1 then "test unit" else "test units")
      Test_fmt.pp_dur dur (Fmt.list pp_fail) fails

let get_tests ~allow_long us =
  let is_test u =
    B0_unit.(has_tag B0_meta.run u && has_tag B0_meta.test u &&
             (allow_long || not (has_tag B0_meta.long u)))
  in
  B0_unit.Set.filter is_test us

let show_what
    ~allow_long ~lock ~may_build ~must_build ~is_locked ~locked_packs c
  =
  Log.if_error' ~use:Os.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let tests = get_tests ~allow_long must_build in
  Log.app (fun m -> m "%a" pp_run_tests tests);
  B0_cmd_build.show_what ~lock ~may_build ~must_build ~is_locked ~locked_packs c

(* Test command *)

let run_test c build u =
  Log.app (fun m -> m "%a %a" Test_fmt.pp_test () B0_unit.pp_name u);
  let exec = B0_meta.find_or_default B0_unit.Action.key (B0_unit.meta u) in
  let b0_env = B0_cmd_build.action_env build (B0_unit.def u) c in
  let* env = B0_unit.Action.get_env b0_env u in
  let env = Os.Env.to_assignments env in
  let* cwd = B0_unit.Action.get_cwd b0_env u in
  let args = Cmd.empty (* We could have a run_args key here *) in
  let run_cmd ~env ~cwd cmd =
    let dur = Os.Mtime.counter () in
    let* st = Os.Cmd.run_status ~env ~cwd cmd in
    Ok (Os.Mtime.count dur, st)
  in
  match exec with
  | `Unit_exe ->
      begin match B0_unit.find_meta B0_unit.exe_file u with
      | None ->
          Fmt.error "No executable file found (no %a key)"
            Fmt.code "B0_unit.exe_file"
      | Some exe_file ->
          let exe_file = Fut.sync exe_file in
          let cmd = Cmd.(path exe_file %% args) in
          run_cmd ~env ~cwd cmd
      end
  | `Cmd (_, cmd) ->
      let* cmd = cmd b0_env u ~args in
      run_cmd ~env ~cwd cmd
  | `Fun (_, cmd) ->
      (* FIXME we should clarify what `Fun is in B0_unit.Exec,
         in particular `Fun should not execv. In fact no Exec should. *)
      let dur = Os.Mtime.counter () in
      let* exit = cmd b0_env u ~args in
      begin match exit with
      | Code rc -> Ok (Os.Mtime.count dur, `Exited rc)
      | Execv _ -> Fmt.error "Unit Exec not supported in tests"
      end

let rec run_tests c build dur fails = function
| [] -> dur, fails
| u :: us ->
    (* XXX Lots could be improved here, parallel spawns and run as
       soon as the build file is ready. *)
    let log_sep () = Log.app (fun m -> m "%s" "") in
    match run_test c build u with
    | Error e ->
        Log.app (fun m -> m "%a: %s" B0_unit.pp_name u e);
        log_sep (); run_tests c build dur ((u, None) :: fails) us
    | Ok (tdur, `Exited 0) ->
        let dur = Mtime.Span.add dur tdur in
        log_sep (); run_tests c build dur fails us
    | Ok (tdur, st) ->
        let dur = Mtime.Span.add dur tdur in
        log_sep (); run_tests c build dur ((u, Some st) :: fails) us

let test allow_long allow_empty units x_units packs x_packs what lock c =
  let total = Os.Mtime.counter () in
  Log.if_error ~use:Os.Exit.no_such_name @@
  (* FIXME select_units here must return units and packs needed by
     every action of [tests] unit and a common store. *)
  let* (may_build, must_build), is_locked, locked_packs  =
    B0_cmd_build.select_units ~units ~x_units ~packs ~x_packs ~lock
  in
  if what
  then
    show_what ~allow_long ~lock ~may_build ~must_build ~is_locked
      ~locked_packs c
  else
  Log.if_error' ~use:B0_driver.Exit.build_error @@
  let tests = get_tests ~allow_long must_build in
  if B0_unit.Set.is_empty tests && not allow_empty
  then (Log.err (fun m ->
      m "@[<v>%a.@,Use option %a to succeed anyways.@]" pp_no_tests ()
        Fmt.code "-e"); Ok exit_no_tests)
  else
  let store = [] in
  let* build = B0_cmd_build.make_build c ~store ~may_build ~must_build in
  match B0_build.run build with
  | Error () -> Ok B0_driver.Exit.build_error
  | Ok () ->
      let tests = B0_unit.Set.elements tests in
      let dur, fails = run_tests c build Mtime.Span.zero [] tests in
      Log.app (fun m -> m "%a" pp_report (Os.Mtime.count total, dur, fails));
      Ok (if fails <> [] then exit_test_error else Os.Exit.ok)

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Build and run tests" in
  let descr = `Blocks
      [ `P "The $(iname) command builds and runs tests. \
            A test is a unit tagged with both $(b,B0_meta.test) and \
            $(b,B0_meta.run). The command builds like \
            $(b,b0 build) does and then runs all the tests that are in \
            the units that $(b,must) build.";
        `P "Tests that are tagged with $(b,B0_meta.long) are only run \
            if the $(b,--long) option is used.";
        `P "The command exits with 1 if one of the tests exits with \
            non-zero. Use option $(b,--what) with a given invocation \
            to understand what builds and which tests are run.";]
  in
  let exits =
    Cmdliner.Cmd.Exit.info
      (Os.Exit.get_code exit_test_error) ~doc:"If a test did not succeed." ::
    Cmdliner.Cmd.Exit.info
      (Os.Exit.get_code exit_no_tests) ~doc:"If there are no tests to run." ::
    B0_driver.Exit.infos
  in
  let allow_empty =
    let doc = "Do not fail if there are no tests in the build." in
    Arg.(value & flag & info ["e";"allow-empty"] ~doc)
  in
  let long =
    let doc = "Run long tests. By default tests tagged with \
               $(b,B0_meta.long) are not run."
    in
    Arg.(value & flag & info ["l";"long"] ~doc)
  in
  B0_tool_std.Cli.subcmd_with_b0_file "test" ~exits ~doc ~descr @@
  Term.(const test $ long $ allow_empty $ B0_cmd_build.units $
        B0_cmd_build.x_units $ B0_cmd_build.packs $ B0_cmd_build.x_packs $
        B0_cmd_build.what $ B0_cmd_build.lock)
