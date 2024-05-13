(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_testing

let exit_test_error = Os.Exit.Code 1

let pp_test_tags ppf () =
  Fmt.pf ppf "%a and %a" Fmt.code "test" Fmt.code "run"

let pp_no_tests ppf () =
  Fmt.pf ppf "%a found in the build (no unit with tags %a)"
    Fmt.(tty [`Fg `Red]) "No test" pp_test_tags ()

let pp_run_tests ppf tests =
  if B0_unit.Set.is_empty tests then pp_no_tests ppf () else
  Fmt.pf ppf "@[<v>These %a will run:@,%a@]"
    Fmt.(tty [`Fg `Green]) "tests"
    Fmt.(list B0_unit.pp_synopsis) (B0_unit.Set.elements tests)

let pp_fail ppf (u, st) =
  Fmt.pf ppf "@[%a %a %a@]"
    Fmt.code "b0 test -u" B0_unit.pp_name u (Fmt.option Os.Cmd.pp_status) st

let pp_report ppf (dur, fails) = match fails with
| [] ->
    Fmt.pf ppf "@[%a The build %a all tests in %a@]" Test_ui.pp_pass ()
      Test_ui.pp_passed () Test_ui.pp_dur dur
| fails ->
    let count = List.length fails in
    Fmt.pf ppf "%a @[<v>The build %a %a %s in %a:@,%a@]"
      Test_ui.pp_fail () Test_ui.pp_failed ()
      Test_ui.pp_count count
      (if count <= 1 then "test unit" else "test units")
      Test_ui.pp_dur dur (Fmt.list pp_fail) fails

let get_tests ~warn_empty us =
  let is_test u = B0_unit.(has_tag B0_meta.run u && has_tag B0_meta.test u) in
  let tests = B0_unit.Set.filter is_test us in
  if B0_unit.Set.is_empty tests && warn_empty
  then (Log.warn (fun m -> m "%a" pp_no_tests ()); tests)
  else tests

let show_what ~lock ~may_build ~must_build ~is_locked ~locked_packs c =
  Log.if_error' ~use:B0_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let tests = get_tests ~warn_empty:false must_build in
  Log.app (fun m -> m "%a" pp_run_tests tests);
  B0_cmd_build.show_what ~lock ~may_build ~must_build ~is_locked ~locked_packs c

(* Test command *)

let run_test c build u =
  Log.app (fun m -> m "%a %a" Test_ui.pp_test () B0_unit.pp_name u);
  let exec = B0_unit.find_or_default_meta B0_unit.Exec.key u in
  let b0_env = B0_cmd_build.executor_env build (B0_unit.def u) c in
  let* env = B0_unit.Exec.get_env b0_env u in
  let env = Os.Env.to_assignments env in
  let* cwd = B0_unit.Exec.get_cwd b0_env u in
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
          Fmt.error "No exectuable file found (no %a key)"
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
      Fmt.error "`Fun execution not supported yet"

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

let test units x_units packs x_packs what lock c =
  Log.if_error ~use:B0_cli.Exit.no_such_name @@
  let* (may_build, must_build), is_locked, locked_packs  =
    B0_cmd_build.select_units ~units ~x_units ~packs ~x_packs ~lock
  in
  if what
  then show_what ~lock ~may_build ~must_build ~is_locked ~locked_packs c else
  let tests = get_tests ~warn_empty:true must_build in
  Log.if_error' ~use:B0_driver.Exit.build_error @@
  let store = [] in
  let* build = B0_cmd_build.make_build c ~store ~may_build ~must_build in
  match B0_build.run build with
  | Error () -> Ok B0_driver.Exit.build_error
  | Ok () ->
      let tests = B0_unit.Set.elements tests in
      let dur, fails = run_tests c build Mtime.Span.zero [] tests in
      Log.app (fun m -> m "%a" pp_report (dur, fails));
      Ok (if fails <> [] then exit_test_error else B0_cli.Exit.ok)

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
        `P "The command exits with 1 if one of the tests exits with \
            non-zero. Use option $(b,--what) with a given invocation \
            to understand what builds and which tests are run.";]
  in
  let exit_test_error =
    Cmdliner.Cmd.Exit.info
      (Os.Exit.get_code exit_test_error) ~doc:"If a test did not succeed."
  in
  let exits = exit_test_error :: B0_driver.Exit.infos in
  B0_tool_std.Cli.subcmd_with_b0_file "test" ~exits ~doc ~descr @@
  Term.(const test $ B0_cmd_build.units $ B0_cmd_build.x_units $
        B0_cmd_build.packs $ B0_cmd_build.x_packs $ B0_cmd_build.what $
        B0_cmd_build.lock)
