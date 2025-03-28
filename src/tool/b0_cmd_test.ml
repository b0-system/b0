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
  Fmt.pf ppf "No unit with tags %a found in the build" pp_test_tags ()

let pp_run_tests ppf tests =
  if B0_unit.Set.is_empty tests then pp_no_tests ppf () else
  Fmt.pf ppf "@[<v>These %a will run:@,%a@]"
    Fmt.(st [`Fg `Green]) "tests"
    Fmt.(list B0_unit.pp_synopsis) (B0_unit.Set.elements tests)

let pp_fail ppf ~allow_long (u, st) =
  Fmt.pf ppf "@[%a %a%a %a %a %a@]"
    (Fmt.st Test.Fmt.fail_color) Test.Fmt.padding
    Fmt.code "b0 test"
    Fmt.code (if allow_long then " -l" else "")
    Fmt.code "-u" B0_unit.pp_name u (Fmt.option Os.Cmd.pp_status) st

let test_unit_msg n = if n <= 1 then "test unit" else "test units"

let pp_report ppf (allow_long, test_count, total, dur, fails) =
  match fails with
  | [] ->
      Fmt.pf ppf "@[<v>%a The build %a all %a %s in %a (+%a for the build)@]"
        Test.Fmt.pass () Test.Fmt.passed ()
        Test.Fmt.count test_count
        (test_unit_msg test_count)
        Test.Fmt.dur dur
        Mtime.Span.pp (Mtime.Span.abs_diff total dur)
  | fails ->
      let count = List.length fails in
      Fmt.pf ppf "@[<v>%a The build %a on %a %s in %a:@,%a@]"
        Test.Fmt.fail () Test.Fmt.failed ()
        Test.Fmt.fail_count_ratio (count, test_count)
        (test_unit_msg test_count)
        Test.Fmt.dur dur (Fmt.list (pp_fail ~allow_long)) fails

let is_test u = B0_unit.(has_tag B0_meta.run u && has_tag B0_meta.test u)
let is_long = B0_unit.has_tag B0_meta.long

let show_what
    ~allow_long ~tests ~lock ~may_build ~must_build ~is_locked ~locked_packs c
  =
  Log.if_error' ~use:Os.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  Log.stdout (fun m -> m "%a@." pp_run_tests tests);
  B0_cmd_build.show_what ~lock ~may_build ~must_build ~is_locked ~locked_packs c

(* Test command *)

let run_test ~long ~seed ~correct c build u =
  Log.stdout (fun m -> m "%a %a" Test.Fmt.test () B0_unit.pp_name u);
  let action = B0_meta.find_or_default B0_unit.Action.key (B0_unit.meta u) in
  let b0_env = B0_cmd_build.env_for_unit c build u in
  let env env =
    let env = Os.Env.add "SEED" (string_of_int seed) env in
    let env = Os.Env.add "LONG_SKIP_EXIT" "true" env in
    let env =
      if correct then Os.Env.add "CORRECT" (string_of_bool correct) env else env
    in
    if long then Os.Env.add "LONG" (string_of_bool long) env else env

  in
  let dur = Os.Mtime.counter () in
  let* st = B0_unit.Action.run ~env b0_env u ~args:Cmd.empty action in
  Ok (Os.Mtime.count dur, st)

let rec run_tests ~long ~seed ~correct c build dur rets = function
| [] -> dur, rets
| u :: us ->
    (* XXX Lots could be improved here, parallel spawns and run as
       soon as the build file is ready. *)
    let log_sep () = Log.stdout (fun m -> m "%s" "") in
    match run_test ~long ~seed ~correct c build u with
    | Error e ->
        Log.stdout (fun m -> m "%a: %s" B0_unit.pp_name u e);
        log_sep ();
        run_tests ~long ~seed ~correct c build dur ((u, None) :: rets) us
    | Ok (tdur, st) ->
        let dur = Mtime.Span.add dur tdur in
        log_sep ();
        run_tests ~long ~seed ~correct c build dur ((u, Some st) :: rets) us

let show_skip_tests us =
  let pp_skipped ppf skipped =
    if skipped = 0 then () else
    Fmt.pf ppf "%s %a long %s %a. Run with %a to execute."
      Test.Fmt.padding
      Test.Fmt.count skipped (test_unit_msg skipped)
      Test.Fmt.skipped () Fmt.code "-l"
  in
  let pp_skip ppf u =
    Fmt.pf ppf "%a Long %a" Test.Fmt.skip () B0_unit.pp_name u
  in
  if B0_unit.Set.is_empty us then () else
  let count = B0_unit.Set.cardinal us in
  Log.stdout @@ fun m ->
  m "@[<v>%a@,%a@,@]" (Fmt.iter B0_unit.Set.iter pp_skip) us pp_skipped count

let show_tests_with_skips us =
  if B0_unit.Set.is_empty us then () else
  let count = B0_unit.Set.cardinal us in
  let pp_skip ppf u =
    Fmt.pf ppf "@[%a %a %a %a@]"
      (Fmt.st Test.Fmt.skip_color) Test.Fmt.padding
      Fmt.code "b0 test"
      Fmt.code "-u" B0_unit.pp_name u
(*    if skipped = 0 then () else
    Fmt.pf ppf "%s %a long %s %a, invoke with %a to run."
      Test.Fmt.padding
      Test.Fmt.count skipped (test_unit_msg skipped)
      Test.Fmt.skipped () Fmt.code "-l" *)
  in
  let pp_skip_stats ppf count =
    Fmt.pf ppf "%a The build had %a %s with %a long tests:"
      Test.Fmt.skip () Test.Fmt.count count (test_unit_msg count)
      Test.Fmt.skipped ()
  in
  let pp_with_l ppf () =
    Fmt.pf ppf "%a Run with %a to execute them."
      (Fmt.st Test.Fmt.skip_color) Test.Fmt.padding Fmt.code "-l"
  in
  Log.stdout @@ fun m ->
  m "@[<v>%a@,%a@,%a@,@]"
    pp_skip_stats count (Fmt.iter B0_unit.Set.iter pp_skip) us pp_with_l ()

let test
    allow_long allow_empty seed correct units x_units packs x_packs what lock c
  =
  let total = Os.Mtime.counter () in
  Log.if_error ~use:Os.Exit.no_such_name @@
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false units in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:false packs in
  let units, packs =
    if units = [] && packs = [] then B0_cmd_build.get_default_build () else
    units, packs
  in
  let* x_units = B0_cli.get_excluded_units ~x_units ~x_packs in
  let tests, skip_tests =
    let packs = B0_pack.Set.of_list packs in
    let us = B0_cmd_build.unit_set_of ~units ~packs in
    let us = B0_unit.Set.diff us x_units in
    let tests = B0_unit.Set.filter is_test us in
    if allow_long then tests, B0_unit.Set.empty else
    let long_tests = B0_unit.Set.filter is_long tests in
    B0_unit.Set.diff tests long_tests, long_tests
  in
  let is_action u = B0_unit.Set.mem u tests in
  let store, units, locked_packs =
    B0_cmd_build.get_must_units_and_locked_packs
      ~is_action ~units ~packs ~args:[] ()
  in
  let is_locked = B0_cmd_build.is_locked ~lock ~locked_packs in
  let may_build, must_build =
    B0_cmd_build.get_may_must ~is_locked ~units ~x_units
  in
  if what
  then
    show_what ~allow_long ~lock ~may_build ~must_build ~is_locked
      ~locked_packs ~tests c
  else
  Log.if_error' ~use:B0_driver.Exit.build_error @@
  if B0_unit.Set.is_empty tests && not allow_empty
  then begin
    let () = show_skip_tests skip_tests in
    Log.err (fun m ->
        m "@[<v>%a.@,Use option %a to succeed anyways.@]" pp_no_tests ()
          Fmt.code "-e");
    Ok exit_no_tests
  end else
  let store = [] in
  let* build = B0_cmd_build.make_build c ~store ~may_build ~must_build in
  match B0_build.run build with
  | Error () -> Ok B0_driver.Exit.build_error
  | Ok () ->
      let test_count = B0_unit.Set.cardinal tests in
      let tests = B0_unit.Set.elements tests in
      let seed = match seed with
      | Some seed -> seed
      | None -> Random.State.bits (Random.State.make_self_init ())
      in
      let dur, rets =
        run_tests
          ~long:allow_long ~seed ~correct c build Mtime.Span.zero [] tests
      in
      let fails, tests_with_skips =
        let rec loop fails skip_tests = function
        | [] -> List.rev fails, skip_tests
        | (u, Some (`Exited 0)) :: us ->
            loop fails skip_tests us
        | (u, Some (`Exited 99)) :: us ->
            loop fails (B0_unit.Set.add u skip_tests) us
        | fail :: us -> loop (fail :: fails) skip_tests us
        in
        loop [] skip_tests rets
      in
      let () = show_tests_with_skips tests_with_skips in
      let () = show_skip_tests skip_tests in
      Log.stdout (fun m ->
          m "%a" pp_report (allow_long,
                            test_count, Os.Mtime.count total, dur, fails));
      Ok (if fails <> [] then exit_test_error else Os.Exit.ok)

(* Command line interface *)

open Cmdliner

let s_test_options = "OPTIONS FOR RUNNING TESTS"

let cmd =
  let doc = "Build and run tests" in
  let descr = `Blocks
      [ `P "The $(iname) command builds and runs tests.";
        `Pre "$(mname) $(b,list --tests)     # List all tests"; `Noblank;
        `Pre "$(iname)             # Run all tests"; `Noblank;
        `Pre "$(iname) $(b,-l)          # Run all tests including long ones";
        `Noblank;
        `Pre "$(iname) $(b,-u mytest)   # Only run test of unit $(b,mytest)";
        `Noblank;
        `Pre "$(iname) $(b,--seed 123)  # Set env $(b,SEED=123) for running";
        `Noblank;
        `Pre "$(iname) $(b,--correct)   # Set env $(b,CORRECT=true) for \
              running";
        `P "A test is a unit tagged with both $(b,B0_meta.test) and \
            $(b,B0_meta.run). The command builds like \
            $(b,b0 build) does and then runs all the tests that are in \
            the units that $(b,must) build. Use option $(b,--what) with \
            a given invocation to understand what builds and which tests \
            are run.";
        `P "The command exits with 1 if one of the tests exits with \
            non-zero. The exit code 2 of tests is interpreted as success but \
            means that long tests having been skipped.";
        `P "Tests that are tagged with $(b,B0_meta.long) are only run \
            if the $(b,--long) option is used. This also sets the environment \
            variable $(b,LONG) to $(b,true) for running tests.";
        `P "If the option $(b,--seed) $(i,NUM) is specified the environment \
            variable $(b,SEED) is set to $(i,NUM) for running tests. This \
            value should be used by randomized tests for seeding the \
            pseudorandom number generator (PRNG).";
        `P "If the option $(b,--correct) is specified the environment variable \
            $(b,CORRECT) is set to $(b,true) for running tests. This should \
            indicate that expected snapshots test mismatches must be updated \
            to the snapshots cmoputed during the program run.";
        `P "The environment variable $(b,LONG_SKIP_EXIT) is set to $(b,true) \
            for running tests.";
        `S s_test_options;
      ]
  in
  let exits =
    Cmdliner.Cmd.Exit.info
      (Os.Exit.get_code exit_test_error) ~doc:"If a test did not succeed." ::
    Cmdliner.Cmd.Exit.info
      (Os.Exit.get_code exit_no_tests) ~doc:"If there are no tests to run." ::
    B0_driver.Exit.infos
  in
  let docs = s_test_options in
  let allow_empty =
    let doc = "Do not fail if there is no test to run in the build." in
    Arg.(value & flag & info ["e"; "allow-empty"] ~doc ~docs)
  in
  let long =
    let doc =
      "Run long tests. By default tests tagged with $(b,B0_meta.long) are \
       not run. Also set environment variable $(b,LONG) to $(b,true) for \
       running tests."
    in
    Arg.(value & flag & info ["l";"long"] ~doc ~docs)
  in
  let rand_seed =
    let doc =
      "Set environment variable $(b,SEED) to $(docv) for running tests."
    in
    let absent = "Randomly generated value" in
    let docv = "INT" in
    Arg.(value & opt (some int) None & info ["seed"] ~doc ~docv ~absent ~docs)
  in
  let correct =
    let doc =
      "Set environment variable $(b,CORRECT) to $(b,true) for running tests."
    in
    Arg.(value & flag & info ["c"; "correct"] ~doc ~docs)
  in
  B0_tool.Cli.subcmd_with_b0_file "test" ~exits ~doc ~descr @@
  Term.(const test $ long $ allow_empty $ rand_seed $ correct $
        B0_cmd_build.units $
        B0_cmd_build.x_units $ B0_cmd_build.packs $ B0_cmd_build.x_packs $
        B0_cmd_build.what $ B0_cmd_build.lock)
