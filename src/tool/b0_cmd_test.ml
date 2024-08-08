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

let is_test ~allow_long u =
  B0_unit.(has_tag B0_meta.run u && has_tag B0_meta.test u &&
           (allow_long || not (has_tag B0_meta.long u)))

let show_what
    ~allow_long ~tests ~lock ~may_build ~must_build ~is_locked ~locked_packs c
  =
  Log.if_error' ~use:Os.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  Log.app (fun m -> m "%a" pp_run_tests tests);
  B0_cmd_build.show_what ~lock ~may_build ~must_build ~is_locked ~locked_packs c

(* Test command *)

let run_test c build u =
  Log.app (fun m -> m "%a %a" Test_fmt.pp_test () B0_unit.pp_name u);
  let action = B0_meta.find_or_default B0_unit.Action.key (B0_unit.meta u) in
  let b0_env = B0_cmd_build.env_for_unit c build u in
  let dur = Os.Mtime.counter () in
  let* st = B0_unit.Action.run b0_env u ~args:Cmd.empty action in
  Ok (Os.Mtime.count dur, st)

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
  let* units = B0_unit.get_list_or_hint ~all_if_empty:false units in
  let* packs = B0_pack.get_list_or_hint ~all_if_empty:false packs in
  let units, packs =
    if units = [] && packs = [] then B0_cmd_build.get_default_build () else
    units, packs
  in
  let* x_units = B0_cmd_build.get_excluded_units ~x_units ~x_packs in
  let tests =
    let packs = B0_pack.Set.of_list packs in
    let us = B0_cmd_build.unit_set_of ~units ~packs in
    let us = B0_unit.Set.diff us x_units in
    (B0_unit.Set.filter (is_test ~allow_long) us)
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
  B0_tool.Cli.subcmd_with_b0_file "test" ~exits ~doc ~descr @@
  Term.(const test $ long $ allow_empty $ B0_cmd_build.units $
        B0_cmd_build.x_units $ B0_cmd_build.packs $ B0_cmd_build.x_packs $
        B0_cmd_build.what $ B0_cmd_build.lock)
