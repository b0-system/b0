(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let () = Printexc.record_backtrace true

module Test_ui = struct
  let test_color = [`Bg `White; `Fg `Black]
  let fail_color = [`Bg (`Hi `Red); `Fg `Black]
  let pass_color = [`Bg (`Hi `Green); `Fg `Black]
  let padding = "      "
  let pp_test ppf () = (Fmt.tty' test_color) ppf " TEST "
  let pp_fail ppf () = (Fmt.tty' fail_color) ppf " FAIL "
  let pp_pass ppf () = (Fmt.tty' pass_color) ppf " PASS "
  let pp_passed ppf () = (Fmt.tty' [`Fg (`Hi `Green)]) ppf "passed"
  let pp_failed ppf () = (Fmt.tty' [`Fg (`Hi `Red)]) ppf "failed"
  let pp_dur = Fmt.tty [`Bold] Mtime.Span.pp
  let pp_count = Fmt.tty [`Bold] Fmt.int
  let pp_slot_loc ppf l =
    Fmt.pf ppf "File \"%s\", line %d, characters %d-%d"
      l.Printexc.filename l.Printexc.line_number
      l.Printexc.start_char l.Printexc.end_char

  let munge_bt bt = match Printexc.backtrace_slots bt with
  | None -> [Fmt.str "No backtrace. Did you compile with %a ?" Fmt.code' "-g"]
  | Some slots ->
      let rec loop acc = function
      | [] -> List.rev acc
      | s :: ss ->
          match Printexc.Slot.location s with
          | None -> loop acc ss
          | Some ({ filename } as l)  ->
              if String.ends_with ~suffix:"b0_testing.ml" filename
              then loop acc ss
              else loop ((Fmt.str "%a" pp_slot_loc l) :: acc) ss
      in
      loop [] (Array.to_list slots)
end

module Test = struct
  exception Fail of string
  let failf fmt = Fmt.kstr (fun msg -> raise (Fail msg)) fmt

  let log' fmt = Fmt.epr (fmt ^^ "@.")
  let log fmt =
    Fmt.epr ("%a " ^^ fmt ^^ "@.") (Fmt.tty' Test_ui.test_color) Test_ui.padding

  let log_fail fmt =
    Fmt.epr ("%a " ^^ fmt ^^ "@.") (Fmt.tty' Test_ui.fail_color) Test_ui.padding

  let log_bt_msg bt msg =
    log_fail "%s" msg; List.iter (log_fail "%s") (Test_ui.munge_bt bt)

  let log_fail_loc bt = function
  | Fail msg -> log_bt_msg bt msg
  | Assert_failure _ -> log_bt_msg bt "Assertion failed"
  | exn ->
      log_fail "%a" Fmt.exn exn;
      List.iter (log_fail "%s") (Test_ui.munge_bt bt)

  let test_count = ref 0
  let fail_count = ref 0

  let test name f =
    log "Test %s" name;
    incr test_count;
    match f () with
    | () -> ()
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        log_fail_loc bt exn; incr fail_count; ()

  let report_pass ~dur = match !test_count with
  | 0 ->
      log' "@[%a Test %a in %a@]" Test_ui.pp_pass () Test_ui.pp_passed ()
        Test_ui.pp_dur (Os.Mtime.count dur); 0
  | n ->
      log' "@[%a %a %s %a in %a@]" Test_ui.pp_pass ()
        Test_ui.pp_count !test_count
        (if !test_count <= 1 then "test" else "tests")
        Test_ui.pp_passed () Test_ui.pp_dur (Os.Mtime.count dur); 0

  let report_fail ~dur =
    log' "@[%a %a/%a %s %a in %a@]"
      Test_ui.pp_fail ()
      Test_ui.pp_count !fail_count Test_ui.pp_count !test_count
      (if !test_count <= 1 then "test" else "tests")
      Test_ui.pp_failed () Test_ui.pp_dur (Os.Mtime.count dur);
    !fail_count

  let main f =
    let () = Fmt.set_tty_cap () in
    let dur = Os.Mtime.counter () in
    match f () with
    | () -> if !fail_count = 0 then report_pass ~dur else report_fail ~dur
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        log_fail_loc bt exn;
        log' "@[%a A test %a unexpectedly in %a@]"
          Test_ui.pp_fail () Test_ui.pp_failed ()
          Test_ui.pp_dur (Os.Mtime.count dur); 1

  (* Testing Combinators *)

  let rec repeat ~fail n f =
    if n <= 0 then () else match f n with
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        log_fail (fail ^^ ":") n; Printexc.raise_with_backtrace e bt
    | _ -> repeat ~fail (n - 1) f


  let invalid_arg f = match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failf "Expression did not raise %a" Fmt.code' "Invalid_argument"

  let failure f = match f () with
  | exception Failure _ -> ()
  | _ -> failf "Expression did not raise %a" Fmt.code' "Failure"

  let raises exn f = match f () with
  | exception e when exn e -> ()
  | exception e -> raise (Fail "Expression did not raise expected exception")
  | _ -> raise (Fail "Expression did not raise")
end
