(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let () = Printexc.record_backtrace true
let out = ref Format.err_formatter

module Test_fmt = struct
  let test_color = [`Bg `White; `Fg `Black]
  let fail_color = [`Bg (`Hi `Red); `Fg `Black]
  let pass_color = [`Bg (`Hi `Green); `Fg `Black]
  let skip_color = [`Bg (`Hi `Yellow); `Fg `Black]
  let padding = "      "
  let pp_test ppf () = (Fmt.st test_color) ppf " TEST "
  let pp_fail ppf () = (Fmt.st fail_color) ppf " FAIL "
  let pp_pass ppf () = (Fmt.st pass_color) ppf " PASS "
  let pp_skip ppf () = (Fmt.st skip_color) ppf " SKIP "
  let pp_passed ppf () = (Fmt.st [`Fg (`Hi `Green)]) ppf "passed"
  let pp_failed ppf () = (Fmt.st [`Fg (`Hi `Red)]) ppf "failed"
  let pp_skipped ppf () = (Fmt.st [`Fg (`Hi `Yellow)]) ppf "skipped"
  let pp_dur = Fmt.st' [`Bold] Mtime.Span.pp
  let pp_count = Fmt.st' [`Bold] Fmt.int
  let pp_slot_loc ppf l =
    Fmt.pf ppf "File \"%s\", line %d, characters %d-%d"
      l.Printexc.filename l.Printexc.line_number
      l.Printexc.start_char l.Printexc.end_char

  let pp_pos ppf (file, lnum, cnum, enum) =
    Fmt.pf ppf "File %S, line %d, characters %d-%d:@," file lnum cnum enum

  let munge_bt bt = match Printexc.backtrace_slots bt with
  | None -> [Fmt.str "No backtrace. Did you compile with %a ?" Fmt.code "-g"]
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
  open Test_fmt

  type pos = string * int * int * int

  exception Fail of pos option * string

  let fail ?__POS__:pos msg = raise (Fail (pos, msg))
  let failf ?__POS__:pos fmt = Fmt.kstr (fun msg -> raise (Fail (pos, msg))) fmt

  let log' fmt = Fmt.pf !out (fmt ^^ "@.")
  let log fmt = Fmt.pf !out ("%a " ^^ fmt ^^ "@.") (Fmt.st test_color) padding
  let log_fail fmt =
    Fmt.pf !out ("%a " ^^ fmt ^^ "@.") (Fmt.st fail_color) padding

  let log_bt_msg bt pos msg = match pos with
  | None -> log_fail "%s" msg; List.iter (log_fail "%s") (munge_bt bt)
  | Some pos -> log_fail "%s" msg; log_fail "%a" pp_pos pos

  let log_fail_loc bt = function
  | Fail (pos, msg) -> log_bt_msg bt pos msg
  | Assert_failure _ -> log_bt_msg bt None "Assertion failed"
  | exn ->
      log_fail "%a" Fmt.exn exn;
      List.iter (log_fail "%s") (munge_bt bt)

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
      log' "@[%a Test %a in %a@]" pp_pass () pp_passed ()
        pp_dur (Os.Mtime.count dur); 0
  | n ->
      log' "@[%a %a %s %a in %a@]" pp_pass ()
        pp_count !test_count
        (if !test_count <= 1 then "test" else "tests")
        pp_passed () pp_dur (Os.Mtime.count dur); 0

  let report_fail ~dur =
    log' "@[%a %a/%a %s %a in %a@]"
      pp_fail () pp_count !fail_count pp_count !test_count
      (if !fail_count <= 1 then "test" else "tests")
      pp_failed () pp_dur (Os.Mtime.count dur);
    1

  let setup () = match Sys.backend_type with
  | Other "js_of_ocaml" ->
      out := Format.std_formatter; Fmt.set_tty_cap ~cap:`Ansi ()
  | _ -> Fmt.set_tty_cap ()

  let main f =
    let () = setup () in
    let dur = Os.Mtime.counter () in
    match f () with
    | () -> if !fail_count = 0 then report_pass ~dur else report_fail ~dur
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        log_fail_loc bt exn;
        log' "@[%a A test %a unexpectedly in %a@]"
          pp_fail () pp_failed () pp_dur (Os.Mtime.count dur); 1

  (* Testing Combinators *)

  let rec repeat ~fail n f =
    if n <= 0 then () else match f n with
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        log_fail (fail ^^ ":") n; Printexc.raise_with_backtrace e bt
    | _ -> repeat ~fail (n - 1) f

  let invalid_arg f = match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failf "Expression did not raise %a" Fmt.code "Invalid_argument"

  let failure f = match f () with
  | exception Failure _ -> ()
  | _ -> failf "Expression did not raise %a" Fmt.code "Failure"

  let raises exn f = match f () with
  | exception e when exn e -> ()
  | exception e ->
      raise (Fail (None, "Expression did not raise expected exception"))
  | _ -> raise (Fail (None, "Expression did not raise"))

  let is_ok ?__POS__ = function
  | Ok _ -> ()
  | Error e -> failf ?__POS__ "@[<v>Exp: Ok _@,Fnd: Error %S@]" e

  let is_error = function
  | Ok _ -> failf "@[<v>Fnd: Ok _@,Exp: Error _@]" | Error _ -> ()

  let is_error' ?msg r = match msg with
  | None -> is_error r
  | Some msg ->
      match r with
      | Error m when String.equal msg m -> ()
      | Error m -> failf "@[<v>Fnd: Error %S@,Exp: Error %S@]" m msg
      | Ok _ -> failf "Fnd: Ok _@,Exp: Error %S@]" msg

  (* Testing values *)

  let bool ?__POS__ b0 b1 =
    if Bool.equal b0 b1 then () else failf ?__POS__ "@[%b <> %b@]" b0 b1

  let int ?__POS__ i0 i1 =
    if Int.equal i0 i1 then () else failf ?__POS__ "@[%d <> %d@]" i0 i1

  let float ?__POS__ f0 f1 =
    if Float.equal f0 f1 then () else failf ?__POS__ "@[%g <> %g@]" f0 f1

  let string ?__POS__ s0 s1 =
    if String.equal s0 s1 then () else failf ?__POS__ "@[<hov>%S <>@ %S@]" s0 s1

  let bytes ?__POS__ b0 b1 =
    string ?__POS__ (Bytes.unsafe_to_string b0) (Bytes.unsafe_to_string b1)

  module type EQ = sig
    type t
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  let eq ?__POS__ (type a) (module M : EQ with type t = a) v0 v1 =
    if M.equal v0 v1 then () else
    failf ?__POS__ "@[<hov>%a <>@ %a@]" M.pp v0 M.pp v1

  let neq ?__POS__ (type a) (module M : EQ with type t = a) v0 v1 =
    if not (M.equal v0 v1) then () else
    failf ?__POS__ "@[<hov>%a <>@ %a@]" M.pp v0 M.pp v1
end
