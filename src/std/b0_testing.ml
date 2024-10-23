(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let array_equal equal a0 a1 = (* 5.2, still no Array.equal (?!) *)
  let l0 = Array.length a0 and l1 = Array.length a1 in
  Int.equal l0 l1 &&
  try
    for i = 0 to l0 - 1 do
      if not (equal (Array.get a0 i) (Array.get a1 i)) then raise Exit
    done;
    true
  with Exit -> false

let () = Printexc.record_backtrace true
let out = ref Format.std_formatter

module Test_fmt = struct
  let padding = "      "
  let test_color = [`Bg `White; `Fg `Black]
  let fail_color = [`Bg (`Hi `Red); `Fg `Black]
  let pass_color = [`Bg (`Hi `Green); `Fg `Black]
  let skip_color = [`Bg (`Hi `Yellow); `Fg `Black]
  let test ppf () = (Fmt.st test_color) ppf " TEST "
  let fail ppf () = (Fmt.st fail_color) ppf " FAIL "
  let pass ppf () = (Fmt.st pass_color) ppf " PASS "
  let skip ppf () = (Fmt.st skip_color) ppf " SKIP "
  let passed ppf () = (Fmt.st [`Fg (`Hi `Green)]) ppf "passed"
  let failed ppf () = (Fmt.st [`Fg (`Hi `Red)]) ppf "failed"
  let skipped ppf () = (Fmt.st [`Fg (`Hi `Yellow)]) ppf "skipped"
  let dur = Fmt.st' [`Bold] Mtime.Span.pp
  let count = Fmt.st' [`Bold] Fmt.int
  let count_ratio ppf (c, t) = count ppf c; Fmt.char ppf '/'; count ppf t
  let slot_loc ppf l =
    Fmt.pf ppf "File \"%s\", line %d, characters %d-%d"
      l.Printexc.filename l.Printexc.line_number
      l.Printexc.start_char l.Printexc.end_char

  let pos ppf (file, lnum, cnum, enum) =
    Fmt.pf ppf "File %S, line %d, characters %d-%d:" file lnum cnum enum

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
              else loop ((Fmt.str "%a" slot_loc l) :: acc) ss
      in
      loop [] (Array.to_list slots)

  let exn ppf e = Fmt.string ppf (Printexc.to_string e)

  let text_string ppf s = Fmt.pf ppf "%S" s (* TODO keep UTF-8  *)
  let hex_string =
    let pp_byte ppf c = Fmt.pf ppf "%02x" (Char.code c) in
    Fmt.iter String.iter pp_byte

  let list pp_v ppf l =
    let pp_sep ppf () = Fmt.pf ppf ";@ " in
    Fmt.pf ppf "@[<1>[%a]@]" (Format.pp_print_list ~pp_sep pp_v) l

  let array pp_v ppf a =
    (* Once 5.1 is required use pp_print_array *)
    let pp_sep ppf () = Fmt.pf ppf ";@ " in
    let l = Array.to_list a in
    Fmt.pf ppf "@[<1>[|%a|]@]" (Format.pp_print_list ~pp_sep pp_v) l

  let anon ppf _ = Fmt.string ppf "_"

  let result ~ok ~error ppf = function
  | Ok v -> Fmt.pf ppf "Ok %a" ok v
  | Error e -> Fmt.pf ppf "Error %a" error e

  let option pp_v ppf = function
  | None -> Fmt.string ppf "None"
  | Some v -> Fmt.pf ppf "Some %a" pp_v v
end

module Test = struct
  exception Stop
  let stop ?__POS__ () = raise Stop

  type pos = string * int * int * int

  let log' fmt = Fmt.pf !out (fmt ^^ "@.")

  let _log fmt =
    let pad = (Fmt.st Test_fmt.test_color) in
    Fmt.pf !out ("%a @[" ^^ fmt ^^ "@]@.") pad Test_fmt.padding

  let _log_fail fmt =
    let pad = Fmt.st Test_fmt.fail_color in
    Fmt.pf !out ("%a @[" ^^ fmt ^^ "@]@.") pad Test_fmt.padding

  let _klog_fail k fmt =
    let pad = Fmt.st Test_fmt.fail_color in
    Fmt.kpf k !out ("%a @[" ^^ fmt ^^ "@]@.") pad Test_fmt.padding

  let log ?__POS__:pos fmt = match pos with
  | None -> _log fmt
  | Some pos -> _log "%a" Test_fmt.pos pos; _log fmt

  let log_fail ?__POS__:pos fmt = match pos with
  | None -> _log_fail fmt
  | Some pos -> _log_fail "%a" Test_fmt.pos pos; _log_fail fmt

  let klog_fail ?__POS__:pos k fmt = match pos with
  | None -> _klog_fail k fmt
  | Some pos -> _log_fail "%a" Test_fmt.pos pos; _klog_fail k fmt

  let log_exn_fail_loc bt = function
  | Assert_failure _ ->
      _log_fail "%s" "Assertion failed";
      List.iter (_log_fail "%s") (Test_fmt.munge_bt bt)
  | exn ->
      _log_fail "%a raised" (Fmt.code' Fmt.exn) exn;
      List.iter (_log_fail "%s") (Test_fmt.munge_bt bt)

  (* Testing state *)

  let test_count = ref 0
  let test_fail_count = ref 0

  let pass_count = ref 0
  let fail_count =
    (* Note when we can afford effects we should move this to test.
       Basically all fail combinators (except failstop) raise an
       effect to increment and we continue. *)
    ref 0
  let pass ?__POS__ () = incr pass_count
  let fail' () = incr fail_count

  module Rand = struct
    let env_test_seed = "TEST_SEED"
    let rseed = ref None
    let rstate = ref None
    let rec init_random_state seed = match seed with
    | None ->
        (* auto-seed *)
        let s = Random.State.make_self_init () in
        init_random_state (Some (Random.State.bits s))
    | Some seed ->
        log "Using random seed %a" Fmt.(code' int) seed;
        rseed := Some seed;
        rstate := Some (Random.State.make [| seed |])

    let rec state () = match !rstate with
    | Some s -> s
    | None ->
        match Os.Env.find ~empty_is_none:true env_test_seed with
        | None -> init_random_state None; state ()
        | Some i ->
            match int_of_string_opt i with
            | Some _ as seed -> init_random_state seed; state ()
            | None ->
                log_fail "%s: can't parse integer from TEST_SEED, ignoring." i;
                init_random_state None; state ()


    let seed_opt =
      "--seed", Arg.Int (fun i -> init_random_state (Some i)),
      "<nat> random seed to use (auto-seeded or TEST_SEED by default)."
  end

  module Cli = struct
    let parse ?(opts = []) () =
      let exec = Filename.basename Sys.executable_name in
      let usage = Printf.sprintf "Usage: %s <options>\nOptions:" exec in
      let anon _ = raise (Arg.Bad "positional arguments unsupported") in
      Arg.parse (opts @ [Rand.seed_opt]) anon usage;
  end

  (* Let's resist a little longer doing that and try to only
     control this at the unit level.

     The idea here was to add a ~long optional argument to [test].

  let env_test_long = "TEST_LONG"
  let long_request = ref false
  let long = ref false
  let rec get_long () =
    if !long_request then !long else
    match Os.Env.find ~empty_is_none:true env_test_long with
    | None -> long := false; !long
    | Some "true" -> long := false; !long
    | Some "false" -> long := false; !long
    | Some l ->
        log_fail "%s: expected %a or %a when parsing %a, ignoring."
          l Fmt.code "true" Fmt.code "false" Fmt.code env_test_long;
        !long

     let is_long () = let l = get_long () in long_request := true; l
     *)

  (* Passing and failling *)

  let fail ?__POS__ fmt = fail' (); log_fail ?__POS__ fmt
  let failstop ?__POS__ fmt =
    fail' ();  klog_fail ?__POS__ (fun _ -> stop ()) fmt

  let test ?__POS__:_ name f =
    log "Test %s" name;
    incr test_count;
    pass_count := 0;
    fail_count := 0;
    begin match f () with
    | () -> ()
    | exception Stop -> ()
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        log_exn_fail_loc bt exn;
        fail' ();
    end;
    if !fail_count <> 0 then incr test_fail_count

  let report_pass ~dur = match !test_count with
  | 0 ->
      log' "@[%a Test %a in %a@]" Test_fmt.pass () Test_fmt.passed ()
        Test_fmt.dur (Os.Mtime.count dur); 0
  | n ->
      log' "@[%a %a %s %a in %a@]" Test_fmt.pass ()
        Test_fmt.count !test_count
        (if !test_count <= 1 then "test" else "tests")
        Test_fmt.passed () Test_fmt.dur (Os.Mtime.count dur); 0

  let report_fail ~dur =
    log' "@[%a %a/%a %s %a in %a@]"
      Test_fmt.fail () Test_fmt.count !test_fail_count
      Test_fmt.count !test_count
      (if !test_fail_count <= 1 then "test" else "tests")
      Test_fmt.failed () Test_fmt.dur (Os.Mtime.count dur);
    1

  let main ?__POS__:_ f =
    let finish dur =
      if !test_fail_count = 0 && !fail_count = 0
      then report_pass ~dur else report_fail ~dur
    in
    let dur = Os.Mtime.counter () in
    match f () with
    | () -> finish dur
    | exception Stop -> finish dur
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        log_exn_fail_loc bt exn;
        log' "@[%a A test %a unexpectedly in %a@]"
          Test_fmt.fail () Test_fmt.failed () Test_fmt.dur
          (Os.Mtime.count dur); 1

  (* Loops *)

  let rec range ?(kind = "Test") ~first ~last ?__POS__:pos f =
    let log_fail ?__POS__ ~kind ~first ~last n =
      log_fail ?__POS__ "%s in range [%d;%d] failed on %a" kind first last
        (Fmt.code' Fmt.int) n
    in
    let rec loop pos ~kind ~first ~last n f =
      if n > last then () else
      let before_fail_count = !fail_count in
      match f n with
      | exception Stop ->
          if before_fail_count <> !fail_count
          then log_fail ?__POS__:pos ~kind ~first ~last n
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          log_exn_fail_loc bt exn;
          log_fail ?__POS__:pos ~kind ~first ~last n;
          loop pos ~kind ~first ~last (n + 1) f
      | () ->
          if before_fail_count <> !fail_count
          then log_fail ?__POS__:pos ~kind ~first ~last n;
          loop pos ~kind ~first ~last (n + 1) f
    in
    loop pos ~kind ~first ~last first f

  let block ?fail ?__POS__:pos f =
    let before_pass_count = !pass_count in
    let before_fail_count = !fail_count in
    let finish () =
      let fail_diff = !fail_count - before_fail_count in
      if fail_diff = 0 then () else
      let checks = !pass_count - before_pass_count + fail_diff in
      match fail with
      | Some fail -> fail ?__POS__:pos fail_diff ~checks
      | None ->
          log_fail ?__POS__:pos
            "Block %a %a checks" Test_fmt.failed ()
            Test_fmt.count_ratio (fail_diff, checks)
    in
    begin match f () with
    | exception Stop -> ()
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        log_exn_fail_loc bt exn;
    | () -> ()
    end;
    finish ()

  let don't_stop f = try f () with
  | Stop -> ()

  (* Test combinators *)

  let failneq ?__POS__ pp v0 v1 =
    fail ?__POS__ "@[<hov>%a <>@ %a@]" (Fmt.code' pp) v0 (Fmt.code' pp) v1

  (* Assertions *)

  let holds ?msg ?__POS__ b =
    if b then pass () else match msg with
    | None -> fail ?__POS__ "Failed assertion"
    | Some msg -> fail ?__POS__ "@[<v>Failed assertion:@,%s@]" msg

  (* Exceptions *)

  let raises exn ?__POS__ f = match f () with
  | exception e when e = exn -> pass ()
  | exception e -> failneq ?__POS__ Test_fmt.exn e exn
  | _ -> fail ?__POS__ "@[No exception raised, expected@ %a@]" Test_fmt.exn exn

  let raises' is_exn ?__POS__ f = match f () with
  | exception e when is_exn e -> pass ()
  | exception e ->
      fail ?__POS__
        "Exception %a does not match expected exception" Test_fmt.exn e
  | _ -> fail ?__POS__ "@[No exception raised, expecting one@]"

  let invalid_arg ?__POS__ f = match f () with
  | exception Invalid_argument _ -> pass ()
  | _ -> fail ?__POS__ "Expression did not raise %a" Fmt.code "Invalid_argument"

  let failure ?__POS__ f = match f () with
  | exception Failure _ -> pass ()
  | _ -> fail ?__POS__ "Expression did not raise %a" Fmt.code "Failure"

  (* Values *)

  module Eq = struct
    module type T = sig
      type t
      val equal : t -> t -> bool
      val pp : Format.formatter -> t -> unit
    end

    type 'a t = (module T with type t = 'a)

    let make (type a) ?(equal = ( = )) ?(pp = Test_fmt.anon) () : a t =
      (module (struct type t = a let equal = equal let pp = pp end))

    let true' (type a) : a t =
      (module struct type t = a let equal _ _ = true let pp = Test_fmt.anon end)

    let false' (type a) : a t =
      (module struct
        type t = a let equal _ _ = false let pp = Test_fmt.anon end)

    let any (type a) : a t =
      (module struct type t = a let equal = ( = ) let pp = Test_fmt.anon end)

    let bool : bool t =
      (module struct type t = bool let equal = Bool.equal let pp = Fmt.bool end)

    let char : char t =
      (module struct
        type t = char let equal = Char.equal let pp = Fmt.ascii_char end)

    let int : int t =
      (module struct type t = int let equal = Int.equal let pp = Fmt.int end)

    let int32 : int32 t =
      (module struct
        type t = int32
        let equal = Int32.equal
        let pp ppf v = Fmt.pf ppf "%ld" v end)

    let uint32 : int32 t =
      (module struct
        type t = int32
        let equal = Int32.equal
        let pp ppf v = Fmt.pf ppf "%lu" v end)

    let int64 : int64 t =
      (module struct
        type t = int64
        let equal = Int64.equal
        let pp ppf v = Fmt.pf ppf "%Ld" v end)

    let uint64 : int64 t =
      (module struct
        type t = int64
        let equal = Int64.equal
        let pp ppf v = Fmt.pf ppf "%Lu" v end)

    let nativeint : nativeint t =
      (module struct
        type t = nativeint
        let equal = Nativeint.equal
        let pp ppf v = Fmt.pf ppf "%nu" v end)

    let float : float t =
      (module struct
        type t = float
        let equal = Float.equal
        let pp ppf v = Fmt.pf ppf "%g" v end)

    let string : string t =
      (module struct
        type t = string
        let equal = String.equal
        let pp = Test_fmt.text_string end)

    let binary_string : string t =
      (module struct
        type t = string
        let equal = String.equal
        let pp = Test_fmt.hex_string end)

    let bytes : bytes t =
      (module struct
        type t = bytes
        let equal = Bytes.equal
        let pp ppf v = Test_fmt.hex_string ppf (Bytes.unsafe_to_string v) end)

    let list (type a) (module E : T with type t = a) : a list t =
      (module struct
        type t = a list
        let equal l0 l1 = List.equal E.equal l0 l1
        let pp ppf l = Test_fmt.list E.pp ppf l end)

    let array (type a) (module E : T with type t = a) : a array t =
      (module struct
        type t = a array
        let equal a0 a1 = array_equal E.equal a0 a1
        let pp ppf a = Test_fmt.array E.pp ppf a end)

    let option (type a) (module V : T with type t = a) : a option t =
      (module struct
        type t = a option
        let equal o0 o1 = Option.equal V.equal o0 o1
        let pp ppf o = Test_fmt.option V.pp ppf o end)

    let result
        (type a) ~ok:(module V : T with type t = a)
        (type e) ~error:(module E : T with type t = e) : (a, e) result t =
      (module struct
        type t = (a, e) result
        let equal r0 r1 = Result.equal ~ok:V.equal ~error:E.equal r0 r1
        let pp ppf r = Test_fmt.result ~ok:V.pp ~error:E.pp ppf r end)
  end

  let eq (type a) (module V : Eq.T with type t = a) ?__POS__ v0 v1 =
    if V.equal v0 v1
    then pass ()
    else failneq ?__POS__ V.pp v0 v1

  let neq (type a) (module V : Eq.T with type t = a) ?__POS__ v0 v1 =
    if not (V.equal v0 v1)
    then pass ()
    else fail ?__POS__ "@[<hov>%a =@ %a@]" V.pp v0 V.pp v1

  let bool ?__POS__ v0 v1 = eq ?__POS__ Eq.bool v0 v1
  let char ?__POS__ c0 c1 = eq ?__POS__ Eq.char c0 c1
  let int ?__POS__ i0 i1 = eq ?__POS__ Eq.int i0 i1
  let int32 ?__POS__ i0 i1 = eq ?__POS__ Eq.int32 i0 i1
  let uint32 ?__POS__ i0 i1 = eq ?__POS__ Eq.uint32 i0 i1
  let int64 ?__POS__ i0 i1 = eq ?__POS__ Eq.int64 i0 i1
  let uint64 ?__POS__ i0 i1 = eq ?__POS__ Eq.uint64 i0 i1
  let nativeint ?__POS__ i0 i1 = eq ?__POS__ Eq.nativeint i0 i1
  let float ?__POS__ f0 f1 = eq ?__POS__ Eq.float f0 f1
  let string ?__POS__ s0 s1 = eq ?__POS__ Eq.string s0 s1
  let binary_string ?__POS__ s0 s1 = eq ?__POS__ Eq.binary_string s0 s1
  let bytes ?__POS__ b0 b1 = eq ?__POS__ Eq.bytes b0 b1
  let list
      (type e) ?elt:((module E : Eq.T with type t = e) = Eq.any) ?__POS__ l0 l1
    =
    if List.equal E.equal l0 l1 then pass () else
    failneq ?__POS__ (Test_fmt.list E.pp) l0 l1

  let array
      (type e) ?elt:((module E : Eq.T with type t = e) = Eq.any) ?__POS__ a0 a1
    =
    if array_equal E.equal a0 a1
    then pass ()
    else failneq ?__POS__ (Test_fmt.array E.pp) a0 a1

  (* Options *)

  let get_some ?__POS__ = function
  | Some v -> pass (); v
  | None -> fail ?__POS__ "@[Expected Some _ Found: None@]"; stop ()

  let option
      (type a) ?some:((module V : Eq.T with type t = a) = Eq.any) ?__POS__ v0 v1
    =
    if Option.equal V.equal v0 v1
    then pass ()
    else failneq ?__POS__ (Test_fmt.option V.pp) v0 v1

  (* Results *)

  let get_ok ?__POS__ = function
  | Ok v -> pass (); v
  | Error e -> fail ?__POS__ "@[<v>Expected Ok _@,Found: Error %S@]" e; stop ()

  let result
      (type a) ?ok:((module V : Eq.T with type t = a) = Eq.any) ?__POS__ v0 v1
    =
    if Result.equal ~ok:V.equal ~error:String.equal v0 v1
    then pass ()
    else failneq ?__POS__ (Test_fmt.result ~ok:V.pp ~error:String.pp) v0 v1

  let result'
      (type a e)
      ?ok:((module V : Eq.T with type t = a) = Eq.any)
      ?error:((module E : Eq.T with type t = e) = Eq.any) ?__POS__ r0 r1
    =
    if Result.equal ~ok:V.equal ~error:E.equal r0 r1
    then pass ()
    else failneq ?__POS__ (Test_fmt.result ~ok:V.pp ~error:E.pp) r0 r1

  module Fmt = Test_fmt
end
