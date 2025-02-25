(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let array_equal equal a0 a1 =
  (* 5.3, still no Array.equal (?!)
     https://github.com/ocaml/ocaml/pull/13836 *)
  let l0 = Array.length a0 and l1 = Array.length a1 in
  Int.equal l0 l1 &&
  try
    for i = 0 to l0 - 1
    do if not (equal (Array.get a0 i) (Array.get a1 i)) then raise Exit done;
    true
  with Exit -> false

module Text = struct
  let find_newline ~nth s =
    (* Returns the byte position after the [nth] newline (if any).
       Note, also work on \r\n delimited lines *)
    let rec loop nth ~start s =
      if nth = 0 then Some start else
      match String.index_from_opt s start '\n' with
      | None -> None
      | Some nl_pos -> loop (nth - 1) ~start:(nl_pos + 1) s
    in
    loop nth ~start:0 s

  let find_col ~start s = (* zero based column of [start] on the line. *)
    let i = ref start in
    while (!i >= 0 && s.[!i] <> '\n') do decr i done;
    if !i < 0 then start else start - !i

  let teststr count = if count <= 1 then "test" else "tests"
end

module Test_fmt = struct
  let padding  = "      "
  let long_str = " LONG "
  let pass_str = " PASS "
  let skip_str = " SKIP "
  let fail_str = " FAIL "
  let test_color = [`Bg `White; `Fg `Black]
  let fail_color = [`Bg `Red_bright; `Fg `Black]
  let pass_color = [`Bg `Green_bright; `Fg `Black]
  let skip_color = [`Bg `Yellow_bright; `Fg `Black]
  let neq ppf () = Fmt.st [`Fg `Red_bright; `Bold] ppf "<>"
  let eq ppf () = Fmt.st [`Fg `Red_bright; `Bold] ppf "="
  let test ppf () = Fmt.st test_color ppf " TEST "
  let fail ppf () = Fmt.st fail_color ppf fail_str
  let pass ppf () = Fmt.st pass_color ppf pass_str
  let skip ppf () = Fmt.st skip_color ppf skip_str
  let skip_long ppf () = Fmt.st skip_color ppf long_str
  let long ppf () = Fmt.st test_color ppf long_str
  let passed ppf () = Fmt.st [`Fg `Green_bright] ppf "passed"
  let failed ppf () = Fmt.st [`Fg `Red_bright] ppf "failed"
  let incorrect ppf () = Fmt.st [`Fg `Red_bright] ppf "incorrect"
  let skipped ppf () = Fmt.st [`Fg `Yellow_bright] ppf "skipped"
  let dur = Fmt.st' [`Bold] Mtime.Span.pp
  let fail_count = Fmt.st' [`Bold; `Fg `Red_bright] Fmt.int
  let count = Fmt.st' [`Bold] Fmt.int
  let fail_count_ratio ppf (c, t) =
    fail_count ppf c; Fmt.char ppf '/'; count ppf t

  let raw_loc ppf (file, lnum, cnum, enum) =
    Fmt.pf ppf "File \"%s\", line %d, characters %d-%d" file lnum cnum enum

  let loc ppf loc = Fmt.pf ppf "%a:" raw_loc loc
  let bt_slot_loc ppf slot = match Printexc.Slot.location slot with
  | None -> ()
  | Some l ->
      raw_loc ppf
        (l.Printexc.filename, l.Printexc.line_number, l.Printexc.start_char,
         l.Printexc.end_char)

  let anon ppf _ = Fmt.string ppf "_"
end

module Test = struct
  type loc = string * int * int * int

  (* Logging *)

  type 'a log = ?__POS__:loc -> ('a, Format.formatter, unit, unit) format4 -> 'a

  let out = ref Format.std_formatter
  let log_raw fmt = Fmt.pf !out fmt
  let log_raw_flush fmt = log_raw (fmt ^^ "@.")

  let log_pad_noflush ?(pad = Test_fmt.padding) ~color fmt =
    Fmt.pf !out ("%a @[" ^^ fmt ^^ "@]@?") (Fmt.st color) pad

  let log_pad_flush ?(pad = Test_fmt.padding) ~color fmt =
    Fmt.pf !out ("%a @[" ^^ fmt ^^ "@]@.") (Fmt.st color) pad

  let klog_pad_flush ?(pad = Test_fmt.padding) ~color k fmt =
    Fmt.kpf k !out ("%a @[" ^^ fmt ^^ "@]@.") (Fmt.st color) pad

  let log_padded_loc_flush ?(pad = Test_fmt.padding) ~color ~mark_none loc =
    match loc with
    | Some loc -> log_pad_flush ~pad ~color "%a" Test_fmt.loc loc
    | None -> if mark_none then Fmt.pf !out "%a@."  (Fmt.st color) pad

  let loc_log ?pad ~color loc fmt =
    log_padded_loc_flush ~color ~mark_none:false loc;
    log_pad_flush ?pad ~color fmt

  let loc_klog ?pad ~color loc k fmt =
    log_padded_loc_flush ~color ~mark_none:false loc;
    klog_pad_flush ?pad ~color k fmt

  let log_finish fmt = Fmt.pf !out ("@[" ^^ fmt ^^ "@]@.")
  let log_start ?__POS__:loc fmt =
    let color = Test_fmt.test_color in
    log_padded_loc_flush ~color ~mark_none:false loc;
    log_pad_noflush ~color fmt

  let log ?__POS__:loc fmt = loc_log ~color:Test_fmt.test_color loc fmt
  let log_pass ?__POS__:loc fmt = loc_log ~color:Test_fmt.pass_color loc fmt

  let log_fail ?__POS__:loc fmt = loc_log ~color:Test_fmt.fail_color loc fmt
  let klog_fail ?__POS__:loc k fmt =
    loc_klog ~color:Test_fmt.fail_color loc k fmt

  let log_exn_fail bt exn =
    let color = Test_fmt.fail_color in
    begin match exn with
    | Assert_failure _ -> log_pad_flush ~color "%s" "Assertion failed";
    | exn -> log_pad_flush ~color "%a raised" (Fmt.code' Fmt.exn) exn
    end;
    match Printexc.backtrace_slots bt with
    | None ->
        log_pad_flush ~color "No backtrace. Did you compile with %a ?"
          Fmt.code "-g"
    | Some slots ->
        Array.iter (log_pad_flush ~color "%a" Test_fmt.bt_slot_loc) slots

  (* Testing state, note this stuff is not thread safe.

     When we can afford effects we could move this to [test].
     Basically all fail combinators (except failstop) raise an
     effect to increment local references and we continue. *)

  let run_test_count = Atomic.make 0

  module Pass = struct
    let count = Atomic.make 0
    let incr () = Atomic.incr count
    let report ~dur =
      if Atomic.get run_test_count = 0 then begin
        log_pad_flush ~pad:Test_fmt.pass_str ~color:Test_fmt.pass_color
          "@[Test %a in %a@]"
          Test_fmt.passed () Test_fmt.dur (Os.Mtime.count dur)
      end else begin
        log_pad_flush ~pad:Test_fmt.pass_str ~color:Test_fmt.pass_color
          "@[%a %s %a in %a@]"
          Test_fmt.count (Atomic.get run_test_count)
          (Text.teststr (Atomic.get run_test_count))
          Test_fmt.passed () Test_fmt.dur (Os.Mtime.count dur)
      end
  end

  module Fail = struct
    let count = Atomic.make 0
    let incr () = Atomic.incr count
    let test_count = Atomic.make 0
    let report ~dur =
      let pad = Test_fmt.fail_str and color = Test_fmt.fail_color in
      if Atomic.get test_count = 0 then begin
        log_pad_flush ~pad ~color "@[Test %a in %a@]"
          Test_fmt.failed () Test_fmt.dur (Os.Mtime.count dur)
      end else begin
        let ratio = Atomic.get test_count, Atomic.get run_test_count in
        log_pad_flush ~pad ~color "@[%a %s %a in %a@]"
          Test_fmt.fail_count_ratio ratio
          (Text.teststr (Atomic.get test_count))
          Test_fmt.failed () Test_fmt.dur (Os.Mtime.count dur)
      end
  end

  module Skip = struct
    let count = Atomic.make 0
    let incr () = Atomic.incr count
    let test_count = Atomic.make 0
    let incr_test () = Atomic.incr test_count
    let report () =
      let count = Atomic.get test_count in
      if count <> 0 then begin
        log_pad_flush ~color:Test_fmt.skip_color "@[%a %s %a@]"
          Test_fmt.count count (Text.teststr count) Test_fmt.skipped ()
      end
  end

  (* Stop, pass and fail *)

  exception Stop
  exception Skip

  let stop () = raise_notrace Stop
  let skip ?__POS__:pos fmt =
    Skip.incr ();
    loc_klog ~pad:Test_fmt.skip_str ~color:Test_fmt.skip_color pos
      (fun _ -> raise_notrace Skip) fmt

  let pass () = Pass.incr ()
  let fail ?__POS__ fmt = Fail.incr (); log_fail ?__POS__ fmt
  let failstop ?__POS__ fmt =
    Fail.incr (); klog_fail ?__POS__ (fun _ -> stop ()) fmt

  (* Blocks and loops *)

  let block ?fail ?__POS__ f =
    let before_pass_count = Atomic.get Pass.count in
    let before_fail_count = Atomic.get Fail.count in
    let finish () =
      let fail_diff = Atomic.get Fail.count - before_fail_count in
      if fail_diff = 0 then () else
      let assertions = Atomic.get Pass.count - before_pass_count + fail_diff in
      match fail with
      | Some fail -> fail ?__POS__ fail_diff ~assertions
      | None ->
          log_fail ?__POS__ "Block %a on %a assertions"
            Test_fmt.failed () Test_fmt.fail_count_ratio (fail_diff, assertions)
    in
    begin try f () with
    | Stop | Skip -> ()
    | exn when not (Os.exn_don't_catch exn) ->
        let bt = Printexc.get_raw_backtrace () in
        log_exn_fail bt exn;
        Fail.incr ()
    end;
    finish ()

  let rec range ?(kind = "Test") ~first ~last ?__POS__:loc f =
    let log_fail ?__POS__ ~kind ~first ~last n =
      log_fail ?__POS__ "%s in range [%d;%d] failed on %a" kind first last
        (Fmt.code' Fmt.int) n
    in
    let rec loop loc ~kind ~first ~last n f =
      if n > last then () else
      let before_fail_count = Atomic.get Fail.count in
      try
        f n;
        if before_fail_count <> Atomic.get Fail.count
        then log_fail ?__POS__:loc ~kind ~first ~last n;
        loop loc ~kind ~first ~last (n + 1) f
      with
      | Stop | Skip ->
          if before_fail_count <> Atomic.get Fail.count
          then log_fail ?__POS__:loc ~kind ~first ~last n
      | exn when not (Os.exn_don't_catch exn) ->
          let bt = Printexc.get_raw_backtrace () in
          log_exn_fail bt exn;
          log_fail ?__POS__:loc ~kind ~first ~last n;
          loop loc ~kind ~first ~last (n + 1) f
    in
    loop loc ~kind ~first ~last first f

  (* Testers *)

  module type T = sig
    type t
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  module T = struct
    type 'a t = (module T with type t = 'a)

    let repr_equal v0 v1 = Stdlib.compare v0 v1 = 0 (* works on nan values *)

    let make (type a) ?(equal = repr_equal) ?(pp = Test_fmt.anon) () : a t =
      (module (struct type t = a let equal = equal let pp = pp end))

    let equal (type a) (module T : T with type t = a) = T.equal
    let pp (type a) (module T : T with type t = a) = T.pp
    let with' (type a) ?equal ?pp (module T : T with type t = a) =
      let equal = Option.value ~default:T.equal equal in
      let pp = Option.value ~default:T.pp pp in
      make ~equal ~pp ()

    (* Predefined testers *)

    let true' (type a) : a t =
      let equal _ _ = true and pp = Test_fmt.anon in
      (module struct type t = a let equal = equal let pp = pp end)

    let false' (type a) : a t =
      let equal _ _ = false and pp = Test_fmt.anon in
      (module struct type t = a let equal = equal let pp = pp end)

    let any (type a) : a t =
      let equal = repr_equal and pp = Test_fmt.anon in
      (module struct type t = a let equal = equal let pp = pp end)

    let exn = make ~equal:repr_equal ~pp:Fmt.exn ()
    let invalid_arg =
      let equal exn _ = match exn with
      | Invalid_argument _ -> true | _ -> false
      in
      let pp ppf = function
      | Invalid_argument _ -> Fmt.string ppf "Invalid_argument _"
      | exn -> Fmt.exn ppf exn
      in
      make ~equal ~pp ()

    let failure =
      let equal exn _ = match exn with
      | Failure _ -> true | _ -> false
      in
      let pp ppf = function
      | Failure _ -> Fmt.string ppf "Failure _"
      | exn -> Fmt.exn ppf exn
      in
      make ~equal ~pp ()

    let unit = make ~equal:(fun () () -> true) ~pp:Fmt.Lit.unit ()
    let bool = make ~equal:Bool.equal ~pp:Fmt.Lit.bool ()
    let int = make ~equal:Int.equal ~pp:Fmt.Lit.int ()
    let int32 = make ~equal:Int32.equal ~pp:Fmt.Lit.int32 ()
    let uint32 = make ~equal:Int32.equal ~pp:Fmt.Lit.uint32 ()
    let int64 = make ~equal:Int64.equal ~pp:Fmt.Lit.int64 ()
    let uint64 = make ~equal:Int64.equal ~pp:Fmt.Lit.uint64 ()
    let nativeint = make ~equal:Nativeint.equal ~pp:Fmt.Lit.nativeint ()
    let nativeuint = make ~equal:Nativeint.equal ~pp:Fmt.Lit.nativeuint ()
    let float = make ~equal:Float.equal ~pp:Fmt.Lit.float ()
    let hex_float = make ~equal:Float.equal ~pp:Fmt.Lit.hex_float ()
    let char = make ~equal:Char.equal ~pp:Fmt.Lit.char ()
    let ascii_string = make ~equal:String.equal ~pp:Fmt.Lit.ascii_string ()
    let string = make ~equal:String.equal ~pp:Fmt.Lit.string ()
    let binary_string = make ~equal:String.equal ~pp:Fmt.binary_string ()
    let styled_string =
      make ~equal:String.equal ~pp:Fmt.styled_text_string_literal ()

    let bytes : bytes t = make ~equal:Bytes.equal ~pp:Fmt.bytes  ()
    let option (type a) (module Some : T with type t = a) : a option t =
      make ~equal:(Option.equal Some.equal) ~pp:(Fmt.Lit.option Some.pp) ()

    let either
        (type a) ~left:(module Left : T with type t = a)
        (type b) ~right:(module Right : T with type t = b)
      =
      let equal = Either.equal ~left:Left.equal ~right:Right.equal in
      let pp = Fmt.Lit.either ~left:Left.pp ~right:Right.pp in
      make ~equal ~pp ()

    let result'
        (type a) ~ok:(module Ok : T with type t = a)
        (type b) ~error:(module Error : T with type t = b)
      =
      let equal = Result.equal ~ok:Ok.equal ~error:Error.equal in
      let pp = Fmt.Lit.result ~ok:Ok.pp ~error:Error.pp in
      make ~equal ~pp ()

    let result ~ok = result' ~ok ~error:string
    let pair
        (type a) (module Fst : T with type t = a)
        (type b) (module Snd : T with type t = b)
      =
      let equal (x0, y0) (x1, y1) = Fst.equal x0 x1 && Snd.equal y0 y1 in
      let pp = Fmt.Lit.pair Fst.pp Snd.pp in
      make ~equal ~pp ()

    let list (type a) (module Elt : T with type t = a) =
      make ~equal:(List.equal Elt.equal) ~pp:(Fmt.Lit.list Elt.pp) ()

    let array (type a) (module Elt : T with type t = a) =
      make ~equal:(array_equal Elt.equal) ~pp:(Fmt.Lit.array Elt.pp) ()
  end

  module Diff = struct
    type 'a t = 'a T.t -> fnd:'a -> exp:'a -> unit Fmt.t
    let make show = show

    open Result.Syntax

    let dumb (type a) t ~fnd ~exp ppf () =
      Fmt.pf ppf "@[<v>@[%a@]@,%a@,@[%a@]@]"
        (T.pp t) fnd Test_fmt.neq () (T.pp t) exp

    let run_diff_cmd ?env cmd ~fnd ~exp =
      Result.join @@ Os.Dir.with_tmp @@ fun dir ->
      let force = false and make_path = false in
      let* () = Os.File.write ~force ~make_path Fpath.(dir / "found") fnd in
      let* () = Os.File.write ~force ~make_path Fpath.(dir / "expected") exp in
      let diff = Cmd.(cmd % "found" % "expected") in
      match Os.Cmd.run_status_out ?env ~trim:false ~cwd:dir diff with
      | Error _ as e -> e
      | Ok (st, diff) -> Ok diff

    let of_cmd cmd t ~fnd ~exp ppf () =
      (* The newlines are to avoid 'No newline at the end of file' reported
         by diffing tools. *)
      let fnd = Fmt.str "%a@\n" (T.pp t) fnd in
      let exp = Fmt.str "%a@\n" (T.pp t) exp in
      match run_diff_cmd cmd ~fnd ~exp with
      | Error e -> Fmt.pf ppf  "diff command error: %s" e
      | Ok diff -> Fmt.pf ppf "@[%a@]" Fmt.lines diff

    let git_diff = lazy (Os.Cmd.get Cmd.(tool "git" % "diff"))
    let git_diff ~fnd ~exp =
      let* git_diff = Lazy.force git_diff in
      let opts =
        let color = match Fmt.styler () with
        | Fmt.Plain -> "--color=never"
        | Fmt.Ansi -> "--color=always"
        in
        Cmd.(arg "--ws-error-highlight=all" %
             "--no-index" % "--no-prefix" % "--patience" % color)
      in
      let env = ["GIT_CONFIG_SYSTEM=/dev/null"; "GIT_CONFIG_GLOBAL=/dev/null"]in
      (* Add newlines to avoid 'No newline at the end of file' reported
         by diffing tools. *)
      let fnd = fnd ^ "\n" in
      let exp = exp ^ "\n" in
      let cmd = Cmd.(git_diff %% opts) in
      let* diff = run_diff_cmd ~env cmd ~fnd ~exp in
      (* Trim the first two lines these are: diff --git …\n index …  *)
      match Text.find_newline ~nth:2 diff with
      | None -> Ok diff
      | Some first -> Ok (String.subrange ~first diff)

    let git t ~fnd ~exp ppf () =
      let fnd = Fmt.str "%a" (T.pp t) fnd in
      let exp = Fmt.str "%a" (T.pp t) exp in
      match git_diff ~fnd ~exp with
      | Error e -> Fmt.pf ppf "git diff error: %s" e
      | Ok diff -> Fmt.pf ppf "@[%a@]" Fmt.lines diff

    type default = { default : 'a. 'a t }
    let default = ref { default = git }
    let set_cmd = function
    | None -> ()
    | Some cmd ->
        let d = match Cmd.to_list cmd with
        | ["dumb"] -> { default = dumb }
        | _  :: _ ->
            let default : 'a. 'a t =
              fun t ~fnd ~exp ppf () -> of_cmd cmd t ~fnd ~exp ppf ()
            in
            { default }
        | [] ->
            (Log.warn @@ fun m ->
             m "No diff command specified. Using %a." Fmt.code "dumb");
            { default = dumb }
        in
        default := d

    let default () = !default.default

    let pp ?diff t ~fnd ~exp =
      let diff = match diff with None -> default () | Some diff -> diff in
      diff t ~fnd ~exp
  end

  module Patch = struct
    type subst = { first : int; last : int; subst : string }
    type t = { src : string; substs : subst list; }

    let make ~src = { src; substs = [] }
    let src p = p.src
    let substs p = p.substs
    let is_empty p = List.is_empty p.substs
    let add_subst p subst = { p with substs = subst :: p.substs }
    let apply { src; substs } =
      let rec loop acc src ~start = function
      | [] ->
          let last = String.subrange ~first:start src in
          String.concat "" (List.rev (last :: acc))
      | { first; last; subst } :: substs ->
          let before = String.subrange ~first:start ~last:(first - 1) src in
          loop (subst :: before :: acc) src ~start:(last + 1) substs
      in
      let compare s0 s1 = Int.compare s0.first s1.first in
      loop [] src ~start:0 (List.stable_sort compare (List.rev substs))

    (* Files to patch *)

    let src_root = ref None
    let set_src_root root = src_root := root

    let files : t option Fpath.Map.t ref = ref Fpath.Map.empty
    let get file =
      let file = match !src_root with
      | None -> file | Some root -> Fpath.append root file
      in
      match Fpath.Map.find_opt file !files with
      | Some v -> v
      | None ->
          let substs = match Os.File.read file with
          | Ok src -> Some { src; substs = [] }
          | Error e ->
              log_fail "Correction failure: %a: %s" Fpath.pp file e;
              None
          in
          files := Fpath.Map.add file substs !files;
          substs

    let update file patch =
      files := Fpath.Map.add file (Some patch) !files

    let write_files () =
      let write_file file = function
      | None -> ()
      | Some patch when is_empty patch -> ()
      | Some patch ->
          let force = true and make_path = false in
          match Os.File.write ~force ~make_path file (apply patch) with
          | Error e -> log_fail "Correction failure: %a: %s" Fpath.pp file e
          | Ok () -> ()
      in
      Fpath.Map.iter write_file !files

    let src_root () = !src_root
  end

  (* Assertions *)

  type 'a eq = ?diff:'a Diff.t -> ?__POS__:loc -> 'a -> 'a -> unit

  let failneq ?diff ?__POS__:loc t fnd exp =
    Fail.incr ();
    log_padded_loc_flush ~color:Test_fmt.fail_color ~mark_none:true loc;
    log_raw_flush "%a" (Diff.pp ?diff t ~fnd ~exp) ()

  let eq t ?diff ?__POS__ fnd exp =
    if T.equal t fnd exp then pass () else failneq ?diff ?__POS__ t fnd exp

  let neq t ?diff ?__POS__ v0 v1 =
    if not (T.equal t v0 v1) then pass () else
    fail "@[<v>Must not be equal:@,%a@,%a@,%a@]"
      (T.pp t) v0 Test_fmt.eq () (T.pp t) v1 ?__POS__

  let holds ?msg ?__POS__ b =
    if b then pass () else match msg with
    | None -> fail ?__POS__ "@[Assertion failed@]"
    | Some msg -> fail ?__POS__ "@[Assertion failed: %s@]" msg

  (* Exceptions *)

  let noraise ?__POS__ f = try f () with
  | Stop | Skip as exn -> raise_notrace exn
  | exn when not (Os.exn_don't_catch exn) ->
      Fail.incr ();
      log_fail ?__POS__ "@[Unexpected exception:@]";
      log_raw_flush "%a" Fmt.exn exn;
      stop ()

  let catch ?ret ?__POS__ f k =
    try
      let v = f () in
      match ret with
      | None -> fail ?__POS__ "@[Expected an exception but got a value.@]"
      | Some ret ->
          Fail.incr ();
          log_fail ?__POS__ "@[Expected an exception but got value:";
          log_raw_flush "%a" (T.pp ret) v
    with
    | Stop | Skip as exn -> raise_notrace exn
    | exn when not (Os.exn_don't_catch exn) -> k exn

  let raises ?ret ?(exn = T.exn) ?diff ?__POS__ exp f =
    catch ?ret ?__POS__ f @@ fun fnd -> eq exn ?diff ?__POS__ fnd exp

  let invalid_arg ?ret ?diff ?__POS__ f =
    raises ?ret ~exn:T.invalid_arg ?diff ?__POS__ (Invalid_argument "") f

  let failure ?ret ?diff ?__POS__ f =
    raises ?ret ~exn:T.failure ?diff ?__POS__ (Failure "") f

  (* Values *)

  let exn = eq T.exn
  let unit = eq T.unit
  let bool = eq T.bool
  let char = eq T.char
  let int = eq T.int
  let int32 = eq T.int32
  let uint32 = eq T.uint32
  let int64 = eq T.int64
  let uint64 = eq T.uint64
  let nativeint = eq T.nativeint
  let nativeuint = eq T.nativeuint
  let float = eq T.float
  let string = eq T.string
  let binary_string = eq T.binary_string
  let styled_string = eq T.styled_string
  let bytes = eq T.bytes
  let pair fst snd = eq (T.pair fst snd)
  let list elt = eq (T.list elt)
  let array elt = eq (T.array elt)
  let option some = eq (T.option some)
  let either ~left ~right = eq (T.either ~left ~right)
  let result ~ok = eq (T.result ~ok)
  let result' ~ok ~error = eq (T.result' ~ok ~error)

  (* Randomized testing *)

  module Rand = struct
    let cli_seed = "seed"
    let env_seed = "SEED"
    let rseed = ref None
    let rstate = ref None

    let set_seed seed = rseed := seed
    let rec init_random_state seed = match seed with
    | None ->
        (* auto-seed *)
        let s = Random.State.make_self_init () in
        init_random_state (Some (Random.State.bits s))
    | Some seed ->
        rseed := Some seed;
        rstate := Some (Random.State.make [| seed |])

    let rec state () = match !rstate with
    | Some s -> s
    | None ->
        match Os.Env.find ~empty_is_none:true env_seed with
        | None -> init_random_state None; state ()
        | Some i ->
            match int_of_string_opt i with
            | Some _ as seed -> init_random_state seed; state ()
            | None ->
                log_fail "Cannot parse integer from %a env value %s, ignoring."
                  Fmt.code env_seed i;
                init_random_state None; state ()

    (* Reporting *)

    let report (log : 'a log) = match !rstate with
    | None -> () (* Was not used *)
    | Some _ ->
        log "Run with %a %a to reproduce randomness."
          Fmt.code ("--" ^ cli_seed) Fmt.(code' int) (Option.get !rseed);
  end

  (* Snapshot testing *)

  module Snapshot = struct
    let cli_correct = "correct"
    let correct = ref false
    let set_correct c = correct := c
    let correct () = !correct

    let cli_force_correct = "force-correct"
    let force_correct = ref false
    let set_force_correct f = force_correct := f
    let force_correct () = !force_correct

    (* Reporting *)

    let fail_count = Atomic.make 0
    let incr_fail () = Atomic.incr fail_count
    let report_fail () =
      let fail_count = Atomic.get fail_count in
      if fail_count = 0 then () else
      let plural, it =
        if fail_count > 1 then "s are", "them" else " is", "it"
      in
      log_fail "%a snapshot%s %a. Run with %a to correct %s."
        Test_fmt.fail_count fail_count plural
        Test_fmt.incorrect () Fmt.code ("--" ^ cli_correct) it

    let log_correcting ?__POS__:loc fmt =
      let color = Test_fmt.pass_color in
      log_padded_loc_flush ~color ~mark_none:true loc;
      log_raw_flush "%a reference snapshot to:"
        (Fmt.st [`Fg `Green_bright]) "Correcting";
      log_raw_flush fmt

    (* Correcting sources *)

    let reindent ~indent s = (* indent by [indent] after each newline *)
      if indent = 0 then s else
      (* The first line is kept unchanged which is what we want. *)
      let sep = "\n" ^ String.make indent ' ' in
      String.concat sep (String.split_on_char '\n' s)

    let relayout_to_multi_line_string_literal ~indent s =
      (* Note we could perhaps have something adequate directly in Fmt.
         This breaks on '\n' escapes with a slash and newline and indents *)
      let white_start l = if l = "" then false else Char.Ascii.is_white l.[0] in
      let escape_start l = if white_start l then "\\" ^ l else " " ^ l in
      let lines = String.cuts_left ~sep:"\\n" s in
      let split_line = "\\n\\\n" in
      if indent = 0 then String.concat split_line lines else
      let indent = String.make indent ' ' in
      let lines = match lines with
      | [] -> [] | [l] -> [l]
      | fst :: rest -> fst :: (List.map escape_start rest)
      in
      String.concat (split_line ^ indent) lines

    let find_value_subrange (fname, lnum, first, last) s =
      (* First find the line on which we are. Then skip over
         __POS_OF__ and whitepace. The actual range spanned by
         __POS_OF__ position is not entirely clear, for example it
         will span the parenthese here (( __POS_OF__ "bla" ) ).  For
         now we don't deal with that and assume [last - 1] is spanned by
         the (possibly parenthesised) value. *)
      match Text.find_newline ~nth:(lnum - 1) s with
      | None -> Fmt.failwith "%s: no line %d found" fname lnum
      | Some line_start ->
          let start = line_start + first in
          let last =
            line_start + last - 1 (* last is a position in the String sense  *)
          in
          let loc_of = "__POS_OF__" in
          match String.find_sub ~start ~sub:loc_of s with
          | None -> Fmt.failwith "%s:%d: no __POS_OF__ found" fname lnum
          | Some start ->
              let start = start + String.length loc_of in
              let is_value_start = Fun.negate Char.Ascii.is_white in
              match String.find_index ~start is_value_start s with
              | None -> Fmt.failwith "%s:%d: no value found" fname lnum
              | Some vstart -> vstart, last

    (* Snapshots *)

    type 'a t = loc * 'a
    type 'a subst = 'a T.t -> loc * 'a -> by:'a -> src:string -> Patch.subst

    let loc = fst
    let value = snd
    let generic_subst t exp ~by ~src =
      let subst = Fmt.str "@[%a@]" (T.pp t) by in
      let first, last = find_value_subrange (loc exp) src in
      let first, last =
        if src.[first] = '(' && subst.[0] <> '('
        then first + 1, last - 1 (* keep parens *)
        else first, last
      in
      let indent = Text.find_col ~start:(first - 1) src in
      let subst = reindent ~indent subst in
      { Patch.first; last; subst }

    let string_subst _ exp ~by ~src =
      let first, last = find_value_subrange (loc exp) src in
      if src.[first] = '\"' then begin
        let indent = Text.find_col ~start:(first - 1) src in
        let subst = Fmt.str "%a" Fmt.text_string_literal by in
        let subst = relayout_to_multi_line_string_literal ~indent subst in
        { Patch.first; last; subst }
      end else
      (* assume "{|" for now, but be more subtle here. First parse any quoted
         string and recognize some to drive layout (and automatically
         expand the token if it happens in [by]. *)
      { Patch.first = first + 2; last = last - 2; subst = by }

    let correct_snapshot ?subst t fnd ((fname, _, _, _), _ as exp) =
      let file = Fpath.of_string fname |> Result.error_to_failure in
      match Patch.get file with
      | None -> ()
      | Some patch ->
          let subst = match subst with
          | None -> generic_subst t exp ~by:fnd ~src:patch.src
          | Some subst -> subst t exp ~by:fnd ~src:patch.src
          in
          Patch.update file (Patch.add_subst patch subst)

    let snap ?subst t ?diff fnd exp =
      try
        if T.equal t fnd (value exp) then begin
          if force_correct () then correct_snapshot ?subst t fnd exp;
          pass ()
        end else begin
          incr_fail ();
          if not (correct () || force_correct ())
          then failneq ~__POS__:(loc exp) ?diff t fnd (value exp) else
          begin
            log_correcting ~__POS__:(loc exp) "%a" (T.pp t) fnd;
            correct_snapshot ?subst t fnd exp
          end
        end
      with
      | Failure e -> log_fail "Correction failure: %s" e
  end

  type 'a snap = ?diff:'a Diff.t -> 'a -> 'a Snapshot.t -> unit
  let snap = Snapshot.snap

  module Long = struct
    let cli_l = "l"

    let count = Atomic.make 0
    let incr () = Atomic.incr count

    let run = ref false
    let set_run l = run := l
    let run () = !run

    let skip_exit = ref false
    let set_skip_exit l = skip_exit := l
    let skip_exit () = !skip_exit

    let report () =
      let count = Atomic.get count in
      if not (run ()) && count > 0 then begin
        log_pad_flush ~color:Test_fmt.skip_color
          "@[%a long %s %a. Run with %a to execute.@]"
          Test_fmt.count count (Text.teststr count) Test_fmt.skipped ()
          Fmt.code ("-" ^ cli_l)
      end
  end

  module Name = struct
    let includes = ref []
    let set_includes incs = includes := (List.map String.lowercase_ascii incs)
    let match_includes name =
      List.exists (fun prefix -> String.starts_with ~prefix name) !includes

    let excludes = ref []
    let set_excludes excs = excludes := (List.map String.lowercase_ascii excs)
    let match_excludes name =
      List.exists (fun prefix -> String.starts_with ~prefix name) !excludes

    let selected name =
      let name = String.lowercase_ascii name in
      (!includes = [] || match_includes name) && not (match_excludes name)
  end

  (* Tests *)

  module Arg = struct
    type 'a t = 'a Type.Id.t
    type value = Value : 'a t * 'a -> value
    let make = Type.Id.make
    let value arg v = Value (arg, v)
    let rec find_value : type a. a t -> value list -> a option =
      fun arg -> function
      | [] -> None
      | Value (arg', v) :: vs ->
          match Type.Id.provably_equal arg arg' with
          | Some Type.Equal -> Some v
          | None -> find_value arg vs
  end

  module Def = struct
    type func =
    | Unit : (unit -> unit) -> func
    | Arg : 'a Type.Id.t * ('a -> unit) -> func

    type t = { name : string; long : bool; func : func }

    let count = Atomic.make 0
    let incr () = Atomic.incr count
    let list = ref []
    let add ~long name func = incr (); list := { name; long; func} :: !list

    let output_list = ref false
    let set_output_list o = output_list := o
    let output_list () = !output_list

    let show_list () =
      let defs = List.filter (fun def -> Name.selected def.name) !list in
      let defs = List.sort (fun d0 d1 -> String.compare d0.name d1.name) defs in
      let output def = Fmt.pr "%s@." def.name in
      List.iter output defs
  end

  let run_test ~long ~name f = fun v ->
    if not (Name.selected name) then () else begin
      if long && not (Long.run ()) then begin
        let pad = Test_fmt.long_str and color = Test_fmt.skip_color in
        log_pad_flush ~pad ~color "Test %s" name
      end else begin
        let pad = if long then Test_fmt.long_str else Test_fmt.padding in
        log_pad_flush ~pad "Test %s" ~color:Test_fmt.test_color name;
        Atomic.incr run_test_count;
        Atomic.set Pass.count 0;
        Atomic.set Fail.count 0;
        Atomic.set Skip.count 0;
        begin try f v with
        | Stop | Skip -> ()
        | exn when Os.exn_don't_catch exn ->
            let bt = Printexc.get_raw_backtrace () in
            log_exn_fail bt exn;
            Fail.incr ();
        end;
        if Atomic.get Fail.count <> 0 then Atomic.incr Fail.test_count;
        if Atomic.get Skip.count <> 0 then Atomic.incr Skip.test_count
      end
    end

  let make ~long ~name func =
    if long then Long.incr ();
    Def.incr ();
    Def.add ~long name func

  let test ?(long = false) name f =
    let func = Def.Unit f in
    make ~long ~name func;
    run_test ~long ~name f

  let test' arg ?(long = false) name f =
    let func = Def.Arg (arg, f) in
    make ~long ~name func;
    run_test ~long ~name f

  let autorun ?(args = []) () =
    let exec t = match t.Def.func with
    | Unit f -> run_test ~long:t.long ~name:t.name f ()
    | Arg (arg, f) ->
        match Arg.find_value arg args with
        | Some arg -> run_test ~long:t.long ~name:t.name f arg
        | None ->
            try skip "Test %s: missing test argument" t.Def.name
            with Skip -> Skip.incr_test ()
    in
    List.iter exec (List.rev !Def.list)

  module Cli = struct
    let setup
        ~correct ~diff_cmd ~force_correct ~seed ~src_root ~long ~long_skip_exit
        ~includes ~excludes ~output_list ~locs:_
      =
      Name.set_includes includes;
      Name.set_excludes excludes;
      Def.set_output_list output_list;
      Long.set_run long;
      Long.set_skip_exit long_skip_exit;
      Patch.set_src_root src_root;
      Diff.set_cmd diff_cmd;
      Rand.set_seed seed;
      Snapshot.set_correct correct;
      Snapshot.set_force_correct force_correct

    open Cmdliner

    let exit_long_skip = 99
    let exits =
      Cmd.Exit.info 1 ~doc:"on test failure." ::
      Cmd.Exit.info exit_long_skip
        ~doc:"on success but skipped long tests if \
              $(b,--long-skip-exit) is specified." ::
      Cmd.Exit.defaults

    let s_common_options = "COMMON TESTING OPTIONS"
    let default_man =
      [ (* `S Cmdliner.Manpage.s_description; *)
        `S Cmdliner.Manpage.s_arguments;
        `S Cmdliner.Manpage.s_options;
        `S s_common_options ]

    let docs = s_common_options

    let diff_cmd =
      let doc =
        "$(docv) is the command used for making textual diffs. The tool is \
         invoked as $(docv) $(b,fnd) $(b,exp) with $(b,fnd) and $(b,exp) the \
         text files to compare. The tool should output a visual represention \
         of the differences between the file contents. This representation
         can be ANSI styled. If $(docv) is $(b,dumb) no external tool \
         is invoked, an internal, non-helpful depiction of the value \
         differences is used."
      in
      let none = B0_std.Cmd.(tool "git" % "diff") in
      Arg.(value & opt (some' ~none B0_std_cli.cmd) None &
           info ["diff-cmd"] ~doc ~docv:"CMD" ~docs)

    let correct =
      let doc = "Update expected snapshot mismatches to the snapshots computed \
                 during the run. See also $(b,--force-correct)."
      in
      let env = Cmd.Env.info "CORRECT" in
      Arg.(value & flag & info [Snapshot.cli_correct] ~env ~doc ~docs)

    let force_correct =
      let doc = "Force all expected snapshots to update to the snapshots \
                 computed during the run, regardless of their correctness. \
                 Use for example if you changed a snapshot printer."
      in
      Arg.(value & flag & info [Snapshot.cli_force_correct] ~doc ~docs)

    let includes =
      let doc = "Select tests whose lowercased name match prefix \
                 $(docv). Repeatable. The selection is filtered by \
                 $(b,-x) options which take over. Test names are printed \
                 during execution and can be (partially) listed with \
                 $(b,--list)."
      in
      let absent = "All tests are selected" in
      Arg.(value & opt_all string [] &
           info ["i"; "include"] ~doc ~docv:"PREFIX" ~absent ~docs)

    let excludes =
      let doc = "Drop selected tests whose lowercased names match \
                 prefix $(docv), takes over $(b,-i). Repeatable."
      in
      Arg.(value & opt_all string [] &
           info ["x"; "exclude"] ~doc ~docv:"PREFIX" ~docs)

    let locs =
      let doc = "No effect. Option name reserved for the future." in
      Arg.(value & flag & info ["locs"] ~doc ~docs)

    let long_skip_exit =
      let doc = "Exit with a special code on success but skipped long tests." in
      let env = Cmd.Env.info "LONG_SKIP_EXIT" in
      Arg.(value & flag & info ["long-skip-exit"] ~env ~doc ~docs)

    let output_list =
      let doc =
        "Do not run selected tests, output their names on $(b,stdout), \
         one per line. Depending on how the test executable is defined \
         the list may be incomplete since tests can be defined dynamically."
      in
      Arg.(value & flag & info ["list"] ~doc ~docs)

    let long =
      let doc = "Do not skip long tests, run them." in
      let env = Cmd.Env.info "LONG" in
      Arg.(value & flag & info [Long.cli_l; "long"] ~doc ~env ~docs)

    let src_root =
      let doc = "$(docv) is prependend to relative snapshot source file \
                 locations to look them up for patching."
      in
      Arg.(value & opt (some' B0_std_cli.fpath) None &
           info ["src-root"] ~doc ~docv:"PATH" ~docs)

    let seed =
      let doc =
        "$(docv) is the integer used to seed pseudo-random generation."
      in
      let env = Cmd.Env.info Rand.env_seed in
      let absent = "Automatically generated" in
      Arg.(value & opt (some int) None &
           info [Rand.cli_seed] ~absent ~env ~doc ~docv:"SEED" ~docs)

    let setup =
      let open Cmdliner.Term.Syntax in
      let+ seed and+ correct and+ force_correct and+ diff_cmd and+ src_root
      and+ long and+ long_skip_exit and+ includes and+ excludes
      and+ output_list and+ locs in
      setup ~diff_cmd ~correct ~force_correct ~seed ~src_root ~long
        ~long_skip_exit ~includes ~excludes ~output_list ~locs
  end

  (* Main *)

  let report_pass ~dur =
    Skip.report ();
    Long.report ();
    Rand.report log_pass;
    Pass.report ~dur

  let report_fail ~dur =
    Snapshot.report_fail ();
    Rand.report log_fail;
    Fail.report ~dur

  let main' ?(man = Cli.default_man) ?doc ?name args f =
    let run () args =
      if Def.output_list () then (Def.show_list (); 0) else
      let () = Printexc.record_backtrace true in
      let dur = Os.Mtime.counter () in
      let exit_main dur =
        Patch.write_files ();
        if Atomic.get Fail.test_count = 0 && Atomic.get Fail.count = 0
        then (report_pass ~dur;
              if Long.skip_exit () && not (Long.run ()) &&
                 Atomic.get Long.count <> 0 then Cli.exit_long_skip else 0)
        else (report_fail ~dur; 1)
      in
      try (f args; exit_main dur) with
      | Stop -> exit_main dur
      | Skip ->
          log_raw_flush "@[%a The test was %a in %a@]"
            Test_fmt.skip () Test_fmt.skipped ()
            Test_fmt.dur (Os.Mtime.count dur);
          0
      | exn when not (Os.exn_don't_catch exn) ->
          let bt = Printexc.get_raw_backtrace () in
          log_exn_fail bt exn;
          log_raw_flush "@[%a The test %a unexpectedly in %a@]"
            Test_fmt.fail () Test_fmt.failed () Test_fmt.dur
            (Os.Mtime.count dur);
          1
    in
    let name = match name with
    | Some name -> name
    | None -> Filename.basename Sys.executable_name
    in
    let info = Cmdliner.Cmd.info ?doc ~man ~exits:Cli.exits name in
    let term = Cmdliner.Term.(const run $ Cli.setup $ args) in
    let cmd = Cmdliner.Cmd.v info term in
    Cmdliner.Cmd.eval' cmd

  let main ?doc ?name f = main' ?doc (Cmdliner.Term.const ()) f

  module Fmt = Test_fmt
end

module Snap = struct
  module T = Test.T

  let exn = Test.(snap T.exn)
  let raise ?ret ?(exn = T.exn) ?diff ?__POS__:pos f exp =
    let pos = match pos with None -> Test.Snapshot.loc exp | Some pos -> pos in
    Test.catch ?ret ~__POS__:pos f @@ fun fnd ->
    Test.snap exn ?diff fnd exp

  let unit = Test.(snap T.unit)
  let bool = Test.(snap T.bool)
  let int = Test.(snap T.int)
  let int32 = Test.(snap T.int32)
  let uint32 = Test.(snap T.uint32)
  let int64 = Test.(snap T.int64)
  let uint64 = Test.(snap T.uint64)
  let nativeint = Test.(snap T.nativeint)
  let nativeuint = Test.(snap T.nativeuint)
  let negative_parens t =
    let pp ppf v =
      if Float.sign_bit v && not (Float.equal v neg_infinity)
      then (Fmt.parens (Test.T.pp t)) ppf v
      else (Test.T.pp t ppf v)
    in
    Test.T.with' ~pp t

  let float = Test.(snap (negative_parens T.float))
  let hex_float = Test.(snap (negative_parens T.hex_float))
  let char = Test.(snap T.char)
  let string = Test.snap ~subst:Test.Snapshot.string_subst Test.T.string
  let ascii_string =
    Test.snap ~subst:Test.Snapshot.string_subst Test.T.ascii_string

  let line =
    Test.snap (Test.T.make ~equal:String.equal ~pp:Fmt.text_string_literal ())

  let option some = Test.(snap (T.option some))
  let either left right = Test.(snap (T.either ~left ~right))
  let result ok  = Test.(snap (T.result ~ok))
  let result' ok error = Test.(snap (T.result' ~ok ~error))
  let pair fst snd = Test.(snap (T.pair fst snd))
  let list elt = Test.(snap (T.list elt))
  let array elt = Test.(snap (T.array elt))

  (* Command spawns. *)

  let stdout ?__POS__ ?diff ?env ?cwd ?stdin ?stderr ~trim cmd exp =
    match Os.Cmd.run_status_out ?env ?cwd ?stdin ?stderr ~trim cmd with
    | Error e -> Test.fail ?__POS__ "%s" e
    | Ok (_status, out) -> string ?diff out exp
end

let ( !! ) ?(loc = ("/dev/null", 0, 0, 0)) v = (loc, v)
