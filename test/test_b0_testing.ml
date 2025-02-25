(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open B0_std

let test_any =
  Test.test "T.any" @@ fun () ->
  Test.eq Test.T.any nan nan;
  ()

let test_assertions =
  Test.test "Test.* assertions" @@ fun () ->
  Test.invalid_arg ~__POS__ @@ (fun () -> String.get "" 0);
  Test.char (String.get "a" 0) 'a' ~__POS__;
  Test.(list T.int) (List.map succ [1; 2; 3]) [2; 3; 4];
  Test.string (String.cat "a" "b") "ab" ~__POS__;
  Test.block ~__POS__ @@ fun () ->
  Test.int 4 4 ~__POS__;
  Test.int 3 3 ~__POS__;
  ()

let test_exn_assertions =
  Test.test "Test.* exception assertions"  @@ fun () ->
  Test.exn (Failure "bla") (Failure "bla") ~__POS__;
  Test.(raises ~ret:T.int) (Failure "bla") (fun () -> failwith "bla") ~__POS__;
  Test.(catch ~ret:T.int) (fun () -> invalid_arg "") ignore ~__POS__;
  ()

let test_randomized =
  Test.test "Randomized" @@ fun () ->
  let rstate = Test.Rand.state () in
  let n = Random.State.int rstate 10 in
  let a = Array.init (n + 1) Fun.id in
  let asum = (n * (n + 1)) / 2 in
  Test.int (Array.fold_left ( + ) 0 a) asum ~__POS__;
  ()

let test_snap_exns =
  Test.test "Snap.exn" @@ fun () ->
  Snap.exn (Failure "bla") @@ __POS_OF__ (Failure("bla"));
  Snap.raise (fun () -> failwith "bla") @@ __POS_OF__ (Failure("bla"));
  Snap.(raise ~ret:T.int) (fun () -> invalid_arg "ha\nha") @@ __POS_OF__
    (Invalid_argument("ha\nha"));
  ()

let test_long =
  Test.test "Should be long" ~long:true @@ fun () ->
  ()

let test_snap_base =
  Test.test "Snap.* base types" @@ fun () ->
  Snap.unit () @@ __POS_OF__ ();
  Snap.bool true @@ __POS_OF__ true;
  Snap.int (3 * 4) @@ __POS_OF__ 12;
  Snap.int32 123l @@ __POS_OF__ 123l;
  Snap.uint32 0xffffffffl @@ __POS_OF__ 0xffffffffl;
  Snap.int64 123L @@ __POS_OF__ 123L;
  Snap.uint64 0xffff_ffff_ffff_ffffL @@ __POS_OF__ 0xffffffffffffffffL;
  Snap.nativeint 0123n @@ __POS_OF__ 123n;
  Snap.nativeuint 0123n @@ __POS_OF__ 0x7bn;
  (* floats *)
  Snap.float 1.0 @@ __POS_OF__ 1.;
  Snap.float (-0.0) @@ __POS_OF__ (-0.);
  Snap.float infinity @@ __POS_OF__ infinity;
  Snap.float neg_infinity @@ __POS_OF__ neg_infinity;
  Snap.float nan @@ __POS_OF__ nan;
  (* hex float *)
  Snap.hex_float 1.0 @@ __POS_OF__ 0x1p+0;
  Snap.hex_float (-0.0) @@ __POS_OF__ (-0x0p+0);
  Snap.hex_float infinity @@ __POS_OF__ infinity;
  Snap.hex_float neg_infinity @@ __POS_OF__ neg_infinity;
  Snap.hex_float nan @@ __POS_OF__ nan;
  ()

let test_snap_strings =
  Test.test "Snap.* characters and strings " @@ fun () ->
  Snap.string (String.concat "" ["a";"\n";" c";"\n"; "abc"]) @@ __POS_OF__
    "a\n\
    \ c\n\
     abc";
  Snap.stdout ~trim:false Cmd.(tool "printf" % "a\\nb") @@ __POS_OF__
    {|a
b|};
  ()

let test_snap_parametric_types =
  Test.test "Snap.* parametric types" @@ fun () ->
  Snap.(option T.string) None @@ __POS_OF__ None;
  Snap.(option T.string) (Some "hey\n") @@ __POS_OF__ (Some "hey\n");
  Snap.(either T.int T.string) (Left 3) @@ __POS_OF__ (Either.Left 3);
  Snap.(either T.int T.string) (Right "hu") @@ __POS_OF__
    (Either.Right "hu");
  Snap.(result T.int) (Ok 2) @@ __POS_OF__ (Ok 2);
  Snap.(result T.int) (Error "hey") @@ __POS_OF__
    (Error "hey");
  Snap.(result' T.int T.int) (Error 345) @@ __POS_OF__
    (Error 345);
  Snap.(pair T.int T.string) (2, "bla") @@ __POS_OF__
    (2, "bla");
  Snap.(list T.int) (List.map succ [1; 2; 3]) @@ __POS_OF__ [2; 3; 4];
  Snap.(list T.string) (["bla"; "bli"; "blo"]) @@ __POS_OF__
    ["bla"; "bli"; "blo"];
  Snap.(array T.int) (Array.map succ [|1; 2; 3|]) @@ __POS_OF__ [|2; 3; 4|];
  ()

let ctx = Test.Arg.make ()
let test_arg =
  Test.test' ctx "With argument" @@ fun arg ->
  Test.int arg 3

let main () =
  Test.main ~doc:"B0_testing tests" @@ fun () ->
  Test.autorun ~args:Test.Arg.[value ctx 3] ()

let () = if !Sys.interactive then () else exit (main ())
