(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open B0_testing

let test_string_get =
  Test.test "String.get" @@ fun () ->
  Test.char (String.get "a" 0) 'a' ~__POS__;
  Snap.char (String.get "ab" 1) @@ __POS_OF__ 'b';
  Snap.raise (fun () -> String.get "" 0) @@ __POS_OF__
    (Invalid_argument("index out of bounds"));
  ()

let test_list_map =
  Test.test "List.map" @@ fun () ->
  Test.(list T.int) (List.map succ [1; 2; 3]) [2; 3; 4] ~__POS__;
  Snap.(list T.int) (List.map succ [1; 2; 3]) @@ __POS_OF__
    [2; 3; 4];
  ()

let test_randomized =
  Test.test "Randomized" @@ fun () ->
  let rstate = Test.Rand.state () in
  let n = Random.State.int rstate 10 in
  let a = Array.init (n + 1) Fun.id in
  let asum = (n * (n + 1)) / 2 in
  Test.int (Array.fold_left ( + ) 0 a) asum ~__POS__;
  ()

let test_snapshots =
  Test.test "Snapshots" @@ fun () ->
  Snap.string (String.concat "," ["a";"b";"c"]) @@ __POS_OF__
    "a,b,c";
  ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
