(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Test_memo_setup

let cat = B0_memo.Tool.by_name "cat"
let echo = B0_memo.Tool.by_name "echo"
let echo m ~reads ~writes f msg =
  let echo = B0_memo.tool m echo in
  B0_memo.spawn m ~reads ~writes ~stdout:(`File f) @@ echo (Cmd.arg msg)

let cannot_read build_dir m =
  let cat = B0_memo.tool m cat in
  let f file = Fpath.(build_dir / file) in
  let r = f "nosuchfile" in
  let w = f "out" in
  B0_memo.file_ready m r;
  B0_memo.spawn m ~reads:[r] ~writes:[w] ~stdout:(`File w) @@
  cat Cmd.(path r)

let did_not_write build_dir m =
  let f p = Fpath.(build_dir // p) in
  echo m ~reads:[] ~writes:[f (Fpath.v "heyho/bla"); f (Fpath.v "bli")]
    Fpath.null "echoooo"

let failures build_dir m =
  let f file = Fpath.(build_dir / file) in
  let cat = B0_memo.tool m cat in
  let r = f "doesnotexist" and o = f "o" in
  B0_memo.spawn
    m ~reads:[] ~writes:[o] ~stdout:(`File o) @@ cat (Cmd.path r)

(* Test stuck builds *)

let never_ready build_dir m =
  let f file = Fpath.(build_dir / file) in
  let e0 = f "e0" and e1 = f "e1" and e2 = f "e2" in
  let n0 = f "never0" and n1 = f "never1" and n2 = f "never2" in
  echo m ~reads:[n0; n1] ~writes:[e0] e0 "e0";
  echo m ~reads:[e0] ~writes:[e1] e1 "e1";
  echo m ~reads:[n0; n2] ~writes:[e2] e2 "e2";
  ()

let cycle0 build_dir m =
  let f file = Fpath.(build_dir / file) in
  let c0 = f "c0" in
  echo m ~reads:[c0] ~writes:[c0] c0 "c0"

let cycle1 build_dir m =
  let f file = Fpath.(build_dir / file) in
  let c0 = f "c0" and c1 = f "c1" in
  echo m ~reads:[c1] ~writes:[c0] c0 "c0";
  echo m ~reads:[c0] ~writes:[c1] c1 "c1"

let cycle2 build_dir m =
  let f file = Fpath.(build_dir / file) in
  let c0 = f "c0" and c1 = f "c1" and c2 = f "c2" in
  echo m ~reads:[c2] ~writes:[c0] c0 "c0";
  echo m ~reads:[c0] ~writes:[c1] c1 "c1";
  echo m ~reads:[c1] ~writes:[c2] c2 "c2"

let test_failures () =
  with_memo did_not_write;
  with_memo cannot_read;
  with_memo failures;
  with_memo never_ready;
  with_memo cycle0;
  with_memo cycle1;
  with_memo cycle2;
  ()

let () = test_failures ()
