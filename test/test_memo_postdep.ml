(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Test_memo_setup

let deps_of_string s =
  try
    let to_path s = Fpath.of_string s |> Result.to_failure in
    Ok (List.map to_path (B0_lines.of_string s))
  with
  | Failure e -> Error e

let read_deps m deps k =
  match Os.File.exists deps |> Result.value ~default:false with
  | false -> k None
  | true ->
      B0_memo.Memo.read m deps @@ fun data ->
      k (Some (deps_of_string data |> B0_memo.Memo.fail_if_error m))

let adjust_deps o =
  if B0_zero.Op.revived o || B0_zero.Op.status o <> B0_zero.Op.Done then () else
  let reads = Os.File.read deps @@ fun data -> deps_of_string data in
  match reads with
  | Error e -> B0_zero.Op.set_status_from_result e
  | Ok reads -> B0_zero.Op.set_reads reads


let post_dep build_dir m =
  let f file = Fpath.(build_dir / file) in
  let src = f "src" in
  let tool = f "tool" in
  let deps = f "a.d" in
  B0_memo.Memo.write m src (fun () -> Ok "yooo") ~k:begin fun () ->
    B0_memo.Memo.write m ~mode:0x755 tool @@ begin fun () ->
      Ok ("#!/bin/sh\ncat src > a.o\necho \"src\" > a.d")
    end;
    let tool = B0_memo.Memo.tool m (Tool.v tool) in
    read_deps m deps @@ fun deps ->
    let reads = match deps with None -> [] | Some reads -> reads in
    let post_exec o = failwith "ha"
    in
    B0_memo.Memo.spawn ~reads ~post_exec m (tool Cmd.empty);
  end;
  ()

let test_memo_post () =
  with_memo post_dep;
  ()

let () = test_memo_post ()
