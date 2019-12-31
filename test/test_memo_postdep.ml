(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00
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
      Memo.read m deps @@ fun data ->
      k (Some (deps_of_string data |> Memo.fail_if_error m))

let adjust_deps o =
  if B000.Op.revived o || B000.Op.status o <> B000.Op.Done then () else
  let reads = Os.File.read deps @@ fun data -> deps_of_string data in
  match reads with
  | Error e -> B000.Op.set_status_from_result e
  | Ok reads -> B000.Op.set_reads reads


let post_dep build_dir m =
  let f file = Fpath.(build_dir / file) in
  let src = f "src" in
  let tool = f "tool" in
  let deps = f "a.d" in
  in
  Memo.write m src (fun () -> Ok "yooo") ~k:begin fun () ->
    Memo.write m ~mode:0x755 tool @@ begin fun () ->
      Ok ("#!/bin/sh\ncat src > a.o\necho \"src\" > a.d")
    end;
    let tool = Memo.tool m (Tool.v tool) in
    read_deps m deps @@ fun deps ->
    let reads = match deps with None -> [] | Some reads -> reads in
    let post_exec o =


    in
    Memo.spawn ~reads ~post_exec m (tool Cmd.empty);
  end;
  ()

let test_memo_post () =
  with_memo post_dep;
  ()

let () = test_memo_post ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
