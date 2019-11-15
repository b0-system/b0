(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00
open Test_memo_setup

let echo = Tool.by_name "echo"
let echo m ?unready_reads ?reads ?unready_writes ?writes ?post_exec ?k f msg =
  let echo = Memo.tool m echo in
  Memo.spawn m
    ?unready_reads ?reads ?unready_writes ?writes ?post_exec ?k
    ~stdout:(`File f) @@ echo (Cmd.arg msg)

let is_odd build_dir m =
  let f file = Fpath.(build_dir / file) in
  let start = f "start" in
  let res = f "res" in
  let rec is_odd _ =
    Memo.read m ~unready_read:true res @@ fun n ->
    match int_of_string (String.trim n) with
    | 0 -> echo m ~writes:[res] res "false"
    | n -> echo m ~unready_writes:[res] res (string_of_int (n - 1)) ~k:is_even
  and is_even _ =
    Memo.read m ~unready_read:true res @@ fun n ->
    match int_of_string (String.trim n) with
    | 0 -> echo m ~writes:[res] res "true"
    | n -> echo m ~unready_writes:[res] res (string_of_int (n - 1)) ~k:is_odd
  in
  let is_odd () = is_odd 0 (* don't care this exit code *) in
  let start_num = "4" in
  echo m ~writes:[start] start start_num;
  Memo.read m start @@ fun n ->
  Memo.write m
    ~reads:[start] ~unready_write:true ~k:is_odd res (fun () -> Ok n);
  Memo.read m res @@ fun res ->
  Log.app (fun m -> m "%s is odd: %s" start_num res)

let test_memo_fix () =
  with_memo is_odd;
  ()

let () = test_memo_fix ()

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
