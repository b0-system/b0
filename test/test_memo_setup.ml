(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00


let feedback =
  let show_ui = Log.Error and show_op = Log.Info and level = Log.level () in
  B00_ui.Memo.pp_leveled_feedback ~show_op ~show_ui ~level
    Fmt.stderr

let with_memo ?jobs f =
  let () = B0_std.Fmt.set_tty_styling_cap `Ansi in
  Result.to_failure @@
  Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
  let tmp_dir = (* Os.Dir.default_tmp () *) Fpath.v "/tmp" in
  let log_file = Fpath.(tmp_dir / "b0-test" / "log") in
  let cache_dir = Fpath.(tmp_dir / "b0-test" / "cache") in
  let trash_dir = Fpath.(tmp_dir / "b0-test" / "trash") in
  let build_dir = Fpath.(tmp_dir / "b0-test") in
  Result.bind (Memo.memo ~cwd ~cache_dir ~trash_dir ?jobs ~feedback ()) @@
  fun m ->
  f build_dir m;
  Memo.stir m ~block:true;
  begin match Memo.status m with
  | Ok () -> ()
  | Error e -> (B00_ui.Memo.pp_error ()) Fmt.stderr e
  end;
  Log.if_error ~use:() (B00_ui.Memo.Log.write_file log_file m);
  Ok ()

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
