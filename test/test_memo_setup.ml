(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let feedback =
  let output_op_level = Log.Info and output_ui_level = Log.Error in
  let level = Log.level () in
  B0_memo_cli.pp_leveled_feedback
    ~output_op_level ~output_ui_level ~level Fmt.stderr

let with_memo ?jobs f =
  Result.error_to_failure @@
  Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
  let tmp_dir = (* Os.Dir.default_tmp () *) Fpath.v "/tmp" in
  let log_file = Fpath.(tmp_dir / "b0-test" / "log") in
  let cache_dir = Fpath.(tmp_dir / "b0-test" / "cache") in
  let trash_dir = Fpath.(tmp_dir / "b0-test" / "trash") in
  let build_dir = Fpath.(tmp_dir / "b0-test") in
  Result.bind
    (B0_memo.make ~cwd ~cache_dir ~trash_dir ?jobs ~feedback ()) @@
  fun m ->
  f build_dir m;
  B0_memo.stir m ~block:true;
  begin match B0_memo.status m with
  | Ok () -> ()
  | Error e -> (B0_zero_conv.Op.pp_aggregate_error ()) Fmt.stderr e
  end;
  Log.if_error ~use:() (B0_memo_log.(write log_file (of_memo m)));
  Ok ()
