(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let ready_and_copy_dir
    ?(rel = false) ?follow_symlinks ?(prune = fun _ _ _ -> false) m
    ~recurse src_root ~dst:dst_root
  =
  let copy_file st name src () =
    if prune st name src then () else
    let dst = match rel with
    | true -> Fpath.(dst_root // src)
    | false -> Fpath.reroot ~src_root ~dst_root src
    in
    B0_memo.ready_file m src;
    B0_memo.copy m ~mode:st.Unix.st_perm src ~dst
  in
  B0_memo.fail_if_error m @@
  let* () = Os.Dir.must_exist src_root in
  let prune_dir st name dir () = prune st name dir and dotfiles = true in
  Os.Dir.fold_files
    ~rel ~dotfiles ?follow_symlinks ~prune_dir ~recurse copy_file src_root ()
