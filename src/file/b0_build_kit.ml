(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let vcs_describe_scope build =
  let open Result.Syntax in
  B0_memo.fail_if_error (B0_build.memo build) @@
  let dir = B0_build.scope_dir build in
  let* vcs = B0_vcs_repo.find () ~dir in
  match vcs with
  | None -> Ok None
  | Some vcs ->
      let* descr = B0_vcs_repo.describe vcs ~dirty_mark:true "HEAD" in
      Ok (Some descr)
