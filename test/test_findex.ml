(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let () =
  try
    let dirs = List.tl (Array.to_list Sys.argv) in
    let dirs = List.map Fpath.of_string dirs in
    let dirs = List.map Result.error_to_failure dirs in
    let c = Os.Mtime.counter () in
    let dotfiles = true and follow_symlinks = true in
    let _index =
      B0_findex.of_dirs ~dotfiles ~follow_symlinks dirs
      |> Result.error_to_failure
    in
    Fmt.pr "@[%a@]@." Mtime.Span.pp (Os.Mtime.count c);
    exit 0;
  with
  | Failure e -> Fmt.epr "%s@." e; exit 1
