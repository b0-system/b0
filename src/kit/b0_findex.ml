(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type t =
  { root_dirs : Fpath.t list;
    root_root_dirs : Fpath.t list;
    dirs : Fpath.Set.t;
    dirs_by_dir : Fpath.t list Fpath.Map.t;
    dirs_by_name : Fpath.t list String.Map.t;
    files : Fpath.Set.t;
    files_by_dir : Fpath.t list Fpath.Map.t;
    files_by_name : Fpath.t list String.Map.t; }

let empty =
  { root_dirs = []; root_root_dirs = []; dirs = Fpath.Set.empty;
    dirs_by_dir = Fpath.Map.empty; dirs_by_name = String.Map.empty;
    files = Fpath.Set.empty; files_by_dir = Fpath.Map.empty;
    files_by_name = String.Map.empty; }

let root_dirs d = d.root_dirs
let root_root_dirs d = d.root_root_dirs
let dirs d = d.dirs
let find_dirname i n = match String.Map.find n i.dirs_by_name with
| dirs -> dirs | exception Not_found -> []

let dir_files i d =
  match Fpath.Map.find (Fpath.ensure_trailing_dir_sep d) i.files_by_dir with
  | files -> files | exception Not_found -> []

let dir_dirs i d =
  match Fpath.Map.find (Fpath.ensure_trailing_dir_sep d) i.dirs_by_dir with
  | dirs -> dirs | exception Not_found -> []

let files i = i.files
let find_filename i n = match String.Map.find n i.files_by_name with
| files -> files | exception Not_found -> []

let of_dirs ?prune_dir ~dotfiles ~follow_symlinks root_dirs =
  let sort files =
    let rec loop ds ds_by_dir ds_by_name fs fs_by_dir fs_by_name = function
    | [] -> ds, ds_by_dir, ds_by_name, fs, fs_by_dir, fs_by_name
    | d :: ps when Fpath.is_syntactic_dir d ->
        let ds = Fpath.Set.add d ds in
        let dir = Fpath.parent d in
        let ds_by_dir = Fpath.Map.add_to_list dir d ds_by_dir in
        let n = Fpath.basename d in
        let ds_by_name = String.Map.add_to_list n d ds_by_name in
        loop ds ds_by_dir ds_by_name fs fs_by_dir fs_by_name ps
    | f :: ps ->
        let fs = Fpath.Set.add f fs in
        let dir = Fpath.parent f in
        let fs_by_dir = Fpath.Map.add_to_list dir f fs_by_dir in
        let n = Fpath.basename f in
        let fs_by_name = String.Map.add_to_list n f fs_by_name in
        loop ds ds_by_dir ds_by_name fs fs_by_dir fs_by_name ps
    in
    loop
      Fpath.Set.empty Fpath.Map.empty String.Map.empty
      Fpath.Set.empty Fpath.Map.empty String.Map.empty files
  in
  let dir_paths acc dir =
    let prune_dir = match prune_dir with
    | None -> None
    | Some prune -> Some (fun st n f _ -> prune st n f)
    in
    let recurse = true in
    Result.error_to_failure @@ Os.Dir.fold
      ?prune_dir ~dotfiles ~follow_symlinks ~recurse Os.Dir.path_list dir acc
  in
  try
    let root_root_dirs =
      Fpath.remove_strictly_prefixed (Fpath.distinct root_dirs)
    in
    let files = List.fold_left dir_paths [] root_root_dirs in
    let dirs, dirs_by_dir, dirs_by_name, files, files_by_dir, files_by_name =
      sort files
    in
    Ok { root_dirs; root_root_dirs; dirs; dirs_by_name; dirs_by_dir; files;
         files_by_dir; files_by_name }
  with
  | Failure e -> Error e
