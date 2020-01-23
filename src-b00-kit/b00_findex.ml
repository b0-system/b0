(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

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
  match Fpath.Map.find (Fpath.to_dir_path d) i.files_by_dir with
  | files -> files | exception Not_found -> []

let dir_dirs i d =
  match Fpath.Map.find (Fpath.to_dir_path d) i.dirs_by_dir with
  | dirs -> dirs | exception Not_found -> []

let files i = i.files
let find_filename i n = match String.Map.find n i.files_by_name with
| files -> files | exception Not_found -> []

let of_dirs ?dotfiles ?follow_symlinks ?prune root_dirs =
  let sort files =
    let rec loop ds ds_by_dir ds_by_name fs fs_by_dir fs_by_name = function
    | [] -> ds, ds_by_dir, ds_by_name, fs, fs_by_dir, fs_by_name
    | d :: ps when Fpath.is_dir_path d ->
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
    let prune = match prune with
    | None -> None
    | Some prune -> Some (fun st n f _ -> prune st n f)
    in
    Result.to_failure @@ Os.Dir.fold
      ?dotfiles ?follow_symlinks ?prune ~recurse:true Os.Dir.path_list dir acc
  in
  try
    let root_root_dirs = Fpath.drop_prefixed (Fpath.uniquify root_dirs) in
    let files = List.fold_left dir_paths [] root_root_dirs in
    let dirs, dirs_by_dir, dirs_by_name, files, files_by_dir, files_by_name =
      sort files
    in
    Ok { root_dirs; root_root_dirs; dirs; dirs_by_name; dirs_by_dir; files;
         files_by_dir; files_by_name }
  with
  | Failure e -> Error e

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
