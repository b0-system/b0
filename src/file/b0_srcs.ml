(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* At a certain point we might want to cache the directory folds and
   file stats. But for now that seems good enough. *)

(* FIXME we should gather sets or sorted lists for build repo *)

type sel =
[ `Dir of Filepath.t
| `Dir_rec of Filepath.t
| `X of Filepath.t
| `File of Filepath.t
| `Fut of B0_build.t -> Filepath.Set.t Fut.t ]

type sels = sel list

type t =
  { by_ext : B0_file_exts.map; (* selected files sorted by extension. *)
    roots : Filepath.t Filepath.Map.t;(* maps selected files to their root. *) }

let by_ext s = s.by_ext

let fail_if_error m u = function
| Error e -> B0_memo.fail m "Source selection: %s" e | Ok v -> v

let select_files m u (seen, by_ext) fs =
  let rec loop m u seen by_ext = function
  | [] -> seen, by_ext
  | f :: fs ->
      match Os.File.exists f |> fail_if_error m u with
      | false ->
          let pp_file = Fmt.(code' Filepath.pp) in
          B0_memo.fail m "Source file@ %a@ does not exist." pp_file f
      | true ->
          if Filepath.Set.mem f seen then loop m u seen by_ext fs else
          let seen = Filepath.Set.add f seen in
          let by_ext =
            String.Map.add_to_list (Filepath.take_ext ~multi:false f) f by_ext
          in
          loop m u seen by_ext fs
  in
  loop m u seen by_ext fs

let select_files_in_dirs m u xs (seen, by_ext as acc) ds =
  let exclude =
    let ds =
      List.fold_left (fun s (d, _) -> Filepath.Set.add d s)
        Filepath.Set.empty ds
    in
    fun fname p ->
      let auto_exclude = function
      | "" | "." | ".." -> false
      | s when s.[0] = '.' -> true
      | _ -> false
      in
      if auto_exclude fname
      then not (Filepath.Set.mem p ds) (* allow explicit *)
      else Filepath.Set.mem p xs
  in
  let add_file st fname p (seen, by_ext as acc) =
    if exclude fname p then acc else
    match st.Unix.st_kind with
    | Unix.S_DIR -> acc
    | _ ->
        if Filepath.Set.mem p seen then acc else
        Filepath.Set.add p seen,
        String.Map.add_to_list (Filepath.take_ext ~multi:false p) p by_ext
  in
  let rec loop m u xs (seen, by_ext as acc) = function
  | [] -> acc
  | (d, recurse) :: ds ->
      let d = Filepath.drop_trailing_dir_sep d in
      if Filepath.Set.mem d xs then loop m u xs acc ds else
      match Os.Dir.exists d |> fail_if_error m u with
      | false ->
          let pp_dir = Fmt.(code' Filepath.pp) in
          B0_memo.fail m "Source directory@ %a@ does not exist." pp_dir d
      | true ->
          let prune_dir _ dname dir _ = exclude dname dir  in
          let dotfiles = true (* exclusions handled by prune *) in
          let follow_symlinks = true in
          let acc =
            Os.Dir.fold
              ~prune_dir ~dotfiles ~follow_symlinks ~recurse add_file d acc
          in
          loop m u xs (acc |> fail_if_error m u) ds
  in
  loop m u xs acc ds

let select b sels =
  let open B0_std.Fut.Syntax in
  let m = B0_build.memo b in
  let u = B0_build.current b in
  let abs d = B0_build.in_scope_dir b d in
  let fs, ds, xs, futs =
    let rec loop fs ds xs futs = function
    | [] -> fs, ds, xs, futs
    | `Dir d :: ss -> loop fs ((abs d, false) :: ds) xs futs ss
    | `Dir_rec d :: ss -> loop fs ((abs d, true) :: ds) xs futs ss
    | `X x :: ss ->
        let x = Filepath.drop_trailing_dir_sep (abs x) in
        loop fs ds (Filepath.Set.add x xs) futs ss
    | `File f :: ss -> loop ((abs f) :: fs) ds xs futs ss
    | `Fut f :: ss -> loop fs ds xs (f b :: futs) ss
    in
    loop [] [] Filepath.Set.empty [] sels
  in
  let acc = Filepath.Set.empty, String.Map.empty in
  let acc = select_files m u acc fs in
  let (seen, _ as acc) = select_files_in_dirs m u xs acc ds in
  Filepath.Set.iter (B0_memo.ready_file m) seen;
  let* futs = Fut.of_list futs in
  let add_files acc files =
    let add_file file (seen, by_ext as acc) =
      if Filepath.Set.mem file seen then acc else
      let ext = Filepath.take_ext ~multi:false file in
      let by_ext = String.Map.add_to_list ext file by_ext in
      (Filepath.Set.add file seen), by_ext
    in
    Filepath.Set.fold add_file files acc
  in
  let _, acc = List.fold_left add_files acc futs in
  Fut.return { by_ext = acc; roots = Filepath.Map.empty }

(*
let root_for_file s f = match Filepath.Map.find_opt f s.roots with
| Some r -> r
| None ->
    Fmt.invalid_arg "%a: not in B0_src selection result" Filepath.pp_unquoted f
*)
