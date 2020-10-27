(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

(* At a certain point we might want to cache the directory folds and
   file stats. But for now that seems good enough. *)

(* FIXME we should gather sets or sorted lists for build repo *)

type sel =
[ `Dir of Fpath.t
| `Dir_rec of Fpath.t
| `X of Fpath.t
| `File of Fpath.t
| `Fut of B0_build.t -> Fpath.Set.t Fut.t ]

type sels = sel list

type t =
  { by_ext : B00_fexts.map; (* selected files sorted by extension. *)
    roots : Fpath.t Fpath.Map.t; (* maps selected files to their root. *) }

let by_ext s = s.by_ext

let fail_if_error m u = function
| Error e -> Memo.fail m "Source selection: %s" e | Ok v -> v

let select_files m u (seen, by_ext) fs =
  let rec loop m u seen by_ext = function
  | [] -> seen, by_ext
  | f :: fs ->
      match Os.File.exists f |> fail_if_error m u with
      | false ->
          let pp_file = Fmt.(code Fpath.pp_unquoted) in
          Memo.fail m "Source file@ %a@ does not exist." pp_file f
      | true ->
          if Fpath.Set.mem f seen then loop m u seen by_ext fs else
          let seen = Fpath.Set.add f seen in
          let by_ext = String.Map.add_to_list (Fpath.get_ext f) f by_ext in
          loop m u seen by_ext fs
  in
  loop m u seen by_ext fs

let select_files_in_dirs m u xs (seen, by_ext as acc) ds =
  let exclude =
    let ds =
      List.fold_left (fun s (d, _) -> Fpath.Set.add d s) Fpath.Set.empty ds
    in
    fun fname p ->
      let auto_exclude = function
      | "" | "." | ".." -> false
      | s when s.[0] = '.' -> true
      | _ -> false
      in
      if auto_exclude fname then not (Fpath.Set.mem p ds) (* allow explicit *)
      else Fpath.Set.mem p xs
  in
  let add_file st fname p (seen, by_ext as acc) =
    if exclude fname p then acc else
    match st.Unix.st_kind with
    | Unix.S_DIR -> acc
    | _ ->
        if Fpath.Set.mem p seen then acc else
        Fpath.Set.add p seen, String.Map.add_to_list (Fpath.get_ext p) p by_ext
  in
  let rec loop m u xs (seen, by_ext as acc) = function
  | [] -> acc
  | (d, recurse) :: ds ->
      let d = Fpath.rem_empty_seg d in
      if Fpath.Set.mem d xs then loop m u xs acc ds else
      match Os.Dir.exists d |> fail_if_error m u with
      | false ->
          let pp_dir = Fmt.(code Fpath.pp_unquoted) in
          Memo.fail m "Source directory@ %a@ does not exist." pp_dir d
      | true ->
          let prune _ dname dir _ = exclude dname dir  in
          let dotfiles = true (* exclusions handled by prune *) in
          let acc = Os.Dir.fold ~dotfiles ~prune ~recurse add_file d acc in
          loop m u xs (acc |> fail_if_error m u) ds
  in
  loop m u xs acc ds

let select b sels =
  let open B00_std.Fut.Syntax in
  let m = B0_build.memo b in
  let u = B0_build.current b in
  let scope = B0_build.current_scope_dir b in
  let abs d = Fpath.(scope // d) in
  let fs, ds, xs, futs =
    let rec loop fs ds xs futs = function
    | [] -> fs, ds, xs, futs
    | `Dir d :: ss -> loop fs ((abs d, false) :: ds) xs futs ss
    | `Dir_rec d :: ss -> loop fs ((abs d, true) :: ds) xs futs ss
    | `X x :: ss ->
        let x = Fpath.rem_empty_seg (abs x) in
        loop fs ds (Fpath.Set.add x xs) futs ss
    | `File f :: ss -> loop ((abs f) :: fs) ds xs futs ss
    | `Fut f :: ss -> loop fs ds xs (f b :: futs) ss
    in
    loop [] [] Fpath.Set.empty [] sels
  in
  let acc = Fpath.Set.empty, String.Map.empty in
  let acc = select_files m u acc fs in
  let (seen, _ as acc) = select_files_in_dirs m u xs acc ds in
  Fpath.Set.iter (Memo.file_ready m) seen;
  let* futs = Fut.of_list futs in
  let add_files acc files =
    let add_file file (seen, by_ext as acc) =
      if Fpath.Set.mem file seen then acc else
      let ext = Fpath.get_ext file in
      let by_ext = String.Map.add_to_list ext file by_ext in
      (Fpath.Set.add file seen), by_ext
    in
    Fpath.Set.fold add_file files acc
  in
  let _, acc = List.fold_left add_files acc futs in
  Fut.return { by_ext = acc; roots = Fpath.Map.empty }

(*
let root_for_file s f = match Fpath.Map.find_opt f s.roots with
| Some r -> r
| None ->
    Fmt.invalid_arg "%a: not in B00_src selection result" Fpath.pp_unquoted f
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
