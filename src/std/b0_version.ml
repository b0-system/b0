(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type t = int * int * int * string option

let next_major ?info (maj, _, _, _) = (maj + 1, 0, 0, info)
let next_minor ?info (maj, min, _, _) = (maj, min + 1, 0, info)
let next_patch ?info (maj, min, patch, _) = (maj, min, patch + 1, info)
let with_info info (maj, min, patch, _) = (maj, min, patch, info)

(* Converting *)

let string_drop_initial_v s =
  if s = "" then s else match s.[0] with
  | 'v' | 'V' -> String.subrange ~first:1 s
  | _ -> s

let of_string s =
  if s = "" then None else
  let cut_left_plus_or_tilde s =
    let cut = match String.index_opt s '+', String.index_opt s '~' with
    | None, None -> None
    | (Some _ as i), None | None, (Some _ as i) -> i
    | Some i, Some i' -> Some (if i < i' then i else i')
    in
    match cut with
    | None -> None
    | Some i -> Some (String.subrange ~last:(i - 1) s,
                      String.subrange ~first:i s)
  in
  try match String.split_first ~sep:"." s with
  | None -> None
  | Some (maj, rest) ->
      let maj = int_of_string (string_drop_initial_v maj) in
      match String.split_first ~sep:"." rest with
      | None ->
          begin match cut_left_plus_or_tilde rest with
          | None -> Some (maj, int_of_string rest, 0, None)
          | Some (min, i) ->  Some (maj, int_of_string min, 0, Some i)
          end
      | Some (min, rest) ->
          let min = int_of_string min in
          begin match cut_left_plus_or_tilde rest with
          | None -> Some (maj, min, int_of_string rest, None)
          | Some (p, i) -> Some (maj, min, int_of_string p, Some i)
          end
  with
  | Failure _ -> None

let to_string (major, minor, patchlevel, info) =
  Fmt.str "%d.%d.%d%a" major minor patchlevel Fmt.(option string) info

(* Formatting *)

let version_st = [`Bold; `Fg `Magenta]
let pp ppf v = Fmt.st version_st ppf (to_string v)
let pp_string ppf v = Fmt.st version_st ppf v
