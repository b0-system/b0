(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* N.B. most of this is an extract of astring. *)

let strf = Format.asprintf
let dump ppf s = Format.fprintf ppf "%S" s

include String

let head s = if s = "" then None else Some s.[0]
let of_char c = String.make 1 c

(* Predicates *)

let is_prefix ~affix s =
  let len_a = length affix in
  let len_s = length s in
  if len_a > len_s then false else
  let max_idx_a = len_a - 1 in
  let rec loop i =
    if i > max_idx_a then true else
    if unsafe_get affix i <> unsafe_get s i then false else loop (i + 1)
  in
  loop 0

let is_suffix ~affix s =
  let max_idx_a = length affix - 1 in
  let max_idx_s = length s - 1 in
  if max_idx_a > max_idx_s then false else
  let rec loop i =
    if i > max_idx_a then true else
    if unsafe_get affix (max_idx_a - i) <> unsafe_get s (max_idx_s - i)
    then false
    else loop (i + 1)
  in
  loop 0

let for_all sat s =
  let max_idx = length s - 1 in
  let rec loop i =
    if i > max_idx then true else
    if sat (unsafe_get s i) then loop (i + 1) else false
  in
  loop 0

let exists sat s =
  let max_idx = length s - 1 in
  let rec loop i =
    if i > max_idx then false else
    if sat (unsafe_get s i) then true else loop (i + 1)
  in
  loop 0

(* Extracting substrings *)

let with_index_range ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else
  String.sub s first (last - first + 1)

let span ~sat s =
  let max = String.length s - 1 in
  let rec loop max s i = match i > max with
  | true -> s, ""
  | false ->
      match sat s.[i] with
      | true -> loop max s (i + 1)
      | false -> with_index_range ~last:(i - 1) s, with_index_range ~first:i s
  in
  loop max s 0

let take ~sat s =
  let max = String.length s - 1 in
  let rec loop max s i = match i > max with
  | true -> s
  | false ->
      match sat s.[i] with
      | true -> loop max s (i + 1)
      | false -> with_index_range ~last:(i - 1) s
  in
  loop max s 0

let drop ~sat s =
  let max = String.length s - 1 in
  let rec loop max s i = match i > max with
  | true -> ""
  | false ->
      match sat s.[i] with
      | true -> loop max s (i + 1)
      | false -> with_index_range ~first:i s
  in
  loop max s 0


let err_empty_sep = "~sep is an empty string"

let fcut ~sep s =
  let sep_len = length sep in
  if sep_len = 0 then invalid_arg err_empty_sep else
  let s_len = length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - sep_len in
  let rec check_sep i k =
    if k > max_sep_idx then
      let r_start = i + sep_len in
      Some (String.sub s 0 i,
            String.sub s r_start (s_len - r_start))
    else
      if unsafe_get s (i + k) = unsafe_get sep k
      then check_sep i (k + 1)
      else scan (i + 1)
  and scan i =
    if i > max_s_idx then None else
    if String.get s i = String.get sep 0 then check_sep i 1 else scan (i + 1)
  in
  scan 0

let rcut ~sep s =
  let sep_len = length sep in
  if sep_len = 0 then invalid_arg err_empty_sep else
  let s_len = length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - 1 in
  let rec check_sep i k =
    if k > max_sep_idx then
      let r_start = i + sep_len in
      Some (String.sub s 0 i,
            String.sub s r_start (s_len - r_start))
    else
      if unsafe_get s (i + k) = unsafe_get sep k
      then check_sep i (k + 1)
      else rscan (i - 1)
  and rscan i =
    if i < 0 then None else
    if String.get s i = String.get sep 0 then check_sep i 1 else rscan (i - 1)
  in
  rscan (max_s_idx - max_sep_idx)

let cut ?(rev = false) ~sep s = if rev then rcut ~sep s else fcut ~sep s

let fcuts ~no_empty ~sep s =
  let rec loop acc s = match fcut ~sep s with
  | Some (v, vs) -> loop (if no_empty && v = "" then acc else (v :: acc)) vs
  | None -> List.rev (if no_empty && s = "" then acc else (s :: acc))
  in
  loop [] s

let rcuts ~no_empty ~sep s =
  let rec loop acc s = match rcut ~sep s with
  | Some (vs, v) -> loop (if no_empty && v = "" then acc else (v :: acc)) vs
  | None -> List.rev (if no_empty && s = "" then acc else (s :: acc))
  in
  loop [] s

let cuts ?(rev = false) ?(empty = true) ~sep s =
  let no_empty = not empty in
  if rev then rcuts ~no_empty ~sep s else fcuts ~no_empty ~sep s

(* Traversing *)

let mapi f s =
  let max = length s - 1 in
  let rec try_no_alloc i = match i > max with
  | true -> s
  | false ->
      let c = String.get s i in
      let cm = f i c in
      match cm = c with
      | true -> try_no_alloc (i + 1)
      | false ->
          let b = Bytes.of_string s in
          Bytes.set b i cm;
          with_buf b (i + 1)
  and with_buf b i = match i > max with
  | true -> Bytes.unsafe_to_string b
  | false -> Bytes.set b i (f i (String.get s i)); with_buf b (i + 1)
  in
  try_no_alloc 0

let map f s = mapi (fun _ c -> f c) s

(* Version strings *)

let parse_version v =
  let version = if is_prefix "v" v then with_index_range ~first:1 v else v in
  try match cut ~sep:"." version with
  | None -> None
  | Some (maj, rest) ->
      let maj = int_of_string maj in
      match cut ~sep:"." rest with
      | None ->
          begin match cut ~sep:"+" rest with
          | None -> Some (maj, int_of_string rest, 0, None)
          | Some (min, i) ->  Some (maj, int_of_string min, 0, Some i)
          end
      | Some (min, rest) ->
          let min = int_of_string min in
          begin match cut ~sep:"+" rest with
          | None -> Some (maj, min, int_of_string rest, None)
          | Some (p, i) -> Some (maj, min, int_of_string p, Some i)
          end
  with
  | Failure _ -> None

let drop_initial_v version = match head version with
| Some ('v' | 'V') -> with_index_range ~first:1 version
| None | Some _ -> version

(* Pretty-printing *)

let pp = Format.pp_print_string
let dump ppf s = Format.fprintf ppf "%S" s

(* String map and sets *)

module Set = struct
  include Set.Make (String)

  let pp ?sep:(pp_sep = Format.pp_print_cut) pp_elt ppf ss =
    let pp_elt elt is_first =
      if is_first then () else pp_sep ppf ();
      pp_elt ppf elt; false
    in
    ignore (fold pp_elt ss true)

  let dump_str = dump
  let dump ppf ss =
    let pp_elt elt is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "%a" dump_str elt;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_elt ss true);
    Format.fprintf ppf "}@]";
    ()
end

type set = Set.t

module Map = struct
  include Map.Make (String)

  let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
  let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

  let pp ?sep:(pp_sep = Format.pp_print_cut) pp_binding ppf (m : 'a t) =
    let pp_binding k v is_first =
      if is_first then () else pp_sep ppf ();
      pp_binding ppf (k, v); false
    in
    ignore (fold pp_binding m true)

  let dump_str = dump
  let dump pp_v ppf m =
    let pp_binding k v is_first =
      if is_first then () else Format.fprintf ppf "@ ";
      Format.fprintf ppf "@[<1>(@[%a@],@ @[%a@])@]" dump_str k pp_v v;
      false
    in
    Format.fprintf ppf "@[<1>{";
    ignore (fold pp_binding m true);
    Format.fprintf ppf "}@]";
    ()

  let dump_string_map ppf m = dump dump_str ppf m
end

type 'a map = 'a Map.t

(* Uniqueness *)

let uniquify ss =
  let rec loop seen acc = function
  | [] -> List.rev acc
  | s :: ss when Set.mem s seen -> loop seen acc ss
  | s :: ss -> loop (Set.add s seen) (s :: acc) ss
  in
  loop Set.empty [] ss

let unique ~exists n =
  let rec loop i n = match i > 1_000_000_000 with
  | true -> B0_result.R.error_msgf "Could not uniquify %s after 1e9 retries." n
  | false ->
      let r = strf "%s~%d" n i in
      if exists r then loop (i + 1) n else Ok r
  in
  if exists n then loop 1 n else Ok n

(* Suggesting *)

let edit_distance s0 s1 =
  (* As found here http://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
  let minimum a b c = min a (min b c) in
  let m = String.length s0 in
  let n = String.length s1 in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in
  for i = 0 to m do d.(i).(0) <- i done;
  for j = 0 to n do d.(0).(j) <- j done;
  for j = 1 to n do
    for i = 1 to m do
      if s0.[i-1] = s1.[j-1]
      then d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
      d.(i).(j) <- minimum
          (d.(i-1).(j) + 1)   (* a deletion *)
          (d.(i).(j-1) + 1)   (* an insertion *)
          (d.(i-1).(j-1) + 1) (* a substitution *)
    done;
  done;
  d.(m).(n)

let suggest ?(dist = 2) candidates s =
  let add (min, acc) name =
    let d = edit_distance s name in
    if d = min then min, (name :: acc) else
    if d < min then d, [name] else
    min, acc
  in
  let d, suggs = List.fold_left add (max_int, []) candidates in
  if d <= dist (* suggest only if not too far *) then suggs else []

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
