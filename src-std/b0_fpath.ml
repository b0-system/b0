(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Simplified port of fpath. *)

let windows = Sys.win32

(* Errors *)

let err_invalid_seg s = B0_string.(strf "%a: invalid segment" dump s)
let err_invalid_ext s = B0_string.(strf "%a: invalid extension" dump s)
let err s = Error (`Msg (B0_string.(strf "%a: invalid path" dump s)))

(* Platform specifics. *)

module Windows = struct

  (* FIXME the {of_string,path_start} needs reviewing/testing *)

  let dir_sep_char = '\\'
  let char_is_dir_sep c = c = '\\' || c = '/'
  let dir_sep = "\\"

  let is_seg s =
    let valid c = c <> dir_sep_char && c <> '/' && c <> '\x00' in
    B0_string.for_all valid s

  let is_unc_path p = B0_string.is_prefix "\\\\" p
  let has_drive p = B0_string.exists (Char.equal ':') p
  let non_unc_path_start p = match B0_string.rindex p ':' with
  | exception Not_found -> 0
  | i -> i + 1 (* exists by construction once injected *)

  let path_start p = (* once injected this does not raise *)
    match is_unc_path p with
    | false -> non_unc_path_start p
    | true ->
        let plen = B0_string.length p in
        if plen = 2 then raise Not_found else
        let sep_from p from = B0_string.rindex_from p from dir_sep_char in
        let i = sep_from p 2 in
        let j = sep_from p (i + 1) in
        match p.[i - 1] with
        | '.' when i = 3 -> j
        | '?' when i = 3 ->
            if p.[j - 1] = ':' then j else
            if i + 3 < plen
               && p.[i + 1] = 'U' && p.[i + 2] = 'N' && p.[i + 3] = 'C'
            then sep_from p (sep_from p (j + 1) + 1)
            else sep_from p (j + 1)
        | _ -> sep_from p j

  let last_non_empty_seg_start p = match B0_string.rindex p dir_sep_char with
  | exception Not_found -> path_start p
  | k ->
      match k = B0_string.length p - 1 with
      | false -> k + 1
      | true ->
          match B0_string.rindex_from p (k - 1) dir_sep_char with
          | exception Not_found -> path_start p
          | k -> k + 1

  let chop_volume p = B0_string.with_index_range ~first:(path_start p) p

  let of_string = function
  | "" as s -> err s
  | s ->
      let p = B0_string.map (fun c -> if c = '/' then '\\' else c) s in
      match path_start p with
      | exception Not_found -> err s
      | n ->
          let p = match n = String.length p with
          | true -> (* add a root if there's only a UNC volume *) p ^ dir_sep
          | false -> p
          in
          Ok p

  let append p0 p1 =
    match is_unc_path p1 || has_drive p1 || p1.[0] = dir_sep_char with
    | true (* with volume or absolute *) -> p1
    | false ->
        let p0_last_is_sep = p0.[B0_string.length p0 - 1] = dir_sep_char in
        let sep = if p0_last_is_sep then "" else dir_sep in
        B0_string.concat sep [p0; p1]

  let is_rel p = match is_unc_path p with
  | true -> false
  | false -> p.[non_unc_path_start p] <> dir_sep_char

  let is_root p = p.[path_start p] = dir_sep_char
end

module Posix = struct
  let dir_sep_char = '/'
  let char_is_dir_sep c = c = '/'
  let dir_sep = "/"
  let is_seg s =
    let valid c = c <> dir_sep_char && c <> '\x00' in
    B0_string.for_all valid s

  let of_string s = if s = "" then err s else Ok s

  let last_non_empty_seg_start p = match B0_string.rindex p dir_sep_char with
  | exception Not_found -> 0
  | k ->
      match k = B0_string.length p - 1 with
      | false -> k + 1
      | true ->
          match B0_string.rindex_from p (k - 1) dir_sep_char with
          | exception Not_found -> 0
          | k -> k + 1

  let dir_sep_char = '/'
  let last_non_empty_seg_start p = match B0_string.rindex p dir_sep_char with
  | exception Not_found -> 0
  | k ->
      match k = B0_string.length p - 1 with
      | false -> k + 1
      | true ->
          match B0_string.rindex_from p (k - 1) dir_sep_char with
          | exception Not_found -> 0
          | k -> k + 1

  let chop_volume p = p

  let append p0 p1 =
    if p1.[0] = dir_sep_char (* absolute *) then p1 else
    let p0_last_is_sep = p0.[B0_string.length p0 - 1] = dir_sep_char in
    let sep = if p0_last_is_sep then "" else dir_sep in
    B0_string.concat sep [p0; p1]

  let is_rel p = p.[0] <> dir_sep_char
  let is_root p = B0_string.equal p dir_sep || B0_string.equal p "//"
end

let chop_volume = if windows then Windows.chop_volume else Posix.chop_volume

(* Separators and segments *)

let dir_sep_char = if windows then Windows.dir_sep_char else Posix.dir_sep_char
let dir_sep = if windows then Windows.dir_sep else Posix.dir_sep
let char_is_dir_sep =
  if windows then Windows.char_is_dir_sep else Posix.char_is_dir_sep

let last_is_dir_sep p = Char.equal (p.[String.length p - 1]) dir_sep_char

let is_seg = if windows then Windows.is_seg else Posix.is_seg
let is_rel_seg = function "." | ".." -> true | _ -> false
let rel_to_empty = function "." | ".." -> "" | seg -> seg

let last_seg_len p = match B0_string.rindex p dir_sep_char with
| exception Not_found -> B0_string.length p
| k -> B0_string.length p - (k + 1)

let last_non_empty_seg_start = match windows with
| true -> Windows.last_non_empty_seg_start
| false -> Posix.last_non_empty_seg_start

(* Paths *)

module T = struct
  type t = string (* N.B. a path is never "" *)
  let equal = B0_string.equal
  let compare = B0_string.compare
end

include T

let unsafe_of_string s = s
let of_string = if windows then Windows.of_string else Posix.of_string
let v s = match of_string s with Ok p -> p | Error (`Msg m) -> invalid_arg m
let to_string p = p

let add_seg p seg =
  if not (is_seg seg) then invalid_arg (err_invalid_seg seg) else
  let sep = if last_is_dir_sep p then "" else dir_sep in
  B0_string.concat sep [p; seg]

let append = if windows then Windows.append else Posix.append
let ( / ) = add_seg
let ( // ) = append

(* File and directory paths *)

let is_dir_path p = (* check is . .. or ends with / /. or /.. *)
  let k = B0_string.length p - 1 in
  if k < 0 then (* should not happen *) false else
  match p.[k] with
  | '/' -> true
  | '.' ->
      let k = k - 1 in
      if k < 0 then true else
      begin match p.[k] with
      | '/' -> true
      | '.' ->
          let k = k - 1 in
          k < 0 || p.[k] = '/'
      | _ -> false
      end
  | _ -> false

let is_file_path p = not (is_dir_path p)
let to_dir_path p = add_seg p ""

let filename p = match B0_string.rindex p dir_sep_char with
| exception Not_found -> if is_rel_seg p then "" else chop_volume p
| i -> rel_to_empty @@ B0_string.with_index_range ~first:(i + 1) p

let filename_equal p0 p1 =
  let p0_last_len = last_seg_len p0 in
  let p1_last_len = last_seg_len p1 in
  match p0_last_len = p1_last_len with
  | false -> false
  | true ->
      let p0_len = B0_string.length p0 in
      let p1_len = B0_string.length p1 in
      let rec loop k = match k <= 0 with
      | true -> true
      | false -> if p0.[p0_len - k] = p1.[p1_len - k] then loop (k-1) else false
      in
      loop p0_last_len

let basename p = match B0_string.rindex p dir_sep_char with
| exception Not_found -> if is_rel_seg p then "" else chop_volume p
| k ->
    match k <> B0_string.length p - 1 (* seg not empty ? *) || k = 0 with
    | true -> rel_to_empty @@ B0_string.with_index_range ~first:(k + 1) p
    | false ->
        let j = k - 1 in
        match B0_string.rindex_from p j dir_sep_char with
        | exception Not_found ->
            rel_to_empty @@ B0_string.with_index_range ~last:j p
        | i ->
            rel_to_empty @@ B0_string.with_index_range ~first:(i + 1) ~last:j p

let basename_equal p0 p1 = (* Could be improved along filename_equal *)
  B0_string.equal (basename p0) (basename p1)

let parent p =
  let plen = B0_string.length p in
  let seg_start = last_non_empty_seg_start p in
  let seg_stop = match last_is_dir_sep p with
  | true -> plen - 2
  | false -> plen - 1
  in
  let seg_len = seg_stop - seg_start + 1 in
  match seg_len with
  | 0 -> p
  | 1 when p.[seg_start] = '.' -> add_seg p ".."
  | 2 when p.[seg_start] = '.' && p.[seg_stop + 1] = '.' -> add_seg p ".."
  | _ -> add_seg (B0_string.with_index_range ~last:(seg_start - 1) p) ""

(* Predicates *)

let is_rel = if windows then Windows.is_rel else Posix.is_rel
let is_abs p = not (is_rel p)
let is_root = if windows then Windows.is_root else Posix.is_root

(* Extensions *)

type ext = string

let ext_sep_char = '.'

let rec ext_single_range spos epos k p =
  let i = String.rindex_from p k ext_sep_char (* raises if not fnd *) in
  match i <= spos with
  | true -> raise Not_found
  | false ->
      match not (Char.equal p.[i - 1] ext_sep_char) with
      | true -> i, epos
      | false -> ext_single_range spos epos (i - 1) p

let rec ext_multi_range epos k p =
  let i = String.index_from p k ext_sep_char (* raises if not fnd *) in
  match i > epos with
  | true -> raise Not_found
  | false ->
      match not (Char.equal p.[i - 1] ext_sep_char) with
      | true -> i, epos
      | false -> ext_multi_range epos (i + 1) p

let ext_range ?(multi = false) p =
  let plen = String.length p in
  let seg_start = last_non_empty_seg_start p in
  let seg_stop = match last_is_dir_sep p with
  | true -> plen - 2
  | false -> plen - 1
  in
  if seg_start >= seg_stop then raise Not_found else
  match multi with
  | true -> ext_multi_range seg_stop (seg_start + 1) p
  | false -> ext_single_range seg_start seg_stop seg_stop p

let get_ext ?multi p = match ext_range ?multi p with
| exception Not_found -> ""
| first, last -> B0_string.with_index_range ~first ~last p

let has_ext e p = match ext_range ~multi:true p with
| exception Not_found -> B0_string.equal e ""
| first, last ->
    let plen = last - first + 1 in
    let elen = B0_string.length e in
    match plen < elen with
    | true -> false
    | false ->
        let rec loop pi ei = match ei < 0 with
        | true -> true
        | false -> Char.equal p.[pi] e.[ei] && loop (pi - 1) (ei - 1)
        in
        loop last (elen - 1)

let mem_ext exts p = List.exists (fun ext -> has_ext ext p) exts

let add_ext e p =
  let plen = B0_string.length p - 1 in
  match last_is_dir_sep p with
  | false -> p ^ e
  | true ->
      let elen = B0_string.length e in
      let nlen = plen + elen in
      let n = Bytes.create nlen in
      Bytes.blit_string p 0 n 0 (plen - 1);
      Bytes.blit_string e 0 n (plen - 1) elen;
      Bytes.set n (nlen - 1) dir_sep_char;
      Bytes.unsafe_to_string n

let _rem_ext efirst elast p =
  let plen = B0_string.length p in
  match elast = plen - 1 with
  | true -> B0_string.with_index_range ~last:(efirst - 1) p
  | false ->
      let elen = elast - efirst + 1 in
      let nlen = plen - elen in
      let n = Bytes.create nlen in
      Bytes.blit_string p 0 n 0 nlen;
      Bytes.set n (nlen - 1) dir_sep_char;
      Bytes.unsafe_to_string n

let rem_ext ?multi p = match ext_range ?multi p with
| exception Not_found -> p
| efirst, elast -> _rem_ext efirst elast p

let set_ext ?multi e p = add_ext e (rem_ext ?multi p)

let split_ext ?multi p = match ext_range ?multi p with
| exception Not_found -> p, ""
| efirst, elast ->
    let ext = B0_string.with_index_range ~first:efirst ~last:elast p in
    let p = _rem_ext efirst elast p in
    p, ext

let ( + ) p e = add_ext e p
let ( -+ ) p e = set_ext e p

(* Pretty printing *)

let pp = B0_string.pp
let dump = B0_string.dump

(* String map and sets *)

module Set = struct
  include Set.Make (T)

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
  include Map.Make (T)

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
end

type 'a map = 'a Map.t

(* Uniqueness *)

let uniquify ps =
  let rec loop seen acc = function
  | [] -> List.rev acc
  | p :: ps when Set.mem p seen -> loop seen acc ps
  | p :: ps -> loop (Set.add p seen) (p :: acc) ps
  in
  loop Set.empty [] ps

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
