(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Errors *)

let err_invalid_seg s =
  B0__fmt.str "%a: Invalid path segment" B0__fmt.OCaml.string s

let err_start s =
  B0__fmt.error "%a: Not a path" B0__fmt.OCaml.string s

let err_null s =
  B0__fmt.error "%a: Not a path: has null bytes" B0__fmt.OCaml.string s

let err_empty s =
  B0__fmt.error "%a: Not a path: is empty" B0__fmt.OCaml.string s

let err_ext_root s ~ext =
  B0__fmt.str "Cannot append extension %s to root path %s" ext s

(* Pct encoding *)

let pct_esc_len ~escape_space = function
| '%' | '#' | '?' -> 3
| ' ' when escape_space -> 3
| c when B0__char.Ascii.is_control c -> 3
| _ -> 1

let set_pct_encoded b i c =
  let c = Char.code c in
  let hi = B0__char.Ascii.upper_hex_digit_of_int (c lsr 4) in
  let lo = B0__char.Ascii.upper_hex_digit_of_int (c      ) in
  Bytes.set b i '%'; Bytes.set b (i + 1) hi; Bytes.set b (i + 2) lo;
  i + 3

let pct_esc_set_char ~escape_space b i = function
| '%' | '#' | '?' as c -> set_pct_encoded b i c
| ' ' as c when escape_space -> set_pct_encoded b i c
| c when B0__char.Ascii.is_control c -> set_pct_encoded b i c
| c -> Bytes.set b i c; i + 1

(* Platform specifics. *)

let undouble_sep sep dbl_sep s =
  let rec loop last_is_sep b k s i max =
    if i > max then Bytes.unsafe_to_string b else
    let c = String.get s i in
    let c_is_sep = Char.equal c sep in
    let is_dbl = last_is_sep && c_is_sep in
    if is_dbl
    then loop c_is_sep b k s (i + 1) max (* skip over *)
    else (Bytes.set b k c; loop c_is_sep b (k + 1) s (i + 1) max)
  in
  let len = String.length s in
  loop false (Bytes.create (len - dbl_sep)) 0 s 0 (len - 1)

module Windows = struct

  (* XXX the {of_string,path_start} needs reviewing/testing *)

  let natural_dir_sep_char = '\\'
  let natural_dir_sep = "\\"
  let is_dir_sep_char c = c = '\\' || c = '/'
  let is_segment s =
    let valid c = c <> natural_dir_sep_char && c <> '/' && c <> '\x00' in
    String.for_all valid s

  let is_unc_path p = String.starts_with ~prefix:"\\\\" p
  let has_drive p = String.exists (Char.equal ':') p
  let non_unc_path_start p = match String.rindex p ':' with
  | exception Not_found -> 0
  | i -> i + 1 (* exists by construction once injected *)

  let path_start p = (* once [p] is injected this does not raise *)
    if not (is_unc_path p) then non_unc_path_start p else
    let plen = String.length p in
    if plen = 2 then raise Not_found else
    let sep_from p from = String.index_from p from natural_dir_sep_char in
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

  (* TODO decide what to do with the : of drives *)
  let with_volume vol p = vol ^ (B0__string.subrange ~first:(path_start p) p)
  let cut_volume p = B0__string.cut_first (path_start p) p
  let drop_volume p = B0__string.drop_first (path_start p) p
  let take_volume p = B0__string.take_first (path_start p) p

  let last_non_empty_seg_start p =
    match String.rindex p natural_dir_sep_char with
    | exception Not_found -> path_start p
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) natural_dir_sep_char with
            | exception Not_found -> path_start p
            | k -> k + 1

  let backslashify s =
    let b = Bytes.copy (Bytes.unsafe_of_string s) in
    for i = 0 to Bytes.length b - 1 do
      if Bytes.get b i = '/' then Bytes.set b i '\\'
    done;
    Bytes.unsafe_to_string b

  let of_string s =
    if s = "" then err_empty s else
    try
      let p =
        let rec loop has_slash last_is_sep dbl_sep i max = match i > max with
        | true ->
            let s = if has_slash then backslashify s else s in
            if dbl_sep > 0
            then undouble_sep natural_dir_sep_char dbl_sep s
            else s
        | false ->
            let c = String.unsafe_get s i in
            if Char.equal c '\x00' then raise Exit else
            let is_slash = Char.equal c '/' in
            let has_slash = has_slash || is_slash in
            let c_is_sep =
              (is_slash || Char.equal c natural_dir_sep_char) && i <> 0
            in
            let is_dbl = last_is_sep && c_is_sep in
            let dbl_sep = if is_dbl then dbl_sep + 1 else dbl_sep in
            loop has_slash c_is_sep dbl_sep (i + 1) max
        in
        loop false false 0 0 (String.length s - 1)
      in
      match path_start p with
      | exception Not_found -> err_start p
      | n ->
          let p = match n = String.length p with
          | true ->
              (* add root if there's only a UNC volume *) p ^ natural_dir_sep
          | false -> p
          in
          Ok p
    with Exit -> err_null s

  let append p0 p1 =
    if is_unc_path p1 || has_drive p1 || p1.[0] = natural_dir_sep_char
    then (* with volume or absolute *) p1 else
    let p0_last_is_sep = p0.[String.length p0 - 1] = natural_dir_sep_char in
    let sep = if p0_last_is_sep then "" else natural_dir_sep in
    String.concat sep [p0; p1]

  let is_relative p =
    not (is_unc_path p) && p.[non_unc_path_start p] <> natural_dir_sep_char

  let to_url_path ?(escape_space = true) p =
    let set_char b i = function
    | '\\' -> Bytes.set b i '/'; i + 1
    | c -> pct_esc_set_char ~escape_space b i c
    in
    let p = B0__string.byte_escaper (pct_esc_len ~escape_space) set_char p in
    if has_drive p then "/" ^ p else p
end

module Posix = struct
  let natural_dir_sep_char = '/'
  let natural_dir_sep = "/"
  let is_dir_sep_char c = Char.equal c '/'
  let has_dir_sep ~start s =
    try ignore (String.index_from s start '/'); true with
    | Not_found -> false

  let is_segment s =
    String.for_all (fun c -> c <> natural_dir_sep_char && c <> '\x00') s

  let normalize s dbl_sep = (* assert (dbl_sep >= 1) *)
    let len = String.length s in
    let dbl_start = s.[0] = '/' && s.[1] = '/' in
    if len = 2 then "/" (* [s] can only be // *) else
    if dbl_start && dbl_sep = 1
    then (if has_dir_sep ~start:2 s then s else s ^ "/") else
    let unsep = undouble_sep natural_dir_sep_char dbl_sep s in
    if not dbl_start || s.[2] = '/' (* had no volume *) then unsep else
    "/" ^ unsep

  let of_string = function
  | "" as s -> err_empty s
  | s ->
      try
        let rec loop last_is_sep dbl_sep i max =
          if i > max
          then (if dbl_sep = 0 then Ok s else Ok (normalize s dbl_sep))
          else
          let c = String.unsafe_get s i in
          if Char.equal c '\x00' then raise Exit else
          let c_is_sep = Char.equal c natural_dir_sep_char in
          let is_dbl = last_is_sep && c_is_sep in
          let dbl_sep = if is_dbl then dbl_sep + 1 else dbl_sep in
          loop c_is_sep dbl_sep (i + 1) max
        in
        loop false 0 0 (String.length s - 1)
      with
      | Exit -> err_null s

  let last_non_empty_seg_start p =
    match String.rindex p natural_dir_sep_char with
    | exception Not_found -> 0
    | k ->
        match k = String.length p - 1 with
        | false -> k + 1
        | true ->
            match String.rindex_from p (k - 1) natural_dir_sep_char with
            | exception Not_found -> 0
            | k -> k + 1

  let path_start p =
    let len = String.length p in
    if len < 2 || not (p.[0] = '/' && p.[1] = '/') then 0 else
    try String.index_from p 2 '/' with
    | Not_found -> assert false (* by invariant see normalize_volume *)

  let drop_volume p = B0__string.subrange ~first:(path_start p) p
  let take_volume p =
    B0__string.subrange ~first:2 (* skip // *) ~last:(path_start p - 1) p

  let cut_volume p =
    let path_start = path_start p in
    if path_start = 0 then "", p else
    (B0__string.subrange ~first:2 (* skip // *) ~last:(path_start - 1) p,
     B0__string.subrange ~first:path_start p)

  let with_volume vol p =
    if vol = "" then p else
    if not (is_segment vol) then invalid_arg (err_invalid_seg vol) else
    let start = path_start p in
    let q = if start = 0 then p else B0__string.subrange ~first:start p in
    let sep = if q.[0] = '/' then "" else natural_dir_sep in
    String.concat "" ["//"; vol; sep; p]

  let append p0 p1 =
    if p1.[0] = natural_dir_sep_char (* absolute *) then p1 else
    let p0_last_is_sep = p0.[String.length p0 - 1] = natural_dir_sep_char in
    let sep = if p0_last_is_sep then "" else natural_dir_sep in
    String.concat sep [p0; p1]

  let is_relative p = p.[0] <> natural_dir_sep_char

  let to_url_path ?(escape_space = true) p =
    let esc_len = pct_esc_len ~escape_space in
    B0__string.byte_escaper esc_len (pct_esc_set_char ~escape_space) p
end

(* Separators and segments *)

let natural_dir_sep_char =
  if Sys.win32 then Windows.natural_dir_sep_char else Posix.natural_dir_sep_char

let natural_dir_sep =
  if Sys.win32 then Windows.natural_dir_sep else Posix.natural_dir_sep

let is_dir_sep_char =
  if Sys.win32 then Windows.is_dir_sep_char else Posix.is_dir_sep_char

let is_dir_sep s = String.length s = 1 && is_dir_sep_char s.[0]

let last_is_sep p =
  Char.equal (p.[String.length p - 1]) natural_dir_sep_char

let is_segment = if Sys.win32 then Windows.is_segment else Posix.is_segment
let is_segment_relative = function "." | ".." -> true | _ -> false

let last_seg_len p = match String.rindex p natural_dir_sep_char with
| exception Not_found -> String.length p
| k -> String.length p - (k + 1)

let last_non_empty_seg_start =
  if Sys.win32 then Windows.last_non_empty_seg_start else
  Posix.last_non_empty_seg_start

(* Paths *)

type t = string
(* Note: a path is never "" and on POSIX if it starts with // there is always
   at least a subseqent /. *)

let equal = String.equal
let compare = String.compare

let of_string = if Sys.win32 then Windows.of_string else Posix.of_string
let to_string p = p
let v s = match of_string s with Ok p -> p | Error m -> invalid_arg m
let fmt fmt = B0__fmt.kstr v fmt

let append = if Sys.win32 then Windows.append else Posix.append

let path_start = if Sys.win32 then Windows.path_start else Posix.path_start

(* FIXME this is wrong on windows or on Unix with volumes *)
let current_dir = "."
let current_dir_dir = "." ^ natural_dir_sep
let is_current_dir p = String.equal p "." || String.equal p current_dir_dir
let parent_dir_dir = ".." ^ natural_dir_sep
let is_parent_dir p = String.equal p ".." || String.equal p parent_dir_dir

(* Volumes *)

let take_volume = if Sys.win32 then Windows.take_volume else Posix.take_volume
let drop_volume = if Sys.win32 then Windows.take_volume else Posix.drop_volume
let cut_volume = if Sys.win32 then Windows.cut_volume else Posix.cut_volume
let with_volume = if Sys.win32 then Windows.with_volume else Posix.with_volume

(* Segments *)

let take_last_segment p = match String.rindex p natural_dir_sep_char with
| exception Not_found -> B0__string.subrange ~first:(path_start p) p
| sep -> B0__string.subrange ~first:(sep + 1) p

let drop_last_segment p = match String.rindex p natural_dir_sep_char with
| exception Not_found -> with_volume (take_volume p) current_dir
| sep ->
    let last = if sep = path_start p then sep else sep - 1 in
    B0__string.subrange ~last p

let cut_last_segment p = (drop_last_segment p, take_last_segment p)

let append_seg' p seg =
  if not (is_segment seg) then invalid_arg (err_invalid_seg seg) else
  let sep = if last_is_sep p then "" else natural_dir_sep in
  String.concat sep [p; seg]

let append_segment p seg = try Ok (append_seg' p seg) with
| Invalid_argument e -> Error e

let segments_when_no_volume p = String.split_on_char natural_dir_sep_char p

let to_segments p = segments_when_no_volume (drop_volume p)
let of_segments ?(volume = "") segs = match segs with
| [] -> invalid_arg "[] is not a valid of segments"
| [""] -> invalid_arg "[\"\"] is not a valid list of segments"
| segs ->
    let check s = if not (is_segment s) then invalid_arg (err_invalid_seg s) in
    List.iter check segs;
    let p = v (String.concat natural_dir_sep segs) in
    let p = (* on POSIX empty segs can make [v] parse a volume, drop it *)
      if String.length p >= 2
      && p.[0] = natural_dir_sep_char
      && p.[1] = natural_dir_sep_char then B0__string.drop_first 1 p else p
    in
    if volume <> "" then with_volume volume p else p

(* Famous file paths *)

let null = v (if Sys.win32 then "NUL" else "/dev/null")
let dash = v "-"
let is_null p = equal p null
let is_dash p = equal p dash

let is_root p =
  let max = String.length p - 1 in
  let path_start = path_start p in
  path_start = max && is_dir_sep_char p.[path_start]

let root_of p = with_volume (take_volume p) natural_dir_sep

let drop_root_sep p =
  let path_start = path_start p in
  if not (is_dir_sep_char p.[path_start]) then p else
  if not Sys.win32 then
    let max = String.length p - 1 in
    if max = path_start then "." else
    B0__string.drop_first (path_start + 1) p
  else
  let volume, path = cut_volume p in
  with_volume volume (B0__string.drop_first 1 path)

let ensure_root_sep p =
  let drop_dots p = if is_current_dir p || is_parent_dir p then "" else p in
  let path_start = path_start p in
  if is_dir_sep_char p.[path_start] then p else
  if path_start = 0 then natural_dir_sep ^ drop_dots p else
  let volume, path = cut_volume p in
  with_volume volume (natural_dir_sep ^ (drop_dots p))

(* Syntactic directory paths *)

let is_syntactic_dir p = (* check is . .. or ends with / /. or /.. *)
  let k = String.length p - 1 in
  if k < 0 then (* should not happen *) false else
  match p.[k] with
  | c when Char.equal c natural_dir_sep_char -> true
  | '.' ->
      let k = k - 1 in
      if k < 0 then true else
      begin match p.[k] with
      | c when Char.equal c natural_dir_sep_char -> true
      | '.' ->
          let k = k - 1 in
          k < 0 || Char.equal p.[k] natural_dir_sep_char
      | _ -> false
      end
  | _ -> false

let ensure_trailing_dir_sep p = append_seg' p ""

let drop_trailing_dir_sep p = match String.length p with
| 1 -> p
| 2 ->
    if p.[0] <> natural_dir_sep_char && p.[1] = natural_dir_sep_char
    then B0__string.of_char p.[0]
    else p
| len ->
    let max = len - 1 in
    if p.[max] <> natural_dir_sep_char then p else
    B0__string.subrange p ~last:(max - 1)

let try_drop_relative_dirs p =
  let rec loop acc = function
  | [] -> List.rev acc
  | "." :: segs -> loop acc segs
  | ".." :: segs ->
      let acc = match acc with
      | "" :: _ -> (* absolute root *) acc
      | ([] | ".." :: _) as acc -> (* unresovable rel *) ".." :: acc
      | _ :: acc -> acc
      in
      loop acc segs
  | seg :: segs -> loop (seg :: acc) segs
  in
  let volume, path = cut_volume p in
  let segs = segments_when_no_volume path in
  let is_absolute = List.hd segs = "" in
  let segs = match loop [] segs with
  | [""] when is_absolute -> ["";""]
  | [] | [""] -> ["."]
  |  segs -> segs
  in
  of_segments ~volume segs

(* Strict prefixes *)

let strictly_starts_with ~prefix:pre p =
  String.starts_with ~prefix:pre p &&
  let suff_start = String.length pre in
  let p_len = String.length p in
  (* Check [prefix] and [p] are not equal modulo directoryness. *)
  if suff_start = p_len then false else
  if suff_start = p_len - 1 && p.[suff_start] = natural_dir_sep_char
  then false else
  (* Check the prefix is segment based *)
  (pre.[suff_start - 1] = natural_dir_sep_char ||
   p.[suff_start] = natural_dir_sep_char)

let drop_strict_prefix ~prefix:pre p =
  if not (strictly_starts_with ~prefix:pre p) then None else
  let len = String.length pre in
  let first = if p.[len] = natural_dir_sep_char then len + 1 else len in
  Some (B0__string.subrange p ~first)

let remove_strictly_prefixed dirs =
  let is_strictly_prefixed d by = strictly_starts_with ~prefix:by d in
  let not_prefixed ~by:dirs d =
    not (List.exists (is_strictly_prefixed d) dirs)
  in
  List.filter (not_prefixed ~by:dirs) dirs

let reroot ~src_root ~dst_root src =
  let rel_file = Option.get (drop_strict_prefix ~prefix:src_root src) in
  append dst_root rel_file

(* Predicates and comparisons *)

let is_relative = if Sys.win32 then Windows.is_relative else Posix.is_relative
let is_absolute p = not (is_relative p)

(* File extensions *)

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
  let seg_stop = plen - (if last_is_sep p then 2 else 1) in
  if seg_start >= seg_stop then raise Not_found else
  if multi
  then ext_multi_range seg_stop (seg_start + 1) p
  else ext_single_range seg_start seg_stop seg_stop p

let exists_ext p = match ext_range ~multi:false p with
| exception Not_found -> false
| _ -> true

let exists_multi_ext p = match ext_range ~multi:false p with
| exception Not_found -> false
| first, last ->
    let dots = ref 0 in
    let i = ref first in
    while !i <= last && !dots <= 0 do
      if Char.equal p.[!i] ext_sep_char then incr dots;
      incr i
    done;
    !dots > 1

let has_ext e p = match ext_range ~multi:true p with
| exception Not_found -> String.equal e ""
| first, last ->
    let plen = last - first + 1 in
    let elen = String.length e in
    match plen < elen with
    | true -> false
    | false ->
        let rec loop pi ei = match ei < 0 with
        | true -> true
        | false -> Char.equal p.[pi] e.[ei] && loop (pi - 1) (ei - 1)
        in
        loop last (elen - 1)

let mem_ext exts p = List.exists (fun ext -> has_ext ext p) exts

let take_ext ~multi p = match ext_range ~multi p with
| exception Not_found -> ""
| first, last -> B0__string.subrange ~first ~last p

let rem_ext' efirst elast p =
  let plen = String.length p in
  match elast = plen - 1 with
  | true -> B0__string.subrange ~last:(efirst - 1) p
  | false ->
      let elen = elast - efirst + 1 in
      let nlen = plen - elen in
      let n = Bytes.create nlen in
      Bytes.blit_string p 0 n 0 nlen;
      Bytes.set n (nlen - 1) natural_dir_sep_char;
      Bytes.unsafe_to_string n

let drop_ext ~multi p = match ext_range ~multi p with
| exception Not_found -> p
| efirst, elast -> rem_ext' efirst elast p

let cut_ext ~multi p = match ext_range ~multi p with
| exception Not_found -> p, ""
| efirst, elast ->
    let ext = B0__string.subrange ~first:efirst ~last:elast p in
    let p = rem_ext' efirst elast p in
    p, ext

let append_ext p ext =
  if ext <> "" && not (is_segment ext) then invalid_arg (err_invalid_seg ext);
  if is_root p then invalid_arg (err_ext_root p ~ext) else
  let plen = String.length p in
  match last_is_sep p with
  | false -> p ^ ext
  | true ->
      let elen = String.length ext in
      let nlen = plen + elen in
      let n = Bytes.create nlen in
      Bytes.blit_string p 0 n 0 (plen - 1);
      Bytes.blit_string ext 0 n (plen - 1) elen;
      Bytes.set n (nlen - 1) natural_dir_sep_char;
      Bytes.unsafe_to_string n

let with_ext ~multi e p = append_ext (drop_ext ~multi p) e

(* Basename and parent directory *)

let basename ?(drop_exts = false) p =
  let max = String.length p - 1 in
  let first, last = match String.rindex p natural_dir_sep_char with
  | exception Not_found -> (* B *) path_start p, max
  | k when k <> max || k = 0 -> (* /B or .../B *) k + 1, max
  | k -> (* .../ *)
      let j = k - 1 in
      match String.rindex_from p j natural_dir_sep_char with
      | exception Not_found -> (* B/ *) path_start p, j
      | i -> (* .../B/ *) i + 1, j
  in
  match last - first + 1 with
  | 1 when p.[first] = '.' -> ""
  | 2 when p.[first] = '.' && p.[first + 1] = '.' -> ""
  | _ when not drop_exts -> B0__string.subrange ~first ~last p
  | _ -> (* Drop multiple extension *)
      let rec loop first last i = match i > last with
      | true -> B0__string.subrange ~first ~last p
      | false ->
          match p.[i] = ext_sep_char with
          | false -> loop first last (i + 1)
          | true ->
              if p.[i - 1] = ext_sep_char then loop first last (i + 1) else
              B0__string.subrange ~first ~last:(i - 1) p
      in
      loop first last (first + 1)

let basepath ?drop_exts p = match basename ?drop_exts p with
| "" -> "." | p -> p

let rec parent p =
  let plen = String.length p in
  let path_start = path_start p in
  let seg_first = last_non_empty_seg_start p in
  let seg_last = if last_is_sep p then plen - 2 else plen - 1 in
  let seg_len = seg_last - seg_first + 1 in
  match seg_len with
  | 0 -> p
  | 1 when p.[seg_first] = '.' ->
      if seg_first = path_start then ".." else (* Chop '.' and try again *)
      parent (B0__string.subrange ~last:(seg_first - 1) p)
  | 2 when p.[seg_first] = '.' && p.[seg_last] = '.' ->
      let via_dotdot p = append_seg' p ".." in
      via_dotdot p
  | _ when seg_first = path_start -> "."
  | _ -> append_seg' (B0__string.subrange ~last:(seg_first - 1) p) ""

let equal_basename p0 p1 = (* XXX could avoid alloc *)
  String.equal (basename p0) (basename p1)

let relative ~to_dir p =
  (* FIXME this function needs to be rewritten *)
  (* XXX dirty, need a normalization function and/or a better parent
     to handle that. Also the results should be normalized again.  *)
  if B0__string.includes
      ~affix:".." to_dir (* cmon that's obvi..ously wrong *)
  then B0__fmt.invalid_arg "%s: no dotdot allowed" p;
  let to_dir = ensure_trailing_dir_sep to_dir in
  match drop_strict_prefix ~prefix:to_dir p with
  | Some q -> q
  | None ->
      let rec loop loc dir =
        if is_current_dir dir then p else
        if is_root dir then
          begin match drop_strict_prefix ~prefix:"/" p with
          | None -> p
          | Some rel_root -> append loc rel_root
          end
        else
        match drop_strict_prefix ~prefix:dir p with
        | Some q -> append loc q
        | None -> loop (append_seg' loc "..") (parent dir)
      in
      loop ".." (parent to_dir)

(* Converting *)

let to_url_path = if Sys.win32 then Windows.to_url_path else Posix.to_url_path

let pp_quoted ppf p = B0__string.pp ppf (Filename.quote p)
let pp_unquoted = B0__string.pp
let pp ppf p =
  if String.exists (Char.equal ' ') p
  then pp_quoted ppf p
  else B0__string.pp ppf p

let pp_dump = B0__fmt.OCaml.string

let error p fmt = B0__fmt.error ("%a: " ^^ fmt) pp_unquoted p
let prefix_msg p msg = B0__fmt.str "%a: %s" pp_unquoted p msg

(* Uniqueness *)

let distinct = B0__string.distinct

(* Path and sets *)

type path = t
module Set = struct
  let pp_set ppf ss =
    B0__fmt.pf ppf "@[<1>{%a}@]" (B0__string.Set.pp ~sep:B0__fmt.sp pp) ss

  include B0__string.Set
end
module Map = B0__string.Map

(* Sorts *)

let sort_by_parent ps =
  let add_path p acc = Map.add_to_set (module Set) (parent p) p acc in
  Set.fold add_path ps Map.empty

let sort_by_ext ~multi ps =
  let add_path p acc =
    B0__string.Map.add_to_set (module Set) (take_ext ~multi p) p acc
  in
  Set.fold add_path ps B0__string.Map.empty

(* Search paths *)

let search_path_sep = if Sys.win32 then ";" else ":"
let list_of_search_path ?(sep = search_path_sep) path =
  let rec loop acc = function
  | ""  -> Ok (List.rev acc)
  | rest ->
      let dir, rest = match B0__string.split_first ~sep rest with
      | None -> rest, ""
      | Some (dir, rest) -> dir, rest
      in
      if dir = "" then loop acc rest else
      match of_string dir with
      | Ok dir -> loop (dir :: acc) rest
      | Error e -> B0__fmt.error "Illegal path %a in search path: %s" pp dir e
  in
  loop [] path

(* Operators *)

let ( / ) = append_seg'
let ( // ) = append
let ( + ) = append_ext
let ( -+ ) p e = with_ext ~multi:false e p
