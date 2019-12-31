(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B00

module Ocamlpath = struct
  let get_var parse var m =
    let env = Env.env (Memo.env m) in
    match String.Map.find var env with
    | exception Not_found -> None
    | "" -> None
    | v ->
        match parse v with
        | Error e -> Memo.fail m "%s parse: %s" var v
        | Ok v -> Some v

  let get m ps k = match ps with
  | Some ps -> k ps
  | None ->
      match get_var Fpath.list_of_search_path "OCAMLPATH" m with
      | Some ps -> k ps
      | None ->
          match get_var Fpath.of_string "OPAM_SWITCH_PREFIX" m with
          | Some p -> k [Fpath.(p / "lib")]
          | None ->
              Memo.fail m
                "No %a determined in the build environment."
                Fmt.(code string) "OCAMLPATH"
end

(* Library names. *)

module Name = struct
  let fpath_to_name s =
    let b = Bytes.of_string (Fpath.to_string s) in
    for i = 0 to Bytes.length b - 1 do
      if Bytes.get b i = Fpath.dir_sep_char then Bytes.set b i '.';
    done;
    Bytes.unsafe_to_string b

  let name_to_fpath s =
    let err s exp = Fmt.error "%S: not a library name, %s" s exp in
    let err_start s = err s "expected a starting lowercase ASCII letter" in
    let b = Bytes.of_string s in
    let max = String.length s - 1 in
    let rec loop i ~id_start = match i > max with
    | true ->
        if id_start then err_start s else
        Ok (Fpath.v (Bytes.unsafe_to_string b))
    | false when id_start ->
        begin match Bytes.get b i with
        | 'a' .. 'z' -> loop (i + 1) ~id_start:false
        | _ -> err_start s
        end
    | false ->
        begin match Bytes.get b i with
        | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> loop (i + 1) ~id_start:false
        | '.' -> Bytes.set b i Fpath.dir_sep_char; loop (i + 1) ~id_start:true
        | c -> err s (Fmt.str "illegal character %C" c)
        end
    in
    loop 0 ~id_start:true

  type t = { dir : Fpath.t; archive_name : string option }

  let of_string s = match String.cut_left ~sep:"/" s with
  | None ->
      Result.bind (name_to_fpath s) @@ fun name ->
      Ok { dir = name; archive_name = None }
  | Some (n, archive) ->
      Result.bind (name_to_fpath n) @@ fun name ->
      Ok { dir = name; archive_name = Some archive }

  let to_string n = match n.archive_name with
  | None -> fpath_to_name n.dir
  | Some aname -> String.concat Fpath.dir_sep [fpath_to_name n.dir; aname]

  let v s = match of_string s with Ok v -> v | Error e -> invalid_arg e
  let is_legacy n = Option.is_some n.archive_name
  let equal n0 n1 = compare n0 n1 = 0
  let compare n0 n1 = compare n0 n1
  let pp = Fmt.using to_string (Fmt.code Fmt.string)

  module T = struct
    type nonrec t = t let compare = compare
  end
  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

(* Libraries *)

type t = { name : Name.t; dir : Fpath.t; }

let name l = l.name
let dir l = l.dir

let archive_name l = match l.name.Name.archive_name with
| None -> "lib" | Some a -> a

let archive ~code l =
  let a = archive_name l ^ B0_ocaml.Cobj.archive_ext_of_code code in
  Fpath.(l.dir / a)

(* Resolver *)

module Resolver = struct
  (* TODO cache results.
     Base that on Cobjs. See b0caml resolver. *)

  type lib = t
  type t =
    { memo : B00.Memo.t;
      memo_dir : Fpath.t;
      ocamlpath : Fpath.t list; }

  let create memo ~memo_dir ~ocamlpath =  { memo; memo_dir; ocamlpath }

  let find r n k =
    let rec loop n = function
    | d :: ds ->
        let libdir = Fpath.(d // n.Name.dir) in
        begin match Log.if_error ~use:false (Os.Dir.exists libdir) with
        | false -> loop n ds
        | true -> k { name = n; dir = libdir }
        end
    | [] ->
        Memo.fail r.memo
          "@[<v>@[OCaml library %a not found in OCAMLPATH@]:@, \
           @[<v>%a@]@]"
          Name.pp n (Fmt.list Fpath.pp_unquoted) r.ocamlpath
    in
    loop n r.ocamlpath
end

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
