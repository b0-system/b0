(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let failwithf fmt =
  Format.kasprintf (fun s -> raise_notrace (Failure s)) fmt

(* Field parsing *)

let is_key = function
| "b0-version" | "libs" | "drop-libs" | "srcs" | "subs"
| "compile-kind" | "compile" | "compile-byte" | "compile-native"
| "link" | "link-byte" | "link-native" | "ocamlc" | "ocamlopt" -> true
| k when String.is_prefix ~affix:"x-" k -> true
| _ -> false

let fpath ~rel_to s =
  Fpath.(rel_to // (Fpath.of_string s |> R.failwith_error_msg))

let fpath_opt ~rel_to s = Some (fpath ~rel_to s)
let string s = s
let string_opt s = Some s

(* Fields *)

let version (map, _ as m) =
  let parse a = match int_of_string a with
  | exception Failure _ -> failwithf "could not parse version number from %S" a
  | 0 as v -> v
  | n -> failwithf "unsupported version (%d)" n
  in
  let version = "b0-version" in
  if String.Map.is_empty map then 0 else
  Sexp.atom_key parse version m

let libs = Sexp.atom_list_key string "libs" ~absent:[]
let drop_libs m =
  String.Set.of_list @@ Sexp.atom_list_key string "drop-libs" ~absent:[] m

let srcs ~rel_to ~b0_ml m =
  let parse_el k (_, l as sexp) = match Sexp.parse_list k sexp with
  | [`Atom _, _ as p; `List _, _ as l; `Atom _, _ as doc] ->
      let path = Sexp.parse_atom_kind (fpath ~rel_to) k p in
      let libs = List.map (Sexp.parse_atom k) (Sexp.parse_list k l) in
      let doc = Sexp.parse_atom k doc in
      (path, libs, doc)
  | _ ->
      failwithf "%a: %s: list not of the form (<path> (<libname>...) <doc>)"
        Sexp.pp_loc l k
  in
  let srcs = Sexp.list_key parse_el "srcs" ~absent:[] m in
  match b0_ml with
  | None -> srcs
  | Some b0_ml -> List.rev ((b0_ml, [], "B0.ml file") :: List.rev srcs)

let subs m =
  let parse_dirname s = match Fpath.is_seg s with
  | false -> failwithf "%S: not a directory name (cannot be path)" s
  | true -> s
  in
  let get_dirname k sexp = Sexp.parse_atom_kind parse_dirname k sexp in
  let parse_op dirs = function
  | "include" -> `Include dirs | "exclude" -> `Exclude dirs
  | v -> failwithf "illegal value %S must be either 'include' or 'exclude'" v
  in
  let parse k (_, loc as sexp) = match Sexp.parse_list k sexp with
  | [`Atom _, _ as op; `List _, _ as l] ->
      let dirs = String.Set.of_list (Sexp.parse_list_kind get_dirname k l) in
      Sexp.parse_atom_kind (parse_op dirs) k op
  | _ ->
      failwithf "%a: %s: list not of the form (<op> (<dirname>...))"
        Sexp.pp_loc loc k
  in
  let absent = `Exclude String.Set.empty in
  Sexp.key parse "subs" ~absent m

let compile_kind ~src m =
  let parse = function
  | "byte" -> `Byte [src]
  | "native" -> `Native [src]
  | "auto" -> `Auto
  | v ->
      failwithf "illegal value %S must be one of 'byte', 'native' or 'auto'" v
  in
  Sexp.atom_key parse "compile-kind" ~absent:`Auto m

let b0_dir ~rel_to m =
  Sexp.atom_key (fpath_opt ~rel_to) "b0-dir" ~absent:None m

let driver_dir ~rel_to m =
  Sexp.atom_key (fpath_opt ~rel_to) "driver-dir" ~absent:None m

let compile m = Sexp.atom_list_key string "compile" ~absent:[] m
let compile_byte m = Sexp.atom_list_key string "compile-byte" ~absent:[] m
let compile_native m = Sexp.atom_list_key string "compile-native" ~absent:[] m
let link m = Sexp.atom_list_key string "link" ~absent:[] m
let link_byte m = Sexp.atom_list_key string "link-byte" ~absent:[] m
let link_native m = Sexp.atom_list_key string "link-native" ~absent:[] m
let ocamlc m = Sexp.atom_key string_opt "ocamlc" ~absent:None m
let ocamlopt m = Sexp.atom_key string_opt "ocamlopt" ~absent:None m

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
