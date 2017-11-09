(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let failwithf fmt =
  Format.kasprintf (fun s -> raise_notrace (Failure s)) fmt

let failwith_error = function
| Error (`Msg m) -> raise_notrace (Failure m) | Ok v -> v

(* Error messages *)

let err_miss_field fn =
  failwithf "missing mandatory field %s" fn

let err_exp_atom fn loc =
  failwithf "%a: %s expected an atom found a list" Sexp.pp_loc loc fn

let err_exp_list fn loc =
  failwithf "%a: %s expected a list found an atom" Sexp.pp_loc loc fn

(* Extractors *)

let get_list fn = function `List l, _ -> l | `Atom _, l -> err_exp_list fn l
let get_list_of parse_el fn sexp = List.map (parse_el fn) (get_list fn sexp)

let get_atom fn = function `Atom a, _ -> a | `List _, l -> err_exp_atom fn l
let get_parsed_atom parse fn = function
| `List _, l -> err_exp_atom fn l
| `Atom a, l ->
    match parse a with
    | exception Failure m -> failwithf "%a: %s: %s" Sexp.pp_loc l fn m
    | v -> v

(* Field parsing *)

type 'a field = (Sexp.t * Sexp.loc) String.Map.t -> 'a

let is_field = function
| "b0-version" | "libs" | "drop-libs" | "srcs" | "subs"
| "compile-kind" | "compile" | "compile-byte" | "compile-native"
| "link" | "link-byte" | "link-native" | "ocamlc" | "ocamlopt" -> true
| k when String.is_prefix ~affix:"x-" k -> true
| _ -> false

let field parse fn ~absent m = match String.Map.find fn m with
| exception Not_found -> absent
| (sexp, _) -> parse fn sexp

let field_atom parse fn ~absent m = field (get_parsed_atom parse) fn ~absent m
let field_list parse_el fn ~absent m = field (get_list_of parse_el) fn ~absent m
let field_atom_list fn ~absent m = field_list get_atom fn ~absent m

let parse_path ~rel_to s =
  Fpath.(rel_to // (Fpath.of_string s |> failwith_error))

let parse_some_path ~rel_to s = Some (parse_path ~rel_to s)
let parse_some_str s = Some s

(* Fields *)

let version m =
  let parse a = match int_of_string a with
  | exception Failure _ -> failwithf "could not parse version number from %S" a
  | 0 as v -> Some v
  | n -> failwithf "unsupported version (%d)" n
  in
  let version = "b0-version" in
  if String.Map.is_empty m then 0 else
  match field_atom parse version ~absent:None m with
  | Some v -> v
  | None -> err_miss_field version

let libs m = field_atom_list "libs" ~absent:[] m
let drop_libs m =
  String.Set.of_list @@ field_atom_list "drop-libs" ~absent:[] m

let srcs ~rel_to ~b0_ml m =
  let parse_el fn (_, l as sexp) = match get_list fn sexp with
  | [`Atom _, _ as p; `List _, _ as l; `Atom _, _ as doc] ->
      let path = get_parsed_atom (parse_path ~rel_to) fn p in
      let libs = List.map (get_atom fn) (get_list fn l) in
      let doc = get_atom fn doc in
      (path, libs, doc)
  | _ ->
      failwithf "%a: %s: list not of the form (<path> (<libname>...) <doc>)"
        Sexp.pp_loc l fn
  in
  let srcs = field_list parse_el "srcs" ~absent:[] m in
  match b0_ml with
  | None -> srcs
  | Some b0_ml -> List.rev ((b0_ml, [], "B0.ml file") :: List.rev srcs)

let subs m =
  let parse_dirname s = match Fpath.is_seg s with
  | false -> failwithf "%S: not a directory name (cannot be path)" s
  | true -> s
  in
  let get_dirname fn sexp = get_parsed_atom parse_dirname fn sexp in
  let parse_op dirs = function
  | "include" -> `Include dirs | "exclude" -> `Exclude dirs
  | v -> failwithf "illegal value %S must be either 'include' or 'exclude'" v
  in
  let parse fn (_, loc as sexp) = match get_list fn sexp with
  | [`Atom _, _ as op; `List _, _ as l] ->
      let dirs = String.Set.of_list (get_list_of get_dirname fn l) in
      get_parsed_atom (parse_op dirs) fn op
  | _ ->
      failwithf "%a: %s: list not of the form (<op> (<dirname>...))"
        Sexp.pp_loc loc fn
  in
  let absent = `Exclude String.Set.empty in
  field parse "subs" ~absent  m

let compile_kind ~src m =
  let parse = function
  | "byte" -> `Byte [src]
  | "native" -> `Native [src]
  | "auto" -> `Auto
  | v ->
      failwithf "illegal value %S must be one of 'byte', 'native' or 'auto'" v
  in
  field_atom parse "compile-kind" ~absent:`Auto m

let b0_dir ~rel_to m =
  field_atom (parse_some_path ~rel_to) "b0-dir" ~absent:None m

let driver_dir ~rel_to m =
  field_atom (parse_some_path ~rel_to) "driver-dir" ~absent:None m

let compile m = field_atom_list "compile" ~absent:[] m
let compile_byte m = field_atom_list "compile-byte" ~absent:[] m
let compile_native m = field_atom_list "compile-native" ~absent:[] m
let link m = field_atom_list "link" ~absent:[] m
let link_byte m = field_atom_list "link-byte" ~absent:[] m
let link_native m = field_atom_list "link-native" ~absent:[] m
let ocamlc m = field_atom parse_some_str "ocamlc" ~absent:None m
let ocamlopt m = field_atom parse_some_str "ocamlopt" ~absent:None m

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
