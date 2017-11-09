(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

(* Descriptions *)

type compile_kind =
  [ `Byte of Fpath.t list
  | `Native of Fpath.t list
  | `Conflict of Fpath.t list * Fpath.t list
  | `Auto ]

type t =
  { file : Fpath.t;
    libs : string list;
    drop_libs : String.set;
    srcs : (Fpath.t * (Fpath.t * string list * string) list) list;
    b0_dir : Fpath.t option;
    driver_dir : Fpath.t option;
    compile_kind : compile_kind;
    compile : string list;
    compile_byte : string list;
    compile_native : string list;
    link : string list;
    link_byte : string list;
    link_native : string list;
    ocamlc : string option;
    ocamlopt : string option; }

let file t = t.file
let dir t = Fpath.parent t.file
let libs t = t.libs
let drop_libs t = t.drop_libs
let srcs t = t.srcs
let driver_dir t = t.driver_dir
let b0_dir t = t.b0_dir
let compile_kind t = t.compile_kind
let compile t = t.compile
let compile_byte t = t.compile_byte
let compile_native t = t.compile_native
let link t = t.link
let link_byte t = t.link_byte
let link_native t = t.link_native
let ocamlc t = t.ocamlc
let ocamlopt t = t.ocamlopt

let compare_descr_file d0 d1 = Fpath.compare d0.file d1.file

(* Description parsing *)

let v ~file ~srcs =
  { file; libs = []; drop_libs = String.Set.empty; srcs;
    b0_dir = None; driver_dir = None;
    compile_kind = `Auto;
    compile = []; compile_byte = []; compile_native = [];
    link = []; link_byte = []; link_native = [];
    ocamlc = None; ocamlopt = None; }

let of_ml_file file = v ~file ~srcs:[file, [(file, [], "B0 file")]]
let of_b0_file file ~b0_ml =
  begin
    let warn_unknown_key k (_, l) =
      Log.warn (fun m -> m "%a: unknown key %S" Sexp.pp_loc l k)
    in
    let known = B0d_sexp_descr.is_field in
    let rel_to = Fpath.parent file in
    Sexp.of_file file >>= fun l ->
    Sexp.list_to_string_map ~known l >>= fun (known, unknown) ->
    String.Map.iter warn_unknown_key unknown;
    let libs = B0d_sexp_descr.libs known in
    let drop_libs = B0d_sexp_descr.drop_libs known in
    let srcs = B0d_sexp_descr.srcs ~rel_to ~b0_ml known in
    let b0_dir = B0d_sexp_descr.b0_dir ~rel_to known in
    let driver_dir = B0d_sexp_descr.driver_dir ~rel_to known in
    let compile_kind =
      (B0d_sexp_descr.compile_kind file known :> compile_kind)
    in
    let compile = B0d_sexp_descr.compile known in
    let compile_byte = B0d_sexp_descr.compile_byte known in
    let compile_native = B0d_sexp_descr.compile_native known in
    let link = B0d_sexp_descr.link known in
    let link_byte = B0d_sexp_descr.link_byte known in
    let link_native = B0d_sexp_descr.link_native known in
    let ocamlc = B0d_sexp_descr.ocamlc known in
    let ocamlopt = B0d_sexp_descr.ocamlopt known in
    Ok ({ file; libs; drop_libs; srcs = [file, srcs];
          b0_dir; driver_dir;
          compile_kind;
          compile; compile_byte; compile_native;
          link; link_byte; link_native;
          ocamlc; ocamlopt },
        B0d_sexp_descr.subs known)
  end |> R.failwith_error_msg

(* Root description lookup *)

let file_exists dir file =
  let path = Fpath.(dir / file) in
  let exists = OS.File.exists path |> R.failwith_error_msg in
  if exists then Some path else None

let find_descr_file ~dir = match file_exists dir "B0.b0" with
| Some file -> Some (`B0_file (file, file_exists dir "B0.ml"))
| None ->
    match file_exists dir "B0.ml" with
    | Some file -> Some (`Ml_file file)
    | None -> None

let find_sub_descr_files ?(add = []) subs_spec ~dir =
  let drop = match subs_spec with
  | `Include incs -> fun d -> not (String.Set.mem (Fpath.to_string d) incs)
  | `Exclude excls ->
      fun d ->
        let d = Fpath.to_string d in
        String.Set.mem d excls || d.[0] = '_' || d.[0] = '.'
  in
  let rec loop acc = function
  | [] -> acc
  | d :: dirs ->
      if drop d then loop acc dirs else
      match find_descr_file ~dir:Fpath.(dir / Fpath.to_string d) with
      | None -> loop acc dirs
      | Some descr_file -> loop (descr_file :: acc) dirs
  in
  let dirs = OS.Dir.dirs ~dotfiles:false ~rel:true dir |> R.failwith_error_msg
  in
  loop add dirs

let merge_compile_kind k0 k1 = match k0, k1 with
| `Native n, `Byte b
| `Byte b, `Native n -> `Conflict (b, n)
| `Auto, k | k, `Auto -> k
| `Native n0, `Native n1 -> `Native (List.rev_append n0 n1)
| `Byte n0, `Byte n1 ->  `Byte (List.rev_append n0 n1)
| `Conflict (b, n0), `Native n1 -> `Conflict (b, List.rev_append n0 n1)
| `Conflict (b0, n), `Byte b1 -> `Conflict (List.rev_append b0 b1, n)
| `Conflict _, _ | _, `Conflict _ -> assert false

let merge = function
| [] -> None
| root :: subs ->
    let add_sub root s =
      let libs = List.append root.libs s.libs in
      let drop_libs = String.Set.union root.drop_libs s.drop_libs in
      let srcs = List.hd s.srcs :: root.srcs  in
      let compile_kind = merge_compile_kind root.compile_kind s.compile_kind in
      let compile = List.append root.compile s.compile in
      let compile_byte = List.append root.compile_byte s.compile_byte in
      let compile_native = List.append root.compile_native s.compile_native in
      let link = List.append root.link s.link in
      let link_byte = List.append root.link_byte s.link_byte in
      let link_native = List.append root.link_native s.link_native in
      { root with
        libs; drop_libs; srcs; compile_kind;
        compile; compile_byte; compile_native;
        link; link_byte; link_native }
    in
    let subs = List.sort compare_descr_file subs in
    let d = List.fold_left add_sub root subs in
    Some { d with
           libs = String.uniquify d.libs;
           compile = String.uniquify d.compile;
           compile_byte = String.uniquify d.compile_byte;
           compile_native = String.uniquify d.compile_native;
           link = String.uniquify d.link;
           link_byte = String.uniquify d.link_byte;
           link_native = String.uniquify d.link_native }

let remove_child_provenance ~subs = function
| [] -> Some subs
| child :: _ ->
    let rec loop acc = function
    | [] -> None (* child was not in subs *)
    | (`Ml_file file | `B0_file (file, _) as sub) :: subs ->
        match Fpath.equal file child.file with
        | true -> Some (List.rev_append acc subs)
        | false -> loop (sub :: acc) subs
    in
    loop [] subs

(* Root description lookup. If [force] is [true] starting from [dir] we
   look for a description and then up in the hierarchy. If initially there
   are no description we move up until we find one. Once we have found a
   description we first look down the hierarchy for other descriptions
   and then up again until either there's a description that excludes
   where we come from or there is no description. If [force] is [false]
   [dir] is the root directory and we only look down. *)

let find ~force ~dir =
  try
    let rec look_up ~only_down ~stop_if_none acc dir =
      match find_descr_file ~dir with
      | None when Fpath.is_root dir || stop_if_none -> merge acc
      | None when only_down (* forced *) ->
          let subs = find_sub_descr_files (`Exclude String.Set.empty) ~dir in
          look_down ~only_down (dir, v dir []) acc subs
      | None -> look_up ~only_down ~stop_if_none:false acc (Fpath.parent dir)
      | Some (`Ml_file file) ->
          let dir = Fpath.parent file in
          let descr = of_ml_file file in
          let subs = find_sub_descr_files (`Exclude String.Set.empty) ~dir in
          look_down ~only_down (dir, descr) acc subs
      | Some (`B0_file (file, ml)) ->
          let dir = Fpath.parent file in
          let descr, subs_spec = of_b0_file file ml in
          let subs = find_sub_descr_files subs_spec ~dir in
          match remove_child_provenance ~subs acc with
          | None -> (* child was not in subs, stop here *) merge acc
          | Some subs -> look_down ~only_down (dir, descr) acc subs
    and look_down ~only_down parent acc = function
    | [] ->
        let dir = Fpath.parent (fst parent) in
        let acc = snd parent :: acc in
        if Fpath.is_root dir || only_down then merge acc else
        look_up ~only_down ~stop_if_none:true acc dir
    | (`Ml_file file) :: todo ->
        let dir = Fpath.parent file in
        let descr = of_ml_file file in
        let exclude_none = (`Exclude String.Set.empty) in
        let subs = find_sub_descr_files ~add:todo exclude_none ~dir in
        look_down ~only_down parent (descr :: acc) subs
    | (`B0_file (file, b0_ml)) :: todo ->
        let dir = Fpath.parent file in
        let descr, subs_spec = of_b0_file file ~b0_ml in
        let subs = find_sub_descr_files ~add:todo subs_spec ~dir in
        look_down ~only_down parent (descr :: acc) subs
    in
    Ok (look_up ~only_down:force ~stop_if_none:false [] dir)
  with Failure m -> R.error_msgf "%s" m

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
