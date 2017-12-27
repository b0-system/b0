(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

let loc = Def.Loc.lib "B0_jbuilder"

let jbuilder = (* Metadata key added to units defined via a jbuild file *)
  let doc = "Unit translated from a jbuild file" in
  Unit.Meta.Key.v ~loc "jbuilder" Conv.bool () ~doc

type stanza =
  [ `Library of Sexp.t
  | `Executable of Sexp.t
  | `Unknown of string * Sexp.t ] * Sexp.loc

type t = { file : Fpath.t; stanzas : stanza list; }

(* Parsing *)

let parse_stanzas ~log se =
  let err_atom loc a =
    Log.msg log (fun m -> m "%a: unexpected atom %s" Sexp.pp_loc loc a);
  in
  let rec parse_stanza acc = function
  | `List ((`Atom "jbuild_version", _) :: [_]), _ -> acc
  | `List ((`Atom "library", _) :: [v]), loc -> (`Library v, loc) :: acc
  | `List ((`Atom "executable", _) :: [v]), loc -> (`Executable v, loc) :: acc
  | `List ((`Atom s, _) :: [v]), loc -> (`Unknown (s, v), loc) :: acc
  | `List _, loc ->
      Log.msg
        log (fun m -> m "%a: not a (key value) pair stanza" Sexp.pp_loc loc);
      acc
  | `Atom a, loc -> err_atom loc a; acc
  in
  match se with
  | `Atom a, loc -> err_atom loc a; []
  | `List l, _ -> List.fold_left parse_stanza [] l

let parse_map ~log known se =
  let log_unhandled_fields ~log m =
    let pp_binding ppf (k, (_, loc)) =
      Fmt.pf ppf "%a: unhandled key %s" Sexp.pp_loc loc k
    in
    let pp_map = Fmt.iter_bindings String.Map.iter pp_binding in
    Log.msg log (fun msg -> msg "@[<v>%a@]" pp_map m)
  in
  Sexp.to_string_map ~known se >>= fun (known, (unknown, _)) ->
  log_unhandled_fields ~log unknown;
  Ok known

let string = fun s -> s
let string_opt = fun s -> Some s
let name_key = Sexp.atom_key string "name"
let public_name_key = Sexp.atom_key ~absent:None string_opt "public_name"
let synopsis_key = Sexp.atom_key ~absent:None string_opt "synopsis"
let libraries_key = Sexp.atom_list_key ~absent:[] string "libraries"

let parse_library ~log file s lib =
  let known = function
  | "name" | "public_name" | "synopsis" | "libraries" -> true | _ -> false
  in
  try
    parse_map ~log known s >>= fun m ->
    let build = `Src_dirs [Fpath.parent file] in
    let name = name_key m in
    let name = match public_name_key m with (* For now *)
    | Some name -> name
    | None -> name
    in
    let doc = synopsis_key m in
    let lib_deps = libraries_key m in
    let meta =
      B0_ocaml.Unit.lib_meta ~lib_deps name |>
      Unit.Meta.add_tag jbuilder
    in
    Ok (lib ~lib_deps ~meta ?doc name build)
  with
  | Failure e -> Error (`Msg e)

let parse_executable ~log file s exe =
  let known = function
  | "name" | "public_name" | "libraries" -> true | _ -> false
  in
  try
    parse_map ~log known s >>= fun m ->
    let build = `Src_dirs [Fpath.parent file] in
    let name = name_key m in
    let name = match public_name_key m with (* For now *)
    | Some name -> name
    | None -> name
    in
    let doc = synopsis_key m in
    let lib_deps = libraries_key m in
    let meta =
      B0_ocaml.Unit.exe_meta ~lib_deps name |>
      Unit.Meta.add_tag jbuilder
    in
    Ok (exe ~lib_deps ~meta ?doc name build)
  with
  | Failure e -> Error (`Msg e)

let default_log = Log.Debug (* for now *)

let of_file ?(log = default_log) file =
  Sexp.of_file file >>= fun se ->
  let stanzas = parse_stanzas ~log se in
  Ok { file; stanzas }

(* Converting to units *)

let library_to_unit ~log file l =
  let lib ~lib_deps ~meta ?doc name build =
    ignore @@ B0_ocaml.Unit.lib ~lib_deps ~meta ?doc name build
  in
  parse_library ~log file l lib

let executable_to_unit ~log file e =
  let exe ~lib_deps ~meta ?doc name build =
    ignore @@ B0_ocaml.Unit.exe ~lib_deps ~meta ?doc name build
  in
  parse_executable ~log file e exe

let stanza_to_unit ~log file s =
  begin match s with
  | `Library l, _ -> library_to_unit ~log file l
  | `Executable e, _ -> executable_to_unit ~log file e
  | `Unknown (s, _), loc ->
      Log.msg log (fun m -> m "%a: unknown stanza %s" Sexp.pp_loc loc s);
      Ok ()
  end
  |> Log.on_error_msg ~use:(fun _ -> ())

let to_units ?(log = default_log) j =
  List.iter (stanza_to_unit ~log j.file) j.stanzas

(* API for B0.ml files *)

let get_src_root () = (* FIXME we need a formal an easy API for that. *)
  let loc = Def.Loc.get_current () in
  match Def.Loc.find_file loc with
  | None -> None
  | Some b0_file -> Some Fpath.((parent @@ b0_file))

let units_of_jbuild f = of_file f >>| fun j -> to_units j

let import_file f =
  let f = match get_src_root () with None -> f | Some r -> Fpath.(r // f) in
  (units_of_jbuild f)
  |> Log.on_error_msg ~use:(fun _ -> ())

let import () = match get_src_root () with
| None -> Log.err (fun m -> m "Cannot import jbuild files, unknown location.")
| Some root ->
    let try_import_jbuild dir =
      begin
        let jbuild = Fpath.(dir / "jbuild") in
        OS.File.exists jbuild >>= function
        | false -> Ok ()
        | true -> units_of_jbuild jbuild
      end
      |> Log.on_error_msg ~use:(fun _ -> ())
    in
    let dirs = OS.Dir.dirs root |> Log.on_error_msg ~use:(fun _ -> []) in
    List.iter try_import_jbuild (root :: dirs)

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
