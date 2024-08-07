(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

(* Project metadata. *)

let find_project_meta () =
  (* XXX the conditional may not be needed. *)
  if not (B0_driver.has_b0_file ()) then None else
  Option.map B0_pack.meta (B0_pack.find_default ())

let get_project_meta () =
  Option.value ~default:B0_meta.empty (find_project_meta ())

let default_root_markers =
  [ "BRZO"; "BRZO.ini"; "B0.ml"; "B0.ini"; "Makefile"; "dune-project"; ]

let find_project_name ?(root_markers = default_root_markers) ~cwd () =
  let has_file dir file =
    Os.Path.exists Fpath.(dir / file) |> Log.if_error ~use:false
  in
  try
    let vcs = B0_vcs_repo.find () |> Result.error_to_failure in
    let project_dir = match vcs with
    | Some t -> B0_vcs_repo.work_dir t
    | None ->
        let rec loop dir =
          if List.exists (has_file dir) root_markers then dir else
          if Fpath.is_root dir then raise Exit else
          loop (Fpath.parent dir)
        in
        loop cwd
    in
    Ok (Some (Fpath.basename ~strip_ext:true project_dir))
  with
  | Exit -> Ok None
  | Failure e -> Fmt.error "@[<v>While looking for a project name:@,%s@]" e

let get_copyright_years = function
| Some years -> years
| None ->
    let current = 1900 + (Unix.gmtime (Unix.gettimeofday ())).Unix.tm_year in
    Fmt.str "%04d" current

(* Changes files *)

type changes = unit -> string

let changes_md () =
  "v0.0.0 YYYY-MM-DD Location\n\
   --------------------------\n\n\
   First release.\n"

let find_changes_generator _file = Ok changes_md

(* License files *)

let find_project_license meta = match B0_meta.find B0_meta.licenses meta with
| None | Some [] -> None | Some l -> Some (List.hd l)

let get_license meta license = match license with
| Some l -> l | None -> Option.value ~default:"ISC" (find_project_license meta )

type spdxid = string

let download_license_template ?httpc ~strip_meta id =
  let strip_meta_block text =
    let marker = "\n---\n" in
    match String.find_sub ~sub:marker text with
    | None -> Fmt.error "Could not strip meta block from license template"
    | Some i ->
        let text = String.subrange ~first:(i + String.length marker) text in
        Ok (String.trim text)
  in
  let open B0_http in
  let* httpc = match httpc with Some c -> Ok c | None -> Http_client.make () in
  let lid = String.Ascii.lowercase id in
  let url =
    Fmt.str "https://raw.githubusercontent.com/github/choosealicense.com/\
             gh-pages/_licenses/%s.txt" lid
  in
  let request = Http.Request.make `GET ~url in
  let* response = Http_client.request ~follow:true httpc request in
  match Http.Response.status response with
  | 200 ->
      let text = Http.Response.body response in
      if strip_meta then strip_meta_block text else Ok text
  | st -> Fmt.error "[%d] Could not download a license template for %s" st id

let substitute_var ~var ~value text =
  let rec loop acc text = match String.find_sub ~sub:var text with
  | None when acc = [] -> None
  | None -> Some (String.concat "" (List.rev (text :: acc)))
  | Some i ->
      let left = String.subrange ~last:(i - 1) text in
      let right = String.subrange ~first:(i + String.length var) text in
      loop (value :: left :: acc) right
  in
  loop [] text

let license
    ?(var_years = "[year]") ?(var_holder = "[fullname]") ~years ~holder text
  =
  let text, ws = match substitute_var ~var:var_years ~value:years text with
  | Some text -> text, []
  | None -> text, ["Could not substitute copyright years in license template"]
  in
  let text, ws = match substitute_var ~var:var_holder ~value:holder text with
  | Some text -> text, ws
  | None ->
      text, "Could not substitute copyright holder in license template" :: ws
  in
  text, ws

(* Readme files *)

type readme =
  project_name:string -> synopsis:string option -> B0_meta.t -> string

let readme_md ~project_name ~synopsis meta =
  let name = String.Ascii.capitalize project_name in
  let title =
    let title =
      let syn = Option.value ~default:"Needs a good synopsis line" synopsis in
      Fmt.str "%s – %s" name syn
    in
    let underline = String.make (String.length title - 2) '=' in
    String.concat "\n" [title; underline]
  in
  let preamble =
    let homepage = match B0_meta.find B0_meta.homepage meta with
    | None -> "" | Some h -> Fmt.str "\n\nHomepage: <%s>" h
    in
    let preamble = Fmt.str "%s needs a good short description." name in
    String.concat "" [preamble; homepage]
  in
  let installation =
    let opam name =
      Fmt.str
        "%s can be installed with `opam`:\n\n    opam install %s\n\n\
         If you don't use `opam` consult the [`opam`](opam) file for build\n\
         instructions." name (String.Ascii.uncapitalize name)
    in
    let instructions =
      if B0_meta.has_tag B0_opam.tag meta then opam name else
      Fmt.str "%s needs install instructions." name
    in
    String.concat "\n\n" ["## Installation"; instructions]
  in
  let documentation =
    let ocaml name =
      let online, online_link = match B0_meta.find B0_meta.online_doc meta with
      | None -> "", "" | Some l -> " [online]", Fmt.str "[online]: %s\n" l
      in
      Fmt.str
        "The documentation can be consulted%s via `odig doc %s`.\n\n\
         Questions are welcome but better asked on the [OCaml forum] than on\n\
         the issue tracker.\n\n%s[OCaml forum]: https://discuss.ocaml.org"
        online (String.Ascii.uncapitalize name) online_link
    in
    let docs =
      if B0_meta.has_tag B0_opam.tag meta then ocaml name else
      Fmt.str "%s needs documentation." name
    in
    String.concat "\n\n" ["## Documentation"; docs]
  in
  let examples = Fmt.str "## Examples\n\n%s needs some examples.\n" name in
  String.concat "\n\n" [title; preamble; installation; documentation; examples]

let find_readme_generator _file = Ok readme_md

(* Source files *)

let get_src_license meta ~example = function
| None when example -> "CC0-1.0" | license -> get_license meta license

type lang =
[ `C | `Css | `Haskell | `Html | `Javascript | `Java | `Ocaml | `Racket
| `Rust | `Sh ]

let lang_dom =
  [ `C; `Css; `Haskell; `Html; `Javascript; `Java; `Ocaml; `Racket; `Rust;
    `Sh ]

let lang_to_id = function
| `C -> "c" | `Css -> "css" | `Haskell -> "haskell"
| `Html -> "html" | `Javascript -> "javascript" | `Java -> "java"
| `Ocaml -> "ocaml" | `Racket -> "racket" | `Rust -> "rust" | `Sh -> "sh"

let pp_lang_id = Fmt.(using lang_to_id code)

let lang_of_id lid = match String.Ascii.lowercase lid with
| "c" -> Ok `C | "css" -> Ok `Css | "haskell" -> Ok `Haskell
| "html" -> Ok `Html | "javascript" -> Ok `Javascript
| "java" -> Ok `Java | "ocaml" -> Ok `Ocaml
| "racket" -> Ok `Racket | "rust" -> Ok `Rust | "sh" -> Ok `Sh
| lang ->
    (* FIXME streamline this kind of nice erroring shit. *)
    let dom = List.rev_map lang_to_id lang_dom in
    let suggs =
      let add_sugg acc v =
        if String.edit_distance lang v <= 2 then v :: acc else acc
      in
      List.fold_left add_sugg [] dom
    in
    let hint, suggs = match suggs with
    | [] -> Fmt.must_be, dom
    | suggs -> Fmt.did_you_mean, suggs
    in
    let pp = Fmt.unknown' ~kind:(Fmt.any "language") Fmt.code ~hint in
    Fmt.error "@[%a@]" pp (lang, suggs)

let lang_of_file_ext = function
| ".c" | ".h" | ".cpp" -> Some `C
| ".css" -> Some `Css
| ".hs" -> Some `Haskell
| ".html" -> Some `Html
| ".js" -> Some `Javascript
| ".java" -> Some `Java
| ".ml" | ".mli" | ".mly" -> Some `Ocaml
| ".rkt" -> Some `Racket
| ".rs" -> Some `Rust
| ".sh" -> Some `Sh
| _ -> None


type src = years:string -> holder:string -> license:B0_meta.spdxid -> string

let header ?(indent = "   ") years holder license =
  Fmt.str "%sCopyright (c) %s %s. All rights reserved.\n\
           %sSPDX-License-Identifier: %s" indent years holder indent license

let multiline oc cc ~years ~holder ~license =
  let line = String.make 75 '-' in
  let header = header years holder license in
  String.concat "" [oc; line; "\n"; header; "\n  "; line; cc; "\n"]

let src_fmt ?indent fmt ~years ~holder ~license =
  Fmt.str fmt (header ?indent years holder license)

let html_src = format_of_string
{|<!DOCTYPE html>
<!--
%s
  -->
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1.0">
  <title>Untitled</title>
</head>
<body>
</body>
</html>
|}

let c = multiline "/*" "*/"
let css ~years ~holder ~license =
  "@charset \"UTF-8\";\n" ^ c ~years ~holder ~license

let haskell = multiline "{-" "-}"
let html = src_fmt html_src
let java = c
let js = c
let ocaml = multiline "(*" "*)"
let racket = multiline "#|" "|#"
let sh ~years ~holder ~license =
  String.concat "\n" ["#!/bin/sh"; header ~indent:"# " years holder license; ""]

let src_generator = function
| `C -> c | `Css -> css | `Haskell -> haskell | `Html -> html
| `Javascript -> js | `Java -> java | `Ocaml -> ocaml
| `Racket -> racket | `Rust -> c | `Sh -> sh
