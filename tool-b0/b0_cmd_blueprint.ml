(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Result.Syntax

(* Eventually add something caracass-like. *)

(* src command *)

let current_year () = 1900 + (Unix.gmtime (Unix.gettimeofday ())).Unix.tm_year

let header ?(indent = "   ") year holder license =
  Fmt.str "%sCopyright (c) %s %s. All rights reserved.\n\
           %sSPDX-License-Identifier: %s" indent year holder indent license

let multiline oc cc year holder license =
  let line = String.make 75 '-' in
  let header = header year holder license in
  String.concat "" [oc; line; "\n"; header; "\n  "; line; cc; "\n"]

let src_fmt ?indent fmt year holder license =
  Fmt.str !fmt (header ?indent year holder license)

let html_src = ref (format_of_string "%s") (* See end of file *)

let c = multiline "/*" "*/"
let css year holder license = "@charset \"UTF-8\";\n" ^ c year holder license
let haskell = multiline "{-" "-}"
let html = src_fmt html_src
let java = c
let js = c
let ocaml = multiline "(*" "*)"
let racket = multiline "#|" "|#"
let sh year holder license =
  String.concat "\n" ["#!/bin/sh"; header ~indent:"# " year holder license; ""]

let srcs =
  [ `C, c, "c", [".c"; ".h"; ".cpp"];
    `Css, css, "css", [".css"];
    `Haskell, haskell, "haskell", [".hs"];
    `Html, html, "html", [".html"];
    `Js, js, "js", [".js"];
    `Java, java, "java", [".java"];
    `Ocaml, ocaml, "ocaml", [".ml"; ".mli"; ".mly"];
    `Racket, racket, "racket", [".rkt"];
    `Rust, c, "rust", [".rs"];
    `Sh, sh, "sh", [".sh"]; ]

let find_src_gen_by_lang lang =
  let find_lang (l, g, _, _) = if lang = l then Some g else None in
  Option.get (List.find_map find_lang srcs)

let find_src_gen_by_file file =
  let ext = Fpath.get_ext file in
  let find_ext (_, g, _, exts) = if List.mem ext exts then Some g else None in
  match List.find_map find_ext srcs with
  | Some src_gen -> Ok src_gen
  | None ->
      Fmt.error
        "@[<v>Could not find a language for extension %a@,\
         Use option %a to specify one.@]"
        Fmt.(code string) ext Fmt.(code string) "--lang"

let find_project_name () =
  let has_file dir file =
    Os.Path.exists Fpath.(dir / file) |> Log.if_error ~use:false
  in
  let root_markers =
    [ "BRZO"; "BRZO.toml"; "B0.ml"; "B0.toml"; "Makefile"; "dune-project"; ]
  in
  try
    let vcs = B00_vcs.find () |> Result.to_failure in
    let cwd = Os.Dir.cwd () |> Result.to_failure in
    let project_dir = match vcs with
    | Some t -> B00_vcs.work_dir t
    | None ->
        let rec loop dir =
          if List.exists (has_file dir) root_markers then dir else
          if Fpath.is_root dir then failwith "" else
          loop (Fpath.parent dir)
        in
        loop cwd
    in
    Ok (Fpath.basename ~no_ext:true project_dir)
  with
  | Failure e ->
      if e <> ""
      then (Fmt.error "@[<v>While looking for a project name:@,%s@]" e)
      else (Fmt.error
              "@[<v>Could not find a project name.@,\
               Use option %a to specify a copyright holder.@]"
              Fmt.(code string) "--holder")

let find_holder = function
| Some holder -> Ok holder
| None ->
    let* name = find_project_name () in
    Ok (Fmt.str "The %s programmers" name)

let find_year = function Some y -> y | None -> Fmt.str "%04d" (current_year ())
let find_license = function
| Some license -> Ok license
| None -> Ok "ISC" (* FIXME lookup in B0.ml *)

let find_src_gen lang file = match lang with
| Some lang -> Ok (find_src_gen_by_lang lang)
| None when file = Fpath.dash -> Ok (find_src_gen_by_lang `Ocaml)
| None -> find_src_gen_by_file file

let src year holder license lang file _conf =
  Log.if_error ~use:B00_cli.Exit.some_error @@
  let year = find_year year in
  let* holder = find_holder holder in
  let* license = find_license license in
  let* src_gen = find_src_gen lang file in
  let src = src_gen year holder license in
  let* () = Os.File.write ~force:false ~make_path:true file src in
  Ok (Os.Exit.code 0)

open Cmdliner

let src =
  let license =
    let doc =
      "$(docv) is the SPDX license identifier. See <https://spdx.dev/ids/>."
    in
    let docv = "SPDX" in
    let absent = "$(b,ISC), TODO lookup in B0.ml metadata" in
    Arg.(value & opt (some string) None & info ["license"] ~doc ~docv ~absent)
  in
  let year =
    let doc = "$(docv) is the copyright year." and docv = "YEAR" in
    let absent = "current year" in
    Arg.(value & opt (some string) None & info ["y"; "year"] ~doc ~docv ~absent)
  in
  let holder =
    let doc = "$(docv) is the copyright holder. If absent \
               a $(b,project name) is derived by finding a project root \
               directory (by detecting a VCS directory or a root build file) \
               and the copyright is assigned to the programmers of that \
               project."
    in
    let absent = "The $(b,project name) programmers" in
    Arg.(value & opt (some string) None &
         info ["h"; "holder"] ~doc ~docv:"HOLDER" ~absent)
  in
  let lang =
    let lang = let enum (lang, _, key, _) = key, lang in List.map enum srcs in
    let doc =
      Fmt.str "$(docv) is the source language. Must be one of %s."
        (Arg.doc_alts_enum lang)
    in
    let absent = "derived from file extension or $(b,ocaml) on stdout" in
    Arg.(value & opt (some (Arg.enum lang)) None &
         info ["lang"] ~doc ~docv:"LANG" ~absent )
  in
  let file =
    let doc = "File path to generate to. Standard output if unspecified." in
    Arg.(value & pos 0 B00_cli.fpath Fpath.dash & info [] ~doc ~docv:"PATH")
  in
  let doc = "Generate empty copyrighted source files" in
  let descr =
    `Blocks
      [`P "The $(iname) command generates an empty copyrighted source file. \
           For example:";
       `P "$(iname) $(b,mysrc.c)"; `Noblank;
       `P "$(iname) $(b,-y 2038 mysrc.mli) "; `Noblank;
       `P "$(iname) $(b,-h Unknown > mysrc.ml)"; `Noblank;
       `P "$(iname) $(b,--lang c > mysrc.h)";
       `P "The command makes best-effort guesses to derive the file's language,
           the copyright year, the copyright holder and the SPDX license. \
           See the corresponding options for more details.";
       `P "The output format is fixed, it cannot be tweaked."]
  in
  B0_b0.Cli.subcmd_with_driver_conf "src" ~doc ~descr
    Term.(const src $ year $ holder $ license $ lang $ file)

let subs = [src]

let cmd =
  let doc = "Generate files from blueprints" in
  let descr = `P "The $(iname) command generates files from blueprints." in
  B0_b0.Cli.cmd_group_with_driver_conf "blueprint" ~doc ~descr subs

(* Format strings *)

let () = begin
  html_src :=
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
|};
end

(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers

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
