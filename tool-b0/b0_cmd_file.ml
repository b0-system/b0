(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let get_b0_file_src c k =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.if_error' ~header:"" ~use:B0_driver.Exit.b0_file_error @@
  let* s = Os.File.read b0_file in
  let* b0_file = B0_file.of_string ~file:b0_file s in
  k b0_file

let boot c root =
  let pp_boots = Fmt.(list @@ hbox @@ list ~sep:sp (using fst string)) in
  get_b0_file_src c @@ fun src ->
  let* boots =
    if root then Ok (B0_file.b0_boots src) else
    let* exp = B0_file.expand src in
    Ok (B0_file.expanded_b0_boots exp)
  in
  Log.app (fun m -> m "Boot is TODO.");
  if boots <> [] then Log.app (fun m -> m "@[<v>%a@]" pp_boots boots);
  Ok B00_cli.Exit.ok

let compile c =
  get_b0_file_src c @@ fun f ->
  let* _ = B0_driver.Compile.compile c ~driver:B0_b0.driver f in
  Ok B00_cli.Exit.ok

let edit c all =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.if_error' ~use:B00_cli.Exit.some_error @@
  let* editor = B00_editor.find () in
  Log.if_error' ~header:"" ~use:B0_driver.Exit.b0_file_error @@
  let* files = match all with
  | false -> Ok [b0_file]
  | true ->
      let* s = Os.File.read b0_file in
      let* src = B0_file.of_string ~file:b0_file s in
      let* exp = B0_file.expand src in
      let incs = B0_file.expanded_b0_includes exp in
      let add_inc acc (_, (p, _)) = p :: acc in
      Ok (List.rev @@ List.fold_left add_inc [b0_file] incs)
  in
  Result.bind (B00_editor.edit_files editor files) @@ function
  | `Exited 0 -> Ok B00_cli.Exit.ok
  | _ -> Ok B00_cli.Exit.some_error

let includes c root details =
  let pp_inc = match details with
  | `Short -> fun ppf (_, (p, _)) -> Fpath.pp_unquoted ppf p
  | `Normal | `Long ->
      fun ppf ((n, _), (p, _)) ->
        Fmt.pf ppf "@[%a %a@]" Fmt.(code string) n Fpath.pp_unquoted p
  in
  get_b0_file_src c @@ fun src ->
  let* incs = match root with
  | true -> Ok (B0_file.b0_includes src)
  | false ->
      let* exp = B0_file.expand src in
      Ok (B0_file.expanded_b0_includes exp)
  in
  if incs <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_inc) incs);
  Ok B00_cli.Exit.ok

let log c format details op_selector =
  Log.if_error ~use:B00_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c || format = `Trace_event in
  let log_file = B0_driver.Compile.build_log c ~driver:B0_b0.driver in
  let* pager = B00_pager.find ~don't () in
  let* () = B00_pager.page_stdout pager in
  let* l = B00_cli.Memo.Log.read log_file in
  B00_cli.Memo.Log.out Fmt.stdout format details op_selector ~path:log_file l;
  Ok B00_cli.Exit.ok

let path c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.app (fun m -> m "%a" Fpath.pp_unquoted b0_file);
  Ok B00_cli.Exit.ok

let source c root =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.if_error' ~header:"" ~use:B0_driver.Exit.b0_file_error @@
  match root with
  | true ->
      let* s = Os.File.read b0_file in
      Log.app (fun m -> m "%s" s); Ok B00_cli.Exit.ok
  | false ->
      let* s = Os.File.read b0_file in
      let* src = B0_file.of_string ~file:b0_file s in
      let* exp = B0_file.expand src in
      let esrc = B0_file.expanded_src exp in
      Log.app (fun m -> m "%s" esrc);
      Ok B00_cli.Exit.ok

let requires c root =
  let pp_require = Fmt.using fst B00_ocaml.Lib.Name.pp in
  get_b0_file_src c @@ fun src ->
  let* reqs = match root with
  | true -> Ok (B0_file.requires src)
  | false ->
      let* exp = B0_file.expand src in
      Ok (B0_file.expanded_requires exp)
  in
  if reqs <> []
  then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_require) reqs);
  Ok B00_cli.Exit.ok

let file c root all details format op_selector = function
| `Boot -> boot c root
| `Compile -> compile c
| `Edit -> edit c all
| `Includes -> includes c root details
| `Log -> log c format details op_selector
| `Path -> path c
| `Requires -> requires c root
| `Source -> source c root

(* Command line interface *)

open Cmdliner

let action =
  let action =
    [ "boot", `Boot; "compile", `Compile; "edit", `Edit; "includes", `Includes;
      "log", `Log; "path", `Path; "requires", `Requires; "source", `Source; ]
  in
  let doc =
    let alts = Arg.doc_alts_enum action in
    Fmt.str "The action to perform. $(docv) must be one of %s." alts
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let root =
  let doc = "Apply on the root B0 file only rather than on its expansion." in
  Arg.(value & flag & info ["root"] ~doc)

let all =
  let doc = "Edit the B0 file and all its includes." in
  Arg.(value & flag & info ["all"] ~doc)

let doc = "Operate on the B0 file"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.infos
let envs = B00_editor.envs ()
let man_xrefs = [ `Main ]
let docs_format = "LOG OUTPUT FORMATS"
let docs_details = "LOG OUTPUT DETAILS"
let docs_select = "OPTIONS FOR SELECTING LOG OPERATIONS"
let man = [
  `S Manpage.s_description;
  `P "$(tname) operates on the B0 file.";
  `S "ACTIONS";
  `I ("$(b,boot)",
      "Install libraries needed for the B0 file.");
  `I ("$(b,compile)",
      "Compile the driver for the B0 file.");
  `I ("$(b,edit) [$(b,--all)]",
      "Edit the B0 file in your editor. If $(b,--all) is specified \
       also edits all includes.");
  `I ("$(b,includes) [$(b,--root)]",
      "Show the scope name and file path of included B0 files. If $(b,--root) \
       is specified only shows the includes of the root B0 file.");
  `I ("$(b,log) [$(b,--path)] [$(i,LOG ARGS)...]",
      "Show the driver compilation log. If $(b,--path) is specified \
       shows the path to the log. See the various section below for \
       the other arguments and $(b,b0 log) for more information.");
  `I ("$(b,path)",
      "Show the file path to the B0 file.");
  `I ("$(b,requires) [$(b,--root)]",
      "Show the OCaml libraries required. If $(b,--root) is specified \
       only shows the requires of the root B0 file.");
  `I ("$(b,source) [$(b,--root)]",
      "Show the expanded B0 file source the driver compiles. \
       If $(b,--root) is specified shows the non-expanded source of \
       the root B0 file.");
  `S Manpage.s_arguments;
  `S Manpage.s_options;
  `S docs_format;
  `S docs_details;
  `P "If applicable.";
  `S docs_select;
  `Blocks B00_cli.Op.query_man;
  B0_b0.Cli.man_see_manual; ]

let cmd =
  Term.(const file $ B0_driver.Cli.conf $ root $ all $
        B00_cli.Arg.output_details ~docs:docs_details () $
        B00_cli.Memo.Log.out_format_cli ~docs:docs_format () $
        B00_cli.Op.query_cli ~docs:docs_select () $ action),
  Term.info "file" ~doc ~sdocs ~envs ~exits ~man ~man_xrefs

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
