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

let boot root c =
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

let edit all c =
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

let gather rel keep_symlinks dirs c =
  Log.if_error ~use:B00_cli.Exit.some_error @@
  let rec gather seen b0s = function
  | d :: dirs ->
      let cwd = B0_driver.Conf.cwd c in
      let b0_file = Fpath.(d / B0_driver.Conf.b0_file_name) in
      let* b0_file' = Os.Path.realpath b0_file in
      let b0_file = match keep_symlinks with
      | true when rel -> Fpath.relative b0_file ~to_dir:cwd
      | true -> Fpath.(cwd // b0_file)
      | false when rel -> Fpath.relative b0_file' ~to_dir:cwd
      | false -> b0_file'
      in
      let scope = Fpath.(basename @@ parent b0_file) in
      let scope, seen = match String.Set.mem scope seen with
      | false -> scope, String.Set.add scope seen
      | true ->
          let exists s = String.Set.mem s seen in
          let scope' = String.unique ~exists scope in
          scope', String.Set.add scope' seen
      in
      gather seen ((scope, b0_file) :: b0s) dirs
  | [] -> Ok (List.sort Stdlib.compare b0s)
  in
  let* incs = gather String.Set.empty [] dirs in
  let pp_inc ppf (s, f) =
    Fmt.pf ppf {|@[[@@@@@@B0.include "%s" "%a"]@]|} s Fpath.pp_unquoted f
  in
  Log.app (fun m -> m "@[<v>%a@]" (Fmt.list pp_inc) incs);
  Ok B00_cli.Exit.ok

let includes root format c =
  let pp_inc = match format with
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

let log format log_format op_selector c =
  Log.if_error ~use:B00_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c || log_format = `Trace_event in
  let log_file = B0_driver.Compile.build_log c ~driver:B0_b0.driver in
  let* pager = B00_pager.find ~don't () in
  let* () = B00_pager.page_stdout pager in
  let* l = B00_cli.Memo.Log.read log_file in
  B00_cli.Memo.Log.out Fmt.stdout log_format format
    op_selector ~path:log_file l;
  Ok B00_cli.Exit.ok

let path c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.app (fun m -> m "%a" Fpath.pp_unquoted b0_file);
  Ok B00_cli.Exit.ok

let requires root c =
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

let source root c =
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

(* Command line interface *)

open Cmdliner

let root =
  let doc = "Apply operation on the root B0 file only rather than on \
             its expansion."
  in
  Arg.(value & flag & info ["root"] ~doc)

let path_term = Term.(const path)

(* Commands *)

let boot =
  let doc = "Install libraries needed for the B0 file" in
  let descr = `P "$(tname) install libraries needed to compile the B0 file." in
  B0_b0.Cli.subcmd_with_driver_conf "boot" ~doc ~descr Term.(const boot $ root)

let compile =
  let doc = "Compile the driver for the B0 file" in
  let descr = `P "$(tname) compiles the driver for the B0 file." in
  B0_b0.Cli.subcmd_with_driver_conf "compile" ~doc ~descr Term.(const compile)

let edit =
  let doc = "Edit the B0 file" in
  let descr = `P "$(tname) opens the B0 file in your editor. If $(b,--all) \
                  is specified also opens all includes."
  in
  let all =
    let doc = "Edit the B0 file and all its includes." in
    Arg.(value & flag & info ["all"] ~doc)
  in
  B0_b0.Cli.subcmd_with_driver_conf "edit" ~doc ~descr
    Term.(const edit $ all)

let gather =
  let doc = "Gathers B0 files from directories into a single one" in
  let descr = `Blocks [
      `P "$(tname) outputs a B0 file that includes the B0 files
           in given $(i,DIR) directories. Typical usage:";
      `P "$(b,mkdir aggregate)"; `Noblank;
      `P "$(mname) $(b,file) $(tname) $(b,myproject repos/mylib > \
          aggregate/B0.ml)";
      `Noblank;
      `P "$(b,cd aggregate) && $(mname)" ]
  in
  let dirs =
    let doc = "Gather the $(docv)$(b,/B0.ml) file" in
    Arg.(non_empty & pos_all B00_cli.fpath [] & info [] ~doc ~docv:"DIR")
  in
  let rel =
    let doc = "Make file paths relative to the cwd." in
    Arg.(value & flag & info ["relative"] ~doc)
  in
  let keep_symlinks =
    let doc = "Don't resolve symlinks." in
    Arg.(value & flag & info ["s"; "keep-symlinks"] ~doc)
  in
  B0_b0.Cli.subcmd_with_driver_conf "gather" ~doc ~descr
    Term.(const gather $ rel $ keep_symlinks $ dirs)

let includes =
  let doc = "Output scope name and paths of included B0 files" in
  let descr = `P "$(tname) outputs the scope name and paths of included B0 \
                  files. If $(b,--root) is specified only shows the includes \
                  of the root B0 file."
  in
  B0_b0.Cli.subcmd_with_driver_conf "includes" ~doc ~descr
    Term.(const includes $ root $ B0_b0.Cli.format)

let log =
  let doc = "Show driver compilation log" in
  let descr = `Blocks [
      `P "$(tname) shows the driver compilation operations \
          in various formats. If $(b,--path) \
          is specified, shows the path to the log.";
      `S Manpage.s_options;
      `S B00_cli.s_output_format_options;
      `S B00_cli.Op.s_selection_options;
      `Blocks B00_cli.Op.query_man; ]
  in
  let envs = B0_b0.Cli.pager_envs in
  B0_b0.Cli.subcmd_with_driver_conf "log" ~doc ~descr ~envs
    Term.(const log $ B00_cli.Arg.output_format () $
          B00_cli.Memo.Log.out_format_cli () $ B00_cli.Op.query_cli ())

let path =
  let doc = "Output the B0 file path (default command)" in
  let descr = `P "$(tname) outputs the B0 file path." in
  B0_b0.Cli.subcmd_with_driver_conf "path" ~doc ~descr path_term

let requires =
  let doc = "Output the OCaml libraries required by the B0 file" in
  let descr = `P "$(tname) outputs the OCaml libraries required to compile \
                  the B0 file. If $(b,--root) is specified only shows the \
                  requires of the root B0 file."
  in
  B0_b0.Cli.subcmd_with_driver_conf "requires" ~doc ~descr
    Term.(const requires $ root)

let source =
  let doc = "Output the expanded B0 source file" in
  let descr = `P "$(tname) outputs the expanded B0 source file compiled
                  by the driver. If $(b,--root) is specified shows the \
                  non-expanded source of the root B0 file."
  in
  B0_b0.Cli.subcmd_with_driver_conf "source" ~doc ~descr
    Term.(const source $ root)

let subs = [boot; compile; edit; gather; includes; log; path; requires; source ]

let cmd =
  let doc = "Operate on the B0 file" in
  let descr =
    `P "$(tname) operates on the B0 file. The default command is $(b,path).";
  in
  let default = path_term in
  B0_b0.Cli.cmd_group_with_driver_conf "file" ~doc ~descr ~default subs

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
