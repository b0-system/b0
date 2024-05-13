(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

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
  Ok B0_cli.Exit.ok

let compile c =
  get_b0_file_src c @@ fun f ->
  let* _ =
    B0_driver.Compile.compile c ~driver:B0_tool_std.driver ~feedback:true f
  in
  Ok B0_cli.Exit.ok

let edit all c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.if_error' ~use:B0_cli.Exit.some_error @@
  let* editor = B0_editor.find () in
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
  Result.bind (B0_editor.edit_files editor files) @@ function
  | `Exited 0 -> Ok B0_cli.Exit.ok
  | _ -> Ok B0_cli.Exit.some_error

let gather dest dirs force keep_symlinks keep_going relative symlink_scopes c =
  let pp_inc_directive ppf (s, f) =
    Fmt.pf ppf {|@[[@@@@@@B0.include "%s" "%a"]@]|} s Fpath.pp_unquoted f
  in
  let gather_b0_file ~dest ~relative ~keep_symlinks ~keep_going ~seen dir =
    let* exists = Os.Dir.exists dir in
    if not exists && keep_going then Ok None else
    let* () = Os.Dir.must_exist dir in (* For the error message *)
    let b0_file = Fpath.(dir / B0_driver.Conf.b0_file_name) in
    let* exists = Os.File.exists b0_file in
    if not exists && keep_going then Ok None else
    let* b0_file' = Os.Path.realpath b0_file in
    let b0_file = match keep_symlinks with
    | true when relative -> Fpath.relative ~to_dir:dest b0_file
    | true -> Fpath.(dest // b0_file)
    | false when relative -> Fpath.relative ~to_dir:dest b0_file'
    | false -> b0_file'
    in
    let* scope =
      if not keep_symlinks then Ok (Fpath.(basename @@ parent b0_file')) else
      match Fpath.basename dir with
      | ".." | "." ->
          Fmt.error "Cannot determine a scope name for %a" Fpath.pp dir
      | scope -> Ok scope
    in
    let scope = String.map (function '.' -> '_' | c -> c) scope in
    let scope, seen = match String.Set.mem scope seen with
    | false -> scope, String.Set.add scope seen
    | true ->
        let exists s = String.Set.mem s seen in
        let scope' = String.unique ~exists scope in
        scope', String.Set.add scope' seen
    in
    Ok (Some (scope, b0_file, seen))
  in
  let rec gather_dirs dest seen acc = function
  | dir :: dirs ->
      let* info =
        gather_b0_file ~dest ~relative ~keep_symlinks ~keep_going ~seen dir
      in
      begin match info with
      | None -> gather_dirs dest seen acc dirs
      | Some (scope, b0_file, seen) when not symlink_scopes ->
          gather_dirs dest seen ((scope, b0_file) :: acc) dirs
      | Some (scope, b0_file, seen) ->
          let src = Fpath.parent b0_file and dst = Fpath.(dest / scope) in
          let* () = Os.Path.symlink ~force ~make_path:false ~src dst in
          let b0_file = Fpath.(v scope / B0_driver.Conf.b0_file_name) in
          gather_dirs dest seen ((scope, b0_file) :: acc) dirs
      end
  | [] -> Ok (List.sort compare acc)
  in
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let* dest, dest_b0_file = match dest with
  | None when symlink_scopes ->
      Fmt.error "Will not symlink scopes without option %a" Fmt.code "--dest"
  | None -> Ok (B0_driver.Conf.cwd c, None)
  | Some dest ->
      let* _exists = Os.Dir.create ~make_path:true dest in
      let dest_b0_file = Fpath.(dest / B0_driver.Conf.b0_file_name) in
      let* exists = Os.File.exists dest_b0_file in
      if exists && not force then
        Fmt.error "%a: file exists, use option %a to overwrite."
          Fpath.pp dest_b0_file Fmt.code "--force"
      else
      Ok (dest, Some dest_b0_file)
  in
  let* incs = gather_dirs dest String.Set.empty [] dirs in
  let incs = Fmt.str "@[<v>%a@]" (Fmt.list pp_inc_directive) incs in
  let b0_file = match dest_b0_file with
  | None -> Fpath.dash | Some file -> file
  in
  let force = true (* The force logic was handled above *) in
  let* () = Os.File.write ~force ~make_path:false b0_file incs in
  Ok B0_cli.Exit.ok

let includes root format c =
  let pp_inc = match format with
  | `Short -> fun ppf (_, (p, _)) -> Fpath.pp_unquoted ppf p
  | `Normal | `Long ->
      fun ppf ((n, _), (p, _)) ->
        Fmt.pf ppf "@[%a %a@]" Fmt.code n Fpath.pp_unquoted p
  in
  get_b0_file_src c @@ fun src ->
  let* incs = match root with
  | true -> Ok (B0_file.b0_includes src)
  | false ->
      let* exp = B0_file.expand src in
      Ok (B0_file.expanded_b0_includes exp)
  in
  if incs <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_inc) incs);
  Ok B0_cli.Exit.ok

let log format log_format op_selector c =
  Log.if_error ~use:B0_cli.Exit.some_error @@
  let don't = B0_driver.Conf.no_pager c || log_format = `Trace_event in
  let log_file = B0_driver.Compile.build_log c ~driver:B0_tool_std.driver in
  let* pager = B0_pager.find ~don't () in
  let* () = B0_pager.page_stdout pager in
  let* l = B0_cli.Memo.Log.read log_file in
  B0_cli.Memo.Log.out Fmt.stdout log_format format op_selector ~path:log_file l;
  Ok B0_cli.Exit.ok

let path c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.app (fun m -> m "%a" Fpath.pp_unquoted b0_file);
  Ok B0_cli.Exit.ok

let requires root c =
  let pp_require = Fmt.using fst B0_ocaml.Libname.pp in
  get_b0_file_src c @@ fun src ->
  let* reqs = match root with
  | true -> Ok (B0_file.requires src)
  | false ->
      let* exp = B0_file.expand src in
      Ok (B0_file.expanded_requires exp)
  in
  if reqs <> []
  then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_require) reqs);
  Ok B0_cli.Exit.ok

let source root c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.if_error' ~header:"" ~use:B0_driver.Exit.b0_file_error @@
  match root with
  | true ->
      let* s = Os.File.read b0_file in
      Log.app (fun m -> m "%s" s);
      Ok B0_cli.Exit.ok
  | false ->
      let* s = Os.File.read b0_file in
      let* src = B0_file.of_string ~file:b0_file s in
      let* exp = B0_file.expand src in
      let esrc = B0_file.expanded_src exp in
      Log.app (fun m -> m "%s" esrc);
      Ok B0_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let root =
  let doc =
    "Apply operation on the root B0 file only rather than on its expansion."
  in
  Arg.(value & flag & info ["root"] ~doc)

let boot =
  let doc = "Install libraries needed for the B0 file" in
  let descr = `P "$(iname) install libraries needed to compile the B0 file." in
  B0_tool_std.Cli.subcmd_with_driver_conf "boot" ~doc ~descr @@
  Term.(const boot $ root)

let compile =
  let doc = "Compile the driver for the B0 file" in
  let descr = `P "$(iname) compiles the driver for the B0 file." in
  B0_tool_std.Cli.subcmd_with_driver_conf "compile" ~doc ~descr @@
  Term.(const compile)

let edit =
  let doc = "Edit the B0 file" in
  let descr = `P "$(iname) opens the B0 file in your editor. If $(b,--all) \
                  is specified also opens all includes."
  in
  let all =
    let doc = "Edit the B0 file and all its includes." in
    Arg.(value & flag & info ["all"] ~doc)
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "edit" ~doc ~descr @@
  Term.(const edit $ all)

let gather =
  let doc = "Gathers and scope B0 files into a single B0.ml file" in
  let descr = `Blocks [
      `P "$(iname) outputs on $(b,stdout) a B0 file that includes and scopes \
          the B0 files existing in given $(i,DIR)… directories.";
      `P "If the $(b,-d) $(i,DEST) option is specified, the B0 file is written \
          in the $(i,DEST) destination directory and, if the $(b,-s) option \
          is specified it symlinks the scope names to their directory \
          in $(i,DEST).";
      `P "Examples:";
      `Pre "$(iname) $(b,/path/to/repos/*)"; `Noblank;
      `Pre "$(iname) $(b,-s -d /tmp/gather) $(b,/path/to/repos/*)"; ]
  in
  let dest =
    let doc = "Directory in which the $(b,B0.ml) is written. If unspecified \
               the file is written on $(b,stdout)."
    in
    let docv = "DIR" in
    Arg.(value & opt (some B0_cli.fpath) None & info ["d"; "dest"] ~doc ~docv)
  in
  let dirs =
    let doc = "Gather the $(docv)$(b,/B0.ml) file." in
    Arg.(non_empty & pos_all B0_cli.fpath [] & info [] ~doc ~docv:"DIR")
  in
  let force =
    let doc = "Write the $(b,B0.ml) file even if it exists in the \
               destination directory of $(b,--dest) and force symlinks \
               creation if $(b,--symlink-scopes) is specified."
    in
    Arg.(value & flag & info ["force"] ~doc)
  in
  let keep_going =
    let doc = "Skip input directories that have no B0 file (default)." in
    let keep_going = true, Arg.info ["k"; "keep-going"] ~doc in
    let doc = "Stop if an input directory has no B0 file." in
    let fail_stop = false, Arg.info ["f"; "fail-stop"] ~doc in
    Arg.(value & vflag true [keep_going; fail_stop])
  in
  let keep_symlinks =
    let doc = "Don't resolve symlinks in input directories." in
    Arg.(value & flag & info ["keep-symlinks"] ~doc)
  in
  let relative =
    let doc = "Make include paths and scope symlinks relative to \
               the cwd or the $(b,--dest) directory." in
    Arg.(value & flag & info ["relative"] ~doc)
  in
  let symlink_scopes =
    let doc =
      "Symlink scopes in the destination directory specified via $(b,--dest) \
       and use these directories as include paths."
    in
    Arg.(value & flag & info ["s"; "symlink-scopes"] ~doc)
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "gather" ~doc ~descr @@
  Term.(const gather $ dest $ dirs $ force $ keep_symlinks $ keep_going $
        relative $ symlink_scopes)

let includes =
  let doc = "Output scope name and paths of included B0 files" in
  let descr = `P "$(iname) outputs the scope name and paths of included B0 \
                  files. If $(b,--root) is specified only shows the includes \
                  of the root B0 file."
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "includes" ~doc ~descr @@
  Term.(const includes $ root $ B0_tool_std.Cli.format)

let log =
  let doc = "Show driver compilation log" in
  let descr = `Blocks [
      `P "$(iname) shows the driver compilation operations \
          in various formats. If $(b,--path) \
          is specified, shows the path to the log.";
      `S Manpage.s_options;
      `S B0_cli.s_output_format_options;
      `S B0_cli.Op.s_selection_options;
      `Blocks B0_cli.Op.query_man; ]
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "log" ~doc ~descr @@
  Term.(const log $ B0_cli.output_format () $
        B0_cli.Memo.Log.out_format_cli () $ B0_cli.Op.query_cli ())

let path =
  let doc = "Output the B0 file path (default command)" in
  let descr = `P "$(iname) outputs the B0 file path." in
  B0_tool_std.Cli.subcmd_with_driver_conf "path" ~doc ~descr @@
  Term.(const path)

let requires =
  let doc = "Output the OCaml libraries required by the B0 file" in
  let descr = `P "$(iname) outputs the OCaml libraries required to compile \
                  the B0 file. If $(b,--root) is specified only shows the \
                  requires of the root B0 file."
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "requires" ~doc ~descr @@
  Term.(const requires $ root)

let source =
  let doc = "Output the expanded B0 source file" in
  let descr = `P "$(iname) outputs the expanded B0 source file compiled
                  by the driver. If $(b,--root) is specified shows the \
                  non-expanded source of the root B0 file."
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "source" ~doc ~descr @@
  Term.(const source $ root)

let cmd =
  let doc = "Operate on the B0 file" in
  let descr = `P "$(iname) operates on the B0 file." in
  B0_tool_std.Cli.cmd_group "file" ~doc ~descr @@
  [ boot; compile; edit; gather; includes; log; path; requires; source ]
