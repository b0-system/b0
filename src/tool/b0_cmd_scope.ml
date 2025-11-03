(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let pp_name = B0_scope.pp_name
let pp_dir = Fmt.st' [`Fg `Blue] Fpath.pp
let pp_scope_dir ppf (_, dir) = pp_dir ppf dir
let pp_scope_name ppf (name, _) = B0_scope.pp_name ppf name
let pp_scope_all ppf (name, dir) =
  Fmt.pf ppf "@[%a %a@]" B0_scope.pp_name name Fpath.pp dir

let warn_incl_excl n =
  Log.warn @@ fun m ->
  m "@[Scope %a is both included and excluded. Excluding.@]"
    B0_scope.pp_name n

let get_scopes _conf ~topmost ~includes ~excludes =
  let scopes = List.sort compare (B0_scope.name_list ()) in
  let scopes = List.map (fun (n, file) -> (n, Fpath.parent file)) scopes in
  let keep (name, _) =
    if topmost && (String.contains name '.') then false else match includes with
    | [] -> not (List.mem name excludes)
    | incs ->
        let inc = List.mem name includes in
        let exc = List.mem name excludes in
        if not inc then false else
        if not exc then true else (warn_incl_excl name; false)
  in
  List.filter keep scopes

let exec_when
    ~prefix_mode ~cond ~topmost ~includes ~excludes ~keep_going ~cmd conf
  =
  let err (_, dir) e =
    Log.err (fun m -> m "@[%a: %s@]" pp_dir dir e); Os.Exit.some_error
  in
  let rec loop = function
  | [] -> Os.Exit.ok
  | (name, dir as s) :: ss ->
      match cond s with
      | Error e -> err s e
      | Ok false -> loop ss
      | Ok true ->
          if not prefix_mode then begin
            Log.stdout (fun m -> m "@[%a: %a@]" pp_name name pp_dir dir);
            match Os.Cmd.run ~cwd:dir cmd with
            | Error e when not keep_going -> err s e
            | Error _
            | Ok () -> Log.stdout (fun m -> m ""); loop ss
          end else begin
            match Os.Cmd.run_out ~trim:false ~cwd:dir cmd with
            | Error e when not keep_going -> err s e
            | Error _ -> loop ss
            | Ok out ->
                let dir = Fpath.ensure_trailing_dir_sep dir in
                let dir = Fmt.str "%a" Fpath.pp_unquoted dir in
                let print_line _ () l =
                  if l = "" then
                    (* No line here according POSIX's text file convention. *)
                    ()
                  else (print_string dir; print_string l)
                in
                String.fold_ascii_lines ~drop_newlines:false print_line () out;
                loop ss
          end
  in
  loop (get_scopes conf ~topmost ~includes ~excludes)

let exec
    ~prefix_mode ~topmost ~includes ~excludes ~keep_going ~tool ~tool_args conf
  =
  let cmd = Cmd.list (tool :: tool_args) in
  let cond = Fun.const (Ok true) in
  exec_when
    ~prefix_mode ~cond ~topmost ~includes ~excludes ~keep_going ~cmd conf

let list ~topmost ~includes ~excludes ~output_details ~path conf =
  let pp_scope =
    if path then pp_scope_dir else match output_details with
    | `Short -> pp_scope_name | `Normal | `Long -> pp_scope_all
  in
  let pp_scopes = Fmt.(list pp_scope) in
  let scopes = get_scopes conf ~topmost ~includes ~excludes in
  if scopes <> [] then Log.stdout (fun m -> m "@[<v>%a@]" pp_scopes scopes);
  Os.Exit.ok

let symlink ~topmost ~includes ~excludes ~dir ~rm conf =
  Log.if_error ~use:Os.Exit.some_error @@
  let scopes = get_scopes conf ~topmost ~includes ~excludes in
  let symlink_scope (scope, path) =
    if scope = "" (* root scope *) then Ok () else
    let err e = Fmt.str "Could not symlink scope %s: %s" scope e in
    Result.map_error err @@
    let symlink = Fpath.(dir / scope) in
    let* exists = Os.Path.exists symlink in
    let force = true and make_path = true in
    if not exists then Os.Path.symlink ~force ~make_path ~src:path symlink else
    let* stat = Os.Path.symlink_stat symlink in
    if stat.st_kind = Unix.S_LNK
    then Os.Path.symlink ~force ~make_path ~src:path symlink else
    begin
      (Log.warn @@ fun m ->
       m "%a: exists and not a symlink, skipped." Fpath.pp symlink);
      Ok ()
    end
  in
  let remove_scope_symlink (scope, _) =
    if scope = "" (* root scope *) then Ok () else
    let err e = Fmt.str "Could not remove symlink for scope %s: %s" scope e in
    Result.map_error err @@
    let symlink = Fpath.(dir / scope) in
    let* exists = Os.Path.exists symlink in
    if not exists then Ok () else
    let* stat = Os.Path.symlink_stat symlink in
    if stat.st_kind = Unix.S_LNK
    then Result.map ignore (Os.Path.delete ~recurse:false symlink) else
    (Log.warn (fun m -> m "%a: not a symlink, skipped." Fpath.pp symlink);
     Ok ())
  in
  let op = if rm then remove_scope_symlink else symlink_scope in
  let* () = List.iter_stop_on_error op scopes in
  Ok Os.Exit.ok

let vcs
    ~prefix_mode ~topmost ~includes ~excludes ~all ~keep_going ~vcs_kind
    ~vcs_args conf
  =
  let cmd = match vcs_kind with
  | B0_vcs_repo.Git -> Cmd.(arg "git" %% list vcs_args)
  | B0_vcs_repo.Hg -> Cmd.(arg "hg" %% list vcs_args)
  in
  let is_vcs_kind (_, dir) = match B0_vcs_repo.find ~kind:vcs_kind ~dir () with
  | Ok None -> Ok false
  | Ok (Some vcs) -> if all then Ok true else B0_vcs_repo.is_dirty vcs
  | Error _ as e -> e
  in
  exec_when
    ~prefix_mode ~cond:is_vcs_kind ~topmost ~includes ~excludes ~keep_going
    ~cmd conf

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let topmost =
  let doc =
    "Only consider topmost scopes included by the root b0 file. Those \
     recursively included by these are excluded."
  in
  let docs = B0_tool_cli.s_scope_selection in
  Arg.(value & flag & info ["topmost"] ~doc ~docs)

let excludes =
  let doc = "Exclude scope $(docv) from the request. Repeatable." in
  let docs = B0_tool_cli.s_scope_selection and docv = "SCOPE" in
  Arg.(value & opt_all string [] & info ["x"; "exclude"] ~doc ~docv ~docs)

let includes =
  let doc = "Include scope $(docv) in the request. Repeatable." in
  let docs = B0_tool_cli.s_scope_selection and docv = "SCOPE" in
  Arg.(value & opt_all string [] & info ["i"; "include"] ~doc ~docv ~docs)

let keep_going =
  let doc = "Do not stop if a tool invocation exits with non-zero (default)." in
  let keep_going = true, Arg.info ["k"; "keep-going"] ~doc in
  let doc = "Stop if a tool invocation exits with non-zero." in
  let fail_stop = false, Arg.info ["f"; "fail-stop"] ~doc in
  Arg.(value & vflag true [keep_going; fail_stop])

let cli_arg ~docv =
  let completion = Arg.Completion.complete_restart in
  Arg.Conv.of_conv ~docv Arg.string ~completion

let tool =
  let doc = "Invoke tool $(docv)." in
  Arg.(required & pos 0 (some (cli_arg ~docv:"TOOL")) None & info [] ~doc)

let all =
  let doc = "Apply command to all VCS scopes, not only those that are dirty." in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let prefix_mode =
  let doc =
    "Rewrite standard output of invocation by prefixing the scope directory on \
     each line. Useful for $(b,git grep) which doesn't seem to \
     have an option to print absolute paths."
  in
  Arg.(value & flag & info ["p"; "prefix-scope-dir"] ~doc)

let vcs_kind =
  let vcss = ["git", B0_vcs_repo.Git; "hg", B0_vcs_repo.Hg] in
  let doc =
    Fmt.str "Invoke vcs $(docv). Must be one of %s." (Arg.doc_alts_enum vcss)
  in
  Arg.(required & pos 0 (some (Arg.enum vcss)) None & info [] ~doc ~docv:"VCS")

let vcs_subcmd =
  let doc = "Invoke VCS subcommand $(docv)." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"SUBCMD")

let tool_args =
  let doc = "Argument for the tool. Start with a $(b,--) \
             token otherwise options get interpreted by $(tool)."
  in
  Arg.(value & pos_right 0 (cli_arg ~docv:"ARG") [] & info [] ~doc)

let vcs_syn =
  "$(cmd) [$(i,OPTION)]… $(b,--) $(i,SUBCMD) [$(i,ARG)]…"

let select_doc =
  `P "By default all scopes are considered and options $(b,--topmost) \
      and $(b,-x) allow to prune them. If the repeatable $(b,-i) \
      option is used only the mentioned scopes are considered."

let exec =
  let doc = "Execute a tool in scope directories" in
  let synopsis = `P "$(cmd) [$(i,OPTION)]… $(b,--) $(i,TOOL) [$(i,ARG)]…" in
  let descr = `Blocks [
    `P "$(cmd) executes $(i,TOOL) with given arguments in the \
        directory of each of the scopes. The process continues \
        if $(i,TOOL) returns with a non-zero exit code, \
        use the option $(b,--fail-stop) to prevent that.";
    select_doc ]
  in
  B0_tool_cli.cmd_with_b0_file "exec" ~doc ~synopsis ~descr @@
  let+ prefix_mode and+ topmost and+ includes and+ excludes and+ keep_going
  and+ tool and+ tool_args in
  exec ~prefix_mode ~topmost ~includes ~excludes ~keep_going ~tool ~tool_args

let list =
  let doc = "List scopes" in
  let descr = `Blocks [
      `P "$(cmd) lists scope names and their location. If $(b,--path) is \
          specified only paths are listed.";
      select_doc ]
  in
  B0_tool_cli.cmd_with_b0_file "list" ~doc ~descr @@
  let+ path =
    let doc = "Only print the scope paths." in
    Arg.(value & flag & info ["path"] ~doc)
  and+ topmost and+ includes and+ excludes
  and+ output_details = B0_cli.output_details in
  list ~topmost ~includes ~excludes ~output_details ~path

let symlink =
  let doc = "Create symlinks to scope directories" in
  let descr = `Blocks [
      `P "$(cmd) symlinks scope names to their directory in the directory \
          specified with the $(b,--in-dir) option or the current working \
          directory if none is specified. If a symlink already exists it is \
          relinked, if a corresponding file or directory exists it is left
          intact";
      `P "If the $(b,--rm) flag is specified removes the symlinks that \
          would be created, only if they exist and are symlinks.";
      select_doc ]
  in
  B0_tool_cli.cmd_with_b0_file "symlink" ~doc ~descr @@
  let+ dir =
    let doc =
      "Create symlinks in directory $(docv). The path to the directory is \
       created if it doesn't exist."
    in
    let docv = "DIR" in
    Arg.(value & opt B0_std_cli.dirpath (Fpath.v ".") &
         info ["d"; "in-dir"] ~doc ~docv)
  and+ rm =
    let doc =
      "Remove symlinks rather than create them. If they exist and are symlinks."
    in
    Arg.(value & flag & info ["rm"] ~doc)
  and+ topmost and+ includes and+ excludes in
  symlink ~topmost ~includes ~excludes ~dir ~rm

let vcs =
  let doc = "Execute a vcs in its managed and dirty scopes" in
  let synopsis = `P "$(cmd) [$(i,OPTION)]… $(b,--) $(i,VCS) [$(i,ARG)]…" in
  let descr = `Blocks [
      `P "$(cmd) executes $(i,VCS) with given arguments in the directory \
          of each of the scopes which are found to be managed by $(i,VCS) \
          and dirty; or all of them if $(b,--all) is specified. It is a \
          specialized $(b,b0 scope exec) for version control systems.";
      `P "Typical worfklow:";
      `P "$(b,b0)"; `Noblank;
      `P "Error: ..."; `Noblank;
      `P "... # Fix errors"; `Noblank;
      `P "$(b,b0)"; `Noblank;
      `P "$(cmd) $(b,-- git status)"; `Noblank;
      `P "$(cmd) $(b,-- git add -p)"; `Noblank;
      `P "$(cmd) $(b,-- git commit -m 'Cope with changes!')"; `Noblank;
      `P "$(cmd) $(b,-a -- git push)";
      `P "Or:";
      `P "$(cmd) $(b,-a -p -- git grep --color) $(i,PATTERN)";
      `P "The process continues if $(i,VCS) returns with a non-zero exit \
          code, use the option $(b,--fail-stop) to prevent that.";
      select_doc;
    ]
  in
  B0_tool_cli.cmd_with_b0_file "vcs" ~doc ~synopsis ~descr @@
  let+ prefix_mode and+ topmost and+ includes and+ excludes and+ all
  and+ keep_going and+ vcs_kind and+ vcs_args = tool_args in
  vcs ~prefix_mode ~topmost ~includes ~excludes ~all ~keep_going ~vcs_kind
    ~vcs_args

let cmd =
  let doc = "Operate on scopes" in
  let descr =
    `P "The command $(cmd) operates on scopes. The $(b,b0 scope exec) \
        command allows to fold over scope directories and invoke an \
        arbitary tool. Use the $(b,b0 vcs) command to invokes a vcs operation \
        on scope directories that are managed by it and dirty.";
  in
  B0_tool_cli.cmd_group "scope" ~doc ~descr @@
  [exec; list; symlink (* vcs *) ]
