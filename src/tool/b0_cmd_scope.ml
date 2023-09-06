(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let pp_name = B0_def.Scope.pp_name
let pp_dir = Fmt.tty [`Fg `Blue] Fpath.pp
let pp_scope_dir ppf (_, dir) = pp_dir ppf dir
let pp_scope_name ppf (name, _) = B0_def.Scope.pp_name ppf name
let pp_scope_all ppf (name, dir) =
  Fmt.pf ppf "@[%a %a@]" B0_def.Scope.pp_name name Fpath.pp dir

let warn_incl_excl n =
  Log.warn @@ fun m ->
  m "@[Scope %a is both included and excluded. Excluding.@]"
    B0_def.Scope.pp_name n

let get_scopes c ~topmost ~includes ~excludes =
  let scopes = List.sort compare (B0_def.Scope.name_list ()) in
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

let exec_when cond c topmost includes excludes keep_going cmd =
  let err (_, dir) e =
    Log.err (fun m -> m "@[%a: %s@]" pp_dir dir e); B0_cli.Exit.some_error
  in
  let rec loop = function
  | [] -> B0_cli.Exit.ok
  | (name, dir as s) :: ss ->
      match cond s with
      | Error e -> err s e
      | Ok false -> loop ss
      | Ok true ->
          Log.app (fun m -> m "@[%a: %a@]" pp_name name pp_dir dir);
          match Os.Cmd.run ~cwd:dir cmd with
          | Error e when not keep_going -> err s e
          | Error _ | Ok () -> Log.app (fun m -> m ""); loop ss
  in
  loop (get_scopes c ~topmost ~includes ~excludes)

let exec topmost includes excludes keep_going tool tool_args c =
  let cmd = tool :: tool_args in
  let always = Fun.const (Ok true) in
  exec_when always c topmost includes excludes keep_going (Cmd.list cmd)

let list topmost includes excludes format path c =
  let pp_scope =
    if path then pp_scope_dir else match format with
    | `Short -> pp_scope_name | `Normal | `Long -> pp_scope_all
  in
  let pp_scopes = Fmt.(list pp_scope) in
  let scopes = get_scopes c ~topmost ~includes ~excludes in
  if scopes <> [] then Log.app (fun m -> m "@[<v>%a@]" pp_scopes scopes);
  B0_cli.Exit.ok

let vcs root includes excludes all keep_going vcs_kind vcs_args c =
  let vcs_cmd = match vcs_kind with
  | B0_vcs.Git -> Cmd.(arg "git" %% list vcs_args)
  | B0_vcs.Hg -> Cmd.(arg "hg" %% list vcs_args)
  in
  let is_vcs_kind (_, dir) = match B0_vcs.find ~kind:vcs_kind ~dir () with
  | Ok None -> Ok false
  | Ok (Some vcs) -> if all then Ok true else B0_vcs.is_dirty vcs
  | Error _ as e -> e
  in
  exec_when is_vcs_kind c root includes excludes keep_going vcs_cmd

(* Command line interface *)

open Cmdliner

let topmost =
  let doc = "Only consider topmost scopes included by the root B0 file. Those \
             recursively included by these are excluded."
  in
  let docs = B0_tool_std.Cli.s_scope_selection in
  Arg.(value & flag & info ["topmost"] ~doc ~docs)

let excludes =
  let doc = "Exclude scope $(docv) from the request. Repeatable." in
  let docs = B0_tool_std.Cli.s_scope_selection in
  Arg.(value & opt_all string [] &
       info ["x"; "exclude"] ~doc ~docv:"SCOPE" ~docs)

let includes =
  let doc = "Include scope $(docv) in the request. Repeatable." in
  let docs = B0_tool_std.Cli.s_scope_selection in
  Arg.(value & opt_all string [] &
       info ["i"; "include"] ~doc ~docv:"SCOPE" ~docs)

let keep_going =
  let doc = "Do not stop if a tool invocation exits with non-zero (default)." in
  let keep_going = true, Arg.info ["k"; "keep-going"] ~doc in
  let doc = "Stop if a tool invocation exits with non-zero." in
  let fail_stop = false, Arg.info ["f"; "fail-stop"] ~doc in
  Arg.(value & vflag true [keep_going; fail_stop])

let tool =
  let doc = "Invoke tool $(docv)." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"TOOL")

let all =
  let doc = "Apply command to all VCS scopes, not only those that are dirty." in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let vcs_kind =
  let vcss = ["git", B0_vcs.Git; "hg", B0_vcs.Hg] in
  let doc =
    Fmt.str "Invoke vcs $(docv). Must be one of %s." (Arg.doc_alts_enum vcss)
  in
  Arg.(required & pos 0 (some (Arg.enum vcss)) None & info [] ~doc ~docv:"VCS")

let vcs_subcmd =
  let doc = "Invoke VCS subcommand $(docv)." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"SUBCMD")

let tool_args =
  let doc = "Argument for the tool. Start with a $(b,--) \
             token otherwise options get interpreted by $(mname)."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let vcs_syn =
  "$(mname) $(b,scope) $(tname) [$(i,OPTION)]… $(b,--) $(i,SUBCMD) [$(i,ARG)]…"

(* Commands *)

let select_doc =
  `P "By default all scopes are considered and options $(b,--topmost) \
      and $(b,-x) allow to prune them. If the repeatable $(b,-i) \
      option is used only the mentioned scopes are considered."

let exec =
  let doc = "Execute a tool in scope directories" in
  let synopsis = `P "$(mname) $(b,scope) $(tname) [$(i,OPTION)]… $(b,--) \
                     $(i,TOOL) [$(i,ARG)]…"
  in
  let descr = `Blocks [
    `P "$(tname) executes $(i,TOOL) with given arguments in the \
        directory of each of the scopes. The process continues \
        if $(i,TOOL) returns with a non-zero exit code, \
        use the option $(b,--fail-stop) to prevent that.";
    select_doc ]
  in
  B0_tool_std.Cli.subcmd_with_b0_file "exec" ~doc ~synopsis ~descr @@
  Term.(const exec $ topmost $ includes $ excludes $ keep_going $ tool $
        tool_args)

let list =
  let doc = "List scopes (default command)" in
  let descr = `Blocks [
      `P "$(tname) lists scope names and their location. \
          If $(b,--path) is specified only paths are listed.";
      select_doc ]
  in
  let envs = B0_tool_std.Cli.pager_envs in
  let path =
    let doc = "Only print the scope paths." in
    Arg.(value & flag & info ["path"] ~doc)
  in
  B0_tool_std.Cli.subcmd_with_b0_file "list" ~doc ~descr ~envs @@
  Term.(const list $ topmost $ includes $ excludes $ B0_tool_std.Cli.format $
        path)

let vcs =
  let doc = "Execute a vcs in its managed and dirty scopes" in
  let synopsis = `P "$(iname) [$(i,OPTION)]… $(b,--) $(i,VCS) [$(i,ARG)]…" in
  let descr = `Blocks [
      `P "$(tname) executes $(i,VCS) with given arguments in the directory \
          of each of the scopes which are found to be managed by $(i,VCS) \
          and dirty; or all of them if $(b,--all) is specified. It is a \
          specialized $(b,b0 scope exec) for version control systems.";
      `P "Typical worfklow:";
      `P "$(b,b0)"; `Noblank;
      `P "Error: ..."; `Noblank;
      `P "... # Fix errors"; `Noblank;
      `P "$(b,b0)"; `Noblank;
      `P "$(iname) $(b,-- git status)"; `Noblank;
      `P "$(iname) $(b,-- git add -p)"; `Noblank;
      `P "$(iname) $(b,-- git commit -m 'Cope with changes!')"; `Noblank;
      `P "$(iname) $(b,-a -- git push)";
      `P "Or:";
      `P "$(iname) $(b,-q -a -- git grep) $(i,PATTERN)";
      `P "The process continues if $(i,VCS) returns with a non-zero exit \
          code, use the option $(b,--fail-stop) to prevent that.";
      select_doc;
    ]
  in
  B0_tool_std.Cli.subcmd_with_b0_file "vcs" ~doc ~synopsis ~descr @@
  Term.(const vcs $ topmost $ includes $ excludes $ all $ keep_going $
        vcs_kind $ tool_args)

let subs = [exec; list; (* vcs *) ]
let cmd =
  let doc = "Operate on B0 scopes" in
  let descr =
    `P "The command $(iname) operates on scopes. The $(b,b0 scope exec) \
        command allows to fold over scope directories and invoke an \
        arbitary tool. Use the $(b,b0 vcs) command to invokes a vcs operation \
        on scope directories that are managed by it and dirty.";
  in
  B0_tool_std.Cli.cmd_group_with_b0_file "scope" ~doc ~descr subs
