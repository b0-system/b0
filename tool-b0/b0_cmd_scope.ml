(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let is_vcs ~all find (_, dir) =
  let* vcs = find ?dir:(Some dir) () in
  match vcs with
  | None -> Ok false
  | Some vcs -> if all then Ok true else B00_vcs.is_dirty vcs

let get_scopes c root excludes k =
  (* XXX shouldn't we rather save them in `B0_def.Scopes` ? *)
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  Log.if_error' ~header:"" ~use:B0_driver.Exit.b0_file_error @@
  let* s = Os.File.read b0_file in
  let* src = B0_file.of_string ~file:b0_file s in
  let* incs = match root with
  | true -> Ok (B0_file.b0_includes src)
  | false ->
      let* e = B0_file.expand src in
      Ok (B0_file.expanded_b0_includes e)
  in
  let inc_to_scope ((n, _), (p, _)) = n, Fpath.parent p in
  let root = ("." (* XXX what should we use here ? *), Fpath.parent b0_file) in
  let scopes = root :: List.sort compare (List.map inc_to_scope incs) in
  k (List.filter (fun (n, _) -> not (List.mem n excludes)) scopes)

let exec_when cond c root excludes keep_going cmd =
  let err (_, dir) e =
    Log.err (fun m -> m "@[%a: %s@]" Fpath.pp_unquoted dir e);
    Ok B00_cli.Exit.some_error
  in
  get_scopes c root excludes @@ function scopes ->
  let rec loop = function
  | [] -> Ok B00_cli.Exit.ok
  | (n, p as s) :: ss ->
      match cond s with
      | Error e -> err s e
      | Ok false -> loop ss
      | Ok true ->
          Log.app begin fun m ->
            m "@[%a: %a@]"
              Fmt.(code string) n (Fmt.tty [`Faint] Fpath.pp_unquoted) p
          end;
          match Os.Cmd.run ~cwd:p cmd with
          | Error e when not keep_going -> err s e
          | Error _ | Ok () -> Log.app (fun m -> m ""); loop ss
  in
  loop scopes

let list root excludes format path c =
  get_scopes c root excludes @@ function scopes ->
    let pp_scope = match path with
    | true -> fun ppf (_, dir) -> Fpath.pp_unquoted ppf dir
    | false ->
        match format with
        | `Short -> fun ppf (n, _) -> Fmt.(code string) ppf n
        | `Normal | `Long ->
            fun ppf (n, dir) ->
              Fmt.pf ppf "@[%a %a@]" Fmt.(code string) n Fpath.pp_unquoted dir
    in
    Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_scope) scopes);
    Ok B00_cli.Exit.ok

let exec root excludes keep_going tool tool_args c =
  let cmd = tool :: tool_args in
  exec_when (fun _ -> Ok true) c root excludes keep_going (Cmd.list cmd)

let git root excludes all keep_going full_cmd subcmd subcmd_args c =
  let cmd = subcmd :: subcmd_args in
  let cmd = if full_cmd then Cmd.list cmd else Cmd.(atom "git" %% list cmd) in
  exec_when (is_vcs ~all B00_vcs.Git.find) c root excludes keep_going cmd

let hg root excludes all keep_going full_cmd subcmd subcmd_args c=
  let cmd = subcmd :: subcmd_args in
  let cmd = if full_cmd then Cmd.list cmd else Cmd.(atom "hg" %% list cmd) in
  exec_when (is_vcs ~all B00_vcs.Hg.find) c root excludes keep_going cmd

(* Command line interface *)

open Cmdliner

let root =
  let doc = "Only consider scopes included by the root B0 file. Those \
             recursively included by these are excluded."
  in
  Arg.(value & flag & info ["root"] ~doc)

let excludes =
  let doc = "Exclude scope $(docv) from the request. Repeatable." in
  Arg.(value & opt_all string [] & info ["x"; "exclude"] ~doc ~docv:"SCOPE")

let keep_going =
  let doc = "Do not stop if a tool invocation exits with non zero." in
  Arg.(value & flag & info ["k"; "keep-going"] ~doc)

let full_cmd =
  let doc = "Specify a full command rather than a subcommand of the VCS." in
  Arg.(value & flag & info ["c"; "full-cmd"] ~doc)

let tool =
  let doc = "Invoke tool $(docv)." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"TOOL")

let all =
  let doc = "Apply command to all VCS scopes, not only those that are dirty." in
  Arg.(value & flag & info ["a"; "all"] ~doc)

let vcs_subcmd =
  let doc = "Invoke VCS subcommand $(docv)." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"SUBCMD")

let tool_args =
  let doc = "Argument for the tool. Start with a $(b,--) \
             token otherwise options get interpreted by $(mname)."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let list_term =
  let path =
    let doc = "Only print the scope paths." in
    Arg.(value & flag & info ["path"] ~doc)
  in
  Term.(const list $ root $ excludes $ B0_b0.Cli.format $ path)

let vcs_syn =
  "$(mname) $(b,scope) $(tname) [$(i,OPTION)]… $(b,--) $(i,SUBCMD) [$(i,ARG)]…"

(* Commands *)

let exec =
  let doc = "Execute a tool in scope directories" in
  let synopsis = `P "$(mname) $(b,scope) $(tname) [$(i,OPTION)]… $(b,--) \
                     $(i,TOOL) [$(i,ARG)]…"
  in
  let descr = `P "$(tname) executes $(i,TOOL) with given arguments in the \
                  directory of each of the scopes. The process is stopped \
                  if $(i,TOOL) returns with a non zero exit code, use the \
                  option $(b,--keep-going) to prevent that."
  in
  B0_b0.Cli.subcmd_with_driver_conf "exec" ~doc ~synopsis ~descr
    Term.(const exec $ root $ excludes $ keep_going $ tool $ tool_args)

let hg =
  let doc = "Execute $(b,hg) in Mercurial managed scopes" in
  let synopsis = `P vcs_syn in
  let descr = `P "$(tname) works exactly like $(b,b0 scope git) but with the \
                  Mercurial VCS, see $(mname) $(b,scope git --help) for
                  more information"
  in
  B0_b0.Cli.subcmd_with_driver_conf "hg" ~doc ~synopsis ~descr
    Term.(const hg $ root $ excludes $ all $ keep_going $ full_cmd $
          vcs_subcmd $ tool_args)

let git =
  let doc = "Execute $(b,git) in Git managed scopes" in
  let synopsis = `P vcs_syn in
  let descr = `Blocks [
      `P "$(tname) executes the Git subcommand $(i,SUBCMD) \
          with given arguments in the directory of each of the scopes
          which are found to be managed by Git and dirty;
          or all of them if $(b,--all) is specified.";
      `P "If $(b,--full-cmd) is specified the positional arguments specify a
          full command like $(b,scope exec) does, not a VCS subcommand.";
      `P "The process is stopped if an execution returns with a non zero exit
          code, use the option $(b,--keep-going) to prevent that." ]
  in
  B0_b0.Cli.subcmd_with_driver_conf "git" ~doc ~synopsis ~descr
    Term.(const git $ root $ excludes $ all $ keep_going $ full_cmd $
          vcs_subcmd $ tool_args)

let list =
  let doc = "List scopes (default command)" in
  let descr = `P "$(tname) lists scope names and their location. \
                  If $(b,--path) is specified only paths are listed."
  in
  let envs = B0_b0.Cli.pager_envs in
  B0_b0.Cli.subcmd_with_driver_conf "list" ~doc ~descr ~envs list_term

let subs = [exec; hg; git; list;]
let cmd =
  let doc = "Operate on B0 scopes" in
  let descr =
    `Blocks [
      `P "$(tname) operates on scopes. The default command is $(tname) \
          $(b,list).";
      `P "$(tname) can fold over scope directories and bulk operate \
          their VCSs (if applicable). Typical usage:";
      `P "$(b,> b0)"; `Noblank;
      `P "Error: ..."; `Noblank;
      `P "$(b,> ... # Fix errors)"; `Noblank;
      `P "$(b,> b0)"; `Noblank;
      `P "$(b,> b0 scope git -- status)"; `Noblank;
      `P "$(b,> b0 scope git -- add -p)"; `Noblank;
      `P "$(b,> b0 scope git -- commit -m 'Cope with changes!')";
      `P "To invoke arbitrary tools in scopes use $(b,b0 scope exec). Options
          $(b,--root) and $(b,-x) allow to prune the list of scopes.";
    ]
  in
  let default = list_term in
  B0_b0.Cli.cmd_group_with_driver_conf "scope" ~doc ~descr ~default subs


(*
let scope
    c details path root excludes all full_cmd keep_going action action_args
  =
  match action with
  | `List -> list c root excludes details path
  | `Exec -> exec c root excludes keep_going action_args
  | `Git -> git c root excludes all keep_going full_cmd action_args
  | `Hg -> hg c root excludes all keep_going full_cmd action_args

let action =
  let action = [ "list", `List; "exec", `Exec; "git", `Git; "hg", `Hg; ] in
  let doc =
    let alts = Arg.doc_alts_enum action in
    Fmt.str "The action to perform. $(docv) must be one of %s." alts
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")


*)

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
