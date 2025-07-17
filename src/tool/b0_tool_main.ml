(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let cmd =
  let doc = "Software construction and deployment kit" in
  let exits = B0_driver.Exit.infos in
  let man = [
    `S Manpage.s_synopsis;
    `P "$(tool) $(b,--) [$(i,ACTION)] [$(i,ARG)]…"; `Noblank;
    `P "$(tool) $(i,COMMAND) …";
    `S Manpage.s_description;
    `P "b0 describes software construction and deployments using modular and \
        customizable definitions written in OCaml.";
    `Pre "$(cmd) $(b,list) # See what can be built"; `Noblank;
    `Pre "$(cmd) $(b,--what) # See what gets built"; `Noblank;
    `Pre "$(cmd) # Build"; `Noblank;
    `Pre "$(cmd) $(b,--) [$(i,ACTION)] [$(i,ARG)]… # Build and run an \
          action or unit."; `Noblank;
    `Pre "$(cmd) $(b,-u) $(i,UNIT) # Build $(i,UNIT)"; `Noblank;
    `Pre "$(cmd) $(b,-p) $(i,PACK) # Build pack $(i,PACK).";
    `Pre "Use $(cmd) [$(i,COMMAND)]… $(b,--help) for help about any \
          command.";
    `P "More information is available in the manuals, see $(b,odig doc b0).";
    B0_tool_cli.man_see_manual;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,https://erratique.ch/software/b0) for contact \
        information."; ]
  in
  let version = "%%VERSION%%" in
  let default =
    B0_driver.with_b0_file ~driver:B0_tool.driver B0_cmd_build.term
  in
  Cmd.group (Cmd.info "b0" ~version ~doc ~exits ~man) ~default @@
  [ B0_cmd_browse.cmd;
    B0_cmd_build.cmd;
    B0_cmd_delete.cmd;
    B0_cmd_cache.cmd;
    B0_cmd_dir.cmd;
    B0_cmd_edit.cmd;
    B0_cmd_file.cmd;
    B0_cmd_info.cmd;
    B0_cmd_init.cmd;
    B0_cmd_key.cmd;
    B0_cmd_list.cmd;
    B0_cmd_lock.cmd;
    B0_cmd_log.cmd;
    B0_cmd_pack.cmd;
    B0_cmd_root.cmd;
    B0_cmd_scope.cmd;
    B0_cmd_tool.cmd;
    B0_cmd_test.cmd;
    B0_cmd_unit.cmd;
    B0_cmd_unlock.cmd;
    B0_cmd_vcs.cmd; ]

(* N.B. our driver scheme is not super compatible with cmdliner
   completion because the b0 file compilation logic is done in the
   terms which do not get evaluated on completion.

   If we get in Cmdliner contextual completion via term evaluation we
   could then do the compilation and execv in there. A bit dirty but
   works, the advantage would also be that we don't need to
   understand which commands really need the b0 file, it is defined by
   completion requests. *)

let try_exec_with_b0_file () =
  (* Doesn't return if we manage to compile the b0_file *)
  let open B0_std in
  let open Result.Syntax in
  Log.if_error ~use:() @@
  let* conf =
    (* FIXME of course that doesn't reflect the state of the cli *)
    B0_driver.Conf.setup_with_cli
      ~b0_dir:None ~b0_file:None ~cache_dir:None ~code:None
      ~hash_fun:None ~jobs:None ~no_color:false ~log_level:Log.Warning
      ~no_pager:true ()
  in
  match B0_driver.Conf.b0_file conf with
  | None -> Ok ()
  | Some b0_file ->
      let* src = Os.File.read b0_file in
      let* src = B0_file.of_string ~file:b0_file src in
      let driver = B0_tool.driver in
      match B0_driver.Compile.compile conf ~driver ~feedback:false src with
      | Error e ->
          (Log.warn @@ fun m -> m "%s. See %a." e Fmt.code "b0 file log -e");
          Ok ()
      | Ok exe ->
          let exe = Fpath.to_string exe in
          let cmd = match Array.to_list Sys.argv with
          | [] -> Cmd.arg exe
          | _ :: args -> Cmd.list (exe :: args)
          in
          Os.Exit.exit (Os.Exit.execv ~argv0:"b0" cmd)

let is_completion_request () =
  Array.length Sys.argv > 1 && Sys.argv.(1) = "--__complete"

let main () =
  if is_completion_request () && not (B0_driver.has_b0_file ())
  then try_exec_with_b0_file ();
  Cmd.eval_value cmd

let () = B0_driver.set ~driver:B0_tool.driver ~main
