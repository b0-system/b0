(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let lock c =
  let warn () = Log.warn @@ fun m ->
    m "@[<v>Some variables unchanged. You may need to first issue:@,%a@]"
      Fmt.code' "eval $(b0 root unlock)"
  in
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let bindings =
    [ B0_driver.Env.b0_file, b0_file;
      B0_driver.Env.b0_dir, b0_dir]
  in
  let env_b0_file = Os.Env.find ~empty_is_none:false B0_driver.Env.b0_file in
  let env_b0_dir = Os.Env.find ~empty_is_none:false B0_driver.Env.b0_dir in
  let () = match env_b0_file, env_b0_dir with
  | Some f, _ when f = Fpath.to_string b0_file -> warn ()
  | _, Some d when d = Fpath.to_string b0_dir -> warn ()
  | _, _ -> ()
  in
  let pp_binding ppf (var, path) =
    Fmt.pf ppf "@[<h>%s=%a; export %s;@]" var Fpath.pp_quoted path var
  in
  Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_binding) bindings);
  Ok B0_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let cmd =
  let doc = "Lock the root and b0 directory" in
  let descr = `Blocks
    [ `P "$(iname) outputs environment variable bindings to lock $(mname) \
          invocations on the currently inferred b0 file and directory. \
          The intended usage is:";
      `Pre "$(b,eval \\$(b0 lock\\))"; `Noblank;
      `Pre "$(b,cd /where/you/want)"; `Noblank;
      `Pre "$(b,b0) …"; `Noblank;
      `Pre "…"; `Noblank;
      `Pre "$(b,eval \\$(b0 unlock\\))"; ]
  in
  B0_tool_std.Cli.subcmd_with_driver_conf "lock" ~doc ~descr @@
  Term.(const lock)
