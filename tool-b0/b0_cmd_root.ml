(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_std.Result.Syntax

let lock c =
  let warn () = Log.warn @@ fun m ->
      m "@[<v>Some variables unchanged. You may need to first issue:@,%a@]"
        Fmt.(code string) "eval $(b0 root unlock)"
  in
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  let b0_dir = B0_driver.Conf.b0_dir c in
  let bindings = [
    B0_driver.Env.b0_file, b0_file;
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
  Ok B00_cli.Exit.ok

let unlock c =
  let vars = [ B0_driver.Env.b0_file; B0_driver.Env.b0_dir ] in
  let pp_unset ppf var = Fmt.pf ppf "unset %s;" var in
  Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_unset) vars);
  B00_cli.Exit.ok

let path c =
  Log.if_error ~use:B0_driver.Exit.no_b0_file @@
  let* b0_file = B0_driver.Conf.get_b0_file c in
  let root = Fpath.parent b0_file in
  Log.app (fun m -> m "%a" Fpath.pp root);
  Ok B00_cli.Exit.ok

(* Command line interface *)

open Cmdliner

let path, path_term =
  let doc = "Show the root directory (default command)" in
  let descr = `P "$(tname) outputs the b0 root directory." in
  let path_term = Term.(const path) in
  B0_b0.Cli.subcmd_with_driver_conf "path" ~doc ~descr path_term, path_term

let lock =
  let doc = "Lock the root and b0 directory" in
  let descr = `Blocks
    [ `P "$(tname) outputs environment variable bindings to lock $(mname) \
          invocations on the currently inferred b0 file and directory. \
          The intended usage is:";
      `Pre "$(b,eval \\$(b0 root lock\\))"; `Noblank;
      `Pre "$(b,cd /where/you/want)"; `Noblank;
      `Pre "$(b,b0) …"; `Noblank;
      `Pre "…"; `Noblank;
      `Pre "$(b,eval \\$(b0 root unlock\\))"; ]
  in
  B0_b0.Cli.subcmd_with_driver_conf "lock" ~doc ~descr Term.(const lock)

let unlock =
  let doc = "Unlock the root and b0 directory" in
  let descr = `Blocks
      [ `P "$(tname) outputs instructions to clear the environment bindings \
            performed by $(b,lock). The indented usage is:";
        `Pre "$(b,eval \\$(b0 root unlock\\))"; ]
  in
  B0_b0.Cli.subcmd_with_driver_conf "unlock" ~doc ~descr Term.(const unlock)

let subs = [lock; path; unlock]

let cmd =
  let doc = "Show and lock the root directory" in
  let descr =
    `P "$(tname) operates on the root directory. The default \
        command is $(b,path).";
  in
  let default = path_term in
  B0_b0.Cli.cmd_group_with_driver_conf "root" ~doc ~descr ~default subs


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
