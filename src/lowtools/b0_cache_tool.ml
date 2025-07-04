(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open B0_zero

(* Exit codes *)

let err_no_cache = 1
let err_no_log_file = 2

(* Commonalities *)

let with_cache_dir cache_dir k =
  let* exists = Os.Dir.exists cache_dir in
  match exists with
  | true -> k cache_dir
  | false ->
      Log.err (fun m ->
          m "@[<v>%a:@,No such cache directory.@]" Fpath.pp cache_dir);
      Ok err_no_cache

let find_used_keys ~err ~cwd ~b0_dir ~log_file k =
  let implicit_file = log_file = None in
  let file = B0_cli.Memo.get_log_file ~cwd ~b0_dir ~log_file in
  let* exists = Os.File.exists file in
  match exists with
  | false when implicit_file && err ->
      Log.err begin fun m ->
        m "@[<v>%a:@,No such log file, specify one explicity.@]"
          Fpath.pp file
      end;
      Ok err_no_log_file
  | false when implicit_file && not err -> k String.Set.empty
  | _ ->
      Log.if_error' ~use:err_no_log_file @@
      Result.map_error (Fmt.str "Cannot determine used keys: %s") @@
      let* l = B0_memo_log.read file in
      k (B0_cli.File_cache.keys_of_success_ops (B0_memo_log.ops l))

let find_dirs ~b0_dir ~cache_dir =
  let* cwd = Os.Dir.cwd () in
  let root = B0_cli.Memo.find_dir_with_b0_dir ~start:cwd in
  let root = Option.value root ~default:cwd in
  let b0_dir = B0_cli.Memo.get_b0_dir ~cwd ~root ~b0_dir in
  let cache_dir = B0_cli.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir in
  Ok (cwd, b0_dir, cache_dir)

(* Commands *)

let delete ~b0_dir ~cache_dir ~keys =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  let* _ = B0_cli.File_cache.delete ~dir keys in
  Ok 0

let gc ~b0_dir ~cache_dir ~log_file =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~err:true ~cwd ~b0_dir ~log_file @@ fun used ->
  let* _ = B0_cli.File_cache.gc ~dir ~used in
  Ok 0

let keys ~b0_dir ~cache_dir =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  let* _ = B0_cli.File_cache.keys ~dir in
  Ok 0

let path ~b0_dir ~cache_dir =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  Log.stdout (fun m -> m "%a" Fpath.pp cache_dir);
  Ok 0

let stats ~b0_dir ~cache_dir ~log_file  =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~err:false ~cwd ~b0_dir ~log_file @@ fun used ->
  let* _ = B0_cli.File_cache.stats ~dir ~used in
  Ok 0

let trim ~b0_dir ~cache_dir ~log_file ~trim_spec:(max_byte_size, pct) =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~err:false ~cwd ~b0_dir ~log_file @@ fun used ->
  let* _ = B0_cli.File_cache.trim ~dir ~used ~max_byte_size ~pct in
  Ok 0

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let man m =
  [ `Blocks m;
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,b0) system. See
        $(i,https:/erratique.ch/software/b0) for contact information."; ]

let exits =
  Cmd.Exit.info err_no_cache ~doc:"no cache directory found." ::
  Cmd.Exit.info err_no_log_file ~doc:"no log file found." ::
  Cmd.Exit.defaults

let configure_log = B0_std_cli.configure_log ()
let b0_dir = B0_cli.Memo.b0_dir ()
let cache_dir = B0_cli.Memo.cache_dir ()
let log_file =
  let doc = "Use $(docv) for the build log file." and docv = "LOG_FILE" in
  let env =
    let doc = "See argument $(docv)." in
    Cmdliner.Cmd.Env.info ~doc B0_cli.Memo.log_file_env
  in
  let absent = "Default $(b,_log) in b0 directory" in
  Arg.(value & pos 0 (some B0_std_cli.fpath) None &
       info [] ~absent ~env ~doc ~docv)

(* Commands *)

let delete =
  let doc = "Delete cache or given keys" in
  let man = man [] in
  Cmd.make (Cmd.info "delete" ~doc ~exits ~man) @@
  let+ configure_log and+ b0_dir and+ cache_dir
  and+ keys = B0_cli.File_cache.keys_none_is_all ~pos_right:(-1) () in
  delete ~b0_dir ~cache_dir ~keys

let gc =
  let doc = "Only keep keys used by the build described by $(i,LOG_FILE)" in
  let man = man [] in
  Cmd.make (Cmd.info "gc" ~doc ~exits ~man) @@
  let+ configure_log and+ b0_dir and+ cache_dir and+ log_file in
  gc ~b0_dir ~cache_dir ~log_file

let keys =
  let doc = "List cache keys" in
  let man = man [] in
  Cmd.make (Cmd.info "keys" ~doc ~exits ~man) @@
  let+ configure_log and+ b0_dir and+ cache_dir in
  keys ~b0_dir ~cache_dir

let path =
  let doc = "Output cache directory path (may not exist)" in
  let man = man [] in
  Cmd.make (Cmd.info "path" ~doc ~exits ~man) @@
  let+ configure_log and+ b0_dir and+ cache_dir in
  path ~b0_dir ~cache_dir

let stats, stats_term =
  let doc = "Output cache statistics (default command)" in
  let man = man
      [ `S Manpage.s_description;
        `P "$(cmd) outputs cache statistics. The $(i,LOG_FILE) is used \
            to determine used keys." ]
  in
  let term =
    let+ configure_log and+ b0_dir and+ cache_dir and+ log_file in
    stats ~b0_dir ~cache_dir ~log_file
  in
  Cmd.make (Cmd.info "stats" ~doc ~exits ~man) term, term

let trim =
  let doc = "Trim the cache to the minimal budget specified" in
  let man = man
      [ `S Manpage.s_description;
        `P "$(cmd) trims the cache to the minimal given budget. \
            Without options trims to 50% of the current size. \
            Keys used by the build described by $(i,LOG_FILE) are \
            preserved if possible."; ]
  in
  Cmd.make (Cmd.info "trim" ~doc ~exits ~man) @@
  let+ configure_log and+ b0_dir and+ cache_dir and+ log_file
  and+ trim_spec = B0_cli.File_cache.trim_cli () in
  trim ~b0_dir ~cache_dir ~log_file ~trim_spec

let tool =
  let doc = "Operate on b0 caches" in
  let man_xrefs = [`Tool "b0"; `Tool "b0-log"; `Tool "b0-hash"] in
  let man = man [
    `S Manpage.s_description;
    `P "The $(tool) tool operate on b0 caches. The default command \
        is $(tool) $(b,stats)."; ]
  in
  let version = "%%VERSION%%" in
  let info = Cmd.info "b0-cache" ~version ~doc ~exits ~man ~man_xrefs in
  Cmd.group ~default:stats_term info @@
  [delete; gc; keys; path; stats; trim ]

let main () = Cmd.eval_result' tool
let () = if !Sys.interactive then () else exit (main ())
