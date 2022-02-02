(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax
open B000

(* Exit codes and errors *)

let err_no_cache = 1
let err_no_log_file = 2
let err_unknown = Cmdliner.Cmd.Exit.some_error

let with_cache_dir cache_dir k = match Os.Dir.exists cache_dir with
| Error _ as e -> e
| Ok true -> k cache_dir
| Ok false ->
    Log.err begin fun m ->
      m "@[<v>%a:@,No such cache directory.@]" Fpath.pp_unquoted cache_dir
    end;
    Ok err_no_cache

let find_used_keys ~err ~cwd ~b0_dir ~log_file k =
  let implicit_file = log_file = None in
  let file = B00_cli.Memo.get_log_file ~cwd ~b0_dir ~log_file in
  let* exists = Os.File.exists file in
  match exists with
  | false when implicit_file && err ->
      Log.err begin fun m ->
        m "@[<v>%a:@,No such log file, specify one explicity.@]"
          Fpath.pp_unquoted file
      end;
      Ok err_no_log_file
  | false when implicit_file && not err -> k String.Set.empty
  | _ ->
      Log.if_error' ~use:err_no_log_file @@
      Result.map_error (Fmt.str "Cannot determine used keys: %s") @@
      let* l = B00_cli.Memo.Log.read file in
      k (B00_cli.File_cache.keys_of_done_ops (B00_cli.Memo.Log.ops l))

let find_dirs ~b0_dir ~cache_dir =
  let* cwd = Os.Dir.cwd () in
  let root = B00_cli.Memo.find_dir_with_b0_dir ~start:cwd in
  let root = Option.value root ~default:cwd in
  let b0_dir = B00_cli.Memo.get_b0_dir ~cwd ~root ~b0_dir in
  let cache_dir = B00_cli.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir in
  Ok (cwd, b0_dir, cache_dir)

let delete setup b0_dir cache_dir keys =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  let keys = match keys with [] -> `All | keys -> `Keys keys in
  let* _ = B00_cli.File_cache.delete ~dir keys in
  Ok 0

let gc setup b0_dir cache_dir log_file =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~err:true ~cwd ~b0_dir ~log_file @@ fun used ->
  let* _ = B00_cli.File_cache.gc ~dir ~used in
  Ok 0

let keys setup b0_dir cache_dir =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  let* _ = B00_cli.File_cache.keys ~dir in
  Ok 0

let path setup b0_dir cache_dir =
  let* (_cwd, _b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  Log.app (fun m -> m "%a" Fpath.pp_unquoted cache_dir);
  Ok 0

let stats setup b0_dir cache_dir log_file  =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~err:false ~cwd ~b0_dir ~log_file @@ fun used ->
  let* _ = B00_cli.File_cache.stats ~dir ~used in
  Ok 0

let trim setup b0_dir cache_dir log_file (max_byte_size, pct) =
  let* (cwd, b0_dir, cache_dir) = find_dirs ~b0_dir ~cache_dir in
  with_cache_dir cache_dir @@ fun dir ->
  find_used_keys ~err:false ~cwd ~b0_dir ~log_file @@ fun used ->
  let* _ = B00_cli.File_cache.trim ~dir ~used ~max_byte_size ~pct in
  Ok 0

(* Command line interface *)

open Cmdliner

let sdocs = Manpage.s_common_options

let setup =
  let setup tty_cap log_level =
    let tty_cap = B00_cli.B00_std.get_tty_cap tty_cap in
    let log_level = B00_cli.B00_std.get_log_level log_level in
    B00_cli.B00_std.setup tty_cap log_level ~log_spawns:Log.Debug
  in
  Term.(const setup $
        B00_cli.B00_std.tty_cap ~docs:sdocs () $
        B00_cli.B00_std.log_level ~docs:sdocs ())

let b0_dir = B00_cli.Memo.b0_dir ~docs:sdocs ()
let cache_dir = B00_cli.Memo.cache_dir ~docs:sdocs ()
let log_file =
  let doc = "Use $(docv) for the build log file." in
  let docv = "LOG_FILE" in
  let env =
    let doc = "See argument $(docv)." in
    Cmdliner.Cmd.Env.info ~doc B00_cli.Memo.log_file_env
  in
  let absent = "Default $(b,_log) in b0 directory" in
  Arg.(value & pos 0 (some B00_cli.fpath) None &
       info [] ~absent ~env ~doc ~docv)

let exits =
  Cmd.Exit.info err_no_cache ~doc:"no cache directory found." ::
  Cmd.Exit.info err_no_log_file ~doc:"no log file found." ::
  Cmd.Exit.defaults

let info ?(man = []) name ~doc =
  let sdocs = Manpage.s_common_options in
  let man =
    [ `Blocks man;
      `S Manpage.s_bugs;
      `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]
  in
  Cmd.info name ~doc ~sdocs ~exits ~man

(* Commands *)

let delete =
  let doc = "Delete cache or given keys" in
  let keys =
    let doc = "The $(docv) to delete. All of them if unspecified." in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"KEY")
  in
  Cmd.v (info "delete" ~doc)
    Term.(const delete $ setup $ b0_dir $ cache_dir $ keys)

let gc =
  let doc = "Only keep keys used by the build described by $(i,LOG_FILE)" in
  Cmd.v (info "gc" ~doc)
    Term.(const gc $ setup $ b0_dir $ cache_dir $ log_file)

let keys =
  let doc = "List cache keys" in
  Cmd.v (info "keys" ~doc) Term.(const keys $ setup $ b0_dir $ cache_dir)

let path =
  let doc = "Output cache directory path (may not exist)" in
  Cmd.v (info "path" ~doc) Term.(const path $ setup $ b0_dir $ cache_dir)

let stats_term = Term.(const stats $ setup $ b0_dir $ cache_dir $ log_file)
let stats =
  let doc = "Output cache statistics (default command)." in
  let man = [`S Manpage.s_description;
             `P "$(tname) outputs cache statistics. The $(i,LOG_FILE) is used \
                 to determine used keys." ]
  in
  Cmd.v (info "stats" ~doc ~man) stats_term

let trim =
  let doc = "Trim the cache to the minimal budget specified." in
  let man = [ `S Manpage.s_description;
              `P "$(tname) trims the cache to the minimal given budget. \
                  Without options trims to 50% of the current size. \
                  Keys used by the build described by $(i,LOG_FILE) are \
                  preserved if possible."; ]
  in
  let trim_args = B00_cli.File_cache.trim_cli () in
  Cmd.v (info "trim" ~doc ~man)
    Term.(const trim $ setup $ b0_dir $ cache_dir $ log_file $ trim_args)

let cmds = [delete; gc; keys; path; stats; trim ]

let tool =
  let version = "%%VERSION%%" in
  let doc = "Operate on b0 caches" in
  let man_xrefs = [`Tool "b0"; `Tool "b00-log"; `Tool "b00-hash"] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) tool operate on b0 caches. The default command \
        is $(tname) $(b,stats).";
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]
  in
  let info = Cmd.info "b00-cache" ~version ~doc ~sdocs ~exits ~man ~man_xrefs in
  Cmd.group ~default:stats_term info cmds

let main () = exit (Cmd.eval_result' tool)
let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
