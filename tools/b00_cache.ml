(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B000

(* Exit codes and errors *)

let err_no_cache = 1
let err_no_log_file = 2
let err_unknown = 123

let with_cache_dir cache_dir k = match Os.Dir.exists cache_dir with
| Ok true -> k cache_dir
| Ok false ->
    Log.err begin fun m ->
      m "@[<v>%a:@,No such cache directory.@]" Fpath.pp_unquoted cache_dir
    end;
    Ok err_no_cache
| Error _ as e -> e

let find_used_keys ~err ~cwd ~b0_dir ~log_file k =
  let implicit_file = log_file = None in
  let file = B00_ui.Memo.get_log_file ~cwd ~b0_dir ~log_file in
  Result.bind (Os.File.exists file) @@ function
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
      Result.bind (B00_ui.Memo.Log.read file) @@ fun l ->
      k (B00_ui.File_cache.keys_of_done_ops (B00_ui.Memo.Log.ops l))

let cache_cmd
    tty_cap log_level no_pager b0_dir cache_dir
    (max_byte_size, pct) (action, log_file, args)
  =
  let tty_cap = B0_std_ui.get_tty_cap tty_cap in
  let log_level = B0_std_ui.get_log_level log_level in
  B0_std_ui.setup tty_cap log_level ~log_spawns:Log.Debug;
  Log.if_error ~use:err_unknown @@
  Result.bind (Os.Dir.cwd ()) @@ fun cwd ->
  let root = B00_ui.Memo.find_dir_with_b0_dir ~start:cwd in
  let root = Option.value root ~default:cwd in
  let b0_dir = B00_ui.Memo.get_b0_dir ~cwd ~root ~b0_dir in
  let cache_dir = B00_ui.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir in
  let action = match action with
  | `Delete ->
      with_cache_dir cache_dir @@ fun dir ->
      let args = match args with [] -> `All | keys -> `Keys keys in
      Result.bind (B00_ui.File_cache.delete ~dir args) @@ fun _ -> Ok 0
  | `Gc ->
      with_cache_dir cache_dir @@ fun dir ->
      find_used_keys ~err:true ~cwd ~b0_dir ~log_file @@ fun used ->
      Result.bind (B00_ui.File_cache.gc ~dir ~used) @@ fun _ -> Ok 0
  | `Keys ->
      with_cache_dir cache_dir @@ fun dir ->
      Result.bind (B00_ui.File_cache.keys ~dir) @@ fun _ -> Ok 0
  | `Path ->
      Log.app (fun m -> m "%a" Fpath.pp_unquoted cache_dir);
      Ok 0
  | `Stats ->
      with_cache_dir cache_dir @@ fun dir ->
      find_used_keys ~err:false ~cwd ~b0_dir ~log_file @@ fun used ->
      Result.bind (B00_ui.File_cache.stats ~dir ~used) @@ fun _ -> Ok 0
  | `Trim ->
      with_cache_dir cache_dir @@ fun dir ->
      find_used_keys ~err:false ~cwd ~b0_dir ~log_file @@ fun used ->
      Result.bind (B00_ui.File_cache.trim ~dir ~used ~max_byte_size ~pct)
      @@ fun _ -> Ok 0
  in
  Log.if_error' ~use:err_unknown @@ action

(* Command line interface *)

open Cmdliner

let version = "%%VERSION%%"
let doc = "Operate on b0 caches"
let sdocs = Manpage.s_common_options
let exits =
  Term.exit_info err_no_cache ~doc:"no cache directory found." ::
  Term.exit_info err_no_log_file ~doc:"no log file found." ::
  Term.exit_info err_unknown ~doc:"unknown error reported on stderr." ::
  Term.default_exits

let man_xrefs = [`Tool "b0"; `Tool "b00-log"; `Tool "b00-hash"]
let man = [
  `S Manpage.s_description;
  `P "The $(tname) tool operate on b0 caches.";
  `S "ACTIONS";
  `P "Below the $(i,LOG_FILE) argument can also be specified with \
      the $(b,--log-file) option and the $(b,B0_LOG_FILE) environment \
      variable.";
  `I ("$(b,delete) [$(i,KEY)]...",
      "Delete the cache or only the given keys.");
  `I ("$(b,gc) [$(i,LOG_FILE)]",
      "Only keep keys used by the build described by $(i,LOG_FILE). \
       Errors if no log file can be found.");
  `I ("$(b,keys)", "List cache keys.");
  `I ("$(b,path)", "Display the path to the cache (may not exist).");
  `I ("$(b,stats) [$(i,LOG_FILE)]",
      "Show cache statistics. $(i,LOG_FILE) is used to determine used keys.");
  `I ("$(b,trim) [$(b,--to-pct) $(i,PCT)] [$(b,--to-mb) $(i,MB)] \
       [$(i,LOG_FILE)]",
      "Trim the cache to the minimal budget specified. Without options \
       trim to 50% of the current size. Keys used by the build described \
       by $(i,LOG_FILE) are preserved if possible.");
  `S Manpage.s_bugs;
  `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]

let action =
  let action =
    [ "delete", `Delete; "gc", `Gc; "keys", `Keys; "path", `Path;
      "stats", `Stats; "trim", `Trim ]
  in
  let doc =
    Fmt.str "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let parse_cli =
  let args = Arg.(value & pos_right 0 string [] & info []) in
  let log_file_opt = B00_ui.Memo.log_file ~docs:sdocs () in
  let parse action log_file_opt args = match action with
  | `Gc | `Stats | `Trim ->
      begin match args with
      | [v] ->
          begin match log_file_opt with
          | None ->
              begin match Fpath.of_string v with
              | Error e -> `Error (false, e)
              | Ok log_file -> `Ok (action, Some log_file, [])
              end
          | Some _ ->
              let e =
                "--log-file option and positional argument cannot be used \
                 together."
              in
              `Error (false, e)
          end
      | [] -> `Ok (action, log_file_opt, args)
      | _ -> `Error (true, "too many positional arguments for this action.")
      end
  | `Keys | `Delete | `Path -> `Ok (action, log_file_opt, args)
  in
  Term.(ret (pure parse $ action $ log_file_opt $ args))

let tool =
  Term.(const cache_cmd $ B0_std_ui.tty_cap ~docs:sdocs () $
        B0_std_ui.log_level ~docs:sdocs () $ B0_pager.don't ~docs:sdocs () $
        B00_ui.Memo.b0_dir ~docs:sdocs () $
        B00_ui.Memo.cache_dir ~docs:sdocs () $
        B00_ui.File_cache.trim_cli () $
        parse_cli),
  Term.info "b00-cache" ~version ~doc ~sdocs ~exits ~man ~man_xrefs

let main () = Term.(exit_status @@ eval tool)
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
