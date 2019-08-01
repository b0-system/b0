(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B000

(* FIXME redo in light of B0_ui improvements.
   FIXME we are not using B0_ui.File_cache.* because this
   errors on missing caches. Unclear whether the behaviour
   here should be changed. *)

(* Exit codes and errors *)

let err_no_cache = 1
let err_key_unknown = 2
let err_key_exists = 3
let err_unknown = 123

let handle_unknown_error = function
| Ok i -> i | Error e -> Log.err (fun m -> m "%s" e); err_unknown

(* Commonalities *)

let with_cache dir f =
  let feedback = function
  | `File_cache_need_copy file ->
      (* FIXME move that to B0_log.ui *)
      Log.warn begin fun m ->
          m "@[<v>Using slow copying cache due to:@,%a@,@[%a@]@]"
          Fpath.pp_quoted file Fmt.text
          "The cache may be on a different file system or hard links \
           are not supported, or the maximal number of links was reached."
      end
  in
  Result.bind (Os.Dir.exists dir) @@ function
  | true -> Result.bind (File_cache.create ~feedback dir) f
  | false ->
      Log.err (fun m -> m "%a: Not a directory cache" Fpath.pp_quoted dir);
      Ok err_no_cache

let keys_exist c keys =
  let rec loop c miss = function
  | k :: ks when File_cache.mem c k -> loop c miss ks
  | k :: ks -> loop c (k :: miss) ks
  | [] when miss = [] -> Ok ()
  | [] ->
      Result.bind (File_cache.keys c) @@ fun dom ->
      let add_error acc n =
        Fmt.(str "%a" (did_you_mean text ~kind:"key") (n, String.suggest dom n))
        :: acc
      in
      Error (String.concat "\n" (List.fold_left add_error [] miss))
  in
  loop c [] keys

let with_key_selection c ~only_unused keys f =
  let filter c ~only_unused keys = match only_unused with
  | false -> keys
  | true ->
      let is_unused k =
        Result.value ~default:false @@ File_cache.is_unused c k
      in
      List.filter is_unused keys
  in
  match keys with
  | `All ->
      Result.bind (File_cache.keys c) @@ fun ks -> f (filter c ~only_unused ks)
  | `Keys [] ->
      Log.err (fun m -> m "No key specified");
      Ok Cmdliner.Term.exit_status_cli_error
  | `Keys keys ->
      match keys_exist c keys with
      | Ok () -> f (filter c ~only_unused keys)
      | Error e -> Log.err (fun m -> m  "%s" e); Ok err_key_unknown

(* Commands *)

let files_to_bind ~recurse paths f =
  let rec loop acc = function
  | [] ->
      if acc <> [] then f (List.rev acc) else
      (Log.err (fun m -> m "No file to store");
       (Ok Cmdliner.Term.exit_status_cli_error))
  | p :: ps ->
      match Os.Dir.exists p with
      | Error _ as e -> e
      | Ok false -> loop (p :: acc) ps
      | Ok true ->
          let dotfiles = true in
          Result.bind Os.Dir.(fold_files ~recurse ~dotfiles path_list p []) @@
          fun files ->
          let rev_sort p0 p1 = Fpath.compare p1 p0 in
          let files = List.sort rev_sort files in
          let acc = List.fold_left (fun acc f -> f :: acc) files acc in
          loop acc ps
  in
  loop [] paths

let add_cmd () dir force key recurse paths =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  match File_cache.mem c key && not force with
  | true ->
      Log.err
        (fun m -> m "%s: key already bound in cache, use -f to force" key);
      Ok err_key_exists
  | false ->
      files_to_bind ~recurse paths @@ fun files ->
      Result.bind (File_cache.add c key "" files) @@ function
      | true -> Ok 0
      | false ->
          let miss f = not @@ Result.value ~default:false @@ Os.File.exists f in
          Log.err
            (fun m ->
               m "@[<v>%s: key not bound, can't access these files:@,%a@]"
            key (Fmt.list Fpath.pp_quoted) (List.filter miss files));
          Ok err_unknown

let delete_cmd () dir only_unused keys =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  match only_unused && keys = `All with
  | true -> Result.bind (File_cache.delete_unused c) @@ fun () -> Ok 0
  | false ->
      with_key_selection c ~only_unused keys @@ fun keys ->
      let rec loop = function
      | [] -> Ok 0
      | k :: ks ->
          match File_cache.rem c k with Error _ as e -> e | Ok _ -> loop ks
      in
      loop keys

let files_cmd () dir only_unused keys out_fmt =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  with_key_selection c ~only_unused keys @@ fun keys ->
  let rec add_key_files files = function
  | [] -> Ok files
  | k :: ks ->
      match File_cache.find c k with
      | Error _ as e -> e
      | Ok None -> add_key_files files ks
      | Ok (Some (_, fs)) ->
          let add_file acc f = (k, f) :: acc in
          add_key_files (List.fold_left add_file files fs) ks
  in
  Result.bind (add_key_files [] keys) @@ function
  | [] -> Ok 0
  | files ->
      begin match out_fmt with
      | `Short ->
          let files = List.sort Fpath.compare @@ List.rev_map snd files in
          Fmt.pr "@[<v>%a@]@." Fmt.(list Fpath.pp_quoted) files
      | `Normal | `Long ->
          let files = List.sort compare files in
          let pp_file = Fmt.(hbox @@ pair ~sep:sp string Fpath.pp_quoted) in
          Fmt.pr "@[<v>%a@]@." Fmt.(list pp_file) files
      end;
      Ok 0

let gc_cmd () dir =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  Result.bind (File_cache.Stats.of_cache c) @@ fun s ->
  Result.bind (File_cache.delete_unused c) @@ fun () ->
  Log.app (fun m ->
      m "deleted: %a"
        File_cache.Stats.pp_keys (File_cache.Stats.unused_keys s));
  Ok 0

let pp_key c byte_size = function
| `Short -> Fmt.string
| `Normal | `Long ->
    let pp_byte_size = if byte_size then Fmt.int else Fmt.byte_size in
    fun ppf k -> match File_cache.Stats.of_keys c [k] with
    | Error _ -> Fmt.string ppf k
    | Ok s ->
        let fc = File_cache.Stats.keys_file_count s in
        let bs = File_cache.Stats.keys_byte_size s in
        Fmt.pr "%d %a %s" fc pp_byte_size bs k

let keys_cmd () dir only_unused keys out_fmt byte_size =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  with_key_selection c ~only_unused keys @@ function
  | [] -> Ok 0
  | keys ->
      Fmt.pr "@[<v>%a@]@." (Fmt.list (pp_key c byte_size out_fmt)) keys; Ok 0

let path_cmd () dir = Fmt.pr "%a@." Fpath.pp_quoted dir; 0

let size_cmd () dir only_unused keys =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  match keys with
  | `All ->
      Result.bind (File_cache.Stats.of_cache c) @@ fun s ->
      if only_unused
      then
        Fmt.pr "@[<v>%a@]@."
          File_cache.Stats.pp_keys (File_cache.Stats.unused_keys s)
      else Fmt.pr "@[<v>%a@]@." File_cache.Stats.pp s;
      Ok 0
  | keys ->
      with_key_selection c ~only_unused keys @@ function
      | [] ->
          Fmt.pr "@[<v>%a@]@."
            File_cache.Stats.pp_keys File_cache.Stats.keys_zero;
          Ok 0
      | keys ->
          Result.bind (File_cache.Stats.of_keys c keys) @@ fun s ->
          Fmt.pr "@[<v>%a@]@." File_cache.Stats.pp_keys s; Ok 0

let revive_cmd () dir key paths =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  with_key_selection c ~only_unused:false (`Keys [key]) @@ fun _ ->
  let paths = match paths with
  | `Explicit paths -> Ok paths
  | `Prefix pre ->
      let pre = Fpath.to_string pre in
      Result.bind (File_cache.find c key) @@ function
      | None -> Ok []
      | Some (_, files) ->
          Ok (List.map (fun f -> Fpath.v (pre ^ (Fpath.basename f))) files)
  in
  Result.bind paths @@ fun paths ->
  Result.bind (File_cache.revive c key paths) @@ function
  | Some (_, existed) ->
      let log_exist f =
        Log.err (fun m -> m "%a: Not bound, file exists." Fpath.pp_quoted f)
      in
      List.iter log_exist existed;
      Ok 0
  | None ->
      Result.bind (File_cache.find c key) @@ fun r ->
      let cfiles = match r with
      | None -> []
      | Some (_, files) -> files
      in
      let ccount = List.length cfiles in
      let pcount = List.length paths in
      Log.err (fun m ->
          m "%s: key has %d cached file(s) but %d file path(s) to bind \
            specified" key ccount pcount);
      Ok err_unknown

let trim_cmd () dir (max_byte_size, pct) =
  handle_unknown_error @@
  with_cache dir @@ fun c ->
  let stats c =
    Result.map File_cache.Stats.all_keys (File_cache.Stats.of_cache c)
  in
  Result.bind (stats c) @@ fun stats_before ->
  Result.bind (File_cache.trim_size c ~max_byte_size ~pct) @@ fun () ->
  Result.bind (stats c) @@ fun stats_after ->
  let deleted = File_cache.Stats.keys_sub stats_before stats_after in
  Log.app (fun m -> m "deleted: %a" File_cache.Stats.pp_keys deleted);
  Ok 0

(* Command line interface *)

open Cmdliner

let exits =
  Term.exit_info err_no_cache ~doc:"the cache does not exist." ::
  Term.exit_info err_key_unknown ~doc:"the key does not exist." ::
  Term.exit_info err_unknown ~doc:"unknown error reported on stderr." ::
  Term.default_exits

let cli_conf = B0_ui.B0_std.cli_setup ()

let only_unused =
  let doc = "Keep only unused keys from the selection. $(b,WARNING) Unused key
             determination is only reliable if your file system supports
             hard links and the cache is located on the same file system
             as the files that reference it. If not $(b,all) keys appear
             unused."
  in
  Arg.(value & flag & info ["u"; "unused"] ~doc)

let out_fmt = B0_ui.Cli.out_fmt ()
let key_arg = B0_ui.File_cache.key_arg
let req_key =
  let doc = "The key." in
  Arg.(required & pos 0 (some key_arg) None & info [] ~doc ~docv:"KEY")

let keys_none_is_all = B0_ui.File_cache.keys_none_is_all ()

(* Command clis *)

let fpath = B0_ui.Cli.Arg.fpath
let dir =
  let get_dir cache_dir = match Os.Dir.cwd () with
  | Error e -> `Error (false, e)
  | Ok cwd ->
      let b0_dir = Fpath.(cwd / B0_ui.Memo.b0_dir_name) in
      `Ok (B0_ui.Memo.get_cache_dir ~cwd ~b0_dir ~cache_dir)
  in
  let cache_dir = B0_ui.Memo.cache_dir ~doc_none:"$(b,_b0/.cache)" () in
  Term.(ret (const get_dir $ cache_dir))

let sdocs = Manpage.s_common_options
let add_cmd =
  let doc = "Bind a key to files" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) stores an ordered sequence of files in the cache under
        a key."; ]
  in
  let force =
    let doc = "Proceed even if the key already exists." in
    Arg.(value & flag & info ["f"; "force"] ~doc)
  in
  let paths =
    let doc = "The ordered file(s) to store. If a directory is specified
               its direct regular files (including dot files) are added, sorted
               by binary lexicographic order; use the $(b,--rec) option to
               add the file hierarchies rooted at directory arguments."
    in
    Arg.(non_empty & pos_right 0 fpath [] & info [] ~doc ~docv:"PATH")
  in
  let recurse =
    let doc = "Add file hierarchies rooted at directory $(b,PATH) arguments. \
               Without this option only direct regular files of directories are
               added."
    in
    Arg.(value & flag & info ["r"; "rec"] ~doc)
  in
  Term.(const add_cmd $ cli_conf $ dir $ force $ req_key $ recurse $ paths),
  Term.info "add" ~doc ~sdocs ~exits ~man ~man_xrefs

let delete_cmd =
  let doc = "Delete keys" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description; `P "$(tname) deletes keys."] in
  Term.(const delete_cmd $ cli_conf $ dir $ only_unused $ keys_none_is_all),
  Term.info "delete" ~doc ~sdocs ~exits ~man ~man_xrefs

let files_cmd =
  let doc = "List key files" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) lists key files. By default outputs one file path per line
        preceeded by the key name to which it belongs." ]
  in
  Term.(const files_cmd $ cli_conf $ dir $ only_unused $ keys_none_is_all $
        out_fmt),
  Term.info "files" ~doc ~sdocs ~exits ~man ~man_xrefs

let gc_cmd =
  let doc ="Delete unused keys" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is $(mname) $(b,delete --unused) put also prints on standard
        output information about the deletion.";
    `P "$(b,WARNING) Unused key determination is only reliable if your
        file system supports hard links and the cache is located on the
        same file system as the files that reference it. If not $(b,all)
        keys appear unused and this command deletes all the cache,
        i.e. is equivalent to $(mname) $(b,delete).";]
  in
  Term.(const gc_cmd $ cli_conf $ dir),
  Term.info "gc" ~doc ~sdocs ~exits ~man ~man_xrefs

let keys_cmd =
  let doc = "List keys" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) lists keys. By default outputs one key per line
        preceeded by the number of files bound to the key and its size."; ]
  in
  let byte_size =
    let doc = "Output the size in bytes (rather than human friendly units)." in
    Arg.(value & flag & info ["byte-size"] ~doc)
  in
  Term.(const keys_cmd $ cli_conf $ dir $ only_unused $ keys_none_is_all $
        out_fmt $ byte_size),
  Term.info "keys" ~doc ~sdocs ~exits ~man ~man_xrefs

let path_cmd =
  let doc = "Show path to cache directory" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) prints on standard output the path to the cache directory \
        (which may not exist)."];
  in
  Term.(const path_cmd $ cli_conf $ dir),
  Term.info "path" ~doc ~sdocs ~exits ~man ~man_xrefs

let revive_cmd =
  let doc = "Use a cache key" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) binds file paths to the cached files of a key."];
  in
  let paths =
    let explicit =
      let doc = "The file path to bind. If intermediate directories of $(docv)
                 do not exist they are created. If the file exists it is not
                 bound but kept intact. The number of files specified must match
                 the length of the file list stored by $(b,KEY)."
      in
      Arg.(value & pos_right 0 fpath [] & info [] ~doc ~docv:"PATH")
    in
    let prefix =
      let doc = "Bind cache files to prefix path $(docv). This results
                 in the creation of files of the form $(docv)[z]X, with X the
                 hexadecimal index of the file in the list and 'z' present
                 if this is the last file of the list. If such a file
                 already exists it is left untouched. If intermediate
                 directories of $(docv) do not exist they are created."
      in
      Arg.(value & opt (some fpath) None & info ["p"; "prefix"]
             ~doc ~docv:"PATH")
    in
    let either explicit prefix = match explicit, prefix with
    | [], None -> Error (`Msg ("No file to bind specified"))
    | [], Some p -> Ok (`Prefix p)
    | fs, None -> Ok (`Explicit fs)
    | fs, Some _ ->
        Error (`Msg ("--prefix is incompatible with path positional arguments"))
    in
    Term.(term_result (const either $ explicit $ prefix))
  in
  Term.(const revive_cmd $ cli_conf $ dir $ req_key $ paths),
  Term.info "revive" ~doc ~sdocs ~exits ~man ~man_xrefs

let size_cmd =
  let doc = "Print cache or key size (default command)" in
  let man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) prints on standard output statistics about the size of the
        cache." ]
  in
  Term.(const size_cmd $ cli_conf $ dir $ only_unused $ keys_none_is_all),
  Term.info "size" ~doc ~sdocs ~exits ~man ~man_xrefs

let trim_cmd =
  let doc = "Reduce cache size" and man_xrefs = [ `Main ] in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) trims the cache to a specific size. Without options
        this is equivalent to $(b,--to-% 50).
        If more than one trim option is specified the smallest requested
        size is met.";
    `P "Keys are deleted by order of increasing access time but unused keys,
        if they can be determined, are deleted first." ]
  in
  let trim_to_mb =
    let doc = "Trim the cache to at most $(docv) megabytes." in
    let docv = "MB" in
    Arg.(value & opt (some int) None & info ["m"; "to-mb"] ~doc ~docv)
  in
  let trim_to_pct =
    let doc = "Trim the cache to at most $(docv)% of the current size." in
    let docv = "PCT" in
    Arg.(value & opt (some int) None & info ["p";"to-%";"to-pct"] ~doc ~docv)
  in
  let trim_spec  =
    let combine trim_to_mb trim_to_pct = match trim_to_mb, trim_to_pct with
    | None, None -> max_int, 50
    | None, Some pct -> max_int, pct
    | Some mb, None -> mb * 1000 * 1000, 100
    | Some mb, Some pct -> mb * 1000 * 1000, pct
    in
    Term.(const combine $ trim_to_mb $ trim_to_pct)
  in
  Term.(const trim_cmd $ cli_conf $ dir $ trim_spec),
  Term.info "trim" ~doc ~sdocs ~exits ~man ~man_xrefs

(* Main command *)

let b00_cache =
  let doc = "Low-level operations on b0 caches" and man_xrefs = [`Tool "b0"] in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) is a low-level tool to operates on b0 caches. Not for the
        casual user.";
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ];
  in
  fst size_cmd,
  Term.info "b00-cache" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man
    ~man_xrefs

let () =
  let cmds =
    [ add_cmd; delete_cmd; files_cmd; gc_cmd; keys_cmd; path_cmd;
      revive_cmd; size_cmd; trim_cmd; ]
  in
  Term.(exit_status @@ eval_choice b00_cache cmds)

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
