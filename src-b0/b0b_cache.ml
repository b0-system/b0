(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0
open B0_driver

(* Selecting elements *)

let compare_elt e0 e1 =
  Fpath.compare (Cache.elt_file_path e0) (Cache.elt_file_path e1)

let select cache selection =
  let elts = Cache.fold List.cons cache [] in
  let selection = match selection with
  | `All -> elts
  | `Selectors sels ->
      let select e =
        let try_select e = function
        | `Age a -> a = Cache.elt_age e
        | `Key k -> k = Cache.elt_key e
        | `Prefix p ->
            String.is_prefix
              ~affix:(Fpath.to_string p)
              (Fpath.to_string (Cache.elt_file_path e))
        | `File f -> Fpath.equal f (Cache.elt_file_path e)
        in
        List.exists (try_select e) sels
      in
      List.filter select elts
  in
  List.sort compare_elt selection

(* Actions *)

let key_style = [`Fg `Yellow]
let age_style = [`Fg `Green]
let key_str k = Cache.key_to_string k

let exit_no_cache = 1
let exit_no_elt = 2
let exit_verify_err = 3

let err_no_elts () =
  Log.err (fun m -> m "no cache element found for selection"); Ok exit_no_elt

let list = function
| [] -> err_no_elts ()
| elts ->
    let age elt = Printf.sprintf "%-4d" @@ Cache.elt_age elt
    in
    let key elt = key_str (Cache.elt_key elt) in
    let file elt = Fpath.to_string @@ Cache.elt_file_path elt in
    let pr_elt elt =
      Printf.printf "%s %s %s\n" (age elt) (key elt) (file elt)
    in
    List.iter pr_elt elts;
    Printf.printf "%!";
    Ok 0

let show = function
| [] -> err_no_elts ()
| elts ->
    let fields =
      [ "file", (fun e -> strf "%s" (Fpath.to_string @@ Cache.elt_file_path e));
        "file-stamp", (fun e -> strf "%s"
                          (Stamp.to_hex @@ Cache.elt_file_stamp e));
        "age", (fun e -> strf "%d" (Cache.elt_age e));
        "op", (fun e -> strf "%s" (Cmd.to_string @@ Cache.elt_op e)) ]
    in
    let show_field e (l, fmt) =
      strf " %s: %s" l (fmt e)
    in
    let show_elt e =
      let fields = List.map (show_field e) fields in
      let key = key_str (Cache.elt_key e) in
      Printf.printf "%s\n%s\n\n" key (String.concat "\n" fields)
    in
    List.iter show_elt elts;
    Printf.printf "%!";
    Ok 0

let delete cache = function
| [] -> err_no_elts ()
| elts ->
    let delete_elt elt =
      Cache.rem cache (Cache.elt_key elt) |> Log.on_error_msg ~use:(fun _ -> ())
    in
    List.iter delete_elt elts;
    Cache.save cache >>= fun () -> Ok 0

let verify ~repair cache = function
| [] -> err_no_elts ()
| elts ->
    let verify_elt problem elt =
      let k = Cache.elt_key elt in
      match Cache.verify ~repair cache k with
      | Ok `Ok -> problem
      | Ok `Miss_file ->
          let drop = if repair then ", dropped index entry" else "" in
          let key = key_str k in
          Log.err (fun m -> m "%s: missing cache file%s" key drop);
          true
      | Ok `Stamp_mismatch ->
          let upd = if repair then ", updated index entry" else "" in
          let key = key_str k in
          Log.err (fun m -> m "%s: file stamp mismatch%s" key upd);
          true
      | Error _ as v -> Log.on_error_msg v ~use:(fun _ -> true)
      | Ok `Miss_index -> assert false
      | Ok `Unknown -> assert false
    in
    let problem = List.fold_left verify_elt false elts in
    (match repair with false -> Ok () | true -> Cache.save cache) >>= fun () ->
    Ok (if problem then 3 else 0)

let foreign ~ignore_keys cache =
  let pr_foreign (kind, p) =
    let path = match kind with
    | `Key -> Fpath.to_string p
    | `Other -> Fpath.to_string p
    in
    Printf.printf "%s\n" path
  in
  Cache.foreign ~ignore_keys cache >>= fun foreign ->
  List.iter pr_foreign foreign;
  Ok 0

(* Command *)

let cache
    cache_dir cache_index action selection ignore_keys repair setup =
  begin
    let build_dir = B0_dir.dir (Driver.b0_dir setup) in
    let index_file = match cache_index with
    | None ->
        failwith "TODO" (* rely on variants *)
(*        Fpath.(build_dir // Cli.default_cache_index) *)
    | Some file -> file
    in
    let dir = match cache_dir with
    | None -> Fpath.(build_dir // Cli.default_cache_dir)
    | Some dir -> dir
    in
    Cache.exists ~index_file >>= function
    | false ->
        Log.err (fun m -> m "No cache found in %a" Fpath.pp index_file);
        Ok exit_no_cache
    | true ->
        Cache.load ~index_file ~dir >>= fun cache ->
        match action with
        | `List -> list (select cache selection)
        | `Show -> show (select cache selection)
        | `Verify -> verify ~repair cache (select cache selection)
        | `Delete -> delete cache (select cache selection)
        | `Foreign -> foreign ~ignore_keys cache
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let s_selectors = "SELECTORS"
let selection =
  let docs = s_selectors in
  let path_arg = Cli.path_arg in
  let all =
    let doc = "Selects all cache elements. This is the default if no
               selector is specified on the command line."
    in
    Arg.(value & flag & info ["all"] ~docs ~doc)
  in
  let ages =
    let doc = "Select cache element created at age $(docv)." in
    Arg.(value & opt_all int [] & info ["age"] ~docs ~doc ~docv:"AGE")
  in
  let keys =
    let key_arg = Arg.conv Cache.(key_of_string, pp_key) in
    let doc = "Select cache element with cache key $(docv)." in
    Arg.(value & opt_all key_arg [] & info ["k";"key"] ~docs ~doc ~docv:"KEY")
  in
  let prefixes =
    let doc = "Select cache elements with file path prefixed by $(docv)." in
    Arg.(value & opt_all path_arg [] &
         info ["p";"prefix"] ~docs ~doc ~docv:"PREFIX")
  in
  let files =
    let doc = "Select cache elements with file path equal to $(docv)." in
    let docv = "FILE" in
    Arg.(value & opt_all path_arg [] & info ["f";"file"] ~docs ~doc ~docv)
  in
  let selection all ages keys prefixes files =
    let ages = List.rev_map (fun k -> `Age k) ages in
    let keys = List.rev_map (fun k -> `Key k) keys in
    let prefixes = List.rev_map (fun p -> `Prefix p) prefixes in
    let files = List.rev_map (fun p -> `File p) files in
    match List.concat [ages; keys; prefixes; files] with
    | [] -> `All
    | sels when all -> `All
    | sels -> `Selectors sels
  in
  Term.(const selection $ all $ ages $ keys $ prefixes $files)

let ignore_keys =
  let doc = "For the $(b,unknown) action, do not list files that are unknown
             to the cache but whose filename is a cache key."
  in
  Arg.(value & flag & info ["ignore-keys"] ~doc)

let repair =
  let doc = "For the $(b,verify) action, repair file stamp mismatches." in
  Arg.(value & flag & info ["repair"] ~doc)

let cache_action =
  B0b_cli.action_arg
    [ ("list", `List); ("show", `Show); ("delete", `Delete);
      ("verify", `Verify); ("foreign", `Foreign) ]

let doc = "Operate on B0 build caches"
let sdocs = Manpage.s_common_options
let exits =
  Term.exit_info exit_no_cache ~doc:"missing cache" ::
  Term.exit_info exit_no_elt ~doc:"no cache element was specified" ::
  Term.exit_info exit_verify_err ~doc:"cache verification error" ::
  Cli.driver_default_exits

let man_xrefs = [ `Main; `Cmd "cache" ]
let man =
  [ B0b_cli.synopsis_cmd_with_action;
    `S Manpage.s_description;
    `P "The $(tname) command operates on b0 build caches.";
    `P "A b0 build cache is a directory of cached files and an index file
        holding metadata about them. These entities are most of the time
        found in $(b,_b0/_cache) and $(b,_b0/_cache/index)
        but can be located elsewhere, see the $(b,--cache-dir) and
        $(b,--cache-index) options.";
    `S Manpage.s_arguments;
    `S "ACTIONS";
    `P "Actions that operate on cache elements apply to all elements if
        no selector is specified. See section SELECTORS.";
    `I ("$(b,list)", "list cache elements. Formatted as age, cache key,
        file path.");
    `I ("$(b,show)", "show all information about cache elements.");
    `I ("$(b,delete)", "delete cache elements.");
    `I ("$(b,verify)", "verify that stamps of cached files match those
         recorded in the cache index.");
    `I ("$(b,foreign)", "list files from the cache directory that are
         unknown to the cache index.");
    `S s_selectors;
    `P "Selectors are options that select elements of the cache.";
    B0_driver.Cli.common_man; ]

let cmd =
  Term.(pure cache $ Cli.cache_dir $ Cli.cache_index $
        cache_action $ selection $ ignore_keys $ repair),
  Term.info "cache" ~doc ~sdocs ~exits ~man ~man_xrefs,
  `Driver

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
