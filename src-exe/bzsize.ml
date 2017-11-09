(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

(* Exit codes and errors *)

let err_no_dir = 1
let err_cache = 2
let err_unknown = 3

let exe = Filename.basename Sys.executable_name
let log_err fmt =  Format.eprintf ("%s: " ^^ fmt ^^ "@.") exe

let handle_unknown_error = function
| Ok i -> i | Error (`Msg m) -> log_err "%s" m; err_unknown

(* Checking the cache dir *)

let is_cache_dir c force = match force with
| true -> Ok true
| false ->
    Cache.suspicious_files c >>= function
    | [] -> Ok true
    | fs ->
        log_err "@[<v1>%a: Really a cache directory ? \
                 These files look suspicious:@,%a@]"
          Fpath.pp (Cache.dir c) (Fmt.list Fpath.pp) fs;
        Ok false

let output_stats c =
  Cache.dir_stats c >>= fun stats ->
  Format.printf "@[<v>%a@,%a@]@."
    (Fmt.field "dir" Fpath.pp) (Cache.dir c) Cache.Dir_stats.pp stats;
  Ok 0

let delete c trim_50 trim_to_pct trim_to_mb delete_unused =
  let delete_unused = match delete_unused with
  | true -> Cache.delete_unused_files c
  | fase -> Ok ()
  in
  let pct = match trim_50, trim_to_pct with
  | true, None -> 50
  | true, Some pct -> min 50 pct
  | false, Some pct -> pct
  | false, None -> 100
  in
  delete_unused >>= fun () -> match pct, trim_to_mb with
  | 100, None -> Ok ()
  | pct, None -> Cache.delete_files c ~pct ~dir_byte_size:None
  | pct, Some s ->
      Cache.delete_files c ~pct ~dir_byte_size:(Some (s * 1000 * 1000))

let bzsize dir force trim_50 trim_to_pct trim_to_mb delete_unused =
  handle_unknown_error begin
    OS.Dir.exists dir >>= function
    | false ->
        log_err "%a: No such directory." Fpath.pp dir;
        Ok err_no_dir
    | true ->
        Cache.create ~dir
        >>= fun c -> is_cache_dir c force
        >>= function
        | false -> Ok err_cache
        | true ->
            match trim_50, trim_to_pct, trim_to_mb, delete_unused with
            | false, None, None, false -> output_stats c
            | _ ->
                delete c trim_50 trim_to_pct trim_to_mb delete_unused
                >>= fun () -> Ok 0
  end

(* Command line interface *)

open Cmdliner

(* Main command *)

let exits =
  Term.exit_info err_no_dir ~doc:"the directory doesn't exist" ::
  Term.exit_info err_cache ~doc:"the directory doesn't seem to be a b0 cache" ::
  Term.exit_info err_unknown ~doc:"unknown error reported on stderr" ::
  Term.default_exits

let dir =
  let doc = "The b0 cache directory to operate on." in
  let env = Arg.env_var "B0_CACHE_DIR" ~doc:"See argument $(docv)" in
  let default = Fpath.v "_b0/cache" in
  let path_arg = Cmdliner.Arg.conv Fpath.(of_string, pp) in
  Arg.(value & pos 0 path_arg default & info [] ~env ~doc ~docv:"DIR")

let force =
  let doc =
    "$(b,WARNING) using this option may result in data loss. Force
     tool operation even though $(b,DIR) doesn't seem to be a b0 cache
     directory."
  in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let trim_50 =
  let doc = "Trim the cache to 50% of the current size." in
  Arg.(value & flag & info ["t"; "trim"] ~doc)

let trim_to_pct =
  let doc = "Trim the cache to $(docv) percent of the current size." in
  let docv = "PCT" in
  Arg.(value & opt (some int) None & info ["p"; "trim-to-pct"] ~doc ~docv)

let trim_to_mb =
  let doc = "Trim the cache to at most $(docv) megabytes (MB)." in
  let docv = "MB" in
  Arg.(value & opt (some int) None & info ["m"; "trim-to-mb"] ~doc ~docv)

let delete_unused =
  let doc = "Delete all the files that are unused in the cache. These
             are the files with a link count of 1."
  in
  Arg.(value & flag & info ["u"; "delete-unused"] ~doc)

let bzsize =
  let doc = "Control the size of b0 cache directories" in
  let man_xrefs = [`Tool "b0"] in
  let man = [
    `S Manpage.s_description;
    `P "The default behaviour of $(mname) is to simply output statistics
        about the cache directory $(b,DIR) on stdout.";
    `P "The size of $(b,DIR) can be brought down by using one of the
        trimming option (see below). When more than one of these
        options is specified it strives to achieve the smallest
        requested size.";
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ];
  in
  Term.(const bzsize $ dir $ force $ trim_50 $ trim_to_pct $ trim_to_mb $
        delete_unused),
  Term.info "bzsize" ~version:"%%VERSION%%" ~doc ~exits ~man ~man_xrefs

let () = Term.(exit_status @@ eval bzsize)

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
