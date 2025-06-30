(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let pp_hash = function
| `Short -> Fmt.using snd Hash.pp
| `Normal | `Long ->
    fun ppf (f, h) -> Hash.pp ppf h; Fmt.char ppf ' '; Fpath.pp_unquoted ppf f

let hash_file (module Hash : Hash.T) file  =
  let* hash =
    if Fpath.is_dash file
    then Result.map Hash.string (Os.File.read file)
    else Hash.file file
  in
  Ok (file, hash)

let hash ~output_format ~hash_fun ~files =
  let pp_hash = pp_hash output_format in
  let hash_fun = B0_cli.Memo.get_hash_fun ~hash_fun in
  let do_hash file =
    let* h = hash_file hash_fun file in
    Ok (Log.stdout (fun m -> m "%a" pp_hash h))
  in
  List.iter_stop_on_error do_hash files

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let tool =
  let doc = "Hash like b0" in
  let man_xrefs = [`Tool "b0"; `Tool "b0-cache"; `Tool "b0-log"] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command hashes files like b0 does.";
    `S Manpage.s_options;
    `S B0_std_cli.s_output_format_options;
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,b0) system. See
        $(i,https:/erratique.ch/software/b0) for contact information.";]
  in
  Cmd.make (Cmd.info "b0-hash" ~version:"%%VERSION%%" ~doc ~man ~man_xrefs) @@
  let+ () = B0_std_cli.log_setup ()
  and+ output_format = B0_std_cli.output_format ()
  and+ hash_fun =
    B0_cli.Memo.hash_fun ~opts:["H"; "hash-fun"] ~docs:Manpage.s_options ()
  and+ files =
    let doc = "File to hash. Use $(b,-) for stdin." in
    Arg.(value & pos_all B0_std_cli.fpath [] & info [] ~doc ~docv:"FILE")
  in
  hash ~output_format ~hash_fun ~files

let main () = Cmd.eval_result tool
let () = if !Sys.interactive then () else exit (main ())
