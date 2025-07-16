(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let pp_hash = function
| `Short -> Fmt.using snd B0_hash.pp
| `Normal | `Long ->
    fun ppf (f, h) ->
      B0_hash.pp ppf h; Fmt.char ppf ' '; Fpath.pp_unquoted ppf f

let hash_file (module Hash : B0_hash.T) file  =
  let* hash =
    if Fpath.is_dash file
    then Result.map Hash.string (Os.File.read file)
    else Hash.file file
  in
  Ok (file, hash)

let hash ~output_details ~hash_fun ~files =
  let pp_hash = pp_hash output_details in
  let hash_fun = B0_memo_cli.Hash.get_hash_fun ~hash_fun in
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
  let man_xrefs = [`Tool "b0"] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command hashes files like b0 does.";
    `S Manpage.s_options;
    `S B0_std_cli.s_output_details_options;
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,b0) system. See
        $(i,https:/erratique.ch/software/b0) for contact information.";]
  in
  Cmd.make (Cmd.info "b0-hash" ~version:"%%VERSION%%" ~doc ~man ~man_xrefs) @@
  let+ () = B0_std_cli.set_log_level ()
  and+ output_details = B0_std_cli.output_details ()
  and+ hash_fun =
    B0_memo_cli.Hash.hash_fun ~opts:["H"; "hash-fun"] ~docs:Manpage.s_options ()
  and+ files =
    let doc = "File to hash. Use $(b,-) for stdin." in
    Arg.(value & pos_all B0_std_cli.filepath [] & info [] ~doc)
  in
  hash ~output_details ~hash_fun ~files

let main () = Cmd.eval_result tool
let () = if !Sys.interactive then () else exit (main ())
