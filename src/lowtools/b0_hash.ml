(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let hash_file (module H : Hash.T) f = match Fpath.equal f Fpath.dash with
| false -> Result.bind (H.file f) @@ fun h -> Ok (f, h)
| true -> Result.bind (Os.File.read f) @@ fun data -> Ok (f, H.string data)

let pp_hash = function
| `Short -> Fmt.using snd Hash.pp
| `Normal | `Long ->
    fun ppf (f, h) -> Hash.pp ppf h; Fmt.char ppf ' '; Fpath.pp_unquoted ppf f

let hash tty_cap log_level hash_fun format files =
  let tty_cap = B0_std_cli.get_tty_cap tty_cap in
  let log_level = B0_std_cli.get_log_level log_level in
  B0_std_cli.setup tty_cap log_level ~log_spawns:Log.Debug;
  let hash_fun = B0_cli.Memo.get_hash_fun ~hash_fun in
  let pp_hash = pp_hash format in
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@ match files with
  | [] -> Ok 0
  | files ->
      let rec loop = function
      | [] -> Fmt.pr "@]"; Fmt.flush Fmt.stdout (); Ok 0
      | f :: fs ->
          match hash_file hash_fun f with
          | Ok h -> Fmt.pr "%a@," pp_hash h; loop fs
          | Error _ as e -> Fmt.pr "@]"; e
      in
      Fmt.pr "@[<v>"; loop files

(* Command line interface *)

open Cmdliner

let files =
  let doc = "File to hash. Use $(b,-) for stdin." in
  Arg.(value & pos_all B0_std_cli.fpath [] & info [] ~doc ~docv:"FILE")

let hash_fun =
  B0_cli.Memo.hash_fun ~opts:["H"; "hash-fun"] ~docs:Manpage.s_options ()

let tool =
  let doc = "Hash like b0" in
  let man_xrefs = [`Tool "b0"; `Tool "b0-cache"; `Tool "b0-log"] in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command hashes files like b0 does.";
    `S Manpage.s_options;
    `S B0_std_cli.s_output_format_options;
    `S Manpage.s_bugs;
    `P "Report them, see $(i,%%PKG_HOMEPAGE%%) for contact information." ]
  in
  Cmd.v (Cmd.info "b0-hash" ~version:"%%VERSION%%" ~doc ~man ~man_xrefs)
    Term.(const hash $ B0_std_cli.tty_cap () $
          B0_std_cli.log_level () $ hash_fun $
          B0_std_cli.output_format () $ files)

let main () = exit (Cmd.eval' tool)
let () = if !Sys.interactive then () else main ()
