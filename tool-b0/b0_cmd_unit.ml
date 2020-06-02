(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let get c format args = match args with
| k :: us -> B0_b0.Def.get_meta_key (module B0_unit) c format k us
| [] ->
    Log.err (fun m -> m "No metadata key specified");
    B0_driver.Exit.some_error

let get_unit args k = match args with
| [] ->
    Log.err (fun m -> m "No unit name specified");
    B0_driver.Exit.some_error
| u :: args ->
    Log.if_error ~use:B0_driver.Exit.no_such_name @@
    let* u = B0_unit.get_list [u] in
    let u = List.hd u in
    Ok (k u args)

let build_unit c u k =
    (* FIXME cut and paste with build. Also we likely want to
       control the same way build does *)
    let may_build = B0_unit.Set.of_list (B0_unit.list ()) in
    let must_build = B0_unit.Set.singleton u in
    let b0_file = Option.get (B0_driver.Conf.b0_file c) in
    let root_dir = Fpath.parent b0_file in
    let b0_dir = B0_driver.Conf.b0_dir c in
    Log.if_error ~use:B0_driver.Exit.build_error @@
    let* m = B0_cmd_build.memo c in
    let build = B0_build.create ~root_dir ~b0_dir m ~may_build ~must_build in
    match B0_build.run build with
    | Error () -> Ok B0_driver.Exit.build_error
    | Ok () -> Ok (k build)

let act c args =
  get_unit args @@ fun u args ->
  build_unit c u @@ fun build ->
  failwith "TODO"

let exec c dry_run args =
  (* TODO we should have a better definition of where
     this should be run. Basically this should be on the host os. *)
  get_unit args @@ fun u cmd ->
  match B0_meta.find B0_meta.exe_path (B0_unit.meta u) with
  | None ->
      Log.err (fun m -> m "Unit %a does not define metadata key %a"
                  B0_unit.pp_name u B0_meta.Key.pp_name B0_meta.exe_path);
      B0_driver.Exit.some_error
  | Some exe ->
      build_unit c u @@ fun build ->
      match Fut.value exe with
      | None ->
          Log.err (fun m -> m "Unit %a did not determine %a"
                      B0_unit.pp_name u B0_meta.Key.pp_name B0_meta.exe_path);
          B0_driver.Exit.some_error
      | Some exe ->
          let cmd = Cmd.(path exe %% args cmd) in
          if not dry_run then B0_driver.Exit.Exec (exe, cmd) else
          (Fmt.pr "%s" (Cmd.to_string cmd); B0_driver.Exit.ok)

let unit action format dry_run args c = match action with
| `Action -> act c args
| `Edit -> B0_b0.Def.edit (module B0_unit) c args
| `Exec -> exec c dry_run args
| `Get -> get c format args
| `List -> B0_b0.Def.list (module B0_unit) c format args
| `Show ->
    let format = if format = `Normal then `Long else format in
    B0_b0.Def.list (module B0_unit) c format args

(* Command line interface *)

open Cmdliner

let dry_run =
  let doc =
    "Do not execute the unit action. If applicable, show the invocation"
  in
  Arg.(value & flag & info ["dry-run"] ~doc)

let action =
  let action =
    ["action", `Action; "edit", `Edit; "exec", `Exec; "get", `Get;
     "list", `List; "show", `Show]
  in
  let doc =
    let alts = Arg.doc_alts_enum action in
    Fmt.str "The action to perform. $(docv) must be one of %s." alts
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let action_args =
  let doc = "Positional arguments for the action." in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARG")

let doc = "Operate on build units"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Exit.Info.base_cmd
let envs = B00_editor.envs ()
let man_xrefs = [ `Main ]
let man = [
  `S Manpage.s_description;
  `P "$(tname) operates on build units.";
  `S "ACTIONS";
  `I ("$(b,action) $(i,UNIT) -- $(i,ARG)...",
      "Builds $(i,UNIT) and execute its action. The unit needs to \
       have an action defined for this to work.");
  `I ("$(b,edit) [$(i,UNIT)]...",
      "Edit in your editor the B0 file(s) in which all or the given units \
       are defined.");
  `I ("$(b,exec) [$(b,--dry-run)] $(i,UNIT) -- $(i,ARG)...",
      "Build $(i,UNIT) and execute with given arguments the executable
       specified in the unit's $(b,B0_meta.exe_path) metadata key.
       If $(b,--dry-run) is specified prints out the invocation.");
  `I ("$(b,get) $(i,KEY) [$(i,UNIT)]...",
      "Get metadata key $(i,KEY) of given or all units.");
  `I ("$(b,list) [$(i,UNIT)]...",
      "List all or given units. Use with $(b,-l) to get more info on \
       unit metadata.");
  `I ("$(b,show) [$(i,UNIT)]...",
      "Show is an alias for $(b,list -l).");
`S Manpage.s_arguments;
  `S Manpage.s_options;
  B0_b0.Cli.man_see_manual; ]

let cmd =
  let unit_cmd =
    Term.(const unit $ action $ B00_ui.Cli.out_details () $ dry_run $
          action_args)
  in
  B0_driver.with_b0_file ~driver:B0_b0.driver unit_cmd,
  Term.info "unit" ~doc ~sdocs ~exits ~envs ~man ~man_xrefs

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
