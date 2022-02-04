(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let driver =
  let libs = [B00_ocaml.Lib.Name.v "b0.b0"] in
  B0_driver.create ~name:"b0" ~version:"%%VERSION%%" ~libs

module Def = struct
  let list (module Def : B0_def.S) c format ds =
    let pp, sep = match format with
    | `Short -> Def.pp_name, Fmt.cut
    | `Normal -> Def.pp_synopsis, Fmt.cut
    | `Long -> Def.pp, Fmt.(cut ++ cut)
    in
    Log.if_error ~use:B00_cli.Exit.no_such_name @@
    let* ds = Def.get_list_or_hint ~empty_means_all:true ds in
    Log.if_error' ~use:B00_cli.Exit.some_error @@
    let don't = B0_driver.Conf.no_pager c in
    let* pager = B00_pager.find ~don't () in
    let* () = B00_pager.page_stdout pager in
    if ds <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) ds);
    Ok B00_cli.Exit.ok

  let edit (module Def : B0_def.S) c ds =
    let rec find_files not_found fs = function
    | [] -> not_found, Fpath.uniquify fs
    | d :: ds ->
        match B0_def.file (Def.def d) with
        | None -> find_files (Def.Set.add d not_found) fs ds
        | Some f -> find_files not_found (f :: fs) ds
    in
    let edit_all = ds = [] in
    Log.if_error ~use:B00_cli.Exit.no_such_name @@
    let* ds = Def.get_list_or_hint ~empty_means_all:true ds in
    let not_found, files = find_files Def.Set.empty [] ds in
    Log.if_error' ~use:B00_cli.Exit.some_error @@
    match not edit_all && not (Def.Set.is_empty not_found) with
    | true ->
        let plural = if (Def.Set.cardinal not_found > 1) then "s" else "" in
        let none = Def.Set.elements not_found in
        Fmt.error "Could not find B0 file for %s%s: @[%a@]"
          Def.def_kind plural Fmt.(list ~sep:sp Def.pp_name) none
    | false ->
        let* editor = B00_editor.find () in
        Result.bind (B00_editor.edit_files editor files) @@ function
        | `Exited 0 -> Ok B00_cli.Exit.ok
        | _ -> Ok B00_cli.Exit.some_error

  let get_meta_key (module Def : B0_def.S) c format key ds =
    Log.if_error ~use:B00_cli.Exit.no_such_name @@
    let* B0_meta.Key.V key = B0_meta.Key.get_or_hint key in
    let* ds = Def.get_list_or_hint ~empty_means_all:true ds in
    let add_meta acc d = match B0_meta.find_binding key (Def.meta d) with
    | None -> acc | Some v -> (d, v) :: acc
    in
    let bs = List.rev @@ List.fold_left add_meta [] ds in
    begin match ds with
    | [d] ->
        (* For single def requests we don't print the def name. *)
        begin match bs with
        | [] -> ()
        | [_, B0_meta.B (k, v)] ->
            Log.app (fun m -> m "@[<h>%a@]" (B0_meta.Key.pp_value k) v)
        | _ -> assert false
        end
    | _ ->
        let pp_bindings ppf (d, B0_meta.B (k, v)) =
          Fmt.pf ppf "@[<h>%a %a@]" Def.pp_name d (B0_meta.Key.pp_value k) v
        in
        Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_bindings) bs)
    end;
    Ok B00_cli.Exit.ok
end

module Cli = struct
  open Cmdliner

  let s_output_format = "OUTPUT FORMAT OPTIONS"

  let man_see_manual = `Blocks
      [ `S Manpage.s_see_also;
        `P "Consult $(b,odig doc b0) for manuals and more details."]

  let man_with_descr ?synopsis descr =
    let man = [`S Manpage.s_description;
               descr;
               `S Manpage.s_commands;
               `S Manpage.s_arguments;
               `S Manpage.s_options;
               `S s_output_format;
               `S Manpage.s_common_options;
               man_see_manual]
    in
    match synopsis with
    | None -> man
    | Some syn -> `S Manpage.s_synopsis :: syn :: man

  let editor_envs = B00_editor.envs ()
  let pager_envs = B00_pager.envs ()
  let format = B00_cli.Arg.output_details ~docs:s_output_format ()
  let pos_key =
    let doc = "The metadata key $(docv) to get." and docv = "KEY" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

  let subcmd_with_b0_file
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      term
    =
    let man = man_with_descr ?synopsis descr in
    let term = B0_driver.with_b0_file ~driver term in
    Cmd.v (Cmd.info name ~doc ~exits ~envs ~man) term

  let cmd_group_with_b0_file
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      ~default subs
    =
    let man = man_with_descr ?synopsis descr in
    let default = B0_driver.with_b0_file ~driver default in
    Cmd.group (Cmd.info name ~doc ~exits ~envs ~man) ~default subs

  let subcmd_with_driver_conf
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      term
    =
    let man = man_with_descr ?synopsis descr in
    let term = Term.(term $ B0_driver.Cli.conf) in
    Cmd.v (Cmd.info name ~doc ~exits ~envs ~man) term

  let cmd_group_with_driver_conf
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      ~default subs
    =
    let man = man_with_descr ?synopsis descr in
    let default = Term.(default $ B0_driver.Cli.conf) in
    Cmd.group (Cmd.info name ~doc ~exits ~envs ~man) ~default subs
end

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
