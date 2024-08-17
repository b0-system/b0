(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let driver =
  let libs = [B0_ocaml.Libname.v "b0.tool"] in
  B0_driver.make ~name:"b0" ~version:"%%VERSION%%" ~libs

let def_list : (module B0_def.S) list =
  [(module B0_pack); (module B0_unit)]

let def_list_list def_list =
  let add_values (type a) acc (module Def : B0_def.S) =
    let vs = Def.list () in
    let add_value acc v =
      ((Def.name v, Def.def_kind), B0_def.V ((module Def), v)) :: acc
    in
    List.fold_left add_value acc vs
  in
  let vs = List.fold_left add_values [] def_list in
  List.map snd (List.sort compare vs)

let def_list_get_or_hint def_list name =
  let rec loop vs suggs = function
  | [] ->
      let rev_compare a b = compare b a in
      if vs <> []
      then Ok (List.map snd (List.sort rev_compare (* sort by rev kind *) vs))
      else
      let kind ppf () = Fmt.string ppf "definition"  in
      let hint = Fmt.did_you_mean in
      let pp = Fmt.unknown' ~kind Fmt.code ~hint in
      Fmt.error "@[%a@]" pp (name, String.Set.elements suggs)
  | (module Def : B0_def.S) :: defs ->
      let vs, suggs = match Def.get_or_suggest name with
      | Ok v -> ((Def.def_kind, B0_def.V ((module Def), v)) :: vs), suggs
      | Error sugg_vs ->
          let add_sugg acc v = String.Set.add (Def.name v) acc in
          let suggs = List.fold_left add_sugg suggs sugg_vs in
          vs, suggs
      in
      loop vs suggs defs
  in
  loop [] String.Set.empty def_list

let def_list_get_list_or_hint def_list ~all_if_empty names =
  if all_if_empty && names = [] then Ok (def_list_list def_list) else
  let rec loop vs es = function
  | [] ->
      if es <> []
      then Error (String.concat "\n" (List.rev es))
      else Ok (List.rev vs)
  | n :: ns ->
      match def_list_get_or_hint def_list n with
      | Ok values -> loop (List.rev_append values vs) es ns
      | Error e -> loop vs (e :: es) ns
  in
  loop [] [] names

module Def = struct
  let list (module Def : B0_def.S) c format ds =
    let pp, sep = match format with
    | `Short -> Def.pp_name, Fmt.cut
    | `Normal -> Def.pp_synopsis, Fmt.cut
    | `Long -> Def.pp, Fmt.(cut ++ cut)
    in
    Log.if_error ~use:Os.Exit.no_such_name @@
    let* ds = Def.get_list_or_hint ~all_if_empty:true ds in
    Log.if_error' ~use:Os.Exit.some_error @@
    let don't = B0_driver.Conf.no_pager c in
    let* pager = B0_pager.find ~don't () in
    let* () = B0_pager.page_stdout pager in
    if ds <> [] then Log.app (fun m -> m "@[<v>%a@]" Fmt.(list ~sep pp) ds);
    Ok Os.Exit.ok

  let edit (module Def : B0_def.S) _c ds =
    let rec find_files not_found fs = function
    | [] -> not_found, Fpath.distinct fs
    | d :: ds ->
        match B0_def.file (Def.def d) with
        | None -> find_files (Def.Set.add d not_found) fs ds
        | Some f -> find_files not_found (f :: fs) ds
    in
    let edit_all = ds = [] in
    Log.if_error ~use:Os.Exit.no_such_name @@
    let* ds = Def.get_list_or_hint ~all_if_empty:true ds in
    let not_found, files = find_files Def.Set.empty [] ds in
    Log.if_error' ~use:Os.Exit.some_error @@
    match not edit_all && not (Def.Set.is_empty not_found) with
    | true ->
        let plural = if (Def.Set.cardinal not_found > 1) then "s" else "" in
        let none = Def.Set.elements not_found in
        Fmt.error "Could not find b0 file for %s%s: @[%a@]"
          Def.def_kind plural Fmt.(list ~sep:sp Def.pp_name) none
    | false ->
        let* editor = B0_editor.find () in
        Result.bind (B0_editor.edit_files editor files) @@ function
        | `Exited 0 -> Ok Os.Exit.ok
        | _ -> Ok Os.Exit.some_error

  let get_meta_key (module Def : B0_def.S) c format key ds =
    Log.if_error ~use:Os.Exit.no_such_name @@
    let* B0_meta.Key.V key = B0_meta.Key.get_or_hint key in
    let* ds = Def.get_list_or_hint ~all_if_empty:true ds in
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
    Ok Os.Exit.ok
end

module Cli = struct
  open Cmdliner

  let s_scope_selection = "SCOPE SELECTION"

  let man_see_manual = `Blocks
      [ `S Manpage.s_see_also;
        `P "Consult $(b,odig doc b0) for manuals and more details."]

  let man_with_descr ?synopsis descr =
    let man =
      [ `S Manpage.s_description;
        descr;
        `S Manpage.s_commands;
        `S Manpage.s_arguments;
        `S Manpage.s_options;
        `S B0_std_cli.s_output_format_options;
        `S s_scope_selection;
        `S Manpage.s_common_options;
       man_see_manual ]
    in
    match synopsis with
    | None -> man
    | Some syn -> `S Manpage.s_synopsis :: syn :: man

  let editor_envs = B0_editor.Env.infos
  let pager_envs = B0_pager.Env.infos
  let format = B0_std_cli.output_format ()
  let no_pager = B0_driver.Cli.no_pager
  let pos_key =
    let doc = "The metadata key $(docv) to get." and docv = "KEY" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv)

  let minimal_setup =
    let minimal_setup_with_cli log_level tty_cap =
      let tty_cap = B0_std_cli.get_tty_cap tty_cap in
      let log_level = B0_std_cli.get_log_level log_level in
      B0_std_cli.setup tty_cap log_level ~log_spawns:Log.Debug;
    in
    Term.(const minimal_setup_with_cli $ B0_driver.Cli.log_level $
          B0_driver.Cli.tty_cap)

  let subcmd ?exits ?(envs = []) ?synopsis name ~doc ~descr term =
    let man = man_with_descr ?synopsis descr in
    let term = Term.(term $ minimal_setup) in
    Cmd.v (Cmd.info name ~doc ?exits ~envs ~man) term

  let subcmd_with_driver_conf
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      term
    =
    let man = man_with_descr ?synopsis descr in
    let envs = List.rev_append envs pager_envs (* driver conf has no-pager *)in
    let term = Term.(term $ B0_driver.Cli.conf) in
    Cmd.v (Cmd.info name ~doc ~exits ~envs ~man) term

  let subcmd_with_b0_file_if_any
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      term
    =
    let man = man_with_descr ?synopsis descr in
    let envs = List.rev_append envs pager_envs (* driver conf has no-pager *)in
    let term = B0_driver.with_b0_file_if_any ~driver term in
    Cmd.v (Cmd.info name ~doc ~exits ~envs ~man) term

  let subcmd_with_b0_file
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      term
    =
    let man = man_with_descr ?synopsis descr in
    let envs = List.rev_append envs pager_envs (* driver conf has no-pager *)in
    let term = B0_driver.with_b0_file ~driver term in
    Cmd.v (Cmd.info name ~doc ~exits ~envs ~man) term

  let cmd_group ?exits ?(envs = []) ?synopsis name ~doc ~descr ?default subs =
    let man = man_with_descr ?synopsis descr in
    Cmd.group (Cmd.info name ~doc ?exits ~envs ~man) ?default subs

  (*

   Let's keep that here for now

  let cmd_group_with_b0_file
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      ?default subs
    =
    let man = man_with_descr ?synopsis descr in
    let default = match default with
    | None -> None
    | Some cmd -> Some (B0_driver.with_b0_file ~driver cmd)
    in
    Cmd.group (Cmd.info name ~doc ~exits ~envs ~man) ?default subs

  let cmd_group_with_driver_conf
      ?(exits = B0_driver.Exit.infos) ?(envs = []) ?synopsis name ~doc ~descr
      ?default subs
    =
    let man = man_with_descr ?synopsis descr in
    let default = match default with
    | Some default -> Some (Term.(default $ B0_driver.Cli.conf))
    | None -> None
    in
    Cmd.group (Cmd.info name ~doc ~exits ~envs ~man) ?default subs
  *)

end
