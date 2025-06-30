(*---------------------------------------------------------------------------
   Copyright (c) 2021 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () = B0_scope.open_lib ~module':__MODULE__ "release"

open B0_std
open Result.Syntax

(* Metadata *)

let tag = B0_meta.Key.make_tag "tag" ~doc:"Releasable entity"

(* VCS repository *)

let err_no_vcs pack e =
  Fmt.str "@[<v>No VCS found for pack %a:@,%s@]" B0_pack.pp_name pack e

let err_no_version pack e =
  Fmt.str "@[<v>Cannot determine release version for pack %a:@,%s@]"
    B0_pack.pp_name pack e

let vcs_repo_of_pack pack =
  Result.map_error (err_no_vcs pack) @@
  let* dir = B0_pack.scope_dir' pack in
  let* repo = B0_vcs_repo.get ~dir () in
  Ok (dir, repo)

let vcs_repo_version_of_pack ?(commit_ish = "HEAD") pack =
  Result.map_error (err_no_version pack) @@
  let* _scope_dir, repo = vcs_repo_of_pack pack in
  let* tag = B0_vcs_repo.latest_tag repo commit_ish in
  match tag with
  | Some t -> Ok (String.drop_initial_v t)
  | None ->
      Fmt.error "No annotated tag for %s in %a" commit_ish B0_vcs_repo.pp repo

(* Package selection *)

let err_no_pack () =
  Fmt.error
    "@[<v>No pack found. Specify one with option %a or define@,\
     a pack named %a.@]" Fmt.code "-p" Fmt.code "default"

let select_packs packs x_packs =
  (* XXX default pack selection. I don't really like that. I think
     it would be better to select packs that have a `B0_release.tag`
     but it's one more thing to add in each project. *)
  let* x_packs = B0_pack.get_list_or_hint ~all_if_empty:false x_packs in
  let x_packs = B0_pack.Set.of_list x_packs in
  match packs with
  | [] ->
      begin match B0_pack.find "default" with
      | Some pack when B0_pack.Set.mem pack x_packs -> Ok []
      | Some pack -> Ok [pack]
      | None ->
          let keep_default pack = match B0_pack.basename pack with
          | "default" ->
              begin match B0_pack.scope_path pack with
              | [first_level] when not (B0_pack.Set.mem pack x_packs) -> true
              | _ -> false
              end
          | _ -> false
          in
          Ok (List.filter keep_default (B0_pack.list ()))
      end
  | packs -> B0_pack.get_list_or_hint ~all_if_empty:false packs

let gather_repos packs =
  let add acc pack =
    Result.error_to_failure @@
    let* dir, vcs = vcs_repo_of_pack pack in
    let vcs_dir = B0_vcs_repo.repo_dir vcs in
    match Fpath.Map.find_opt vcs_dir acc with
    | None -> Ok (Fpath.Map.add dir vcs acc) | Some _ -> Ok acc
  in
  try
    let repos = List.fold_left add Fpath.Map.empty packs in
    let repos = Fpath.Map.fold (fun _ repo acc -> repo :: acc) repos [] in
    Ok (List.rev repos)
  with
  | Failure e -> Error e

(* Source archives *)

let src_archive_name =
  let doc = "Source release archive (base) name" in
  B0_meta.Key.make "archive-name" ~doc ~pp_value:Fmt.string

let src_archive_name_of_pack p =
  match B0_pack.find_meta src_archive_name p with
  | Some n -> n
  | None ->
      let n = B0_pack.basename p in
      if not (String.equal n "default") then n else
      match B0_def.scope_dir (B0_pack.def p) with
      | None -> "unknown" (* unlikely, libraries should not do this. *)
      | Some d -> Fpath.basename d

let src_archive_ext =
  let doc = "Source release archive file extension" in
  let default = ".tbz" and pp_value = Fmt.string in
  B0_meta.Key.make "archive-ext" ~default ~doc ~pp_value

let default_archive =
  "\x25%ARCHIVE_NAME%\x25-\x25%VERSION_NUM%\x25\x25%ARCHIVE_EXT%\x25"

let src_archive_url =
  let doc = "Source release archive URL pattern" in
  B0_meta.Key.make "archive-url" ~doc ~pp_value:Fmt.string

let default_src_archive_url homepage =
  let drop_final_slash s =
    let max = String.length s - 1 in
    if max < 0 then s else
    if s.[max] = '/' then String.subrange ~last:(max - 1) s else s
  in
  Fmt.str "%s/releases/%s" (drop_final_slash homepage) default_archive

let default_github_src_archive_url repo =
  let repo = match String.cut_left ~sep:"git+" repo with
  | Some (_, repo) -> repo | _ -> repo
  in
  let repo = match String.cut_right ~sep:"." repo with
  | Some (repo, _) -> repo | _ -> repo
  in
  Fmt.str "%s/releases/download/\x25\x25VERSION\x25\x25/%s" repo default_archive

let src_archive_url_of_pack ~version p =
  let archive_name = src_archive_name_of_pack p in
  let archive_ext = B0_pack.find_or_default_meta src_archive_ext p in
  let vars = function
  | "ARCHIVE_NAME" -> Some archive_name
  | "ARCHIVE_EXT" -> Some archive_ext
  | "VERSION" -> Some version
  | "VERSION_NUM" -> Some (String.drop_initial_v version)
  | _ -> None
  in
  Result.map_error (Fmt.str "@[<v>Cannot determine release URL:@,%s@]") @@
  let err () = Error "See docs of B0_release.src_archive_url_of_pack." in
  let* archive_url = match B0_pack.find_meta src_archive_url p with
  | Some url -> Ok url
  | None ->
      match B0_pack.find_meta B0_meta.homepage p with
      | None -> err ()
      | Some h ->
          let is_github = match B0_url.authority h with
          | None -> false
          | Some auth ->
              match String.split_on_char '.' auth with
              | ("github" :: _ ) | ( _ :: "github" :: _) -> true | _ -> false
          in
          match is_github with
          | false -> Ok (default_src_archive_url h)
          | true ->
              match B0_pack.find_meta B0_meta.repo p with
              | None -> err ()
              | Some repo -> Ok (default_github_src_archive_url repo)
  in
  Ok (String.subst_pct_vars vars archive_url)

let src_archive_dirname name version =
  let vnum = String.drop_initial_v version in
  Fpath.v (Fmt.str "%s-%s" name vnum)

let default_exclude_paths =
  Fpath.[
    v ".git";
    v ".gitattributes";
    v ".gitignore";
    v ".hg";
    v ".hgignore";
    v "_b0";
    v "_build"; ]

let src_archive_for_pack
    ~repo ~checkout_dir:dir ~keep_checkout_dir ~commit_ish pack
  =
  let* version = B0_vcs_repo.describe repo ~dirty_mark:false commit_ish in
  let name = src_archive_name_of_pack pack in
  let* _existed = Os.Path.delete ~recurse:true dir in
  let* repo = B0_vcs_repo.local_clone repo ~dir in
  let* () = B0_vcs_repo.checkout repo commit_ish in
  let* mtime = B0_vcs_repo.commit_ptime_s repo commit_ish in
  (* Here a release time massaging hook could be invoked.
     Here we need to watermark. Though the idea was rather
     to include watermarks at the source specification level.  *)
  let exclude_paths =
    (* FIXME We should have a key in the pack to override that. *)
    Fpath.Set.of_list default_exclude_paths
  in
  let root = src_archive_dirname name version in
  let* archive = B0_tar.of_dir ~dir ~exclude_paths ~root ~mtime in
  let* () =
    if keep_checkout_dir
    then Ok ()
    else Result.map ignore (Os.Path.delete ~recurse:true dir)
  in
  Ok (version, root, archive)

(* Change logs *)

let changes_file =
  let doc = "Changes file relative to scope directory of definition." in
  let default = Fpath.v "CHANGES.md" and pp_value = Fpath.pp in
  B0_meta.Key.make "changes-file" ~default ~doc ~pp_value

let changes_file_of_pack pack =
  let changes = B0_pack.find_or_default_meta changes_file pack in
  match B0_pack.in_scope_dir pack changes with
  | None -> Ok None
  | Some changes ->
      let* exists = Os.File.exists changes in
      if not exists then Ok None else Ok (Some changes)

let get_changes_file_of_pack pack =
  Result.map_error (fun e -> Fmt.str "pack %a: %s" B0_pack.pp_name pack e) @@
  let changes = B0_pack.find_or_default_meta changes_file pack in
  let* changes = B0_pack.in_scope_dir' pack changes in
  let* () = Os.File.must_exist changes in
  Ok changes

let changes_latest_of_file f =
  let* contents = Os.File.read f in
  Ok (String.commonmark_first_section ~preamble:false contents)

let changes_latest_version_of_title title =
  match String.keep_left Char.Ascii.is_graphic title with
  | "" -> None | token -> Some token

(* Release archive *)

module Archive = struct
  let for_pack env ~commit_ish pack =
    let keep_checkout_dir = false in
    let* scope_dir, repo = vcs_repo_of_pack pack in
    let* is_dirty = B0_vcs_repo.is_dirty repo in
    let* commit_id = B0_vcs_repo.commit_id ~dirty_mark:false repo commit_ish in
    let name = src_archive_name_of_pack pack in
    let checkout_dir = B0_env.in_scratch_dir env Fpath.(v name + ".build") in
    let* (version, archive_base, archive) =
      src_archive_for_pack
        ~repo ~checkout_dir ~keep_checkout_dir ~commit_ish pack
    in
    let archive_file =
      let archive_ext = B0_pack.find_or_default_meta src_archive_ext pack in
      B0_env.in_scratch_dir env Fpath.(archive_base + archive_ext)
    in
    let search = B0_env.get_cmd env ~skip_build:false in
    let force = true and make_path = true in
    let* () = B0_tar.compress ~search ~force ~make_path archive_file ~archive in
    let archive_dir = Fpath.strip_ext archive_file in
    Log.stdout (fun m -> m "Wrote %a" (Fmt.code' Fpath.pp) archive_file);
    Log.stdout (fun m ->
        m "TODO Checking release in %a" (Fmt.code' Fpath.pp) archive_dir);
    let make_path = true and verbose = false in
    let* _exists = Os.Path.delete ~recurse:true archive_dir in
    let in_dir = B0_env.scratch_dir env in
    let* () =
      B0_tar.unarchive ~search ~make_path ~verbose ~src:archive_file ~in_dir ()
    in
    Log.stdout (fun m -> m "TODO Building release");
    Log.stdout (fun m -> m "TODO Testing release");
    let* _exists = Os.Path.delete ~recurse:true archive_dir in
    (Log.stdout @@ fun m ->
    m "@[<v>Release: %a %a@,Commit:  %a@,Archive: %a@]@."
      Fmt.code name String.pp_version_str version
      B0_vcs_repo.pp_commit commit_id
      (Fmt.code' Fpath.pp) archive_file);
    if is_dirty then
      (Log.warn @@ fun m ->
       m "@[<v>%a repo in scope directory %a@,\
          Did you forget to commit some changes ?@]@."
         B0_vcs_repo.pp_dirty () (Fmt.code' Fpath.pp) scope_dir);
    Ok ()

  let cmd env commit_ish packs x_packs =
    Log.if_error ~use:Os.Exit.no_such_name @@
    let* packs = select_packs packs x_packs in
    Log.if_error' ~use:Os.Exit.some_error @@
    (* XXX we likely need to do/say something if there's a pack
       with the same repo, or not. Also maybe we should rather
       use the [B0_release.tag] to select them, but then it's one
       more thing to add. *)
    let* () = List.iter_stop_on_error (for_pack env ~commit_ish) packs in
    Ok Os.Exit.some_error
end

(* Release tagging *)

module Tag = struct

  (* Note some fragments could be exposed in the API. *)

  let tag_of_version pack = function
  | Some version -> Ok version
  | None ->
      let* file = get_changes_file_of_pack pack in
      let* changes = changes_latest_of_file file in
      let version = match changes with
      | None -> None | Some (t, _) -> changes_latest_version_of_title t
      in
      match version with
      | None -> Fmt.error "Cannot extract a version from %a." Fpath.pp file
      | Some v -> Ok v

  let tag_repo repo tag ~commit_ish ~msg ~sign ~force ~delete ~dry_run =
    let log_cmd cmd = Log.stdout (fun m -> m "%s" (Cmd.to_string cmd)) in
    let dry_run = if dry_run then Some log_cmd else None in
    Log.stdout (fun m -> m "Repo %a:" B0_vcs_repo.pp repo);
    match delete with
    | true ->
        let* () = B0_vcs_repo.delete_tag ?dry_run repo tag in
        Log.stdout
          (fun m -> m "Deleted version tag %a" String.pp_version_str tag);
        Ok ()
    | false ->
        let msg = match msg with
        | None -> Fmt.str "Release %s" tag | Some m -> m
        in
        let* () =
          B0_vcs_repo.tag ?dry_run repo ~force ~sign ~msg commit_ish tag
        in
        Log.stdout (fun m -> m "Tagged version %a" String.pp_version_str tag);
        Ok ()

  let gather_repos_and_tags version packs =
    let add_repo acc pack =
      Result.error_to_failure @@
      let* dir = match B0_pack.scope_dir pack with
      | None -> Fmt.error "Pack %a has no scope directory" B0_pack.pp_name pack
      | Some dir -> Ok dir
      in
      let* tag = tag_of_version pack version in
      let* repo = B0_vcs_repo.get ~dir () in
      let repo_dir = B0_vcs_repo.repo_dir repo in
      match Fpath.Map.find_opt repo_dir acc with
      | None -> Ok (Fpath.Map.add repo_dir (pack, repo, tag) acc)
      | Some (pack', _, tag') when tag <> tag' ->
          Fmt.error "@[<v>Cannot tag VCS %a:@,\
                     pack %a has version %a@,\
                     pack %a has version %a@]"
            Fpath.pp repo_dir
            B0_pack.pp_name pack String.pp_version_str tag
            B0_pack.pp_name pack' String.pp_version_str tag'
      | _ -> Ok acc
    in
    try
      let rs = List.fold_left add_repo Fpath.Map.empty packs in
      let rs = Fpath.Map.fold (fun _ (_, r, tag) acc -> (r, tag) :: acc) rs []in
      Ok (List.rev rs)
    with
    | Failure e -> Error e

  let cmd version commit_ish msg sign force delete dry_run packs x_packs =
    Log.if_error ~use:Os.Exit.no_such_name @@
    let* packs = select_packs packs x_packs in
    Log.if_error' ~use:Os.Exit.some_error @@
    let* () = if packs <> [] then Ok () else err_no_pack () in
    let* repos_tags = gather_repos_and_tags version packs in
    let rec loop = function
    | [] -> Ok Os.Exit.ok
    | (repo, tag) :: repos ->
        match tag_repo repo tag ~commit_ish ~msg ~sign ~force ~delete ~dry_run
        with
        | Error _ as e -> e
        | Ok () -> if repos <> [] then Log.stdout (fun m -> m ""); loop repos
    in
    loop repos_tags
end

(* Release status *)

module Status = struct
  let pp_since ppf v = Fmt.pf ppf "since %a" String.pp_version_str v
  let pp_status ppf (dirty, version, changes) = match changes with
  | [] when not dirty -> Fmt.pf ppf "@[<v>No changes %a@]" pp_since version
  | changes ->
      let pp_commit ppf (c, syn) =
        Fmt.pf ppf "@[<h>%a %s@]" B0_vcs_repo.pp_commit c syn
      in
      let pp_dirty ppf = function
      | false -> () | true ->
          Fmt.pf ppf "The repository is %a.@," B0_vcs_repo.pp_dirty ()
      in
      Fmt.pf ppf "@[<v>Changes %a:@,%a%a@]"
        pp_since version pp_dirty dirty (Fmt.list pp_commit) changes

  let find_changes repo ~after ~last = match after with
  | Some after ->
      let* changes = B0_vcs_repo.changes repo ~after ~last () in
      Ok (after, changes)
  | None ->
      let* greatest = B0_vcs_repo.find_greatest_version_tag repo in
      match greatest with
      | Some greatest ->
          let* changes = B0_vcs_repo.changes repo ~after:greatest ~last () in
          Ok (greatest, changes)
      | None ->
          let* changes = B0_vcs_repo.changes repo ~last () in
          let commit = match List.rev changes with
          | [] -> "<none>" | (commit, _synopsis) :: _ -> commit
          in
          Ok (commit, changes)

  let status repo ~after ~last =
    let* dirty = B0_vcs_repo.is_dirty repo in
    let* after, changes = find_changes repo ~after ~last in
    Ok (dirty, after, changes)

  let status_has_changes (dirty, _, changes) = dirty || changes <> []

  let pp_commit ppf (id, log) =
    Fmt.pf ppf "%a %s" B0_vcs_repo.pp_commit id log

  let cmd after last packs x_packs pager_don't =
    Log.if_error ~use:Os.Exit.no_such_name @@
    let* packs = select_packs packs x_packs in
    Log.if_error' ~use:Os.Exit.some_error @@
    let* () = if packs <> [] then Ok () else err_no_pack () in
    let* pager = B0_pager.find ~don't:pager_don't () in
    let* () = B0_pager.page_stdout pager in
    let* repos = gather_repos packs in
    if repos = []
    then Fmt.error "No VCS repository found." else
    let rec loop ~has_changes = function
    | [] -> Ok has_changes
    | repo :: repos ->
        let* status = status repo ~after ~last in
        let has_changes = has_changes || status_has_changes status in
        (Log.stdout @@ fun m ->
         m "@[<v>Repo %a:@,%a@]" B0_vcs_repo.pp repo pp_status status);
        if repos <> [] then Log.stdout (fun m -> m "");
        loop ~has_changes repos
    in
    let* has_changes = loop ~has_changes:false repos in
    Ok (if has_changes then Os.Exit.ok else Os.Exit.code 1)
end

open Cmdliner

let unit =
  let doc = "Source software release support" in
  B0_unit.of_cmdliner_cmd "" ~doc @@ fun env u ->
  let man =
    [ `S Manpage.s_see_also;
      `P "Consult $(b,odig doc b0) for the b0 release manual."]
  in
  let packs =
    let doc =
      "Consult VCS repository in scope directory of $(docv). Repeatable. \
       Defaults to a pack named $(b,default) or if there is no such \
       pack, all the $(b,default) packs in the next scope level (i.e.
       existing packs $(b,\\$SCOPE.default))."
    in
    B0_cli.packs ~doc ()
  in
  let x_packs =
    let doc = "Packs to exclude from selection. Repeatable. \
               Takes over inclusion." in
    B0_cli.x_packs ~doc ()
  in
  let archive =
    let doc = "Create release archives" in
    let man =
      [ `S Cmdliner.Manpage.s_description;
        `P "$(iname) creates an archive file for a release's pack. The \
            generated archive should be bit-wise reproducible. There \
            are a few caveats, see b0 release manual in $(b,odig doc b0).";
        `P "Once the archive is created it is unpacked in the build \
            directory, linted and the package is built using the package \
            description contained in the archive. The build will use the \
            default package configuration so it may fail in the current \
            environment without this necessarily implying an actual \
            problem with the distribution; one should still worry about \
            it though These checks can be prevented by using the \
            $(b,--skip-lint) and $(b,--skip-build) options.";
        `Blocks man ]
    in
    let commit =
      let doc = "Commit-ish $(docv) to checkout for the release." in
      Arg.(value & opt string "HEAD" & info ["commit"] ~doc ~docv:"COMMIT-ISH")
    in
    Cmd.make (Cmd.info "archive" ~doc ~man) @@
    Term.(const Archive.cmd $ const env $ commit $ packs $ x_packs)
  in
  let tag =
    let doc = "Tag VCS repositories with versions" in
    let man =
      [ `S Cmdliner.Manpage.s_description;
        `P "$(iname) looks up the VCS repositories in the scope directories \
            of given packs and for each of them tags the HEAD commit \
            with a version.";
        `P "If the version is not specified on the command line it \
            is automatically extracted from the changes file specified in \
            the pack's $(b,B0_release.changes_file) key. See argument
            $(i,VERSION) for details.";
        `P "Use option $(b,--dry-run) to check the extracted version value \
            and operations that would be performed.";
        `Blocks man ]
    in
    let commit =
      let doc = "Commit-ish $(docv) to tag." in
      Arg.(value & opt string "HEAD" & info ["commit"] ~doc ~docv:"COMMIT-ISH")
    in
    let msg =
      let doc =
        "Commit message for the tag. If absent, the message \
         'Release $(i,VERSION)' is used."
      in
      Arg.(value & opt (some string) None &
           info ["m"; "message"] ~doc ~docv:"MSG")
    in
    let sign =
      let doc = "Sign the tag using the VCS's default signing key." in
      Arg.(value & flag & info ["s"; "sign"] ~doc)
    in
    let force =
      let doc = "If the tag exists, replace it rather than fail." in
      Arg.(value & flag & info ["f"; "force"] ~doc)
    in
    let delete =
      let doc = "Delete the specified tag rather than create it." in
      Arg.(value & flag & info ["d"; "delete"] ~doc)
    in
    let dry_run =
      let doc = "Do not perform any action, just print them." in
      Arg.(value & flag & info ["dry-run"] ~doc)
    in
    let version =
      let doc =
        "The version tag to use. If absent, automatically extracted \
         from the changes file of a pack: the first token of the first
         heading of the changes file is taken. E.g. $(b,vX.Y.Z) for a markdown \
         file with a first heading $(b,# vX.X.X YYYY-MM-DD). Use \
         $(b,--dry-run) to check out the extraction."
      in
      Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"VERSION")
    in
    let exits = B0_std_cli.Exit.infos in
    Cmd.make (Cmd.info "tag" ~doc ~exits ~man) @@
    Term.(const Tag.cmd $ version $ commit $ msg $ sign $ force $ delete $
          dry_run $ packs $ x_packs)
  in
  let status =
    let doc = "List commits for the next release" in
    let man =
      [ `S Cmdliner.Manpage.s_description;
        `P "$(iname) looks up the VCS repositories in the scope directories \
            of given packs and for each of them outputs the list of commits \
            that would get in the next release. This is any commit after \
            the greatest release tag in the repository.";
        `Blocks man ]
    in
    let after =
      let doc =
        "Commit-ish $(docv) after which commits are considered. \
         Defaults to the latest VCS tag of the form \
         $(b,[v]X.Y[.Z][\\(~|+\\)info]) or, if none, to the first commit \
         reachable from $(b,--last)."
      in
      let docv = "COMMIT-ISH" in
      Arg.(value & opt (some string) None & info ["after"] ~doc ~docv)
    in
    let last =
      let doc = "Last commit-ish $(docv) considered." in
      let docv = "COMMIT-ISH" in
      Arg.(value & opt string B0_vcs_repo.head & info ["last"] ~doc ~docv)
    in
    let exits =
      (Cmd.Exit.info 0 ~doc:"when changes have been detected.") ::
      (Cmd.Exit.info 1 ~doc:"when no changes have been detected.") ::
      B0_std_cli.Exit.infos
    in
    let pager_don't = B0_pager.don't () in
    let envs = B0_pager.Env.infos in
    Cmd.make (Cmd.info "status" ~doc ~man ~exits ~envs) @@
    Term.(const Status.cmd $ after $ last $ packs $ x_packs $ pager_don't)
  in
  let man =
    [ `S Cmdliner.Manpage.s_description;
      `P "$(iname) helps to release your software. For now these tools are \
          mostly useful for releasing software as sources.";
      `Blocks man ]
  in
  let name = B0_unit.name u in
  Cmd.group (Cmd.info name ~doc ~man) @@ [ archive; tag; status ]

let () = B0_scope.close ()
