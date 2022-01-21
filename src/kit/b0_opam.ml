(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let () = B0_def.Scope.lib "opam"

let collect_results rs =
  let rec loop ok err = function
  | Ok o :: vs -> loop (o :: ok) err vs
  | Error e :: vs -> loop ok (e :: err) vs
  | [] -> if err = [] then Ok (List.rev ok) else Error (String.concat "\n" err)
  in
  loop [] [] rs

let list_iter_stop_on_error f vs =
  let rec loop f = function
  | [] -> Ok ()
  | v :: vs -> match f v with Error _ as e -> e | Ok () -> loop f vs
  in
  loop f vs

(* [opam] tool *)

let get_cmd ?search ?(cmd = Cmd.atom "opam") () = Os.Cmd.get ?search cmd
let opam = lazy (get_cmd ())

(* [opam] files *)

module File = struct

  (* Generic representation *)

  type value =
    [ `Raw of string | `B of bool | `S of string | `L of bool * value list ]

  type field = string * value
  type section = string * string option * t
  and comment = string
  and item = [ `Comment of comment | `Field of field | `Section of section ]
  and t = item list
  let v2 = `Field ("opam-version", `S "2.0")

  (* Formatting *)

  let escape =
    let char_len = function '"' -> 2 | _ -> 1 in
    let set_char b i = function
    | '"' -> Bytes.set b i '\\'; Bytes.set b (i+1) '"'; i + 2
    | c -> Bytes.set b i c; i + 1
    in
    String.byte_escaper char_len set_char

  let pp ppf file =
    let rec pp_value ppf = function
    | `Raw r -> Fmt.string ppf r
    | `B b -> Fmt.bool ppf b
    | `S s -> Fmt.pf ppf "\"%s\"" (escape s)
    | `L (vert, vs) ->
        let box = if vert then Fmt.vbox else Fmt.hovbox in
        let pp_list ppf vs = Fmt.pf ppf "[%a]" Fmt.(list ~sep:sp pp_value) vs in
        (box ~indent:1 pp_list) ppf vs
    in
    let pp_field ppf (f, v) = Fmt.pf ppf "%s: @[%a@]" f pp_value v in
    let pp_comment = Fmt.(hbox @@ any "# " ++ string) in
    let rec pp_item ppf = function
    | `Comment c -> pp_comment ppf c
    | `Field f -> pp_field ppf f
    | `Section (n, opt, file) ->
        let pp_opt = Fmt.(option (parens string ++ any " " )) in
        Fmt.pf ppf "@[<v2>%s %a{@,%a}@]" n pp_opt opt pp_file file
    and pp_file ppf file = (Fmt.vbox (Fmt.list pp_item)) ppf file in
    pp_file ppf file

  let to_string file =
    let b = Buffer.create (10 * 1024) in
    pp (Format.formatter_of_buffer b) file; Buffer.contents b

  (* Package file generation *)

  type pkg_spec = string * string

  let pp_pkg_spec ppf (n, f) =
    let pp_formula ppf = function "" -> () | f -> Fmt.pf ppf " {%s}" f in
    Fmt.pf ppf "%s%a" n pp_formula f

  let pp_pkg_url ppf (n, u) = Fmt.pf ppf "@[%s %s@]" n u

  let pp_pkg_spec_list = Fmt.vbox @@ Fmt.list pp_pkg_spec
  let pp_pkg_url_list = Fmt.vbox @@ Fmt.list pp_pkg_url

  let name_field =
    let doc = "opam file name: field" in
    B0_meta.Key.v "opam-name" ~doc ~pp_value:Fmt.string

  let file_addendum =
    let doc = "opam file fragment added at the end" in
    B0_meta.Key.v "opam-file-addendum" ~doc ~pp_value:pp

  let build_field =
    let doc = "opam file build: field" in
    B0_meta.Key.v "opam-build" ~doc ~pp_value:Fmt.string

  let conflicts_field =
    let doc = "opam file conflicts: field" in
    B0_meta.Key.v "opam-conflicts" ~doc ~pp_value:pp_pkg_spec_list

  let depends_field =
    let doc = "opam file depends: field" in
    B0_meta.Key.v "opam-depends" ~doc ~pp_value:pp_pkg_spec_list

  let depopts_field =
    let doc = "opam file depopts: field" in
    B0_meta.Key.v "opam-depopts" ~doc ~pp_value:pp_pkg_spec_list

  let install_field =
    let doc = "opam file install: field" in
    B0_meta.Key.v "opam-install" ~doc ~pp_value:Fmt.string

  let pin_depends =
    let doc = "opam file pin-depends: field" in
    B0_meta.Key.v "opam-pin-depends" ~doc ~pp_value:pp_pkg_url_list

  let pkg_of_meta ~with_name m =
    let string field k m acc = match B0_meta.find k m with
    | None -> acc | Some s -> `Field (field, `S s) :: acc
    in
    let string_raw field k m acc = match B0_meta.find k m with
    | None -> acc | Some s -> `Field (field, `Raw s) :: acc
    in
    let string_list ~by_line:l field k m acc = match B0_meta.find k m with
    | None -> acc
    | Some ms -> `Field (field, `L (l, List.map (fun s -> `S s) ms)) :: acc
    in
    let triple_string ~nl field k m acc = match B0_meta.find k m with
    | None -> acc | Some s ->
        let nl = if nl then "\n" else "" in
        let tri = "\"\"\"" in
        `Field (field, `Raw (String.concat "" [tri; nl; s; tri])) :: acc
    in
    let pkg_spec_list field k m acc = match B0_meta.find k m with
    | None -> acc | Some pkgs ->
        let pkg (n, f) = match f with
        | "" -> `S n | f -> `Raw (Fmt.str {|"%s" {%s}|} n f)
        in
        `Field (field, `L (true, List.map pkg pkgs)) :: acc
    in
    let string_pair_list ~by_line:l field k m acc = match B0_meta.find k m with
    | None -> acc | Some ps ->
        let pair (f, s) = `L (false, [`S f; `S s]) in
        `Field (field, `L (l, List.map pair ps)) :: acc
    in
    let fields =
      [ triple_string ~nl:false "synopsis" B0_meta.synopsis;
        string_list ~by_line:true "maintainer" B0_meta.maintainers;
        string_list ~by_line:true "authors" B0_meta.authors;
        string "homepage" B0_meta.homepage;
        string "doc" B0_meta.online_doc;
        string "dev-repo" B0_meta.repo;
        string "bug-reports" B0_meta.issues;
        string_list ~by_line:false "license" B0_meta.licenses;
        string_list ~by_line:false "tags" B0_meta.description_tags;
        pkg_spec_list "depends" depends_field;
        string_pair_list ~by_line:true "pin-depends" pin_depends;
        pkg_spec_list "depopts" depopts_field;
        pkg_spec_list "conflicts" conflicts_field;
        string_raw "build" build_field;
        string_raw "install" install_field;
        triple_string ~nl:true "description" B0_meta.description; ]
    in
    let add_field m acc f = f m acc in
    let start =
      if with_name then add_field m [] (string "name" name_field) else []
    in
    let (fs : item list) = List.fold_left (add_field m) start fields in
    let addendum = Option.value ~default:[] (B0_meta.find file_addendum m) in
    List.rev_append fs addendum
end

(* [opam] metadata *)

let tag = B0_meta.Key.tag "opam" ~doc:"opam related entity"
let pkg_name_of_pack p =
  match B0_meta.find File.name_field (B0_pack.meta p) with
  | Some n -> n
  | None ->
      let n = B0_pack.basename p in
      if not (String.equal n "default") then n else
      match B0_def.scope_dir (B0_pack.def p) with
      | None -> "unknown" (* unlikely, libraries should not do this. *)
      | Some d -> Fpath.basename d

module Meta = struct
  type pkg_spec = string * string
  let build = File.build_field
  let conflicts = File.conflicts_field
  let depends = File.depends_field
  let depopts = File.depopts_field
  let file_addendum = File.file_addendum
  let install = File.install_field
  let name = File.name_field
  let pin_depends = File.pin_depends

  (* Metadata derivation *)

  let synopsis_and_description_of_cmark file =
    let syn_of_title t = (* Get $SYN in "$NAME $SEP $SYN" *)
      let ws = Char.Ascii.is_white and tok c = not @@ Char.Ascii.is_white c in
      let skip = String.lose_left in
      let d = t |> skip ws |> skip tok |> skip ws |> skip tok |> skip ws in
      if d <> "" then Some d else None
    in
    let descr_of_section s =
      (* XXX This is only here because of the person who wrote this.
         This shouldn't exist. *)
      let prefix = "\x25%VERSION%%" in
      if not (String.starts_with ~prefix s) then String.trim s else
      String.trim (String.subrange ~first:(String.length prefix) s)
    in
    let contents = Os.File.read file |> Log.if_error ~use:"" in
    let convert (t, d) = syn_of_title t, descr_of_section d in
    Option.map convert (B00_cmark.first_section ~preamble:true contents)

  let derive_synopsis_and_description p m =
    match B0_meta.(mem synopsis m, mem description m) with
    | true, true -> m
    | has_syn, has_descr ->
        let extracted = match B0_def.scope_dir (B0_pack.def p) with
        | None -> None
        | Some scope_dir ->
            let readme = Fpath.(scope_dir / "README.md") in
            let exists = Os.File.exists readme |> Log.if_error ~use:false in
            if exists then synopsis_and_description_of_cmark readme else None
        in
        match extracted with
        | None -> m
        | Some (syn, d) ->
            let syn = Option.value ~default:"" syn in
            let m = if has_syn then m else B0_meta.(add synopsis syn m) in
            let m = if has_descr then m else B0_meta.(add description d m) in
            m

  let derive_build p m =
    if B0_meta.mem build m then m else
    let b = Fmt.str {|[[ "b0" "--lock" "-p" "%s" ]]|} (B0_pack.basename p) in
    B0_meta.add build b m

  let derive_depends p m =
    if B0_meta.mem depends m then m else
    let unit_pkg_deps u = match B0_unit.find_meta B0_ocaml.Meta.requires u with
    | None -> [] | Some libs ->
        (* FIXME This is a poc and not subtle enough, we should likely
           enrich library name declarations with versions and/or
           package membership. Also this makes dependencies on self at the
           moment. Also we can easily derive {build} and {test} deps by
           looking up unit tags. *)
        List.rev_map (fun l -> B00_ocaml.Lib.Name.last l) libs
    in
    let pkg_deps = List.concat_map unit_pkg_deps (B0_pack.units p) in
    let pkg_deps = List.map (fun n -> n, "") (String.uniquify pkg_deps) in
    B0_meta.add depends pkg_deps m

  let derive_name p m =
    if B0_meta.mem name m then m else B0_meta.add name (pkg_name_of_pack p) m

  let pkg_of_pack p =
    let m = B0_pack.meta p in
    let m = derive_name p m in
    let m = derive_synopsis_and_description p m in
    let m = derive_build p m in
    let m = derive_depends p m in
    m
end

(* Package definition via packs and their lookup. *)

module Pkg = struct
  type t = string * B0_pack.t (* name and defining pack *)
  let name = fst
  let pack = snd
  let pp_name_str = Fmt.code Fmt.string
  let pp_name = Fmt.using fst pp_name_str
  let pp_err ppf pkg = Fmt.pf ppf "Package %a" pp_name pkg
  let pp_err_pack ppf pkg =
    Fmt.pf ppf "Package %a: Pack %a" pp_name pkg B0_pack.pp_name (pack pkg)

  let file ~with_name pkg =
    let meta = Meta.pkg_of_pack (pack pkg) in
    let file = File.v2 :: File.pkg_of_meta ~with_name meta in
    meta, file

  let name_hints ~dom ~unknown =
    let pkg_dom = String.Set.elements dom in
    let add u acc =
      let kind ppf () = Fmt.string ppf "opam package" in
      let hint = Fmt.did_you_mean in
      let pp = Fmt.unknown' ~kind pp_name_str ~hint in
      Fmt.str "@[%a@]" pp (u, String.suggest pkg_dom u) :: acc
    in
    String.concat "\n" (List.rev (String.Set.fold add unknown []))

  let get_list_or_hints pkgs =
    let add_pack p = match B0_pack.has_meta tag p with
    | true -> Some (pkg_name_of_pack p, p) | false -> None
    in
    let packs = List.filter_map add_pack (B0_pack.list ()) in
    if pkgs = [] then Ok packs else
    let dom =
      let add_name acc (n, _) = String.Set.add n acc in
      List.fold_left add_name String.Set.empty packs
    in
    let pkgs = String.Set.of_list pkgs in
    let unknown = String.Set.diff pkgs dom in
    if String.Set.is_empty unknown
    then Ok (List.filter (fun (n, _) -> String.Set.mem n pkgs) packs)
    else Error (name_hints ~dom ~unknown)

  let get_unique_list_or_hints ~constraints pkgs =
    let* constraints = B0_pack.get_list_or_hint constraints in
    let constraints = B0_pack.Set.of_list constraints in
    let* pkg_packs = get_list_or_hints pkgs in
    let by_name =
      let add acc (n, p) = String.Map.add_to_set (module B0_pack.Set) n p acc in
      List.fold_left add String.Map.empty pkg_packs
    in
    let add_unique n set (acc, errs) = match B0_pack.Set.cardinal set with
    | 1 -> (n, B0_pack.Set.choose_opt set |> Option.get) :: acc, errs
    | _ ->
        let set' = B0_pack.Set.inter set constraints in
        match B0_pack.Set.cardinal set' with
        | 1 -> (n, B0_pack.Set.choose_opt set' |> Option.get) :: acc, errs
        | _ ->
            let packs = B0_pack.Set.elements set in
            let err =
              Fmt.str "@[<v>Package %a is defined by packs:@, %a@,\
                       Use option %a to choose a definition.@]"
                pp_name_str n (Fmt.and_enum B0_pack.pp_name) packs
                Fmt.(code string) "-p"
            in
            acc, err :: errs
    in
    match String.Map.fold add_unique by_name ([], []) with
    | pkg_packs, [] -> Ok pkg_packs
    | _, errs -> Error (String.concat "\n" (List.rev errs))

  let write_opam_file pkg ~version file ~dir =
    let file = File.to_string file in
    let fname = match version with
    | None -> name pkg ^ ".opam"
    | Some v -> String.concat "." [name pkg; v]
    in
    Os.File.write ~force:true ~make_path:true Fpath.(dir / fname) file

  let lint_opam_file pkg file =
    let* opam = Lazy.force opam in
    let opam_lint = Cmd.(opam % "lint" % "-") in
    let stdin = Os.Cmd.in_string (File.to_string file) in
    let* e = Os.Cmd.run_status_out ~trim:false ~stdin opam_lint in
    match e with
    | `Exited 0, _ -> Ok ()
    | `Exited 1, out -> Fmt.error "@[<v>opam lint failed with:@,%s@]" out
    | st, _ -> Fmt.error "%a" Os.Cmd.pp_cmd_status (opam_lint, st)
end

(* .opam.list cmdlet *)

let list_cmdlet env pkgs details =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let* pkg_packs = Pkg.get_list_or_hints pkgs in
  match pkg_packs with
  | [] -> Ok B00_cli.Exit.ok
  | ps ->
      let pp_normal ppf pkg =
        Fmt.pf ppf "@[<h>%a %s@]" Pkg.pp_name pkg (B0_pack.name (Pkg.pack pkg))
      in
      let pp_pkg = match details with
      | `Short -> Pkg.pp_name | `Normal | `Long -> pp_normal
      in
      Log.app (fun m -> m "@[<v>%a@]" Fmt.(list pp_pkg) ps);
      Ok B00_cli.Exit.ok

(* .opam.file cmdlet *)

let lint_files pkgs =
  let lint pkg =
    Result.map_error (Fmt.str "%a: %s" Pkg.pp_err_pack pkg) @@
    Pkg.lint_opam_file pkg (snd (Pkg.file ~with_name:false pkg))
  in
  let* _ = collect_results (List.map lint pkgs) in
  Log.app (fun m -> m "%a" (Fmt.tty_string [`Fg `Green]) "Passed.");
  Ok B00_cli.Exit.ok

let gen_files pkgs ~with_name ~dst =
  let gen_file ~with_name ~dst pkg =
    Result.map_error (Fmt.str "%a: %s" Pkg.pp_err_pack pkg) @@
    let* dir = match dst with
    | `Stdout -> Ok None
    | `Dir dir -> Ok (Some dir)
    | `In_scope ->
        match B0_def.scope_dir (B0_pack.def (Pkg.pack pkg)) with
        | Some s -> Ok (Some Fpath.(s / "opam"))
        | None ->
            Fmt.error "can't write opam file, pack %a has no scope."
              B0_pack.pp_name (Pkg.pack pkg)
    in
    let _, file = Pkg.file ~with_name pkg in
    match dir with
    | None -> Log.app (fun m -> m "@[%a@]" File.pp file); Ok ()
    | Some dir -> Pkg.write_opam_file pkg ~version:None file ~dir
  in
  let* () = list_iter_stop_on_error (gen_file ~with_name ~dst) pkgs in
  Ok B00_cli.Exit.ok

let file_cmdlet env constraints pkgs lint dst in_scope no_name =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let action =
    if lint then `Lint else
    if in_scope then `Gen `In_scope else
    match dst with None -> `Gen `Stdout | Some d -> `Gen (`Dir d)
  in
  let* pkgs = match action with
  | `Gen `Stdout | `Lint -> Pkg.get_list_or_hints pkgs
  | _ -> Pkg.get_unique_list_or_hints ~constraints pkgs
  in
  match pkgs with
  | [] ->
      Log.warn (fun m -> m "No opam packages found in B0 root.");
      Ok B00_cli.Exit.ok
  | ps ->
      Log.if_error' ~use:B00_cli.Exit.some_error @@
      match action with
      | `Lint -> lint_files pkgs
      | `Gen dst ->
          let with_name = action = `Gen `Stdout && not no_name in
          gen_files pkgs ~with_name ~dst

(* .opam.publish cmdlet *)

(* Github pull requests

   N.B. though maybe not full general,this is largely independent from
   b0_opam, we could consider moving it to a B00_github.Pr module. *)

module Github_pr = struct
  open B00_serialk_json
  open B00_github

  let info =
    let info url id = url, id in
    Jsonq.(succeed info $ mem "html_url" string $ mem "number" int)

  let head_of_src ~src_repo ~src_branch =
    Fmt.str "%s:%s" (Repo.owner src_repo) src_branch

  let find httpr ~auth ~dst_repo ~dst_branch ~src_repo ~src_branch =
    let params = (* not escaping :-( but these should be US-ASCII ids. *)
      Fmt.str "?state=open&head=%s&base=%s"
        (head_of_src ~src_repo ~src_branch) dst_branch
    in
    let path = Fmt.str "/pulls%s" params in
    let* r = Repo.req_json_v3 httpr auth dst_repo ~path `GET `Empty in
    let* infos = Jsonq.query (Jsonq.array info) r in
    if infos = [] then Ok None else Ok (Some (List.hd infos))

  let update httpr ~auth ~dst_repo ~pr_id ~msg =
    let params = `Json Jsong.(obj |> mem "body" (string msg) |> obj_end) in
    let path = Fmt.str "/pulls/%d" pr_id in
    let* r = Repo.req_json_v3 httpr auth dst_repo ~path `PATCH params in
    Jsonq.query info r

  let create
      httpr ~auth ~dst_repo ~dst_branch ~src_repo ~src_branch ~title ~msg
    =
    let params =
      `Json Jsong.(obj
                   |> mem "title" (string title)
                   |> mem "head" (string (head_of_src ~src_repo ~src_branch))
                   |> mem "base" (string dst_branch)
                   |> mem "body" (string msg)
                   |> mem "maintainer_can_modify" (bool true)
                   |> obj_end)
    in
    let path = "/pulls" in
    let* r = Repo.req_json_v3 httpr auth dst_repo ~path `POST params in
    Jsonq.query info r

  let ensure
      httpr ~auth ~dst_repo ~dst_branch ~src_repo ~src_branch ~title ~msg
    =
    let* dst_repo = Repo.of_url dst_repo in
    let* src_repo = Repo.of_url src_repo in
    let* pr = find httpr ~auth ~dst_repo ~dst_branch ~src_repo ~src_branch in
    let* action, url = match pr with
    | Some (url, pr_id) ->
        (* The branch has already been force pushed which should update the
           contents, we just update the msg *)
        let* url, _id = update httpr ~auth ~dst_repo ~pr_id ~msg in
        Ok ("Updated", url)
    | None ->
        let* url, _id =
          create
            httpr ~auth ~dst_repo ~dst_branch ~src_repo ~src_branch ~title ~msg
        in
        Ok ("Opened", url)
    in
    let pp_action ppf a = Fmt.tty_string [`Fg `Green] ppf a in
    Log.app begin fun m ->
      m "%a pull request %a" pp_action action Fmt.(code string) url
    end;
    Ok ()
end

module Publish = struct
  type info =
    { pkg : Pkg.t;
      version : string;
      file : File.t; (* Without the URL section *)
      file_meta : B0_meta.t;
      changes_file : Fpath.t option;
      changes_latest : (string * string) option;
      url : string; }

  let versioned_name i = Fmt.str "%s.%s" (Pkg.name i.pkg) i.version
  let split_version p = match String.cut_left ~sep:"." p with
  | None -> (p, None) | Some (name, v) -> (name, Some v)

  let warn_miss_changes pkg changes_file changes_latest =
    let warn msg = Log.warn @@ fun m -> m "%a: %s" Pkg.pp_err_pack pkg msg in
    match changes_file with
    | None -> warn "No changelog file found."
    | Some f ->
        match changes_latest with
        | Some _ -> ()
        | None ->
            warn @@
            Fmt.str "@[<v>Could not parse latest changes from:@, %a@]"
              Fpath.pp_unquoted f

  let info_of_pkg (pkg, version) =
    Result.map_error (Fmt.str "%a: %s" Pkg.pp_err_pack pkg) @@
    let* version = match version with
    | Some version -> Ok version
    | None -> B0_release.version_of_pack (Pkg.pack pkg)
    in
    let* file_meta, file =
      let file_meta, file = Pkg.file ~with_name:false pkg in
      let* () = Pkg.lint_opam_file pkg file in
      Ok (file_meta, file)
    in
    let* changes_file = B0_release.changes_file_of_pack (Pkg.pack pkg) in
    let* changes_latest = match changes_file with
    | None -> Ok None | Some f -> B0_release.changes_latest_of_file f
    in
    let* url = B0_release.src_archive_url_of_pack ~version (Pkg.pack pkg) in
    warn_miss_changes pkg changes_file changes_latest;
    Ok { pkg; version; file; file_meta; changes_file; changes_latest; url }

  let add_url_csum csum i =
    let url_section =
      let fields = [`Field ("src", `S i.url); `Field ("checksum", `S csum)] in
      `Section ("url", None, fields)
    in
    (* We patch at the end before the description field, this looks more
       convenient for repo maintainters.  *)
    let file = match List.rev i.file with
    | descr :: rest -> List.rev (descr :: url_section :: rest)
    | _ -> assert false
    in
    { i with file }

  (* Pull request information *)

  let branch_name_of_pkgs is =
    match List.sort compare (List.map versioned_name is) with
    | [vname] -> Fmt.str "b0-publish-%s" vname
    | (vname :: _ as vnames) ->
        let hash = Hash.Xxh_64.string (String.concat " " vnames) in
        Fmt.str "b0-publish-%s-etc-%s" vname (Hash.to_hex hash)
    | [] -> assert false

  let link_issue_numbers pkg s =
    (* Look for #[0-9]+ and link or deactivate them, rough but good enough *)
    let issues_url = B0_meta.find B0_meta.issues pkg.file_meta in
    let link_issue num = match issues_url with
    | None -> Fmt.str "`#%s`" num (* deactivate gh autolink *)
    | Some url -> Fmt.str "[#%s](%s/%s)" num url num
    in
    let rec parse_int ~first i ~max =
      let stop = i > max || not (Char.Ascii.is_digit s.[i]) in
      if stop then String.subrange ~first ~last:(i - 1) s, i else
      parse_int ~first (i + 1) ~max
    in
    let rec loop acc i j max = match j > max with
    | true ->
        let last = String.subrange ~first:i ~last:max s in
        String.concat "" (List.rev (last :: acc))
    | false when s.[j] <> '#' -> loop acc i (j + 1) max
    | false ->
        let k = j + 1 in
        if k > max || not (Char.Ascii.is_digit s.[k]) then loop acc i k max else
        let acc = String.subrange ~first:i ~last:(j - 1) s :: acc in
        let num, k = parse_int ~first:k k ~max in
        let acc = link_issue num :: acc in
        loop acc k k max
    in
    loop [] 0 0 (String.length s - 1)

  let msg_changelogs is = (* Packages with same file share the same notes *)
    let add acc i = match i.changes_file with
    | None -> acc
    | Some file ->
        match i.changes_latest with
        | None -> acc | Some _ -> Fpath.Map.add_to_list file i acc
    in
    let by_file = List.fold_left add Fpath.Map.empty is in
    let with_pkg _ pkgs acc = (* they all have the same notes *)
      let pkg = List.hd pkgs in
      let tit, notes = Option.get ((List.hd pkgs).changes_latest) in
      let notes = link_issue_numbers pkg notes in
      let pkgs = List.map (fun i -> Fmt.str "`%s`" (Pkg.name i.pkg)) pkgs in
      let pkgs = String.concat ", " (List.sort compare pkgs) in
      Fmt.str "\n\n---\n\n#### %s %s\n\n%s" pkgs tit notes :: acc
    in
    List.sort compare (Fpath.Map.fold with_pkg by_file [])

  let msg_links i =
    let pack = Pkg.pack i.pkg in
    let get some k = Option.fold ~none:"" ~some (B0_pack.find_meta k pack) in
    let home = get (Fmt.str "[home](%s)") B0_meta.homepage in
    let doc = get (Fmt.str "[doc](%s)") B0_meta.online_doc in
    let issues = get (Fmt.str "[issues](%s)") B0_meta.issues in
    let links = [home; doc; issues] in
    match List.filter (Fun.negate String.is_empty) links with
    | [] -> "" | ls -> Fmt.str " %s" (String.concat ", " ls)

  let msg_synopsis i = match B0_meta.find B0_meta.synopsis i.file_meta with
  | None -> "\n" | Some syn -> Fmt.str "  \n  *%s*\n" syn

  let msg_title is =
    let versioned_names = List.map versioned_name is in
    "Add: " ^ String.concat ", " versioned_names

  let msg_update_cmd is incompats =
    let vnames = String.concat " " (List.map versioned_name is) in
    let incompats = match List.sort compare incompats with
    | [] -> "" | is -> String.concat "" (List.map (( ^ ) " -i ") is)
    in
    Fmt.str "\n\n---\n\n\
             Use `b0 cmd -- .opam.publish %s%s` to update the pull request."
      vnames incompats

  let msg_for_publish is incompats =
    let title = msg_title is in
    let add_line i =
      Fmt.str "* Add: `%s`%s%s"
        (versioned_name i) (msg_links i) (msg_synopsis i)
    in
    let adds = List.map add_line is in
    let incompats_line = match incompats with
    | [] -> ""
    | incompats ->
        let incompats = List.sort compare incompats in
        let incompats = List.map (Fmt.str "`%s`") incompats in
        Fmt.str "* Incompatible: %s\n" (String.concat ", " incompats)
    in
    let changes = msg_changelogs is in
    let update_cmd = msg_update_cmd is incompats in
    title, String.concat "" (adds @ [incompats_line] @ changes @ [update_cmd])

  let shasum_algo = "512"
  let checksum shasum s =
    let stdin = Os.Cmd.in_string s in
    let shasum = Cmd.(shasum % "-b" % "-a" % shasum_algo % "-") in
    let* csum = Os.Cmd.run_out ~trim:true ~stdin shasum in
    match String.cut_left ~sep:" " csum with
    | None -> Ok csum | Some (csum, _) -> Ok csum

  let add_url_checksums httpr shasum ~check_only is =
    let open B00_http in
    let add csum i = add_url_csum (Fmt.str "sha%s=%s" shasum_algo csum) i in
    let add_url (acc, is) i = match String.Map.find_opt i.url acc with
    | Some csum -> acc, add csum i :: is
    | None ->
        let meth = if check_only then `HEAD else `GET in
        let req = Http.req ~uri:i.url meth in
        let csum =
          Result.to_failure @@
          Result.map_error (Fmt.str "%a: %s" Pkg.pp_err i.pkg) @@
          let* resp = Httpr.perform httpr req in
          match Http.resp_status resp with
          | 200 -> checksum shasum (Http.resp_body resp)
          | c -> Fmt.error "[%a] %s" Fmt.(tty [`Fg `Red] int) c i.url
        in
        String.Map.add i.url csum acc, add csum i :: is
    in
    match List.fold_left add_url (String.Map.empty, []) is with
    | exception Failure e -> Error e
    | _, is -> Ok (List.rev is)

  let log_start is incs check_only =
    let pp_add ppf () = Fmt.tty_string [`Fg `Green] ppf "Add:" in
    let pp_inc ppf () = Fmt.tty_string [`Fg `Yellow] ppf "Incompatible:" in
    let pp_info ppf i =
      Fmt.pf ppf "@[<v>%a %a@,     %s@]"
        pp_add () Fmt.(code string) (versioned_name i) i.url
    in
    let pp_incs ppf = function
    | [] -> () | incs ->
        Fmt.pf ppf "@,@[%a @[%a@]@]" pp_inc () Fmt.(list ~sep:sp string) incs
    in
    let pp_download ppf check_only =
      let action = if check_only then "Checking" else "Downloading" in
      Fmt.pf ppf "@,%s archives…" action
    in
    Log.app @@ fun m ->
    m "@[<v>%a%a%a@]" Fmt.(list pp_info) is pp_incs incs pp_download check_only

  let log_check_success () = Log.app @@ fun m ->
    m "%a" (Fmt.tty_string [`Fg `Green]) "All checks succeeded"

  let stdout_logging () =
    (* Bof bof, maybe we should rather have something in B00_vcs.t *)
    if Log.level () >= Log.Info then None, None else
    Some Os.Cmd.out_null, Some Os.Cmd.out_null

  let rec get_updated_local_repo git ~pkgs_repo ~local_repo:dir =
    let stdout, stderr = stdout_logging () in
    let master = "master" in
    let* local = B00_vcs.Git.find ~dir () in
    match local with
    | Some repo ->
        Log.app (fun m -> m "Updating %a" Fpath.pp_unquoted dir);
        let git = B00_vcs.repo_cmd repo in
        let fetch = Cmd.(atom "fetch" % "origin" % master) in
        let* () = Os.Cmd.run ?stdout ?stderr Cmd.(git %% fetch) in
        Ok repo
    | None ->
        Log.app (fun m -> m "Cloning %s to %a" pkgs_repo Fpath.pp_unquoted dir);
        let clone =
          Cmd.(atom "clone" % "--bare" % "--single-branch" %
               "--branch" % master % pkgs_repo %% path dir)
        in
        let* () = Os.Cmd.run ?stdout ?stderr Cmd.(git %% clone) in
        (* We do that so that we end up in the other branch
           fetch and FETCH_HEAD becomes defined *)
        get_updated_local_repo git ~pkgs_repo ~local_repo:dir

  let commit ~local_repo:repo ~branch ~pkgs_dir is _incs =
    let stdout, stderr = stdout_logging () in
    let fetch_head = Some "FETCH_HEAD" and force = true in
    Log.app (fun m -> m "Branching %s…" branch);
    Result.join @@
    B00_vcs.Git.with_transient_checkout
      ?stdout ?stderr repo ~force ~branch fetch_head
    @@ fun r ->
    let rec add_pkgs ~base_dir acc = function
    | [] -> Ok acc
    | i :: is ->
        let file =
          Fpath.(base_dir / Pkg.name i.pkg / versioned_name i / "opam")
        in
        let contents = File.to_string i.file in
        match Os.File.write ~force:true ~make_path:true file contents with
        | Error _ as e -> e
        | Ok () -> add_pkgs ~base_dir (file :: acc) is
    in
    let base_dir = Fpath.(B00_vcs.work_dir r / pkgs_dir) in
    let* files = add_pkgs ~base_dir [] is in
    (* XXX at that point once we get a usable opam admin add-constraint
       use appropriately with _incs, see
       https://github.com/ocaml/opam/issues/3077 *)
    let msg = msg_title is in
    let* () = B00_vcs.Git.add ?stdout ?stderr r ~force:false files in
    let* () = B00_vcs.commit_files ?stdout ?stderr ~msg r files in
    Ok ()

  let github_auth_env ~github_auth:a =
    let* e = Os.Env.current () in
    let e = Os.Env.add "B0_GITHUB_USER" (B00_github.Auth.user a) e in
    let e = Os.Env.add "B0_GITHUB_TOKEN" (B00_github.Auth.token a) e in
    Ok (Os.Env.to_assignments e)

  let push_branch ~github_auth ~local_repo ~branch ~fork_repo  =
    Log.app (fun m -> m "Pushing %s on %s" branch fork_repo);
    let stdout, stderr = stdout_logging () in
    let repo_cmd = B00_vcs.repo_cmd local_repo in
    let env_creds = (* XXX Windows :-( ? *)
      "!f() { echo \"username=${B0_GITHUB_USER}\n\
                     password=${B0_GITHUB_TOKEN}\"; }; f"
    in
    let creds = Cmd.(repo_cmd % "config" % "credential.helper" % env_creds) in
    let* () = Os.Cmd.run creds in
    let* env = github_auth_env ~github_auth in
    let src = branch and dst = branch in
    let remote = fork_repo in
    B00_vcs.Git.remote_branch_push
      ?stdout ?stderr local_repo ~env ~force:true ~src ~remote ~dst

  let pkgs
      ~pkgs_dir ~pkgs_repo ~fork_repo ~local_repo ~github_auth pkgs incompats
      check_only no_pr
    =
    if incompats <> [] then begin Log.warn @@ fun m ->
       m "@[<v>Incompatibility statements unsupported for now. Ignored.@,\
          See https://github.com/ocaml/opam/issues/3077@]"
    end;
    let* httpr = B00_http.Httpr.get_curl () in
    let* shasum = Os.Cmd.get (Cmd.atom "shasum") in
    let* git = B00_vcs.Git.get_cmd () in
    let* is = collect_results (List.map info_of_pkg pkgs) in
    let title, msg = msg_for_publish is incompats in
    log_start is incompats check_only;
    let* is = add_url_checksums httpr shasum ~check_only is in
    if check_only then (log_check_success (); Ok ()) else
    let* local_repo = get_updated_local_repo git ~pkgs_repo ~local_repo in
    let branch = branch_name_of_pkgs is in
    let* () = commit ~local_repo ~branch ~pkgs_dir is incompats in
    let* () = push_branch ~github_auth ~local_repo ~branch ~fork_repo in
    let* () =
      if no_pr then Ok () else
      let dst_repo = pkgs_repo and dst_branch = "master" in
      let src_repo = fork_repo and src_branch = branch in
      let auth = github_auth in
      Github_pr.ensure
        httpr ~auth ~dst_repo ~dst_branch ~src_repo ~src_branch ~title ~msg
    in
    Ok ()
end

let publish_cmdlet
    env pkgs_dir pkgs_repo fork_repo local_repo github_auth
    constraints pkgs incompats check_only no_pr
  =
  Log.if_error ~use:B00_cli.Exit.no_such_name @@
  let pkgs = List.map Publish.split_version pkgs in
  let* ps = Pkg.get_unique_list_or_hints ~constraints (List.map fst pkgs) in
  match ps with
  | [] ->
      Log.app (fun m -> m "No opam package to publish in B0 root.");
      Ok B00_cli.Exit.ok
  | ps ->
      let add_version p = p, Option.join (List.assoc_opt (Pkg.name p) pkgs) in
      let pkgs = List.map add_version (List.sort compare ps) in
      Log.if_error' ~use:B00_cli.Exit.some_error @@
      let* local_repo = match local_repo with
      | Some d -> Ok d
      | None ->
          let* cache_dir = Os.Dir.cache () in
          Ok (Fpath.(cache_dir / "opam-repository.git"))
      in
      let* github_auth = github_auth in
      let fork_repo = match fork_repo with
      | Some url -> url
      | None ->
          let user = B00_github.Auth.user github_auth in
          Fmt.str "https://github.com/%s/opam-repository" user
      in
      let* () =
        Publish.pkgs ~pkgs_dir ~pkgs_repo ~fork_repo ~local_repo ~github_auth
          pkgs incompats check_only no_pr
      in
      Ok B00_cli.Exit.ok

(* Cmdlets cli interfaces *)

module Cmdlet = struct
  open Cmdliner

  let man =
    [ `S Manpage.s_see_also;
      `P "Consult $(b,odig doc b0) for the B0 opam manual."]

  let pkgs ?(docv = "PKG") ~doc () =
    Arg.(value & pos_all string [] & info [] ~doc ~docv)

  let constraints () =
    let doc =
      "If an opam package is defined by more than one pack use the \
       definition by pack $(docv)."
    in
    B0_cli.Arg.packs ~doc ()

  let list =
    B0_cmdlet.v "list" ~doc:"List opam packages and their defining packs" @@
    fun env args ->
    let pkgs = pkgs ~doc:"Only list opam package $(docv) (repeatable)." () in
    let details = B00_cli.Arg.output_details () in
    let list = Term.(const list_cmdlet $ const env $ pkgs $ details) in
    B0_cmdlet.eval ~man env args list

  let file =
    B0_cmdlet.v "file" ~doc:"Generate opam files" @@
    fun env args ->
    let pkgs =
      let doc = "Only generate file for opam package $(docv) (repeatable)." in
      pkgs ~doc ()
    in
    let dst =
      let doc = "Write files in directory $(docv)." in
      Arg.(value & opt (some ~none:"stdout" B00_cli.fpath) None &
           info ["d"; "dir"] ~doc ~docv:"DIR")
    in
    let in_scope =
      let doc =
        "Write file(s) in an $(b,opam) directory in the scope directories of \
         their defining pack."
      in
      Arg.(value & flag & info ["s"; "in-scope-dir"] ~doc)
    in
    let lint =
      let doc = "Do not generate files, lint them with $(b,opam lint)." in
      Arg.(value & flag & info ["lint"] ~doc)
    in
    let no_name =
      let doc = "On stdout, do not add the $(b,name:) field." in
      Arg.(value & flag & info ["no-name"] ~doc)
    in
    let file =
      Term.(const file_cmdlet $ const env $ constraints () $ pkgs $ lint $ dst $
            in_scope $ no_name)
    in
    B0_cmdlet.eval ~man env args file

  let publish =
    let doc = "Publish opam packages on GitHub" in
    B0_cmdlet.v "publish" ~doc @@
    fun env args ->
    let pkgs_dir =
      let doc = "The directory with the opam files in the package repository."in
      Arg.(value & opt string "packages" & info ["pkgs-dir"] ~doc ~docv:"DIR")
    in
    let pkgs_repo =
      let doc = "The GitHub URL of the package repository."in
      Arg.(value & opt string "https://github.com/ocaml/opam-repository.git" &
           info ["pkgs-repo"] ~doc ~docv:"URL")
    in
    let fork_repo =
      let doc = "The GitHub URL of the user fork of the package repository." in
      let none = "https://github.com/$(i,USER)/opam-repository.git" in
      Arg.(value & opt (some ~none string) None &
           info ["fork-repo"] ~doc ~docv:"URL")
    in
    let local_repo =
      let doc = "The directory to the local (shallow and bare) clone of \
                 the package repository." in
      let none = "$(b,XDG_CACHE_HOME)/opam-repository.git" in
      Arg.(value & opt (some ~none B00_cli.fpath) None &
           info ["local-repo"] ~doc ~docv:"DIR")
    in
    let github_auth = B00_github.Auth.cli () in
    let pkgs =
      let doc = "Only publish opam package $(docv) at version $(i,VERSION)
                 (repeatable)." in
      pkgs ~docv:"PKG[.VERSION]" ~doc ()
    in
    let check_only =
      let doc = "Do not publish. Show various bits that are being \
                 inferred and perform basic checks."
      in
      Arg.(value & flag & info ["c"; "check-only"] ~doc)
    in
    let incompats =
      let doc = "Declare $(docv) to be incompatible with the opam packages \
                 being published (for those which are in their dependencies)."
      in
      let docv = "PKG[.VERSION]" in
      Arg.(value & opt_all string [] & info ["i"; "incompatible"] ~doc ~docv)
    in
    let no_pr =
      let doc = "Do not open (or update) the pull request. All the other \
                 steps, including pushing on the fork repo are done."
      in
      Arg.(value & flag & info ["no-pr"; "no-pull-request"] ~doc)
    in
    let publish =
      Term.(const publish_cmdlet $ const env $ pkgs_dir $ pkgs_repo $
            fork_repo $ local_repo $ github_auth $ constraints () $
            pkgs $ incompats $ check_only $ no_pr)
    in
    let envs = B00_github.Auth.envs in
    B0_cmdlet.eval ~envs ~man env args publish
end

let () = B0_def.Scope.close ()

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
