(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let parse_changes lines =
  try
    let parse_line l = match String.cut_left ~sep:" " l with
    | None -> Fmt.failwith "%S: can't parse log line" l
    | Some cut -> cut
    in
    Ok (List.(rev @@ rev_map parse_line lines))
  with Failure e -> Error e

let parse_ptime ptime = try Ok (int_of_string ptime) with
| Failure _ -> Fmt.error "Could not parse timestamp from %S" ptime

let parse_files ~err o =
  let file_of_string l = Fpath.of_string l |> Result.to_failure in
  try Ok (List.map file_of_string (String.cuts_left ~sep:"\n" o)) with
  | Failure e -> Fmt.error "%s: %s" err e

let dirtify id = id ^ "-dirty"

(* VCS *)

type commit_ish = string
type commit_id = string
type tag = string

type kind = Git | Hg
let pp_kind ppf k = Fmt.string ppf (match k with Git -> "git" | Hg -> "hg")

type r =
  { kind : kind;
    cmd : Cmd.t;
    repo_dir : Fpath.t;
    work_dir : Fpath.t }

module type VCS = sig
  val find : ?dir:Fpath.t -> unit -> (r option, string) result
  val repo_cmd : r -> Cmd.t

  (* Commits *)

  val commit_id :
    r -> dirty_mark:bool -> commit_ish -> (commit_id, string) result

  val commit_ptime_s : r -> commit_ish -> (int, string) result
  val describe : r -> dirty_mark:bool -> commit_ish -> (string, string) result
  val changes :
    r -> after:commit_ish -> until:commit_ish ->
    ((string * string) list, string) result

  val tracked_files : r -> tree_ish:string -> (Fpath.t list, string) result
  val commit_files :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo ->
    ?msg:string -> r -> Fpath.t list -> (unit, string) result

  (* Working directory *)

  val is_dirty : r -> (bool, string) result
  val file_is_dirty : r -> Fpath.t -> (bool, string) result
  val checkout : ?and_branch:string -> r -> commit_ish -> (unit, string) result
  val local_clone : r -> dir:Fpath.t -> (r, string) result

  (* Tags *)

  val tags : r -> (tag list, string) result
  val tag :
    ?msg:string -> r ->  force:bool -> sign:bool -> commit_ish -> tag ->
    (unit, string) result

  val delete_tag : r -> tag -> (unit, string) result
  val latest_tag : r -> commit_ish -> (tag option, string) result
end

type t = r * (module VCS)

let err cmd status = Fmt.error "%a" Os.Cmd.pp_cmd_status (cmd, status)
let run ?stderr r cmd_args =
  let vcs = Cmd.(r.cmd %% cmd_args) in
  Result.bind (Os.Cmd.run_status_out ?stderr ~trim:true vcs) @@ function
  | `Exited 0, v -> Ok v
  | status, _ -> err vcs status

let run_status ?stdout ?stderr r cmd_args =
  let vcs = Cmd.(r.cmd %% cmd_args) in
  Result.bind (Os.Cmd.run_status ?stdout ?stderr vcs) @@ function
  | `Exited 0 -> Ok ()
  | status -> err vcs status

(* Git support *)

module Git_vcs = struct
  let git = lazy (Os.Cmd.find (Cmd.atom "git"))
  let repo_cmd git repo_dir work_dir =
    Cmd.(git % "--git-dir" %% path repo_dir % "--work-tree" %% path work_dir)

  let find ?dir () =
    let get_vcs_path cmd =
      let stderr = `Stdo Os.Cmd.out_null in
      let* status, repo_dir = Os.Cmd.run_status_out ~stderr ~trim:true cmd in
      match status with
      | `Exited 0 ->
          if repo_dir = "" (* that seems to be returned on bare repos *)
          then Ok None
          else Result.map Option.some (Fpath.of_string repo_dir)
      | status -> Ok None
    in
    let* git = Lazy.force git in
    match git with
    | None -> Ok None
    | Some git ->
        let cwd = match dir with
        | Some d -> Cmd.(atom "-C" %% path d)
        | None -> Cmd.empty
        in
        let repo_dir = Cmd.(git %% cwd % "rev-parse" % "--absolute-git-dir") in
        let work_dir = Cmd.(git %% cwd % "rev-parse" % "--show-toplevel") in
        let* repo_dir = get_vcs_path repo_dir in
        match repo_dir with
        | None -> Ok None
        | Some repo_dir ->
            let* work_dir = get_vcs_path work_dir in
            let default = Fpath.null (* for bare repos *) in
            let work_dir = Option.value ~default work_dir in
            let cmd = repo_cmd git repo_dir work_dir in
            Ok (Some { kind = Git; cmd; repo_dir; work_dir })

  let repo_cmd r = r.cmd
  let work_tree r = Cmd.(atom "--work-tree" %% path (Fpath.parent r.repo_dir))

  let is_dirty r =
    let stderr = `Stdo Os.Cmd.out_null in
    let status = Cmd.(work_tree r % "status" % "--porcelain") in
    Result.bind (run ~stderr r status) @@ function
    | "" -> Ok false | _ -> Ok true

  let handle_dirt ~dirty_mark commit_ish r id =
    match dirty_mark && commit_ish = "HEAD" with
    | false -> Ok id
    | true ->
        Result.bind (is_dirty r) @@ function
        | true -> Ok (dirtify id) | false -> Ok id

  (* Commits *)

  let commit_id r ~dirty_mark commit_ish =
    let typed_commit_ish = Fmt.str "%s^{commit}" commit_ish in
    let args = Cmd.(atom "rev-parse" % "--verify" % typed_commit_ish) in
    Result.bind (run r args) (handle_dirt ~dirty_mark commit_ish r)

  let commit_ptime_s r commit_ish =
    let args = Cmd.(atom "show" % "-s" % "--format=%ct" % commit_ish) in
    Result.bind (run r args) parse_ptime

  let changes r ~after ~until =
    let range = if after = "" then until else Fmt.str "%s..%s" after until in
    let args = Cmd.(atom "log" % "--oneline" % "--no-decorate" % range) in
    Result.bind (run r args) @@ fun o ->
    parse_changes (String.cuts_left ~sep:"\n" o)

  let tracked_files r ~tree_ish =
    let args = Cmd.(atom "ls-tree" % "--name-only" % "-r" % tree_ish) in
    Result.bind (run r args) (parse_files ~err:"tracked files")

  let commit_files ?stdout ?stderr ?msg:m r files =
    let msg = match m with None -> Cmd.empty | Some m -> Cmd.(atom "-m" % m) in
    let args = Cmd.(atom "commit" %% msg %% paths files) in
    run_status ?stdout ?stderr r args

  (* Working directory *)

  let file_is_dirty r file =
    let stderr = Os.Cmd.out_null in
    let cmd =
      Cmd.(r.cmd %% work_tree r % "diff-index" % "--quiet" % "HEAD" %%
           path file)
    in
    Result.bind (Os.Cmd.run_status ~stderr cmd) @@ function
    | `Exited 0 -> Ok false
    | `Exited 1 -> Ok true
    | _ as status -> err cmd status

  let checkout ?and_branch:b r commit_ish =
    let b = match b with None -> Cmd.empty | Some b -> Cmd.(atom "-b" % b) in
    let work_tree = work_tree r in
    let args = Cmd.(work_tree % "checkout" % "--quiet" %% b % commit_ish) in
    run_status r args

  let local_clone r ~dir =
    let args = Cmd.(atom "clone" % "--local" %% path r.repo_dir %% path dir) in
    Result.bind (run_status r args) @@ fun () ->
    Result.bind (find ~dir ()) @@ function
    | Some r -> Ok r
    | None -> Fmt.error "%a: no clone found" Fpath.pp_quoted dir

  (* Tags *)

  let tags r =
    let args = Cmd.(atom "tag" % "--list") in
    Result.bind (run r args) @@ fun o -> Ok (String.cuts_left ~sep:"\n" o)

  let tag ?msg:m r ~force ~sign commit_ish tag =
    let msg = match m with None -> Cmd.empty | Some m -> Cmd.(atom "-m" % m) in
    let flags = Cmd.(if' force (atom "-f") %% if' sign (atom "-s")) in
    let args = Cmd.(atom "tag" % "-a" %% flags %% msg % tag % commit_ish) in
    run_status r args

  let delete_tag r tag =
    let args = Cmd.(atom "tag" % "-d" % tag) in
    run_status r args

  let describe r ~dirty_mark commit_ish =
    let args = Cmd.(atom "describe" % "--always" % commit_ish) in
    Result.bind (run r args) (handle_dirt ~dirty_mark commit_ish r)

  let latest_tag r commit_ish =
    let stderr = `Stdo Os.Cmd.out_null in
    let args = Cmd.(atom "describe" % "--abbrev=0" % commit_ish) in
    let cmd = Cmd.(r.cmd %% args) in
    Result.bind (Os.Cmd.run_status_out ~stderr ~trim:true cmd) @@ function
    | `Exited 0,  v -> Ok (Some v)
    | `Exited 128, _ -> Ok None
    | status, _  -> err cmd status
end

(* Hg support *)

module Hg_vcs = struct
  let hg = lazy (Os.Cmd.find (Cmd.atom "hg"))
  let repo_cmd hg repo_dir = Cmd.(hg % "--repository" %% path repo_dir)

  let find ?dir () = match dir with
  | Some dir ->
      begin
        let work_dir = dir in
        let repo_dir = Fpath.(work_dir / ".hg") in
        Result.bind (Os.Dir.exists repo_dir) @@ function
        | false -> Ok None
        | true ->
            Result.bind (Lazy.force hg) @@ function
            | Some hg ->
                Ok (Some { kind = Hg; cmd = repo_cmd hg repo_dir; repo_dir;
                           work_dir})
            | None ->
              Fmt.error "%a: repo found but no hg executable in PATH"
                Fpath.pp_quoted repo_dir
      end
  | None ->
      Result.bind (Lazy.force hg) @@ function
      | None -> Ok None
      | Some hg ->
          let hg_root = Cmd.(hg % "root") in
          let stderr = `Stdo Os.Cmd.out_null in
          let res = Os.Cmd.run_status_out ~stderr ~trim:true hg_root in
          Result.bind res @@ function
          | `Exited 0, repo_dir ->
              Result.bind (Fpath.of_string repo_dir) @@ fun repo_dir ->
              let work_dir = Fpath.parent repo_dir in
              Ok (Some { kind = Hg; cmd = repo_cmd hg repo_dir; repo_dir;
                         work_dir })
          | _ -> Ok None

  let repo_cmd r = r.cmd
  let revision commit_ish = match commit_ish with "HEAD" -> "tip" | c -> c

  let id r ~rev =
    let* id = run r Cmd.(atom "id" % "-i" % "--rev" % rev) in
    let len = String.length id in
    let is_dirty = String.length id > 0 && id.[len - 1] = '+' in
    let id = if is_dirty then String.sub id 0 (len - 1) else id in
    Ok (id, is_dirty)

  let is_dirty r =
    Result.bind (id r ~rev:"tip") @@ function (_, is_dirty) -> Ok is_dirty

  let handle_dirt ~dirty_mark commit_ish r id =
    match dirty_mark && commit_ish = "HEAD" with
    | false -> Ok id
    | true ->
        Result.bind (is_dirty r) @@ function
        | true -> Ok (dirtify id) | false -> Ok id

  (* Commits *)

  let commit_id r ~dirty_mark commit_ish =
    let rev = revision commit_ish in
    Result.bind (id r ~rev) @@ fun (id, _) ->
    handle_dirt ~dirty_mark commit_ish r id

  let commit_ptime_s r commit_ish =
    let rev = revision commit_ish in
    let date = "{date(date, \"%s\")}" in
    let args = Cmd.(atom "log" % "--template" % date % "--rev" % rev) in
    Result.bind (run r args) parse_ptime

  let changes r ~after ~until =
    let after = revision after and until = revision until in
    let rev = Fmt.str "%s::%s" after until in
    let template = "{node|short} {desc|firstline}\\n" in
    let args = Cmd.(atom "log" % "--template" % template % "--rev" % rev) in
    Result.bind (run r args) @@ fun o ->
    Result.bind (parse_changes (String.cuts_left ~sep:"\n" o)) @@ function
    | [] -> Ok []
    | after :: rest -> Ok (List.rev rest) (* hg order is reverse from git *)

  let tracked_files r ~tree_ish =
    let rev = revision tree_ish in
    let args = Cmd.(atom "manifest" % "--rev" % rev) in
    Result.bind (run r args) (parse_files ~err:"tracked files")

  let commit_files ?stdout ?stderr ?msg:m r files =
    let msg = match m with None -> Cmd.empty | Some m -> Cmd.(atom "-m" % m) in
    let args = Cmd.(atom "commit" %% msg %% paths files) in
    run_status ?stdout ?stderr r args

  (* Working directory *)

  let file_is_dirty r file =
    let args = Cmd.(atom "status" %% path file) in
    Result.bind (run r args) @@ function "" -> Ok false | _ -> Ok true

  let checkout ?and_branch:branch r commit_ish =
    let rev = revision commit_ish in
    let args = Cmd.(atom "update" % "--rev" % rev) in
    Result.bind (run r args) @@ fun _ ->
    match branch with
    | None -> Ok ()
    | Some branch ->
        let args = Cmd.(atom "branch" % branch) in
        Result.bind (run r args) @@ fun _ -> Ok ()

  let local_clone r ~dir =
    let args = Cmd.(atom "clone" %% path r.repo_dir %% path dir) in
    Result.bind (run_status r args) @@ fun _ ->
    Result.bind (find ~dir ()) @@ function
    | Some r -> Ok r
    | None -> Fmt.error "%a: no clone found" Fpath.pp_quoted dir

  (* Tags *)

  let tags r =
    let args = Cmd.(atom "tags" % "--quiet" (* sic *)) in
    Result.bind (run r args) @@ fun o -> Ok (String.cuts_left ~sep:"\n" o)

  let tag ?msg:m r ~force ~sign commit_ish tag =
    if sign then Error "Tag signing is not supported by hg" else
    let rev = revision commit_ish in
    let msg = match m with None -> Cmd.empty | Some m -> Cmd.(atom "-m" % m) in
    let may_force = Cmd.(if' force (atom "-f")) in
    let args = Cmd.(atom "tag" %% may_force %% msg % "--rev" % rev % tag) in
    run_status r args

  let delete_tag r tag =
    let args = Cmd.(atom "tag" % "--remove" % tag) in
    run_status r args

  let describe r ~dirty_mark commit_ish =
    let get_distance s = try Ok (int_of_string s) with
    | Failure _ ->
        Fmt.error "%a: Could not parse hg tag distance."
          Fpath.pp_quoted r.repo_dir
    in
    let rev = revision commit_ish in
    let parent t = Cmd.(atom "parent" % "--rev" % rev % "--template" % t) in
    Result.bind (run r (parent "{latesttagdistance}")) @@ fun dist ->
    Result.bind (get_distance dist) @@ fun dist ->
    let template = match dist with
    | 1 -> "{latesttag}"
    | n -> "{latesttag}-{latesttagdistance}-{node|short}"
    in
    Result.bind (run r (parent template)) (handle_dirt ~dirty_mark commit_ish r)

  let latest_tag r commit_ish =
    let rev = revision commit_ish in
    let args = Cmd.(atom "parent" % "--rev" % rev % "--template" %
                    "{latesttag}")
    in
    Result.map Option.some (run r args)
end

let kind (r, _) = r.kind
let repo_dir (r, _) = r.repo_dir
let work_dir (r, _) = r.work_dir
let repo_cmd (r, (module Vcs : VCS)) = Vcs.repo_cmd r
let pp ppf (r, _) =
  Fmt.pf ppf "(%a, %a)" pp_kind r.kind Fpath.pp_quoted r.repo_dir

(* Finding reposistories *)

let find ?dir () = Result.bind (Git_vcs.find ?dir ()) @@ function
| Some r -> Ok (Some (r, (module Git_vcs : VCS)))
| None ->
    Result.bind (Hg_vcs.find ()) @@ function
    | Some r -> Ok (Some (r, (module Hg_vcs : VCS)))
    | None -> Ok None

let get ?dir () = Result.bind (find ?dir ()) @@ function
| Some r -> Ok r
| None ->
    let dir = match dir with None -> Os.Dir.cwd () | Some dir -> Ok dir in
    Result.bind dir @@ (Fmt.error "%a: No VCS repository found" Fpath.pp_quoted)

(* Commits *)

let head = "HEAD"
let commit_id (r, (module Vcs : VCS)) = Vcs.commit_id r
let commit_ptime_s (r, (module Vcs : VCS)) = Vcs.commit_ptime_s r
let changes (r, (module Vcs : VCS)) = Vcs.changes r
let tracked_files (r, (module Vcs : VCS)) = Vcs.tracked_files r
let commit_files ?stdout ?stderr ?msg (r, (module Vcs : VCS)) =
  Vcs.commit_files ?stdout ?stderr ?msg r

(* Working directory *)

let is_dirty (r, (module Vcs : VCS)) = Vcs.is_dirty r
let not_dirty r = Result.bind (is_dirty r) @@ function
| false -> Ok ()
| true -> Error "The VCS repository is dirty, commit or stash your changes."

let file_is_dirty (r, (module Vcs : VCS)) = Vcs.file_is_dirty r
let checkout ?and_branch (r, (module Vcs : VCS)) = Vcs.checkout ?and_branch r
let local_clone (r, (module Vcs : VCS)) ~dir =
  Result.bind (Vcs.local_clone r ~dir) @@ fun r -> Ok (r, (module Vcs : VCS))

(* Tags *)

let tags (r, (module Vcs : VCS)) = Vcs.tags r
let tag ?msg (r, (module Vcs : VCS)) = Vcs.tag ?msg r
let delete_tag (r, (module Vcs : VCS)) = Vcs.delete_tag r
let describe (r, (module Vcs : VCS)) = Vcs.describe r
let latest_tag (r, (module Vcs : VCS)) = Vcs.latest_tag r

(* Git specific *)

module Git = struct
  let get_cmd ?search ?(cmd = Cmd.atom "git") () = Os.Cmd.get ?search cmd
  let find ?dir () =
    let* vcs = Git_vcs.find ?dir () in
    Ok (Option.map (fun r -> r, (module Git_vcs : VCS)) vcs)

  let check_kind (r, _) = match r.kind with
  | Hg -> Fmt.error "%a: not a git repository" Fpath.pp_quoted r.repo_dir
  | Git -> Ok ()

  (* Branches *)

  type remote = string
  type branch = string

  let pp_branch = Fmt.tty_string [`Fg `Green; `Bold;]
  let pp_remote_branch =
    Fmt.tty [`Fg `Red; `Bold] (fun ppf (r, b) -> Fmt.pf ppf "%s/%s" r b)

  let remote_branch_exists ?env (r, _) ~remote ~branch =
    let ui = Cmd.(atom "--exit-code" % "--quiet") in
    let stdout = Os.Cmd.out_null (* not so quiet *) in
    let ls_remote = Cmd.(r.cmd % "ls-remote" %% ui % remote % branch) in
    Result.bind (Os.Cmd.run_status ?env ~stdout ls_remote) @@ function
    | `Exited 0 -> Ok true
    | `Exited 2 -> Ok false
    | status -> Fmt.error "%a" Os.Cmd.pp_cmd_status (ls_remote, status)

  let remote_branch_fetch ?env ?stdout ?stderr (r, _) ~remote ~branch =
    let fetch = Cmd.(r.cmd % "fetch" % remote % branch) in
    Os.Cmd.run ?env ?stdout ?stderr fetch

  let remote_branch_push ?env ?stdout ?stderr (r, _) ~force ~src ~remote ~dst =
    let refspec = Fmt.str "%s:%s" src dst in
    let force = Cmd.(if' force (atom "--force")) in
    let push = Cmd.(r.cmd % "push" %% force % remote % refspec) in
    Os.Cmd.run ?env ?stdout ?stderr push

  let remote_branch_delete ?env ?stdout ?stderr (r, _) ~force ~remote ~branch =
    let force = Cmd.(if' force (atom "--force")) in
    let push = Cmd.(r.cmd % "push" % "--delete" %% force % remote % branch) in
    Os.Cmd.run ?env ?stdout ?stderr push

  let branch_delete ?env ?stdout ?stderr (r, _) ~force ~branch =
    let force = Cmd.(if' force (atom "--force")) in
    let del = Cmd.(r.cmd % "branch" % "-d" %% force % branch) in
    Os.Cmd.run ?env ?stdout ?stderr del

  (* Transient checkouts *)

  let transient_checkout ?stdout ?stderr (r, _) ~force ~branch dir = function
  | Some cish ->
      let b = if force then "-B" else "-b" in
      let add = Cmd.(atom "worktree" % "add" % b % branch %% path dir % cish) in
      let* () = Os.Cmd.run ?stdout ?stderr Cmd.(r.cmd %% add) in
      get ~dir ()
  | None ->
      (* worktree add has no --orphan option... The following is a contrived
         way of getting an empty branch. *)
      let add = Cmd.(r.cmd % "worktree" % "add" % "--detach" %% path dir) in
      let* () = Os.Cmd.run ?stdout ?stderr add in
      let* repo = get ~dir () in
      let orphan = Cmd.(r.cmd % "checkout" % "--orphan" % branch) in
      let* () = Os.Cmd.run ?stdout ?stderr orphan in
      let* () = Os.Cmd.run ?stdout ?stderr Cmd.(r.cmd % "rm" % "-rf" % ".") in
      Ok repo

  let transient_checkout_delete ?stdout ?stderr (r, _) ~force =
    let force = Cmd.(if' force (atom "--force")) in
    let rem = Cmd.(atom "worktree" % "remove" %% force %% path r.work_dir) in
    Os.Cmd.run ?stdout ?stderr Cmd.(r.cmd %% rem)

  let with_transient_checkout ?stdout ?stderr ?dir r ~force ~branch cish f =
    let* dir = match dir with None -> Os.Path.tmp () | Some d -> Ok d in
    let* tr = transient_checkout r ?stdout ?stderr ~force ~branch dir cish in
    let cleanup () =
      Log.if_error ~use:() (transient_checkout_delete ?stdout ?stderr tr ~force)
    in
    try
      Os.Exit.on_sigint ~hook:cleanup @@ fun () ->
      let v = f tr in
      (cleanup (); Ok v)
    with e -> cleanup (); raise e

  (* Working dir *)

  let add (r, _) ~force fs =
    let force = Cmd.(if' force (atom "--force")) in
    let add = Cmd.(r.cmd % "add" %% force % "--" %% paths fs) in
    Os.Cmd.run add

  let has_staged_changes (r, _) =
    let diff = Cmd.(r.cmd % "diff" % "--exit-code"% "--quiet" % "--staged") in
    Result.bind (Os.Cmd.run_status diff) @@ function
    | `Exited 0 -> Ok false
    | `Exited 1 -> Ok true
    | status -> Fmt.error "%a" Os.Cmd.pp_cmd_status (diff, status)

  let commit
      ?stdout ?stderr ?(sign = false) ?(reset_author = false) ?(amend = false)
      ?msg:m (r, _)
    =
    let sign = Cmd.(if' sign (atom "--signoff")) in
    let reset_author = Cmd.(if' reset_author (atom "--reset-author")) in
    let amend = Cmd.(if' amend (atom "--amend")) in
    let msg = match m with None -> Cmd.empty | Some m -> Cmd.(atom "-m" % m) in
    let commit = Cmd.(atom "commit" %% sign %% reset_author %% amend %% msg) in
    Os.Cmd.run ?stdout ?stderr Cmd.(r.cmd %% commit)

  let commit_exists (r, _) cish =
    let reflog = Cmd.(r.cmd % "reflog" % "exists" % cish) in
    Result.bind (Os.Cmd.run_status reflog) @@ function
    | `Exited 0 -> Ok true
    | `Exited 1 -> Ok false
    | status -> Fmt.error "%a" Os.Cmd.pp_cmd_status (reflog, status)

  let rm ?stdout ?stderr (r, _) ~force ~recurse ~ignore_unmatch files =
    let force = Cmd.(if' force (atom "--force")) in
    let recurse = Cmd.(if' recurse (atom "-r")) in
    let ign = Cmd.(if' ignore_unmatch (atom "--ignore-unmatch")) in
    let rm = Cmd.(r.cmd % "rm" %% force %% recurse %% ign %% paths files) in
    Os.Cmd.run ?stdout ?stderr rm
end

module Hg = struct
  let get_cmd ?search ?(cmd = Cmd.atom "hg") () = Os.Cmd.get ?search cmd
  let find ?dir () =
    let* vcs = Hg_vcs.find ?dir () in
    Ok (Option.map (fun r -> r, (module Hg_vcs : VCS)) vcs)
end



(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
