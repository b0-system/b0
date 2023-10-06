(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Version control system (VCS) repositories.

    The toplevel module abstracts operations over VCS. The
    {!module-Hg} and {!module-Git} modules offer VCS specific
    operations. *)

open B0_std

(** {1:dry_run Dry runs} *)

type dry_run = Cmd.t -> unit
(** The type for dry runs. In dry run for effectful operations, the
    actual command is given to this function and the operation
    succeeds without performing it. *)

(** {1:kind VCS kinds} *)

type kind =
| Git (** {{:https://git-scm.com/}[git]} *)
| Hg (** {{:https://www.mercurial-scm.org/}[hg]} *)
(** The type of VCS supported by the module. *)

val pp_kind : kind Fmt.t
(** [pp_kind] formats kinds of VCS. *)

val kind_to_string : kind -> string
(** [kind_to_string k] is a string for [k]. *)

val kinds : kind list
(** [kinds] is the list of supported VCS. *)

(** {1:repos Repositories} *)

type t
(** The type for VCS repositories. *)

val kind : t -> kind
(** [kind r] is [r]'s VCS kind. *)

val repo_dir : t -> Fpath.t
(** [repo_dir r] is [r]'s repository directory (not the working directory). *)

val work_dir : t -> Fpath.t
(** [work_dir r] is [r]'s working directory. On a git bare repo this
    may be {!B0_std.Fpath.null}. *)

val repo_cmd : t -> Cmd.t
(** [repo_cmd r] is the base command to use to act on [r]. Use only if you
    need VCS specific functionality not provided by the module. See also
    {!module-Git} and {!module-Hg}. *)

val pp : t Fmt.t
(** [pp] formats a repository. This formats {!repo_dir}. *)

val pp_long : t Fmt.t
(** [pp_long] formats a repository. This formats {!val-kind} followed by
    {!repo_dir}. *)

(** {1:get Finding local repositories} *)

val find :
  ?kind:kind -> ?dir:Fpath.t -> unit -> (t option, string) result
(** [find ~dir ()] finds, using VCS functionality, a repository
    starting in directory [dir] (if unspecified this is the [cwd]). If [kind]
    is specified, will only look for a vcs of kind [kind].
*)

val get :
  ?kind:kind -> ?dir:Fpath.t -> unit -> (t, string) result
(** [get] is like {!find} but errors if no VCS was found. *)

(** {1:commits Commits} *)

type commit_ish = string
(** The type for {e symbols} resolving to a commit.

    {b Important}, the module uses {{!head}["HEAD"]} for specifying
    the commit currently checkout in the working directory; use this
    symbol even if the underlying VCS is [Hg]. *)

type commit_id = string
(** The type for commit identifiers.

    {b Note.} The module sometimes appends the string ["-dirty"] to
    these identifiers in which case they are no longer proper
    identifiers. *)

val head : commit_ish
(** [head] is ["HEAD"]. A symbol to represent the commit currently
    checked out in the working directory. *)

val commit_id : t -> dirty_mark:bool -> commit_ish -> (commit_id, string) result
(** [commit_id repo ~dirty_mark ~commit_ish] is the object name
    (identifier) of [commit_ish]. If [commit_ish] is ["HEAD"] and
    [dirty_mark] is [true] and the working tree of [repo] {!is_dirty},
    a mark gets appended to the commit identifier. *)

val commit_ptime_s : t -> commit_ish -> (int, string) result
(** [commit_ptime_s repo commit_ish] is the POSIX time in seconds of
    commit [commit_ish] of repository [repo]. *)

val changes :
  t -> ?after:commit_ish -> ?last:commit_ish -> unit ->
  ((commit_id * string) list, string) result
(** [changes repo ~after ~last] is the list of commits with their
    one-line synopsis from commit-ish [after] (defaults to the start
    of the branch) to commit-ish [last] (defaults to {!head}). *)

val tracked_files : t -> tree_ish:string -> (Fpath.t list, string) result
(** [tracked_files repo ~tree_ish] are the files tracked by the tree
    object [tree_ish]. *)

val commit_files :
  ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo -> ?msg:string -> t ->
  Fpath.t list -> (unit, string) result
(** [commit_files rrepo ~msg files] commits the file [files] with message
    [msg] (if unspecified the VCS should prompt). *)

val pp_commit : commit_ish Fmt.t
(** [pp_commit] formats a commit. *)

(** {1:work_dir Working directory} *)

val is_dirty : t -> (bool, string) result
(** [is_dirty repo] is [Ok true] iff the working directory of [repo] has
    uncommited changes. *)

val not_dirty : t -> (unit, string) result
(** [not_dirty repo] is [Ok ()] iff the working directory of [repo] is not dirty
    and an error that enjoins to stash or commit otherwise. *)

val file_is_dirty : t -> Fpath.t -> (bool, string) result
(** [file_is_dirty repo file] is [Ok true] iff [file] has uncommited changes. *)

val checkout : ?and_branch:string -> t -> commit_ish -> (unit, string) result
(** [checkout repo ~and_branch commit_ish] checks out [commit_ish] in the
    working directory of [repo]. Checks out in a new branch [and_branch]
    if provided. This fails if the current working directory {!is_dirty}. *)

val local_clone : t -> dir:Fpath.t -> (t, string) result
(** [local_clone repo ~dir] clones [repo] to a working directory [dir] and
    returns a repo to operate on it. *)

val pp_dirty : unit Fmt.t
(** [pp_dirty] formats the string ["dirty"]. *)

(** {1:tags Tags} *)

type tag = string
(** The type for VCS tags. *)

val tags : t -> (tag list, string) result
(** [tags repo] is the list of tags in the repo [repo]. *)

val tag :
  ?dry_run:dry_run -> ?msg:string -> t -> force:bool -> sign:bool ->
  commit_ish -> tag -> (unit, string) result
(** [tag repo ~force ~sign ~msg commit_ish t] tags [commit_ish] with [t]
    and message [msg] (if unspecified the VCS should prompt). If
    [sign] is [true] (defaults to [false]) signs the tag ([`Git] repos
    only).  If [force] is [true] (default to [false]) doesn't fail if
    the tag already exists. *)

val delete_tag :
  ?dry_run:dry_run -> t -> tag -> (unit, string) result
(** [delete_tag repo t] deletes tag [t] in [repo]. *)

val describe : t -> dirty_mark:bool -> commit_ish -> (string, string) result
(** [describe repo dirty_mark commit_ish] identifies [commit_ish]
    using tags from [repo]. If [commit_ish] is ["HEAD"] and
    [dirty_mark] is [true] (default) and the working tree of [repo]
    {!is_dirty}, a mark gets appended to the description. *)

val latest_tag : t -> commit_ish -> (tag option, string) result
(** [latest_tag repo commit_ish] finds the latest tag in the current branch
    describing [commit_ish]. *)

val find_greatest_version_tag : t -> (tag option, string) result
(** [find_latest_version_tag repo] lists tags with {!val-tags} sorts
    those who parse with {!B0_std.String.to_version} in increasing order
    and takes the greatest ones. *)

(** {1:vcs VCS specific operations} *)

(** Git specific operations.

    All the following operations assume the repository is of kind [Git].
    Use the repository lookup functions of this module or {!Git.check_kind}
    to assert this first otherwise the operations will fail in non-user
    friendly ways. *)
module Git : sig

  val get_cmd :
    ?search:Fpath.t list -> ?cmd:Cmd.t -> unit -> (Cmd.t, string) result
  (** [cmd ()] looks for [git] with {!B0_std.Os.Cmd.get}. *)

  val find : ?dir:Fpath.t -> unit -> (t option, string) result
  (** [find ~dir ()] finds, using VCS functionality, a git repository
      starting in directory [dir] (if unspecified this is the [cwd]). *)

  val check_kind : t -> (unit, string) result
  (** [check_kind r] is [Ok ()] if [r]'s kind is [Git] and errors
      otherwise. *)

  (** {1:branches Branches} *)

  type remote = string
  (** The type for remote identifiers. *)

  type branch = string
  (** The type for branch identifiers. *)

  val pp_branch : branch Fmt.t
  (** [pp_branch] formats a branch like colorized [git] would. *)

  val pp_remote_branch : (remote * branch) Fmt.t
  (** [pp_remote_branch] formats a remote branch like colorized [git] would. *)

  val remote_branch_exists :
    ?env:Os.Env.assignments ->
    t -> remote:remote -> branch:branch -> (bool, string) result
  (** [remote_branch_exists r remote branch] asserts whether [branch]
      exists on [remote]. *)

  val remote_branch_fetch :
    ?env:Os.Env.assignments -> ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo ->
    t -> remote:remote -> branch:branch -> (unit, string) result
  (** [remote_branch_fetch r remote branch] fetches [branch] of [remote].
      [stderr] and [stdout] indicates where they should be redirected,
      defaults to the values of {!B0_std.Os.Cmd.run_status}. *)

  val remote_branch_push :
    ?env:Os.Env.assignments -> ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo ->
    t -> force:bool -> src:branch -> remote:remote -> dst:remote ->
    (unit, string) result
  (** [remote_branch_push r ~force ~local ~remote ~dst] pushes branch
      [src] on [dst] of [remote]. If [dst] does not exist on [remote]
      a new branch is created. If [force] is [true] this is a forced
      update. [stderr] and [stdout] indicates where they should be redirected,
      defaults to the values of {!B0_std.Os.Cmd.run_status}. *)

  val remote_branch_delete :
    ?env:Os.Env.assignments -> ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo ->
    t -> force:bool -> remote:remote -> branch:branch -> (unit, string) result
  (** [remote_branch_delete r ~remote ~branch] deletes [branch] on
      [remote]. If [force] is [true] this is a forced update.
      [stderr] and [stdout] indicates where they should be redirected,
      defaults to the values of {!B0_std.Os.Cmd.run_status}. *)

  val branch_delete :
    ?env:Os.Env.assignments -> ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo ->
    t -> force:bool -> branch:branch -> (unit, string) result
  (** [branch_delete r ~force ~branch] deletes [branch] in [r]. If
      [force] is [true] this is a forced deletion.  [stderr] and
      [stdout] indicates where they should be redirected, defaults to
      the values of {!B0_std.Os.Cmd.run_status}. *)

  (** {1:transient_checkouts Transient checkouts}

      The following functions use [git worktree] to programmatically
      act on repo branches without disturbing the user's checkout and/or
      needing to clone the repo. Use them for short-lived operations that
      need a work tree and then delete them. The branch that was created
      can the be pushed on other branches and then deleted. *)

  val transient_checkout :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo -> t -> force:bool ->
    branch:branch -> Fpath.t -> commit_ish option -> (t, string) result
  (** [checkout_tmp_branch r ~force ~branch dir commit_ish] creates
      and checkouts and a branch [branch] in [dir] that points to
      [commit_ish] (if [None] an empty orphan branch is
      created). Unless [force] is [true] this fails if [branch]
      already exists. The resulting repo should be used to interact
      with the checkout. Once finished it should be disposed with
      {!transient_checkout_delete}. *)

  val transient_checkout_delete :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo -> t -> force:bool ->
    (unit, string) result
  (** [transient_checkout_delete r] deletes a transient checkout.
      The branch created by {!transient_checkout} is not deleted
      by the operation, only the corresponding working tree. If [force]
      will delete even if the checkout is dirty. *)

  val with_transient_checkout :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo ->
    ?dir:Fpath.t -> t -> force:bool -> branch:branch -> commit_ish option ->
    (t -> 'a) -> ('a, string) result
  (** [with_transient_checkout r ~force ~branch ~dir commit_ish f]
      calls {!transient_checkout} and then [f r] with [r] the repo to
      act on the checkout. Once [f r] returns normally or via an
      exception {!transient_checkout_delete} is called.  [dir]
      defaults to a temporary directory given by {!B0_std.Os.Path.tmp}. *)

  (** {1:working_dir Working directory} *)

  val add :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo -> t -> force:bool ->
    Fpath.t list -> (unit, string) result
  (** [add t ~force fs] adds [fs] to [r]'s staged changes. If [force]
      bypasses the [.gitignore]s. *)

  val has_staged_changes : t -> (bool, string) result
  (** [has_staged_changes r] is [true] if [r] has staged
      changes that can be {!commit}ed and [false] otherwise. *)

  val commit :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo -> ?sign:bool ->
    ?reset_author:bool -> ?amend:bool -> ?msg:string -> t ->
    (unit, string) result
  (** [commit r ~sign ~msg ~ammend ~reset_author] is basically
      [git commit], see [git-commit(1)] for the semantics of options.
      [stderr] and [stdout] indicates where they should be redirected,
      defaults to the values of {!B0_std.Os.Cmd.run_status}. *)

  val commit_exists : t -> commit_ish -> (bool, string) result
  (** [commit_exists r cish] checks whether [cish] exists in [r].
      In particular using this with [HEAD] allows to know if a commit
      exists in the branch checkout. *)

  val rm :
    ?stdout:Os.Cmd.stdo -> ?stderr:Os.Cmd.stdo -> t -> force:bool ->
    recurse:bool -> ignore_unmatch:bool -> Fpath.t list -> (unit, string) result
  (** [rm r ~force ~recurse ~ignore_unmatch files] removes [files]
      from the working tree and from the index. if [force] removes the files
      even if they are not up-to-date. If [recurse] removes directories
      recursively if [true]. If [ignore_unmatch] does not error if elements
      of [files] do not match files. [stderr] and [stdout] indicates
      where they should be redirected, defaults to the values of
      {!B0_std.Os.Cmd.run_status}. *)
end

(** Mercurial specific operations. *)
module Hg : sig
  val get_cmd :
    ?search:Fpath.t list -> ?cmd:Cmd.t -> unit -> (Cmd.t, string) result
  (** [get_cmd ()] looks for [hg] with {!B0_std.Os.Cmd.get}. *)

  val find : ?dir:Fpath.t -> unit -> (t option, string) result
  (** [find ~dir ()] finds, using VCS functionality, an hg repository
      starting in directory [dir] (if unspecified this is the [cwd]). *)
end
