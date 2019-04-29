(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** GitHub interaction. *)

open B0_std
open B0_web
open B0_json

(** {1 GitHub authentication} *)

type auth
(** The type for GitHub authentication. *)

val auth : user:string -> unit -> (auth, string) result
(** [auth ~http ~user ()] determines a personal access token for user
    [user]. It first looks up the contents of the [B0_GITHUB_TOKEN]
    environment variable if that fails it looks up for an existing
    token in the {!Os.Dir.config}[ () /github/b0-$USER.token] file.
    If that fails instructions are printed on how to setup the
    token. *)

(** {1:reqs GitHub API requests} *)

type content_type = string
(** The type for content types. *)

type v3_body = [ `Json of Jsong.t | `Other of content_type * string | `Empty ]
(** The type for GitHub V3 request bodies. Either JSON or something
    else tagged with its content type or nothing. *)

val req_json_v3 :
  ?headers:Http.headers -> Httpr.t -> auth -> path:string -> Http.meth ->
  v3_body -> (Json.t, string) result
(** [req_json_v3 auth path m ~headers body] performs the request for json
    on [path] using method [m], additional headers [headers], body [body] and
    authentication [auth]. *)

val query_v4 : Httpr.t -> auth -> string -> (Json.t, string) result
(** [query_v4 auth q] performs the {{:https://developer.github.com/v4/}
    the GitHub GraphQL V4} query [q] using authentication [auth]. *)

(** {1 Higher-level interface} *)

(** GitHub repositories. *)
module Repo : sig

  type t
  (** The type for GitHub repositories. *)

  val v : owner:string -> string -> t
  (** [repo ~owner name] identifiers a GitHub repository. *)

  val owner : t -> string
  (** [owner r] is [r]'s owner. *)

  val name : t -> string
  (** [name r] is [r]'s name. *)

  val req_json_v3 :
    ?headers:Http.headers -> Httpr.t -> auth -> t -> path:string -> Http.meth ->
    v3_body -> (Json.t, string) result
  (** [req_json_v3] is like {!req_json_v3} but performs given the root
      subpath on the given repo. *)

  val query_v4 : Httpr.t -> auth -> t -> string -> (Json.t, string) result
  (** [query_v4 auth r q] performs the subgraph query [q] on repo [r]
      using authentication [auth]. *)
end

(** Repository issues. *)
module Issue : sig

  (** {1:issues Issues} *)

  type num = int
  (** The type for issue numbers. *)

  type uri = string
  (** The type for uris. *)

  type t
  (** The type for issues. *)

  val number : t -> num
  (** [number i] is the issue's number. *)

  val title : t -> string
  (** [title i] is the issue's title. *)

  val body : t -> string
  (** [body t] is the issue's body. *)

  val url : t -> string
  (** [url i] is the issue's url. *)

  val pp : t Fmt.t
  (** [pp] is a formatter for issues. *)

  val pp_short : t Fmt.t
  (** [pp_short] is a short formatter for issues. *)

  val list : Httpr.t -> auth -> Repo.t -> (int * t list, string) result
  (** [list auth repo] lists the issues for repository [repo].
      The integer is the total number of issues. *)

  (** {1:req Requests} *)

  val create :
    Httpr.t -> auth -> Repo.t -> title:string -> body:string -> unit ->
    (num * uri, string) result
  (** [create auth repo] opens an issue on the repository [repo] with
      the given [title] and [body]. *)

  val close : Httpr.t -> auth -> Repo.t -> num -> (num * uri, string) result
  (** [close auth repo n] closes issues [n] on the repository [repo] *)
end

(** Repository releases. *)
module Release : sig

  (** {1:releases Releases} *)

  type t
  (** The type for releases. *)

  val id : t -> int
  (** [id r] is the id of the release. *)

  val tag_name : t -> string
  (** [tag_name r] is the tag name of the release. *)

  val body : t -> string
  (** [body r] is the description of the release. *)

  val html_url : t -> string
  (** [html_url t] is the issue's HTML url. *)

  val assets_url : t -> string
  (** [assets_url t] is the issue's assets url. *)

  val pp : t Fmt.t
  (** [pp] is a formatter for issues. *)

  val pp_short : t Fmt.t
  (** [pp_short] is a short formatter for issues. *)

  (** {1:req Requests} *)

  val create :
    Httpr.t -> auth -> Repo.t -> tag_name:string -> body:string -> unit ->
    (t, string) result
  (** [create auth repo ~tag_name ~body ()] creates a new release in
      repository [repo] with given [tag_name] and [body] description. *)

  val get :
    Httpr.t -> auth -> Repo.t -> tag_name:string -> unit -> (t, string) result
  (** [get auth repo ~tag_name ()] gets the release with given [tag_name]
      in repo [tag_name]. *)

  val upload_asset :
    Httpr.t -> auth -> Repo.t -> t -> content_type:string -> name:string ->
    string -> (unit, string) result
  (** [upload_asset auth repo r ~content_type ~name asset] uploads
      assets content [asset] with file name [name] and content type
      [content_type] in release [r] of repo [r]. *)
end

(** Publish to GitHub pages. *)
module Pages : sig

  (** {1:update Updating GitHub pages} *)

  type update
  (** The type for updates. *)

  val update : ?follow_symlinks:bool -> src:Fpath.t option -> Fpath.t -> update
  (** [update ~follow_symlinks ~src dst] is an update that given a relative
      path [dst] in the work tree that may not exist:
      {ul
      {- If [src] is [None], deletes [dst] in the work tree.}
      {- If [src] is [Some src] replaces [dst] in the work tree
         by the contents of the file or file hierarchy [src]. If
         [follow_symlinks] is [true] (default) symbolic links are followed in
         [src].}}
      Use [.] for [dst] to denote the root of the work tree; for
      example [update ~src:(Some dir) (Fpath.v ".")] replaces the
      whole website by the file hierarchy rooted at [dir]. *)

  val nojekyll : update
  (** nojekyll is [update ~src:(Some Os.File.null) (Fpath.v ".nojekyll")].
      Add this to the updtaes to prevent the Jekyll processing that is
      automatically performed on GitHub pages. *)

  val default_branch : B0_vcs.Git.branch
  (** [default_branch] is ["gh-pages"], GitHub's default publication branch
      for GitHub pages. *)

  val commit_updates :
    ?allow_hardlinks:bool -> ?log:Log.level ->
    ?branch:B0_vcs.Git.branch -> B0_vcs.t -> amend:bool -> force:bool ->
    remote:B0_vcs.Git.remote -> msg:string -> update list ->
    (bool, string) result
  (** [commit_updates vcs ~log ~remote ~branch ~msg us] updates [branch]
      (defaults to [gh-pages]) on [remote] according to updates [us]
      with commit message [msg]. [Ok false] is returned if there was
      nothing to update.
      {ul
      {- If [amend] is [true], the last commit is amended (if any
         exists) and the author reset rather than a new commit added}
      {- If [force] is [true], the various git operations are forced.}
      {- [log] indicates a logging level used to
         monitor progress (defaults to {!Log.app}).}
      {- If [allow_hardlinks] is [true] (default), updates are, if
         possible hard linked to the working directory rather than copied}}
      More precisely this:
      {ol
      {- Fetches [remote/branch] if it exists.}
      {- Creates a {{!B0_vcs.Git.transient_checkout}transient checkout} with
         a temporary workdir in {!Os.Dir.default_tmp}
         and a branch called [_b0-update-gh-pages] reset to [remote/branch].}
      {- Commits changes with message [msg] according to [us] which
         are applied in order, see {!update}.}
      {- Destroys the transient checkout}
      {- Pushes [_b0-update-gh-pages] on [remote/branch]}
      {- Destroys the branch [_b0-update-gh-pages]}}

      {b Warning.} If you have paths that start with [_] GitHub pages
      won't publish them. You can disable this by adding a [.nojekyll]
      file at the root the gh-page branch, see {!nojekyll}. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
