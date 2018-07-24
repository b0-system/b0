(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** GitHub interaction. *)

open B0

(** {1 GitHub authentication} *)

type auth
(** The type for GitHub authentication. *)

val auth : user:string -> unit -> auth result
(** [auth ~user ()] determines a personal access token for user
    [user]. It first looks up the contents of the [B0_GITHUB_TOKEN]
    environment variable if that fails it looks up for an existing
    token in the [$HOME/.github/b0-$USER.token] file. If that fails
    instructions are printed on how to setup the token. *)

(** {1:reqs GitHub API requests} *)

type content_type = string
(** The type for content types. *)

type v3_body =
  [ `Json of B0_json.G.t | `Other of content_type * string | `Empty ]
(** The type for GitHub V3 request bodies. Either JSON or something
    else tagged with its content type or nothing. *)

val req_json_v3 :
  ?headers:B0_http.headers -> auth -> path:string -> B0_http.meth ->
  v3_body -> B0_json.t result
(** [req_json_v3 auth path m ~headers body] performs the request for json
    on [path] using method [m], additional headers [headers], body [body] and
    authentication [auth]. *)

val query_v4 : auth -> string -> B0_json.t result
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
    ?headers:B0_http.headers -> auth -> t -> path:string -> B0_http.meth ->
    v3_body -> B0_json.t result
  (** [req_json_v3] is like {!req_json_v3} but performs given the root
      subpath on the given repo. *)

  val query_v4 : auth -> t -> string -> B0_json.t result
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

  val list : auth -> Repo.t -> (int * t list) result
  (** [list auth repo] lists the issues for repository [repo].
      The integer is the total number of issues. *)

  (** {1:req Requests} *)

  val create :
    auth -> Repo.t -> title:string -> body:string -> unit -> (num * uri) result
  (** [create auth repo] opens an issue on the repository [repo] with
      the given [title] and [body]. *)

  val close : auth -> Repo.t -> num -> (num * uri) result
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
    auth -> Repo.t -> tag_name:string -> body:string -> unit -> t result
  (** [create auth repo ~tag_name ~body ()] creates a new release in
      repository [repo] with given [tag_name] and [body] description. *)

  val get : auth -> Repo.t -> tag_name:string -> unit -> t result
  (** [get auth repo ~tag_name ()] gets the release with given [tag_name]
      in repo [tag_name]. *)

  val upload_asset :
    auth -> Repo.t -> t -> content_type:string -> name:string -> string ->
    unit result
  (** [upload_asset auth repo r ~content_type ~name asset] uploads
      assets content [asset] with file name [name] and content type
      [content_type] in release [r] of repo [r]. *)
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
