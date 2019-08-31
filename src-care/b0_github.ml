(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std
open B0_serialk_json
open B0_http

let jsonq_int = Jsonq.(map truncate float) (* FIXME *)

let v4_api_uri = "https://api.github.com/graphql"
let v3_api_uri = "https://api.github.com"

(* GitHub authentication. *)

let token_help_uri =
  "https://help.github.com/articles/\
   creating-a-personal-access-token-for-the-command-line"

let token_scope = "repo"
let token_env = "B0_GITHUB_TOKEN"

type auth =
  { user : string;
    token : string;
    token_src : [ `Env | `File of Fpath.t ] }

let pp_token_src ppf = function
| `Env -> Fmt.pf ppf "environment variable %s" token_env
| `File file -> Fmt.pf ppf "file %a" Fpath.pp_quoted file

let github_conf_dir () =
  Result.bind (Os.Dir.config ()) (fun config -> Ok Fpath.(config / "github"))

let auth ~user () = match Os.Env.find ~empty_to_none:true token_env with
| Some token -> Ok { user; token; token_src = `Env }
| None ->
    Result.bind (github_conf_dir ()) @@ fun ghdir ->
    let tokfile = Fpath.(ghdir / Fmt.str "b0-%s.token" user) in
    Result.bind (Os.File.exists tokfile) @@ function
    | false ->
        (* We could bother using oauth to create one here but with 2FA
           it seems more trouble and some people will anyway prefer to
           handle this themselves rather than trust a random cli tool. *)
        Fmt.error
          "No GitHub token found for user '%s' in file %a@\n\
           or in the environment variable %s.@\n@\n\
           Create a GitHub personal access token with scope '%s' for b0@\n\
           by following the instructions here:\n\n  %s@\n@\n\
           and paste the token in the above file e.g. by running:@\n@\n\
           \ \ mkdir -p %a  # Make sure the directory exists@\n\
           \ \ cat - > %a   # Paste the token@\n\
           \ \ chmod 600 %a # Make that readable only by yourself@\n"
          user Fpath.pp_quoted tokfile token_env token_scope token_help_uri
          Fpath.pp_quoted ghdir Fpath.pp_quoted tokfile Fpath.pp_quoted tokfile
    | true ->
        Result.bind (Os.File.read tokfile) @@ function
        | "" -> Fmt.error "token file %a is empty" Fpath.pp_quoted tokfile
        | token -> Ok { user; token; token_src = `File tokfile }

(* GitHub API queries *)

let resp_success auth req resp = match Http.resp_status resp with
| 200 | 201 -> Json.of_string (Http.resp_body resp)
| 401 ->
    Fmt.error "GitHub authentication failure on %s.\n\
               Are you sure the token in %a\n\
               is valid for user '%s' and has scope '%s' ?\n"
      (Http.req_uri req) pp_token_src auth.token_src auth.user token_scope
| st ->
    Fmt.error "GitHub API request returned unexpected status %d for %s on %s"
      st (Http.meth_to_string @@ Http.req_meth req) (Http.req_uri req)

type content_type = string
let content_type c = ("Content-Type", c)

type v3_body =
  [ `Json of Jsong.t | `Other of content_type * string | `Empty ]

let req_json_v3 ?(headers = []) http auth ~path m body =
  let req_v3_headers auth =
    ("Authorization", Fmt.str "token %s" auth.token) ::
    ("Accept", "application/vnd.github.v3+json") :: headers
  in
  let headers = req_v3_headers auth in
  let headers, body = match body with
  | `Json j -> (content_type "application/json") :: headers, Jsong.to_string j
  | `Other (c, body) -> (content_type c) :: headers, body
  | `Empty -> headers, ""
  in
  let uri = v3_api_uri ^ path in
  let req = Http.req ~uri m ~headers ~body in
  let resp = Httpr.perform http req in
  Result.bind resp @@ fun resp -> resp_success auth req resp

let query_v4 http auth q =
  let req_v4_headers auth = ["Authorization", Fmt.str "bearer %s" auth.token] in
  let query = Jsong.(obj |> mem "query" (string q) |> obj_end) in
  let headers = req_v4_headers auth in
  let body = Jsong.to_string query in
  let req = Http.req ~uri:v4_api_uri `POST ~headers ~body in
  let resp = Httpr.perform http req in
  Result.bind resp @@ fun resp -> resp_success auth req resp

(* Higher-level interfaces *)

module Repo = struct
  type t = { owner : string; name : string }
  let v ~owner name = { owner; name }
  let owner r = r.owner
  let name r = r.name
  let query_v4 http auth repo q =
    query_v4 http auth @@
    Fmt.str "query { repository(owner:%s, name:%s) { %s }}"
      (Json.to_string (Json.string repo.owner))
      (Json.to_string (Json.string repo.name)) q

  let req_json_v3 ?headers http auth repo ~path m body =
    let path = Fmt.str "/repos/%s/%s%s" repo.owner repo.name path in
    req_json_v3 ?headers http auth ~path m body
end

module Issue = struct
  type num = int
  type uri = string
  type t = { number : num;  title : string; body : string; url : uri; }

  let v number title body url = { number; title; body; url }
  let number i = i.number
  let title i = i.title
  let body i = i.body
  let url i = i.url
  let pp =
    Fmt.record @@
    [ Fmt.field "number" number Fmt.int;
      Fmt.field "title" title Fmt.string;
      Fmt.field "body" body Fmt.string;
      Fmt.field "url" url Fmt.string ]

  let pp_short ppf i = Fmt.pf ppf "@[%d %s@]" i.number i.title

  (* JSON *)

  let issue_list_gql =
    "issues(first:100 states:OPEN, orderBy:{direction:DESC, field:UPDATED_AT})
      { totalCount edges { node { number title bodyText url }}}"

  let issue_list_q =
    let open Jsonq in
    let issue =
      succeed v $ mem "number" jsonq_int
      $ mem "title" string $ mem "bodyText" string $ mem "url" string
    in
    mem "data" @@ mem "repository" @@ mem "issues" @@
    (succeed (fun count is -> count, is)
       $ mem "totalCount" jsonq_int $ mem "edges" (array (mem "node" issue)))

  let issue_id_q =
    let issue n uri = n, uri in
    Jsonq.(succeed issue $ mem "number" jsonq_int $ mem "url" string)

  let create_g ~title ~body =
    Jsong.(obj |> mem "title" (string title) |> mem "body" (string body) |>
           obj_end)

  let close_g = Jsong.(obj |> mem "state" (string "close") |> obj_end)

  (* Requests *)

  let list http auth repo =
    let resp = Repo.query_v4 http auth repo issue_list_gql in
    Result.bind resp (Jsonq.query issue_list_q)

  let create http auth repo ~title ~body () =
    let body = `Json (create_g ~title ~body) in
    let resp = Repo.req_json_v3 http auth repo ~path:"/issues" `POST body in
    Result.bind resp (Jsonq.query issue_id_q)

  let close http auth repo num =
    let path = Fmt.str "/issues/%d" num in
    let resp = Repo.req_json_v3 http auth repo ~path `PATCH (`Json close_g) in
    Result.bind resp (Jsonq.query issue_id_q)
end

module Release = struct
  type t = { id : int; tag_name : string; body : string; html_url : string;
             assets_url : string }

  let v id tag_name body html_url assets_url =
    { id; tag_name; body; html_url; assets_url }

  let id r = r.id
  let tag_name r = r.tag_name
  let body r = r.body
  let html_url r = r.html_url
  let assets_url r = r.assets_url

  let pp =
    Fmt.record @@
    [ Fmt.field "id" id Fmt.int;
      Fmt.field "tag_name" tag_name Fmt.string;
      Fmt.field "body" body Fmt.string;
      Fmt.field "html_url" html_url Fmt.string;
      Fmt.field "assets_url" assets_url Fmt.string; ]


  let pp_short ppf i = Fmt.pf ppf "@[%s %s@]" i.tag_name i.html_url

  (* JSON *)

  let release_q =
    Jsonq.(succeed v $ mem "id" jsonq_int $ mem "tag_name" string $
           mem "body" string $ mem "html_url" string $ mem "assets_url" string)

  let create_g ~tag_name ~body =
    Jsong.(obj |> mem "tag_name" (string tag_name) |>
           mem "body" (string body) |> obj_end)

  (* Requests *)

  let create http auth repo ~tag_name ~body () =
    let body = `Json (create_g ~tag_name ~body) in
    let resp = Repo.req_json_v3 http auth repo ~path:"/releases" `POST body in
    Result.bind resp (Jsonq.query release_q)

  let get http auth repo ~tag_name () =
    let path = Fmt.str "/releases/tags/%s" tag_name in
    let resp = Repo.req_json_v3 http auth repo ~path `GET `Empty in
    Result.bind resp (Jsonq.query release_q)

  let upload_asset http auth repo r ~content_type ~name asset =
    let path = Fmt.str "%s?name=%s" r.assets_url name in
    let body = `Other (content_type, asset) in
    let resp = Repo.req_json_v3 http auth repo ~path `POST body in
    Result.bind resp (fun _ -> Ok ())
end

module Pages = struct
  let header = "gh-pages" (* log header *)

  type update =
    { dst : Fpath.t;
      src : Fpath.t option;
      follow_symlinks : bool }

  let update ?(follow_symlinks = true) ~src dst = { dst; src; follow_symlinks }
  let nojekyll = update ~src:(Some Os.File.null) (Fpath.v ".nojekyll")

  let fetch_branch r ~log ~remote ~branch =
    let exists = B0_vcs.Git.remote_branch_exists r ~remote ~branch in
    Result.bind exists @@ function
    | false -> Ok None
    | true ->
        Log.msg log begin fun m ->
          m ~header "Fetching %a" B0_vcs.Git.pp_remote_branch (remote, branch)
        end;
        let fetch = B0_vcs.Git.remote_branch_fetch r ~remote ~branch in
        Result.bind fetch @@ fun () -> Ok (Some (Fmt.str "%s/%s" remote branch))

  let do_commit r ~log ~amend ~msg =
    Result.bind (B0_vcs.Git.has_staged_changes r) @@ function
    | true ->
        Log.msg log begin fun m ->
          m ~header "%s changes." (if amend then "Amending" else "Commiting")
        end;
        Result.bind (B0_vcs.Git.commit_exists r "HEAD") @@ fun has_commit ->
        let amend = has_commit && amend in
        let reset_author = amend in
        let stdout = Os.Cmd.out_null in
        Result.bind (B0_vcs.Git.commit ~amend ~reset_author ~stdout ~msg r)
        @@ fun () -> Ok true
    | false ->
        Log.msg log (fun m -> m ~header "No changes to commit.");
        Ok false

  let perform_updates ~allow_hardlinks ~log ~amend ~msg us r =
    Log.msg log (fun m -> m ~header "Copying updates.");
    let rm p = (* This makes sure [p] is in the repo *)
      let stdout = Os.Cmd.out_null in
      B0_vcs.Git.rm r ~stdout ~force:true ~recurse:true ~ignore_unmatch:true [p]
    in
    let cp r ~allow_hardlinks ~follow_symlinks src dst =
      let dst = Fpath.(B0_vcs.(work_dir r) // dst) in
      Os.Path.copy ~allow_hardlinks ~follow_symlinks ~make_path:true
        ~recurse:true ~src dst
    in
    let rec loop r = function
    | [] -> do_commit r ~log ~amend ~msg
    | u :: us ->
        match u.src with
        | None -> (rm u.dst |> Result.to_failure); loop r us
        | Some src ->
            (rm u.dst |> Result.to_failure);
            (cp r ~allow_hardlinks ~follow_symlinks:u.follow_symlinks src u.dst
             |> Result.to_failure);
            (B0_vcs.Git.add r ~force:false [u.dst] |> Result.to_failure);
            loop r us
    in
    try loop r us with
    | Failure e -> Error e

  let update_in_branch
      r ~allow_hardlinks ~log ~amend ~force ~branch cish ~msg us
    =
    Result.join @@
    B0_vcs.Git.with_transient_checkout r ~force ~branch cish
      (perform_updates ~allow_hardlinks ~log ~amend ~msg us)

  let default_branch = "gh-pages"
  let ubr = "_b0-gh-pages-update"
  let commit_updates
      ?(allow_hardlinks = true) ?(log = Log.App) ?branch:(br = default_branch)
      r ~amend ~force ~remote ~msg us
    =
    let cleanup ~commited r =
      let stdout = Os.Cmd.out_null in
      Result.bind (B0_vcs.Git.branch_delete r ~stdout ~force ~branch:ubr) @@
      fun () -> Ok commited
    in
    Result.bind (B0_vcs.Git.check_kind r) @@ fun () ->
    Result.bind (fetch_branch r ~log ~remote ~branch:br) @@ fun cish ->
    match
      update_in_branch
        r ~allow_hardlinks ~log ~amend ~force ~branch:ubr cish ~msg us
    with
    | Error _ as e-> ignore (cleanup ~commited:false r) (* Could be better *); e
    | Ok false -> cleanup ~commited:false r
    | Ok true ->
        Log.msg log begin fun m ->
          m ~header "Pushing %a" B0_vcs.Git.pp_remote_branch (remote, br)
        end;
        match B0_vcs.Git.remote_branch_push r ~force ~src:ubr ~remote ~dst:br
        with
        | Error _ as e -> ignore (cleanup ~commited:true r); e
        | Ok () -> cleanup ~commited:true r
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
