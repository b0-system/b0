(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

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
| `File file -> Fmt.pf ppf "file %a" Fpath.pp file

let github_user_dir user =
  OS.Dir.user () >>| fun home -> Fpath.(home / ".github")

let auth ~user () = match OS.Env.find token_env with
| Some token -> Ok { user; token; token_src = `Env }
| None ->
    github_user_dir user
    >>= fun ghdir -> Ok Fpath.(ghdir / strf "b0-%s.token" user)
    >>= fun file -> OS.File.exists file
    >>= function
    | false ->
        (* We could bother using oauth to create one here but with 2FA
           it seems more trouble and some people will anyway prefer to
           handle this themselves rather than trust a random cli tool. *)
        R.error_msgf
          "No GitHub token found for user '%s' in file %a@\n\
           or in the environment variable %s.@\n@\n\
           Create a GitHub personal access token with scope '%s' for b0@\n\
           by following the instructions here:\n\n  %s@\n@\n\
           and paste the token in the above file e.g. by running:@\n@\n\
           \ \ mkdir -p %a  # Make sure the directory exists@\n\
           \ \ cat - > %a   # Paste the token@\n\
           \ \ chmod 600 %a # Make that readable only by yourself@\n"
          user Fpath.pp file token_env token_scope token_help_uri
          Fpath.pp ghdir Fpath.pp file Fpath.pp file
    | true ->
        OS.File.read file >>= function
          | "" -> R.error_msgf "token file %a is empty" Fpath.pp file
          | token -> Ok { user; token; token_src = `File file }

(* GitHub API queries *)

let resp_success auth req resp = match B0_http.resp_status resp with
| 200 | 201 -> B0_json.of_string (B0_http.resp_body resp)
| 401 ->
    R.error_msgf "GitHub authentication failure on %s.\n\
                  Are you sure the token in %a\n\
                  is valid for user '%s' and has scope '%s' ?\n"
      (B0_http.req_uri req) pp_token_src auth.token_src auth.user token_scope
| st ->
    R.error_msgf "GitHub API request returned unexpected status %d for %s on %s"
      st (B0_http.meth_to_string @@ B0_http.req_meth req) (B0_http.req_uri req)


type content_type = string
let content_type c = ("Content-Type", c)

type v3_body =
  [ `Json of B0_json.G.t | `Other of content_type * string | `Empty ]

let req_json_v3 ?(headers = []) auth ~path m body =
  let req_v3_headers auth =
    ("Authorization", strf "token %s" auth.token) ::
    ("Accept", "application/vnd.github.v3+json") :: headers
  in
  let headers = req_v3_headers auth in
  let headers, body = match body with
  | `Json j ->
      (content_type "application/json") :: headers, B0_json.G.to_string j
  | `Other (c, body) -> (content_type c) :: headers, body
  | `Empty -> headers, ""
  in
  let uri = v3_api_uri ^ path in
  let req = B0_http.req ~uri m ~headers ~body in
  B0_http.perform req >>= fun resp ->
  resp_success auth req resp

let query_v4 auth q =
  let req_v4_headers auth = ["Authorization", strf "bearer %s" auth.token] in
  let query = B0_json.G.(obj @@ mem "query" (string q)) in
  let headers = req_v4_headers auth in
  let body = B0_json.G.to_string query in
  let req = B0_http.req ~uri:v4_api_uri `POST ~headers ~body in
  B0_http.perform req >>= fun resp ->
  resp_success auth req resp

(* Higher-level interfaces *)

module Repo = struct
  type t = { owner : string; name : string }
  let v ~owner name = { owner; name }
  let owner r = r.owner
  let name r = r.name
  let query_v4 auth repo q =
    query_v4 auth @@
    strf "query { repository(owner:%s, name:%s) { %s }}"
      (B0_json.for_string repo.owner) (B0_json.for_string repo.name) q

  let req_json_v3 ?headers auth repo ~path m body =
    let path = strf "/repos/%s/%s%s" repo.owner repo.name path in
    req_json_v3 ?headers auth ~path m body
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

  let pp ppf i =
    Fmt.pf ppf "@[<v>%a@]" begin fun ppf () ->
      Fmt.field "number" Fmt.int ppf i.number; Fmt.cut ppf ();
      Fmt.field "title" Fmt.string ppf i.title; Fmt.cut ppf ();
      Fmt.field "body" Fmt.string ppf i.body; Fmt.cut ppf ();
      Fmt.field "url" Fmt.string ppf i.url; Fmt.cut ppf ();
    end ()

  let pp_short ppf i = Fmt.pf ppf "@[%d %s@]" i.number i.title

  (* JSON *)

  let issue_list_gql =
    "issues(first:100 states:OPEN, orderBy:{direction:DESC, field:UPDATED_AT})
      { totalCount edges { node { number title bodyText url }}}"

  let issue_list_q =
    let open B0_json.Q in
    let issue =
      obj v
      |> mem "number" int |> mem "title" string
      |> mem "bodyText" string |> mem "url" string
    in
    sel "data" @@ sel "repository" @@ sel "issues" @@
    (obj (fun count is -> count, is)
     |> mem "totalCount" int
     |> mem "edges" @@ array @@ sel "node" issue)

  let issue_id_q =
    B0_json.Q.(obj (fun n uri -> n, uri)
               |> mem "number" int |> mem "url" string)

  let create_g ~title ~body =
    B0_json.G.(obj @@ mem "title" (string title) ++ mem "body" (string body))

  let close_g = B0_json.G.(obj @@ mem "state" (string "close"))

  (* Requests *)

  let list auth repo =
    Repo.query_v4 auth repo issue_list_gql
    >>= B0_json.Q.query issue_list_q

  let create auth repo ~title ~body () =
    let body = create_g ~title ~body in
    Repo.req_json_v3 auth repo ~path:"/issues" `POST (`Json body)
    >>= B0_json.Q.query issue_id_q

  let close auth repo num =
    let path = strf "/issues/%d" num in
    Repo.req_json_v3 auth repo ~path `PATCH (`Json close_g)
    >>= B0_json.Q.query issue_id_q
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

  let pp ppf r =
    Fmt.pf ppf "@[<v>%a@]" begin fun ppf () ->
      Fmt.field "id" Fmt.int ppf r.id; Fmt.cut ppf ();
      Fmt.field "tag_name" Fmt.string ppf r.tag_name; Fmt.cut ppf ();
      Fmt.field "body" Fmt.string ppf r.body; Fmt.cut ppf ();
      Fmt.field "html_url" Fmt.string ppf r.html_url; Fmt.cut ppf ();
      Fmt.field "assets_url" Fmt.string ppf r.assets_url; Fmt.cut ppf ();
    end ()

  let pp_short ppf i = Fmt.pf ppf "@[%s %s@]" i.tag_name i.html_url

  (* JSON *)

  let release_q =
    B0_json.Q.(obj v
               |> mem "id" int |> mem "tag_name" string |> mem "body" string
               |> mem "html_url" string |> mem "assets_url" string)

  let create_g ~tag_name ~body =
    B0_json.G.(obj @@ mem "tag_name" (string tag_name) ++
                      mem "body" (string body))

  (* Requests *)

  let create auth repo ~tag_name ~body () =
    let body = create_g ~tag_name ~body in
    Repo.req_json_v3 auth repo ~path:"/releases" `POST (`Json body)
    >>= B0_json.Q.query release_q

  let get auth repo ~tag_name () =
    let path = strf "/releases/tags/%s" tag_name in
    Repo.req_json_v3 auth repo ~path `GET `Empty
    >>= B0_json.Q.query release_q

  let upload_asset auth repo r ~content_type ~name asset =
    let path = strf "%s?name=%s" r.assets_url name in
    Repo.req_json_v3 auth repo ~path `POST (`Other (content_type, asset))
    >>| ignore
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
