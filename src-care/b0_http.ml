(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0

type meth =
  [ `CONNECT | `DELETE | `GET | `HEAD | `OPTIONS | `Other of string
  | `PATCH | `POST | `PUT | `TRACE ]

let meth_to_string = function
| `GET -> "GET" | `HEAD -> "HEAD" | `POST -> "POST" | `PUT -> "PUT"
| `DELETE -> "DELETE" | `CONNECT -> "CONNECT" | `OPTIONS -> "OPTIONS"
| `TRACE -> "TRACE" | `PATCH -> "PATCH" | `Other s -> s

type headers = (string * string) list

let header_to_string (k, v) = String.concat "" [k; ": "; v]

(* Requests *)

type req =
  { req_uri : string;
    req_meth : meth;
    req_headers : headers;
    req_body : string; }

let req ?(headers = []) ?(body = "") ~uri meth =
  { req_uri = uri; req_meth = meth; req_headers = headers; req_body = body }

let req_uri r = r.req_uri
let req_meth r = r.req_meth
let req_headers r = r.req_headers
let req_body r = r.req_body
let req_has_body r = r.req_body <> ""

(* Reponses *)

type resp =
  { resp_status : int;
    resp_headers : headers;
    resp_body : string; }

let resp_status r = r.resp_status
let resp_headers r = r.resp_headers
let resp_body r = r.resp_body

let status_of_status_line l =
  let err i = R.error_msgf "%S: could not parse HTTP status code" i in
  match String.cuts ~sep:" " l with
  | (_ :: code :: _) ->
      (try Ok (int_of_string code) with | Failure _ -> err code)
  | _ -> err l

let headers_and_body_of_string s =
  let rec loop acc s = match String.cut ~sep:"\r\n" s with
  | None -> failwith (strf "%S: could not find CRLF" s)
  | Some ("", body) -> Ok (List.rev acc, body)
  | Some (h, rest) ->
      match String.cut ~sep:":" h with
      | None -> failwith (strf "%S: could not parse HTTP header" h)
      | Some (k, v) ->
          loop ((String.lowercase_ascii k, String.trim v) :: acc) rest
  in
  try loop [] s with Failure e -> R.error_msg e

let resp_of_string resp = match String.cut ~sep:"\r\n" resp with
| None -> R.error_msgf "%S: could not parse status line" resp
| Some (status_line, rest) ->
    status_of_status_line status_line >>= fun resp_status ->
    headers_and_body_of_string rest >>= fun (resp_headers, resp_body) ->
    Ok { resp_status; resp_headers; resp_body }

let redirect_resp visited req resp =
  let find_location resp = try Ok (List.assoc "location" resp.resp_headers) with
  | Not_found -> R.error_msgf "No 'location' header found in 3XX response"
  in
  match resp.resp_status with
  | 301 | 302 | 303 | 305 | 307 ->
      begin
        find_location resp >>= fun uri ->
        match List.mem uri visited with
        | true -> R.error_msgf "Infinite redirection loop"
        | false -> Ok (Some { req with req_uri = uri })
      end
  | _ -> Ok None

(* Perform *)

let get_curl () =
  (* TODO abstract this pattern, see `B0_opam`. *)
  let curl = OS.Env.get_value "B0_CURL" Conv.tool ~absent:(Cmd.v "curl") in
  OS.Cmd.resolve curl

let rec _perform ~follow visited r =
  get_curl () >>= fun curl ->
  let meth = Cmd.(v "-X" % meth_to_string r.req_meth) in
  let headers = Cmd.of_values ~slip:"-H" header_to_string r.req_headers in
  let body = Cmd.(on (req_has_body r) (v "--data-binary" % "@-")) in
  let stdin = match req_has_body r with
  | true -> OS.Cmd.in_string r.req_body
  | false -> OS.Cmd.in_stdin
  in
  OS.Cmd.run_out ~trim:false ~stdin @@
  Cmd.(curl % "-s" % "-i" %% meth %% headers %% body % r.req_uri)
  >>= fun stdout -> resp_of_string stdout
  >>= fun resp -> match follow, r.req_meth with
  | true, (`GET | `HEAD) ->
      begin
        redirect_resp visited r resp >>= function
        | None -> Ok resp
        | Some req -> _perform ~follow (r.req_uri :: visited) req
      end
  | _, _ -> Ok resp

let perform ?(follow = true) r = _perform ~follow [] r

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
