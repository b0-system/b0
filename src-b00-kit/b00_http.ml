(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

module Uri = struct
  type t = string

  let alpha = Char.Ascii.is_letter
  let digit = Char.Ascii.is_digit

  let parse_scheme u =
    let scheme_char c =
      alpha c || digit c || Char.equal c '+' || Char.equal c '-' ||
      Char.equal '.' c
    in
    match String.keep_left scheme_char u with
    | "" -> None
    | s ->
        let ulen = String.length u and slen = String.length s in
        if alpha s.[0] && slen < ulen && u.[slen] = ':' then Some s else None

  let parse_authority u = match String.index u ':' with
  | exception Not_found -> None
  | i ->
      let max = String.length u - 1 in
      if i + 2 >= max then None else
      if not (u.[i + 1] = '/' && u.[i + 2] = '/') then None else
      let first = i + 3 in
      let last = match String.index_from u first '/' with
      | exception Not_found -> max
      | j -> j - 1
      in
      if last - first < 0 then None else
      Some (String.subrange ~first ~last u)

  let parse_path_and_query u = match String.index u ':' with
  | exception Not_found -> None
  | i ->
      let max = String.length u - 1 in
      if i = max then None else
      match u.[i + 1] = '/' with
      | false -> Some (String.subrange ~first:(i + 1) u)
      | true ->
          if i + 1 = max then Some "/" else
          match u.[i + 2] = '/' with
          | false -> Some (String.subrange ~first:(i + 1) u)
          | true ->
              match String.index_from u (i + 3) '/' with
              | exception Not_found -> None
              | i -> Some (String.subrange ~first:i u)

end

module Http = struct
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
  let req_has_body r = not (String.is_empty r.req_body)

  (* Reponses *)

  type resp =
    { resp_status : int;
      resp_headers : headers;
      resp_body : string; }

  let resp_status r = r.resp_status
  let resp_headers r = r.resp_headers
  let resp_body r = r.resp_body
  let resp ?(headers = []) ?(body = "") status =
    { resp_status = status; resp_headers = headers; resp_body = body }

  let status_of_status_line l =
    let err i = Fmt.error "%S: could not parse HTTP status code" i in
    match String.cuts_left ~sep:" " l with
    | (_ :: code :: _) ->
        (try Ok (int_of_string code) with | Failure _ -> err code)
    | _ -> err l

  let headers_and_body_of_string s =
    let rec loop acc s = match String.cut_left ~sep:"\r\n" s with
    | None -> Fmt.failwith "%S: could not find CRLF" s
    | Some ("", body) -> Ok (List.rev acc, body)
    | Some (h, rest) ->
        match String.cut_left ~sep:":" h with
        | None -> Fmt.failwith "%S: could not parse HTTP header" h
        | Some (k, v) ->
            loop ((String.lowercase_ascii k, String.trim v) :: acc) rest
    in
    try loop [] s with Failure e -> Error e

  let resp_of_string resp = match String.cut_left ~sep:"\r\n" resp with
  | None -> Fmt.error "%S: could not parse status line" resp
  | Some (status_line, rest) ->
      Result.bind (status_of_status_line status_line) @@ fun resp_status ->
      Result.bind (headers_and_body_of_string rest) @@
      fun (resp_headers, resp_body) ->
      Ok { resp_status; resp_headers; resp_body }

end

module Httpr = struct

  let redirect_resp visited req resp =
    let find_location req resp =
      try
        let loc = List.assoc "location" (Http.resp_headers resp) in
        if String.length loc > 0 && loc.[0] <> '/' then Ok loc else
        let uri = Http.req_uri req in
        try
          match Uri.parse_scheme uri with
          | None -> raise Exit
          | Some s ->
              match Uri.parse_authority uri with
              | None -> raise Exit
              | Some a -> Ok (String.concat "" [s; "://"; a; loc])
        with Exit ->
          Fmt.error "Could not construct redirect from %s to %s" uri loc
      with
      | Not_found -> Error "No 'location' header found in 3XX response"
    in
    match Http.resp_status resp with
    | 301 | 302 | 303 | 305 | 307 ->
        begin
          Result.bind (find_location req resp) @@ fun uri ->
          match List.mem uri visited with
          | true -> Error "Infinite redirection loop"
          | false -> Ok (Some { req with Http.req_uri = uri })
        end
    | _ -> Ok None

  (* Perform *)

  type t = Cmd.t

  let perform ?(insecure = false) ?(follow = true) curl r =
    let rec loop follow visited r =
      let meth = Cmd.(arg "-X" % Http.(meth_to_string (req_meth r))) in
      let headers =
        Cmd.of_list ~slip:"-H" Http.header_to_string Http.(req_headers r)
      in
      let body =
        Cmd.if' (Http.req_has_body r) Cmd.(arg "--data-binary" % "@-")
      in
      let stdin = match Http.req_has_body r with
      | true -> Os.Cmd.in_string r.req_body
      | false -> Os.Cmd.in_stdin
      in
      let insecure = Cmd.(if' insecure (arg "--insecure")) in
      let out =
        Os.Cmd.run_out ~trim:false ~stdin @@
        Cmd.(curl %% insecure % "-s" % "-i" %% meth %% headers %% body %
             r.req_uri)
      in
      Result.bind out @@ fun stdout ->
      Result.bind (Http.resp_of_string stdout) @@ fun resp ->
      match follow, (Http.req_meth r) with
      | true, (`GET | `HEAD) ->
          begin
            Result.bind (redirect_resp visited r resp) @@ function
            | None -> Ok resp
            | Some req -> loop follow (r.req_uri :: visited) req
          end
      | _, _ -> Ok resp
    in
    loop follow [] r

  let curl ?docs ?env () =
    let open Cmdliner in
    let doc = "The curl command $(docv) to use." in
    let cmd = B00_std_ui.cmd in
    let default = Cmd.arg "curl" in
    Arg.(value & opt cmd default & info ["curl"] ~doc ?docs ?env ~docv:"CMD")

  let find_curl ?search ~curl () = Os.Cmd.must_find ?search curl
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers

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
