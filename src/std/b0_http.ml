(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

module Url = struct
  type t = string

  let alpha = Char.Ascii.is_letter
  let digit = Char.Ascii.is_digit

  let scheme u =
    let scheme_char c =
      alpha c || digit c || Char.equal c '+' || Char.equal c '-' ||
      Char.equal '.' c
    in
    match String.keep_left scheme_char u with
    | "" -> None
    | s ->
        let ulen = String.length u and slen = String.length s in
        if alpha s.[0] && slen < ulen && u.[slen] = ':' then Some s else None

  let authority u = match String.index u ':' with
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

  let path_and_query u = match String.index u ':' with
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
  type method' =
  [ `CONNECT | `DELETE | `GET | `HEAD | `OPTIONS | `Other of string
  | `PATCH | `POST | `PUT | `TRACE ]

  let method_to_string = function
  | `GET -> "GET" | `HEAD -> "HEAD" | `POST -> "POST" | `PUT -> "PUT"
  | `DELETE -> "DELETE" | `CONNECT -> "CONNECT" | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE" | `PATCH -> "PATCH" | `Other s -> s

  type headers = (string * string) list
  let header_to_string (k, v) = String.concat "" [k; ": "; v]

  module Request = struct
    type t =
      { url : string;
        method' : method';
        headers : headers;
        body : string; }

    let v ?(headers = []) ?(body = "") ~url method' =
      { url; method'; headers; body }

    let url r = r.url
    let method' r = r.method'
    let headers r = r.headers
    let body r = r.body
    let has_body r = not (String.is_empty r.body)
  end

  module Response = struct
    type t =
      { status : int;
        headers : headers;
        body : string; }

    let v ?(headers = []) ?(body = "") status = { status; headers; body }
    let status r = r.status
    let headers r = r.headers
    let body r = r.body

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

    let of_string resp = match String.cut_left ~sep:"\r\n" resp with
    | None -> Fmt.error "%S: could not parse status line" resp
    | Some (status_line, rest) ->
        let* status = status_of_status_line status_line in
        let* headers, body = headers_and_body_of_string rest in
        Ok { status; headers; body }
  end
end

module Http_client = struct

  type t = Cmd.t

  let find_location request response =
    match List.assoc_opt "location" (Http.Response.headers response) with
    | None -> Error "No 'location' header found in 3XX response"
    | Some loc ->
        if String.length loc > 0 && loc.[0] <> '/' then Ok loc else
        let url = Http.Request.url request in
        try
          match Url.scheme url with
          | None -> raise Exit
          | Some s ->
              match Url.authority url with
              | None -> raise Exit
              | Some a -> Ok (String.concat "" [s; "://"; a; loc])
        with
        | Exit -> Fmt.error "Could not construct redirect from %s to %s" url loc

  let redirect_response visited request response =
    match Http.Response.status response with
    | 301 | 302 | 303 | 305 | 307 ->
        let* url = find_location request response in
        if List.mem url visited then Error "Infinite redirection loop" else
        Ok (Some { request with url })
    | _ -> Ok None

  let request ?(insecure = false) ?(follow = true) curl request =
    let rec loop follow visited request =
      let method' = Http.Request.method' request in
      let is_head = method' = `HEAD in
      let do_follow = match method' with `GET | `HEAD -> true | _ -> false in
      let follow = follow && do_follow in
      let method' = Http.method_to_string method' in
      let method' = Cmd.(arg "-X" % method' %% if' is_head (arg "--head")) in
      let headers = Http.Request.headers request in
      let headers = Cmd.of_list ~slip:"-H" Http.header_to_string headers in
      let has_body = Http.Request.has_body request in
      let body = Http.Request.body request in
      let stdin = if has_body then Os.Cmd.in_string body else Os.Cmd.in_stdin in
      let body = Cmd.(if' has_body (arg "--data-binary" % "@-")) in
      let insecure = Cmd.(if' insecure (arg "--insecure")) in
      let url = Http.Request.url request in
      let base = Cmd.(arg "-s" (* silent *) % "-i" (* resp. headers *)) in
      let args = Cmd.(base %% insecure %% method' %% headers %% body % url) in
      let* out = Os.Cmd.run_out ~trim:false ~stdin Cmd.(curl %% args) in
      let* response = Http.Response.of_string out in
      if not follow then Ok response else
      let* redirect = redirect_response visited request response in
      match redirect with
      | None -> Ok response
      | Some request -> loop follow (url :: visited) request
    in
    loop follow [] request

  let default = Cmd.arg "curl"
  let curl ?docs ?env () =
    let open Cmdliner in
    let doc = "The curl command $(docv) to use." in
    let cmd = Arg.conv' ~docv:"CMD" (B0_std.Cmd.of_string, B0_std.Cmd.pp_dump)in
    Arg.(value & opt cmd default & info ["curl"] ~doc ?docs ?env ~docv:"CMD")

  let get ?search ?(curl = default) () = Os.Cmd.get ?search curl
end