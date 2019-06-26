(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

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
      Some (String.with_index_range ~first ~last u)

  let parse_path_and_query u = match String.index u ':' with
  | exception Not_found -> None
  | i ->
      let max = String.length u - 1 in
      if i = max then None else
      match u.[i + 1] = '/' with
      | false -> Some (String.with_index_range ~first:(i + 1) u)
      | true ->
          if i + 1 = max then Some "/" else
          match u.[i + 2] = '/' with
          | false -> Some (String.with_index_range ~first:(i + 1) u)
          | true ->
              match String.index_from u (i + 3) '/' with
              | exception Not_found -> None
              | i -> Some (String.with_index_range ~first:i u)

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
    let find_location resp =
      try Ok (List.assoc "location" (Http.resp_headers resp)) with
      | Not_found -> Error "No 'location' header found in 3XX response"
    in
    match Http.resp_status resp with
    | 301 | 302 | 303 | 305 | 307 ->
        begin
          Result.bind (find_location resp) @@ fun uri ->
          match List.mem uri visited with
          | true -> Error "Infinite redirection loop"
          | false -> Ok (Some { req with Http.req_uri = uri })
        end
    | _ -> Ok None

  (* Perform *)

  type t = Cmd.t

  let curl ?docs ?env () =
    let open Cmdliner in
    let doc = "The curl command $(docv) to use." in
    let cmd = B0_ui.Cli.Arg.cmd in
    let default = Cmd.arg "curl" in
    Arg.(value & opt cmd default & info ["curl"] ~doc ?docs ?env ~docv:"CMD")

  let find_curl ?search ~curl () = Os.Cmd.must_find ?search curl
  let perform ?(follow = true) curl r =
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
      let out =
        Os.Cmd.run_out ~trim:false ~stdin @@
        Cmd.(curl % "-s" % "-i" %% meth %% headers %% body % r.req_uri)
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

end


module Htmlg = struct
  module Att = struct
    type name = string
    type t = name * string
    let v n v = (n, v)
    let add_if c att atts = if c then att :: atts else atts
    let add_some n o atts = match o with
    | None -> atts | Some value -> (v n value) :: atts

    type 'a cons = 'a -> t
    let v_bool n = v n ""
    let v_int n i = v n (string_of_int i)
    let autofocus = v_bool "autofocus"
    let charset = v "charset"
    let checked = v_bool "checked"
    let class' = v "class"
    let content = v "content"
    let disabled = v_bool "disabled"
    let for' = v "for"
    let height = v_int "height"
    let href = v "href"
    let id = v "id"
    let media = v "media"
    let name = v "name"
    let placeholder = v "placeholder"
    let rel = v "rel"
    let src = v "src"
    let tabindex = v_int "tabindex"
    let title = v "title"
    let type' = v "type"
    let value = v "value"
    let width = v_int "width"
  end

  module El = struct
    type name = string
    type child =
    | El of name * Att.t list * child list
    | Txt of string
    | Splice of child list
    | Raw of string

    let v n ?(a = []) cs = El (n, a, cs)
    let txt v = Txt v
    let splice cs = Splice cs
    let raw f = Raw f

    (* Output *)

    let addc = Buffer.add_char
    let adds = Buffer.add_string
    let adds_esc b s =
      (* N.B. we also escape @'s since ocamldoc trips over them. *)
      let len = String.length s in
      let max_idx = len - 1 in
      let flush b start i =
        if start < len then Buffer.add_substring b s start (i - start);
      in
      let rec loop start i =
        if i > max_idx then flush b start i else
        let next = i + 1 in
        match String.get s i with
        | '&' -> flush b start i; adds b "&amp;"; loop next next
        | '<' -> flush b start i; adds b "&lt;"; loop next next
        | '>' -> flush b start i; adds b "&gt;"; loop next next
        | '\'' -> flush b start i; adds b "&apos;"; loop next next
        | '\"' -> flush b start i; adds b "&quot;"; loop next next
        | '@' -> flush b start i; adds b "&commat;"; loop next next
        | c -> loop start next
      in
      loop 0 0

    let void_els = String.Set.of_list
        [ "area"; "base"; "br"; "col"; "embed"; "hr"; "img"; "input"; "link";
          "meta"; "param"; "source"; "track"; "wbr" ]

    let rec add_atts b cs atts =
      let add_att b n v = adds b n; adds b "=\""; adds_esc b v; addc b '\"' in
      match atts with
      | ("class", c) :: atts -> add_atts b (c :: cs) atts
      | (n, v) :: atts -> addc b ' '; add_att b n v; add_atts b cs atts
      | [] when cs = [] -> ()
      | [] -> addc b ' '; add_att b "class" (String.concat " " (List.rev cs))

    let rec add_child b = function (* not T.R. *)
    | Raw r -> adds b r
    | Txt txt -> adds_esc b txt
    | Splice cs -> List.iter (add_child b) cs;
    | El (n, atts, cs) ->
        addc b '<'; adds b n; add_atts b [] atts; addc b '>';
        if not (String.Set.mem n void_els) then begin
          List.iter (add_child b) cs;
          adds b "</"; adds b n; addc b '>';
        end

    let add_doc_type b = adds b "<!DOCTYPE html>\n"
    let buffer_add ~doc_type b cs =
      if doc_type then add_doc_type b; add_child b cs

    let to_string ~doc_type g =
      let b = Buffer.create 65525 in
      buffer_add ~doc_type b g; Buffer.contents b

    (* Predefined element constructors *)

    type cons = ?a:Att.t list -> child list -> child
    type void_cons = a:Att.t list -> child
    let v_void e ~a = v e ~a []
    let a = v "a"
    let abbr = v "abbr"
    let address = v "address"
    let area = v_void "area"
    let article = v "article"
    let aside = v "aside"
    let audio = v "audio"
    let b = v "b"
    let base = v_void "base"
    let bdi = v "bdi"
    let bdo = v "bdo"
    let blockquote = v "blockquote"
    let body = v "body"
    let br = v_void "br"
    let button = v "button"
    let canvas = v "canvas"
    let caption = v "caption"
    let cite = v "cite"
    let code = v "code"
    let col = v_void "col"
    let colgroup = v "colgroup"
    let command = v "command"
    let datalist = v "datalist"
    let dd = v "dd"
    let del = v "del"
    let details = v "details"
    let dfn = v "dfn"
    let div = v "div"
    let dl = v "dl"
    let dt = v "dt"
    let em = v "em"
    let embed = v_void "embed"
    let fieldset = v "fieldset"
    let figcaption = v "figcaption"
    let figure = v "figure"
    let footer = v "footer"
    let form = v "form"
    let h1 = v "h1"
    let h2 = v "h2"
    let h3 = v "h3"
    let h4 = v "h4"
    let h5 = v "h5"
    let h6 = v "h6"
    let head = v "head"
    let header = v "header"
    let hgroup = v "hgroup"
    let hr = v_void "hr"
    let html = v "html"
    let i = v "i"
    let iframe = v "iframe"
    let img = v_void "img"
    let input = v_void "input"
    let ins = v "ins"
    let kbd = v "kbd"
    let keygen = v "keygen"
    let label = v "label"
    let legend = v "legend"
    let li = v "li"
    let link = v_void "link"
    let map = v "map"
    let mark = v "mark"
    let menu = v "menu"
    let meta = v_void "meta"
    let meter = v "meter"
    let nav = v "nav"
    let noscript = v "noscript"
    let object' = v "object"
    let ol = v "ol"
    let optgroup = v "optgroup"
    let option = v "option"
    let output = v "output"
    let p = v "p"
    let param = v_void "param"
    let pre = v "pre"
    let progress = v "progress"
    let q = v "q"
    let rp = v "rp"
    let rt = v "rt"
    let ruby = v "ruby"
    let s = v "s"
    let samp = v "samp"
    let script = v "script"
    let section = v "section"
    let select = v "select"
    let small = v "small"
    let source = v_void "source"
    let span = v "span"
    let strong = v "strong"
    let style = v "style"
    let sub = v "sub"
    let summary = v "summary"
    let sup = v "sup"
    let table = v "table"
    let tbody = v "tbody"
    let td = v "td"
    let textarea = v "textarea"
    let tfoot = v "tfoot"
    let th = v "th"
    let thead = v "thead"
    let time = v "time"
    let title = v "title"
    let tr = v "tr"
    let track = v_void "track"
    let u = v "u"
    let ul = v "ul"
    let var = v "var"
    let video = v "video"
    let wbr = v_void "wbr"

    (* Convenience *)

    let title_of_fpath file = match Fpath.basename ~no_ext:true file with
    | "index" | "" ->
        begin match Fpath.(basename ~no_ext:true (parent file)) with
        | "" -> "Untitled"
        | title -> title
        end
    | title -> title

    let basic_page
        ?(generator = "") ?(lang = "") ?(scripts = []) ?(styles = [])
        ?(more_head = []) ?title:(t = "") body
      =
      let viewport = "width=device-width, initial-scale=1.0" in
      let generator = match generator with
      | "" -> []
      | g -> [meta ~a:Att.[name "generator"; content g]];
      in
      let style uri =
        link ~a:Att.[rel "stylesheet"; type' "text/css"; href uri]
      in
      let script uri =
        script ~a:Att.[type' "text/javascript"; v "defer" "defer"; src uri][]
      in
      let title = if t = "" then [] else [title [txt t]] in
      let head =
        head [
          meta ~a:Att.[charset "utf-8"];
          splice generator;
          meta ~a:Att.[name "viewport"; content viewport];
          splice (List.map style styles);
          splice (List.map script scripts);
          splice more_head;
          splice title ]
      in
      let a = if lang = "" then [] else [Att.v "lang" lang] in
      html ~a [head; body]
  end
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
