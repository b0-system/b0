(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

module At = struct
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
  type frag =
  | El of name * At.t list * frag list
  | Txt of string
  | Splice of frag list
  | Raw of string

  let v n ?(a = []) cs = El (n, a, cs)
  let txt v = Txt v
  let splice cs = Splice cs
  let raw f = Raw f
  let void = Splice []

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

  type cons = ?a:At.t list -> frag list -> frag
  type void_cons = a:At.t list -> frag
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
      ?(more_head = void) ?title:(t = "") body
    =
    let viewport = "width=device-width, initial-scale=1.0" in
    let generator = match generator with
    | "" -> void
    | g -> meta ~a:At.[name "generator"; content g]
    in
    let style uri =
      link ~a:At.[rel "stylesheet"; type' "text/css"; href uri]
    in
    let script uri =
      script ~a:At.[type' "text/javascript"; v "defer" "defer"; src uri][]
    in
    let title = if t = "" then void else title [txt t] in
    let head = head [
        meta ~a:At.[charset "utf-8"];
        generator;
        meta ~a:At.[name "viewport"; content viewport];
        splice (List.map style styles);
        splice (List.map script scripts);
        more_head;
        title; ]
    in
    let a = if lang = "" then [] else [At.v "lang" lang] in
    html ~a [head; body]

  let write_page
      ?(generator = "") ?(lang = "") ?(scripts = []) ?(styles = [])
      ?(title = "") ?more_head m ~frag ~o
    =
    B00.Memo.read m frag @@ fun contents ->
    let title = if title = "" then title_of_fpath o else title in
    let stamp =
      let data = generator :: lang :: title :: List.rev_append styles scripts in
      String.concat "" data
    in
    B00.Memo.write m ~stamp ~reads:[frag] o @@ fun () ->
    let body = body [raw contents] in
    let page = basic_page ~generator ~lang ~scripts ~styles ~title body in
    Ok (to_string ~doc_type:true page)
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
