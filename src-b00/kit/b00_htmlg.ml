(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module At = struct
  type name = string
  type t = name * string
  let v n v = (n, v)
  let v_true n = v n ""
  let v_int n i = v n (string_of_int i)
  let add_if c att atts = if c then att :: atts else atts
  let add_some n o atts = match o with
  | None -> atts | Some value -> (v n value) :: atts

  type 'a cons = 'a -> t
  let autofocus = v_true "autofocus"
  let charset = v "charset"
  let checked = v_true "checked"
  let class' = v "class"
  let content = v "content"
  let defer = v_true "defer"
  let disabled = v_true "disabled"
  let for' = v "for"
  let height = v_int "height"
  let href = v "href"
  let id = v "id"
  let lang = v "lang"
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
  | Splice of frag option * frag list
  | Raw of string

  let v n ?(at = []) cs = El (n, at, cs)
  let txt v = Txt v
  let splice ?sep cs = Splice (sep, cs)
  let void = Splice (None, [])
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

  let void_els = B00_std.String.Set.of_list
      [ "area"; "base"; "br"; "col"; "embed"; "hr"; "img"; "input"; "link";
        "meta"; "param"; "source"; "track"; "wbr" ]

  let rec add_ats b cs atts =
    let add_at b n v = adds b n; adds b "=\""; adds_esc b v; addc b '\"' in
    match atts with
    | ("class", c) :: atts -> add_ats b (c :: cs) atts
    | (n, v) :: atts -> addc b ' '; add_at b n v; add_ats b cs atts
    | [] when cs = [] -> ()
    | [] -> addc b ' '; add_at b "class" (String.concat " " (List.rev cs))

  let rec add_child b = function (* not T.R. *)
  | Raw r -> adds b r
  | Txt txt -> adds_esc b txt
  | Splice (sep, cs) ->
      begin match sep with
      | None -> List.iter (add_child b) cs
      | Some sep ->
          begin match cs with
          | [] -> ()
          | c :: cs ->
              let add b c = add_child b sep; add_child b c in
              add_child b c; List.iter (add b) cs
          end
      end
  | El (n, atts, cs) ->
      addc b '<'; adds b n; add_ats b [] atts; addc b '>';
      if not (B00_std.String.Set.mem n void_els)
      then (List.iter (add_child b) cs; adds b "</"; adds b n; addc b '>')

  let add_doc_type b = adds b "<!DOCTYPE html>\n"
  let buffer_add ~doc_type b cs =
    if doc_type then add_doc_type b; add_child b cs

  let to_string ~doc_type g =
    let b = Buffer.create 65525 in
    buffer_add ~doc_type b g; Buffer.contents b

  (* Predefined element constructors *)

  type cons = ?at:At.t list -> frag list -> frag
  type void_cons = at:At.t list -> frag
  let v_void e ~at = v e ~at []
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

  let title_of_fpath file = match B00_std.Fpath.basename ~no_ext:true file with
  | "index" | "" ->
      let title = B00_std.Fpath.(basename ~no_ext:true (parent file)) in
      if title = "" then "Untitled" else title
  | title -> title

  let basic_page
      ?(lang = "") ?(generator = "") ?(styles = []) ?(scripts = [])
      ?(more_head = void) ~title:t body
    =
    let viewport = "width=device-width, initial-scale=1.0" in
    let generator = match generator with
    | "" -> void
    | g -> meta ~at:At.[name "generator"; content g]
    in
    let style uri =
      link ~at:At.[rel "stylesheet"; type' "text/css"; href uri]
    in
    let script uri =
      script ~at:At.[type' "text/javascript"; defer; src uri] []
    in
    let head = head [
        meta ~at:At.[charset "utf-8"];
        generator;
        meta ~at:At.[name "viewport"; content viewport];
        splice (List.map style styles);
        splice (List.map script scripts);
        more_head;
        title [txt (if String.trim t = "" then "Untilted" else t)]]
    in
    let at = if lang = "" then [] else [At.lang lang] in
    html ~at [head; body]

  let write_page
      ?(lang = "") ?(generator = "") ?(styles = []) ?(scripts = [])
      ?more_head ?(title = "") m ~frag ~o
    =
    (* FIXME Ideally we would like the read to be in write.
       The write fun return a future but this has other impacts. *)
    let open B00.Memo.Fut.Syntax in
    ignore @@ (* FIXME maybe get rid of that. *)
    let* contents = B00.Memo.read m frag in
    let title = if title = "" then title_of_fpath o else title in
    let more_head = match more_head with
    | None -> ""
    | Some more_head -> to_string ~doc_type:false more_head
    in
    let stamp = lang :: generator :: more_head :: title :: [] in
    let stamp = List.rev_append styles stamp in
    let stamp = List.rev_append scripts stamp in
    let stamp = String.concat "" stamp in
    B00.Memo.Fut.return m @@
    (B00.Memo.write m ~stamp ~reads:[frag] o @@ fun () ->
    let more_head = raw more_head in
    let body = body [raw contents] in
    let page =
      basic_page ~lang ~generator ~styles ~scripts ~more_head ~title body
    in
    Ok (to_string ~doc_type:true page))
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
