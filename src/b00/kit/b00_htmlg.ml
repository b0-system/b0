(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module At = struct
  type name = string
  type t = name * string
  let v n v = (n, v)
  let true' n = (n, "")
  let int n i = (n, string_of_int i)
  let add_if b at l = if b then at :: l else l
  let add_if_some name o l = match o with None -> l | Some a -> (name, a) :: l
  let to_pair = Fun.id
  module Name = struct
    let accesskey = "accesskey"
    let autofocus = "autofocus"
    let charset = "charset"
    let checked = "checked"
    let class' = "class"
    let content = "content"
    let contenteditable = "contenteditable"
    let cols = "cols"
    let defer = "defer"
    let disabled = "disabled"
    let dir = "dir"
    let draggable = "draggable"
    let for' = "for"
    let height = "height"
    let hidden = "hidden"
    let href = "href"
    let id = "id"
    let lang = "lang"
    let media = "media"
    let name = "name"
    let placeholder = "placeholder"
    let rel = "rel"
    let required = "required"
    let rows = "rows"
    let src = "src"
    let spellcheck = "spellcheck"
    let tabindex = "tabindex"
    let title = "title"
    let type' = "type"
    let value = "value"
    let width = "width"
    let wrap = "wrap"
  end
  type 'a cons = 'a -> t
  let accesskey s = v Name.accesskey s
  let autofocus = true' Name.autofocus
  let charset = v Name.charset
  let checked = true' Name.checked
  let class' s = v Name.class' s
  let cols i = int Name.cols i
  let content s = v Name.content s
  let contenteditable s = true' Name.contenteditable
  let defer = true' Name.defer
  let disabled = true' Name.disabled
  let dir s = v Name.dir s
  let draggable s = true' Name.draggable
  let for' s = v Name.for' s
  let height i = int Name.height i
  let hidden = true' Name.hidden
  let href s = v Name.href s
  let id s = v Name.id s
  let lang s = v Name.lang s
  let media s = v Name.media s
  let name s = v Name.name s
  let placeholder s = v Name.placeholder s
  let rel s = v Name.rel s
  let required = true' Name.required
  let rows i = int Name.rows i
  let src s = v Name.src s
  let spellcheck = v Name.spellcheck
  let tabindex i = int Name.tabindex i
  let title s = v Name.title s
  let type' s = v Name.type' s
  let value s = v Name.value s
  let width i = int Name.width i
  let wrap s = v Name.value s
end

module El = struct
  type name = string
  type frag =
  | El of name * At.t list * frag list
  | Txt of string
  | Splice of frag option * frag list
  | Raw of string

  let v ?(at = []) n cs = El (n, at, cs)
  let txt v = Txt v
  let sp = Txt " "
  let nbsp = Txt "\u{00A0}"
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
  type void_cons = ?at:At.t list -> unit -> frag
  let[@inline] cons e ?at els = v ?at e els
  let[@inline] void_cons e ?at () = v e ?at []
  let a = cons "a"
  let abbr = cons "abbr"
  let address = cons "address"
  let area = void_cons "area"
  let article = cons "article"
  let aside = cons "aside"
  let audio = cons "audio"
  let b = cons "b"
  let base = void_cons "base"
  let bdi = cons "bdi"
  let bdo = cons "bdo"
  let blockquote = cons "blockquote"
  let body = cons "body"
  let br = void_cons "br"
  let button = cons "button"
  let canvas = cons "canvas"
  let caption = cons "caption"
  let cite = cons "cite"
  let code = cons "code"
  let col = void_cons "col"
  let colgroup = cons "colgroup"
  let command = cons "command"
  let datalist = cons "datalist"
  let dd = cons "dd"
  let del = cons "del"
  let details = cons "details"
  let dfn = cons "dfn"
  let div = cons "div"
  let dl = cons "dl"
  let dt = cons "dt"
  let em = cons "em"
  let embed = void_cons "embed"
  let fieldset = cons "fieldset"
  let figcaption = cons "figcaption"
  let figure = cons "figure"
  let footer = cons "footer"
  let form = cons "form"
  let h1 = cons "h1"
  let h2 = cons "h2"
  let h3 = cons "h3"
  let h4 = cons "h4"
  let h5 = cons "h5"
  let h6 = cons "h6"
  let head = cons "head"
  let header = cons "header"
  let hgroup = cons "hgroup"
  let hr = void_cons "hr"
  let html = cons "html"
  let i = cons "i"
  let iframe = cons "iframe"
  let img = void_cons "img"
  let input = void_cons "input"
  let ins = cons "ins"
  let kbd = cons "kbd"
  let keygen = cons "keygen"
  let label = cons "label"
  let legend = cons "legend"
  let li = cons "li"
  let link = void_cons "link"
  let map = cons "map"
  let mark = cons "mark"
  let menu = cons "menu"
  let meta = void_cons "meta"
  let meter = cons "meter"
  let nav = cons "nav"
  let noscript = cons "noscript"
  let object' = cons "object"
  let ol = cons "ol"
  let optgroup = cons "optgroup"
  let option = cons "option"
  let output = cons "output"
  let p = cons "p"
  let param = void_cons "param"
  let pre = cons "pre"
  let progress = cons "progress"
  let q = cons "q"
  let rp = cons "rp"
  let rt = cons "rt"
  let ruby = cons "ruby"
  let s = cons "s"
  let samp = cons "samp"
  let script = cons "script"
  let section = cons "section"
  let select = cons "select"
  let small = cons "small"
  let source = void_cons "source"
  let span = cons "span"
  let strong = cons "strong"
  let style = cons "style"
  let sub = cons "sub"
  let summary = cons "summary"
  let sup = cons "sup"
  let table = cons "table"
  let tbody = cons "tbody"
  let td = cons "td"
  let textarea = cons "textarea"
  let tfoot = cons "tfoot"
  let th = cons "th"
  let thead = cons "thead"
  let time = cons "time"
  let title = cons "title"
  let tr = cons "tr"
  let track = void_cons "track"
  let u = cons "u"
  let ul = cons "ul"
  let var = cons "var"
  let video = cons "video"
  let wbr = void_cons "wbr"

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
    | g -> meta ~at:At.[name "generator"; content g] ()
    in
    let style uri =
      link ~at:At.[rel "stylesheet"; type' "text/css"; href uri] ()
    in
    let script uri =
      script ~at:At.[type' "text/javascript"; defer; src uri] []
    in
    let head = head [
        meta ~at:At.[charset "utf-8"] ();
        generator;
        meta ~at:At.[name "viewport"; content viewport] ();
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
    let open B00_std.Fut.Syntax in
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
    B00_std.Fut.return @@
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
