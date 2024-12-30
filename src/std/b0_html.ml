(*---------------------------------------------------------------------------
   Copyright (c) 2016 The htmlit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* HTML escaping *)

module String_set = Set.Make (String)

let add_escaped b s =
  let adds = Buffer.add_string in
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
    | c -> loop start next
  in
  loop 0 0

let escape s = let b = Buffer.create 255 in add_escaped b s; Buffer.contents b

(* Attributes *)

module At = struct
  type name = string
  type t = name * string
  let v n v = (n, v)
  let void = ("", "")
  let int n i = (n, string_of_int i)
  let true' n = (n, "")
  let if' b at = if b then at else void
  let if_some = function None -> void | Some at -> at
  let is_void (n, _) = String.equal n ""
  let equal = Stdlib.( = )
  let compare = Stdlib.compare
  let to_pair = Fun.id
  let of_pair = Fun.id
  let pp ppf (n, v) = match n with
  | "" -> () | n -> Format.fprintf ppf "@[%s=\"%s\"@]" n v

  type 'a cons = 'a -> t
  let accesskey s = v "accesskey" s
  let action s = v "action" s
  let alt s = v "alt" s
  let autocomplete s = v "autocomplete" s
  let autofocus = true' "autofocus"
  let charset = v "charset"
  let checked = true' "checked"
  let class' s = v "class" s
  let cols i = int "cols" i
  let colspan i = int "cols" i
  let content s = v "content" s
  let contenteditable s = true' "contenteditable"
  let datetime s = v "datetime" s
  let defer = true' "defer"
  let dir s = v "dir" s
  let disabled = true' "disabled"
  let draggable b = v "draggable" (string_of_bool true) (* not a boolean attr *)
  let download s = v "download" s
  let for' s = v "for" s
  let height i = int "height" i
  let hidden = true' "hidden"
  let href s = v "href" s
  let id s = v "id" s
  let lang s = v "lang" s
  let list s = v "list" s
  let media s = v "media" s
  let method' s = v "method" s
  let name s = v "name" s
  let placeholder s = v "placeholder" s
  let popover s = v "popover" s
  let popovertarget s = v "popovertarget" s
  let popovertargetaction s = v "popovertargetaction" s
  let rel s = v "rel" s
  let required = true' "required"
  let role s = v "role" s
  let rows i = int "rows" i
  let rowspan i = int "rowspan" i
  let selected = true' "selected"
  let spellcheck = v "spellcheck"
  let src s = v "src" s
  let style s = v "style" s
  let tabindex i = int "tabindex" i
  let title s = v "title" s
  let type' s = v "type" s
  let value s = v "value" s
  let width i = int "width" i
  let wrap s = v "value" s
end

(* HTML elements and fragments *)

module El = struct
  module Low = struct
    type name = string
    type t =
    | El of name * At.t list * t list
    | Txt of string
    | Splice of t option * t list
    | Raw of string

    let of_html f = f
  end
  type name = Low.name
  type html = Low.t

  let v ?(at = []) n cs = Low.El (n, at, cs)
  let txt v = Low.Txt v
  let txt_of f v = Low.Txt (f v)
  let sp = Low.Txt " "
  let nbsp = Low.Txt "\u{00A0}"
  let splice ?sep cs = Low.Splice (sep, cs)
  let unsafe_raw f = Low.Raw f
  let void = Low.Splice (None, [])
  let is_void = function
  | Low.Splice (_, []) | Low.Txt "" | Low.Raw "" -> true | _ -> false

  (* Rendering *)

  let addc = Buffer.add_char
  let adds = Buffer.add_string
  let add_doctype b = adds b "<!DOCTYPE html>\n"
  let add_at b n v = adds b n; adds b "=\""; add_escaped b v; addc b '\"'
  let rec add_atts b cs (* classes *) ss (* styles *) = function
  | ("", _) :: atts -> add_atts b cs ss atts
  | ("class", c) :: atts -> add_atts b (c :: cs) ss atts
  | ("style", s) :: atts -> add_atts b cs (s :: ss) atts
  | (n, v) :: atts -> addc b ' '; add_at b n v; add_atts b cs ss atts
  | [] ->
      let merge sep vs = String.concat sep (List.rev vs) in
      if cs <> [] then (addc b ' '; add_at b "class" (merge " " cs));
      if ss <> [] then (addc b ' '; add_at b "style" (merge ";" ss))

  let void_els =
    String_set.of_list
      [ "area"; "base"; "br"; "col"; "embed"; "hr"; "img"; "input"; "link";
        "meta"; "param"; "source"; "track"; "wbr" ]

  let rec add_child b = function (* not tail-recursive *)
  | Low.Raw r -> adds b r
  | Low.Txt txt -> add_escaped b txt
  | Low.El (n, atts, cs) ->
      addc b '<'; adds b n; add_atts b [] [] atts; addc b '>';
      if not (String_set.mem n void_els)
      then (List.iter (add_child b) cs; adds b "</"; adds b n; addc b '>')
  | Low.Splice (None, cs) -> List.iter (add_child b) cs
  | Low.Splice (Some sep, []) -> ()
  | Low.Splice (Some sep, c :: cs) ->
      let add b c = add_child b sep; add_child b c in
      add_child b c; List.iter (add b) cs

  let buffer_add ~doctype b cs = if doctype then add_doctype b; add_child b cs
  let to_string ~doctype g =
    let b = Buffer.create 1024 in buffer_add ~doctype b g; Buffer.contents b

  (* Predefined element constructors *)

  type cons = ?at:At.t list -> html list -> html
  type void_cons = ?at:At.t list -> unit -> html
  let cons e ?at els = v ?at e els
  let void_cons e ?at () = v e ?at []
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
  let main = cons "main"
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

  (* Page *)

  let page
      ?(lang = "") ?(generator = "") ?(styles = []) ?(scripts = [])
      ?(more_head = void) ~title:t body
    =
    let viewport = "width=device-width, initial-scale=1.0" in
    let generator = match generator with
    | "" -> void | g -> meta ~at:At.[name "generator"; content g] ()
    in
    let style = function
    | "" -> void
    | url -> link ~at:At.[rel "stylesheet"; type' "text/css"; href url] ()
    in
    let script = function
    | "" -> void
    | url -> script ~at:At.[type' "text/javascript"; defer; src url] []
    in
    let t = match String.trim t with "" -> "Untitled" | t -> t in
    let head = head [
        meta ~at:At.[charset "utf-8"] ();
        generator;
        meta ~at:At.[name "viewport"; content viewport] ();
        splice (List.map style styles);
        splice (List.map script scripts);
        more_head;
        title [txt t]]
    in
    let at = if lang = "" then [] else [At.lang lang] in
    html ~at [head; body]

  let title_of_filepath f =
    let rec base ~snd init f =
      let start = ref init in
      let last = ref (init + 1) in
      while not (!start < 0  || f.[!start] = '/' || f.[!start] = '\\')
      do if f.[!start] = '.' then last := !start; decr start done;
      let first = !start + 1 in
      let last = !last - 1 in
      if first <= last && last <= init then
        begin
          let s = String.sub f first (last - first + 1) in
          if not (s = "index" || s = "") then s else
          if snd = true then "Untitled" else base ~snd:true (!start - 1) f
        end
      else
      if !start < 0 || snd = true then "Untitled" else
      base ~snd:true (!start - 1) f
    in
    base ~snd:false (String.length f - 1) f
end
