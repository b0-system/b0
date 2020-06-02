(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTML generation.

    Open this module to use it.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)

(** {1:attsels HTML attributes and elements} *)

open B00_std

(** Attributes. *)
module At : sig

  (** {1:atts Attributes} *)

  type name = string
  (** The type for attribute names. *)

  type t
  (** The type for attributes. *)

  val v : name -> string -> t
  (** [v n value] is an attribute named [n] with value [value]. *)

  val v_true : name -> t
  (** [v_true n] is [v n ""], the {{:https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes}boolean attribute} [n] set to true
      (the attribute must be omitted to be false). *)

  val v_int : name -> int -> t
  (** [v_int n i] is [v n (string_of_int i)]. *)

  val add_if : bool -> t -> t list -> t list
  (** [add_if c att atts] is [att :: atts] if [c] is [true] and [atts]
        otherwise. *)

  val add_some : name -> string option -> t list -> t list
  (** [add_some n o atts] is [(v n value) :: atts] if [o] is [Some value] and
      [atts] otherwise. *)

  (** {1:predef Predefined attribute constructors}

      See the
      {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}MDN
      HTML attribute reference}.

      {b Convention.} Whenever an attribute name conflicts with
      an OCaml keyword we prime it, see for example {!class'}. *)

  type 'a cons = 'a -> t
  (** The type for attribute constructors with value of type ['a]. *)

  val autofocus : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autofocus}
      autofocus} *)

  val charset : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/charset}
      charset} *)

  val checked : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/checked}
      checked} *)

  val class' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/class}
      class} *)

  val content : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/content}
      content} *)

  val defer : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/defer}
      defer} *)

  val disabled : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled}
      disabled} *)

  val for' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/for'}
      for'} *)

  val height : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/height}
      height} *)

  val href : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/href}
      href} *)

  val id : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/id}
      id} *)

  val lang : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/lang}
      lang} *)

  val media : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/media}
      media} *)

  val name : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/name}
      name} *)

  val placeholder : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/placeholder}
      placeholder} *)

  val rel : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rel}
      rel} *)

  val src : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/src}
      src} *)

  val tabindex : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/tabindex}
      tabindex} *)

  val title : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/title}
      title} *)

  val type' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/type}
      type} *)

  val value : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/value}
      value} *)

  val width : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/width}
      width} *)
end

(** Elements and HTML fragments. *)
module El : sig

  (** {1:els Elements} *)

  type name = string
  (** The type for element names. *)

  type frag
  (** The type for HTML fragments. A fragment is either character data or
      a single element or a sequence of elements. *)

  val v : name -> ?at:At.t list -> frag list -> frag
  (** [v n ~at cs] is an element with name [n], attributes [at]
      (defaults to [[]]) and children [cs].

      Except for {!At.class'} the list [at] must not define an
      attribute more than once; this is not checked by the module.
      The {!At.class'} is treated specially: multiple specifications
      are gathered to form a single, space separated, attribute value
      for the [class] HTML attribute. *)

  val txt : string -> frag
  (** [txt d] is character data [d]. *)

  val splice : ?sep:frag -> frag list -> frag
  (** [splice ?sep fs] when added to a list of children in {!v} splices
      fragments [fs] into the list, separating each fragment by
      [sep] (if any). *)

  val void : frag
  (** [void] is [splice []]. *)

  val raw : string -> frag
  (** [raw s] is the raw string [s] without escaping markup delimiters.
      This can be used to include foreign markup. [s] must be well-formed
      HTML otherwise invalid markup will be generated. *)

  (** {1:output Output} *)

  val buffer_add : doc_type:bool -> Buffer.t -> frag -> unit
  (** [buffer_add ~doc_type b frag] adds fragment [frag]. If
      [doc_type] is [true] an HTML doctype declaration is
      prepended. *)

  val to_string : doc_type:bool -> frag -> string
  (** [to_string] is like {!buffer_add} but returns directly a string. *)

  (** {1:convenience Convenience} *)

  val title_of_fpath : Fpath.t -> string
  (** [title_of_fpath p] is a page title for [p] guaranteed to be non
      empty. Either the basename of [file] without extension or if
      that results in ["index"] or [""] the basename of the parent
      directory without extension or if that results in [""],
      ["Untitled"]. *)

  val basic_page :
    ?lang:string -> ?generator:string -> ?styles:string list ->
    ?scripts:string list -> ?more_head:frag -> title:string -> frag -> frag
  (** [basic_page ~lang ~generator ~styles ~scripts ~more_head ~title body]
      is an {!El.html} element with a {!At.lang} attribute of [lang] (if
      specified and non-empty) containing a {!El.head} element (see below)
      followed by [body] which must be a {!El.body} element.

      The other arguments are used to define the children of the page's
      {!El.head} which are in order:
      {ol
      {- A charset {!El.meta} of UTF-8 (unconditional).}
      {- A generator {!El.meta} of [generator], if specified an non-empty.}
      {- A viewport {!El.meta} with [width=device-width, initial-scale=1]
         (unconditional).}
      {- A stylesheet {!El.link} of type [text/css] for each element
         of [styles], in order (defaults to [[]]).}
      {- A {e deferred} {!El.script} of type [text/javascript] for
         each element of [scripts], in order (defaults to [[]]).}
      {- [more_head] (defaults to {!El.void}).}
      {- The page has a title [title], which must be non-white and
         and non-empty (falls back to ["Untitled"] in any of
         these cases).}} *)

  val write_page :
    ?lang:string -> ?generator:string -> ?styles:string list ->
    ?scripts:string list -> ?more_head:frag -> ?title:string ->
    B00.Memo.t -> frag:Fpath.t -> o:Fpath.t -> unit
  (** [write_page m ~frag ~o] reads [frag] and inserts it
      in an {!El.body} using {!raw} and writes a full HTML document
      to [o] using {!basic_page} (see doc of the corresponding arguments).
      If [title] is [""] or unspecified {!page_title}[ o] is used. *)

  (** {1:predef Predefined element constructors}

      See the
      {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element}MDN
      HTML element reference}.

      {b Convention.} Whenever an element name conflicts with an OCaml
      keyword we prime it, see for example {!object'}. *)

  type cons = ?at:At.t list -> frag list -> frag
  (** The type for element constructors. This is simply {!v} with a
      pre-applied element name. *)

  type void_cons = at:At.t list -> frag
  (** The type for void element constructors. This is simply {!el}
      with a pre-applied element name and without children. *)

  val a : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a}a} *)

  val abbr : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr}abbr} *)

  val address : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address}
      address} *)

  val area : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area}
      area} *)

  val article : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article}
      article} *)

  val aside : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside}
      aside} *)

  val audio : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio}
      audio} *)

  val b : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b}b} *)

  val base : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/base}
      base} *)

  val bdi : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi}
      bdi} *)

  val bdo : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo}
      bdo} *)

  val blockquote : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote}
      blockquote} *)

  val body : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body}
      body} *)

  val br : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br}br} *)

  val button : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button}
      button} *)

  val canvas : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas}
      canvas} *)

  val caption : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption}
      caption} *)

  val cite : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite}
      cite} *)

  val code : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code}
      code} *)

  val col : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col}
      col} *)

  val colgroup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup}
      colgroup} *)

  val command : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/command}
        command} *)

  val datalist : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist}
      datalist} *)

  val dd : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd}dd} *)

  val del : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del}
      del} *)

  val details : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details}
      details} *)

  val dfn : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn}
      dfn} *)

  val div : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div}
      div} *)

  val dl : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl}dl} *)

  val dt : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt}dt} *)

  val em : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em}em} *)

  val embed : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed}
      embed} *)

  val fieldset : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset}
      fieldset} *)

  val figcaption : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption}
      figcaption} *)

  val figure : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure}
      figure} *)

  val footer : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer}
      footer} *)

  val form : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form}
        form} *)

  val h1 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1}h1} *)

  val h2 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2}h2} *)

  val h3 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3}h3} *)

  val h4 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4}h4} *)

  val h5 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5}h5} *)

  val h6 : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6}h6} *)

  val head : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/head}
      head} *)

  val header : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header}
      header} *)

  val hgroup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hgroup}
        hgroup} *)

  val hr : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr}hr} *)

  val html : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/html}
        html} *)

  val i : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i}i} *)

  val iframe : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe}
        iframe} *)

  val img : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img}
        img} *)

  val input : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input}
        input} *)

  val ins : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins}
        ins} *)

  val kbd : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd}
        kbd} *)

  val keygen : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen}
        keygen} *)

  val label : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label}
        label} *)

  val legend : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend}
      legend} *)

  val li : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li}li} *)

  val link : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link}link} *)

  val map : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map}map} *)

  val mark : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark}mark} *)

  val menu : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}menu} *)

  val meta : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}meta} *)

  val meter : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}
      meter} *)

  val nav : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav}nav} *)

  val noscript : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/noscript}
      noscript} *)

  val object' : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object}
      object} *)

  val ol : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol}ol} *)

  val optgroup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup}
      optgroup} *)

  val option : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option}
      option} *)

  val output : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output}
      output} *)

  val p : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p}p} *)

  val param : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param}
      param} *)

  val pre : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre}
      pre} *)

  val progress : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress}
      progress} *)

  val q : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q}q} *)

  val rp : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp}rp} *)

  val rt : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt}rt} *)

  val ruby : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby}ruby} *)

  val s : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s}s} *)

  val samp : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp}
      samp} *)

  val script : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script}
      script} *)

  val section : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section}
      section} *)

  val select : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select}
      select} *)

  val small : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small}
      small} *)

  val source : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source}
      source} *)

  val span : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span}
      span} *)

  val strong : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong}
      strong} *)

  val style : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style}
      style} *)

  val sub : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub}
      sub} *)

  val summary : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary}
      summary} *)

  val sup : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup}
      sup} *)

  val table : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table}
      table} *)

  val tbody : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody}
      tbody} *)

  val td : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td}td} *)

  val textarea : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea}
      textarea} *)

  val tfoot : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot}
      tfoot} *)

  val th : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th}th} *)

  val thead : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead}
      thead} *)

  val time : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time}
      time} *)

  val title : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/title}
      title} *)

  val tr : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr}tr} *)

  val track : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track}
      track} *)

  val u : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u}u} *)

  val ul : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul}ul} *)

  val var : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var}
      var} *)

  val video : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video}
      video} *)

  val wbr : void_cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr}
      wbr} *)
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
