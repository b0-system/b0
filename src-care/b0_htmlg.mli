(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** HTML generation.

    Open this module to use it.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)

(** {1:attsels HTML attributes and elements} *)

open B0_std

(** Attributes. *)
module At : sig

  (** {1:atts Attributes} *)

  type name = string
  (** The type for attribute names. *)

  type t
  (** The type for attributes. *)

  val v : name -> string -> t
  (** [v n value] is an attribute named [n] with value [value]. *)

  val add_if : bool -> t -> t list -> t list
  (** [add_if c att atts] is [att :: atts] if [c] is [true] and [atts]
        otherwise. *)

  val add_some : name -> string option -> t list -> t list
  (** [add_some n o atts] is [(v n value) :: atts] if [o] is [Some value] and
      [atts] otherwise. *)

  (** {1:predef Predefined attribute constructors}

        {b Convention.} Whenever an attribute name conflicts with
        an OCaml keyword we prime it, see for example {!class'}. *)

  type 'a cons = 'a -> t
  (** The type for attribute constructors with value of type ['a]. *)

  val autofocus : t
  val charset : string cons
  val checked : t
  val class' : string cons
  val content : string cons
  val disabled : t
  val for' : string cons
  val height : int cons
  val href : string cons
  val id : string cons
  val media : string cons
  val name : string cons
  val placeholder : string cons
  val rel : string cons
  val src : string cons
  val tabindex : int cons
  val title : string cons
  val type' : string cons
  val value : string cons
  val width : int cons
end

(** Elements. *)
module El : sig

  (** {1:els Elements} *)

  type name = string
  (** The type for element names. *)

  type frag
  (** The type for HTML fragments. Either textual data or an element
      or a sequence thereof. *)

  val v : name -> ?a:At.t list -> frag list -> frag
  (** [v n ~a cs] is an element with name [n], attributes [a]
      (defaults to [[]]) and children [cs]. It is illegal to specify
      an attribute name more than once in [a] except for {!At.class'}
      which is treated specially: multiple specifications are gathered
      to form a single space seperated attribute value for the class
      attribute. *)

  val txt : string -> frag
  (** [txt d] is character data [d]. *)

  val splice : ?sep:frag -> frag list -> frag
  (** [splice ?sep cs] when added to the list of children in {!v} splices
      [cs] into the list, separating each fragment by [sep] (defaults
      to {!void}). *)

  val raw : string -> frag
  (** [raw s] is the raw string [s] without escaping markup delimiters.
      This can be used to include foreign markup. *)

  val void : frag
  (** [void] is [splice []]. *)

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
    ?generator:string -> ?lang:string -> ?scripts:string list ->
    ?styles:string list -> ?more_head:frag -> ?title:string ->
    frag -> frag
  (** [basic_page ~generator ~lang ~scripts ~styles ~more_head ~title body]
      is an {!El.html} element containing a {!El.head} element followed
      by [body]. The parameters are used to define a [head] as follows.
      {ul
      {- The page has a charset of UTF-8 (unconditional).}
      {- The page has a viewport with [width=device-width, initial-scale=1]
         (unconditional).}
      {- The page has a generator {!El.meta} tag of [generator], if
         specified and non-empty.}
      {- The page has language [lang], if specified and non-empty.}
      {- The page has a defered script of type [text/javascript] for
         each element of [scripts], in order (defaults to [[]]).}
      {- The page has a stylesheet link of type [text/css] for each element
         of [styles], in order (defaults to [[]]).}
      {- The page has [more_head] in the {!El.head} element
         (defaults to {!void}).}
      {- The page has a title [title], if specified and non-empty.}} *)

  val write_page :
    ?generator:string -> ?lang:string -> ?scripts:string list ->
    ?styles:string list -> ?title:string -> ?more_head:frag -> B00.Memo.t ->
    frag:Fpath.t -> o:Fpath.t -> unit
  (** [write_page m ~title ~frag ~o] writes to [o] a full HTML
      document whose {!body} contains the contents of file [frag]
      (inserted using {!raw}). If title is [""] or unspecified it is
      {!page_title}[ o], for the other arguments and more information
      see {!basic_page}. *)

  (** {1:predef Predefined element constructors}

      {b Convention.} Whenever an element name conflicts with an OCaml
      keyword we prime it, see for example {!object'}. *)

  type cons = ?a:At.t list -> frag list -> frag
  (** The type for element constructors. This is simply {!v} with a
      pre-applied element name. *)

  type void_cons = a:At.t list -> frag
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
