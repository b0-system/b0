(*---------------------------------------------------------------------------
   Copyright (c) 2016 The htmlit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTML generation.

    See the {{!page-index.quick_start}quick start}.

    Open this module to use it, it defines only modules in your scope.

    {b Convention.} Whenever an attribute or element name conflicts
    with an OCaml keyword we prime it. See for example {!At.class'} or
    {!El.object'}.

    {b Warning.} The module takes care of escaping the data you
    provide but it assumes you give it UTF-8 encoded strings; this is
    not checked by the module. Also be careful with
    {{!El.style}[style]} elements. *)

(** HTML attributes.

    See the {{!At.constructors}attribute constructors}. *)
module At : sig

  (** {1:attributes Attributes} *)

  type name = string
  (** The type for attribute names. *)

  type t
  (** The type for attributes. *)

  val v : name -> string -> t
  (** [v n value] is an attribute named [n] with value [value].
      Favour use of {{!constructors}attribute constructors} whenever
      possible. *)

  val void : t
  (** [void] is [v "" ""], an attribute that doesn't get rendered. See also
      {!is_void}. *)

  val int : name -> int -> t
  (** [int n i] is [v n (string_of_int i)]. *)

  val true' : name -> t
  (** [true' n] is [v n ""]. This sets the
      {{:https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes}boolean attribute}
      [n] to true. The attribute must be omitted to be false. See also
      {!if'} and {!if_some}. *)

  val if' : bool -> t -> t
  (** [if' cond at] is [if cond then at else void]. *)

  val if_some : t option -> t
  (** [if_some o] is [at] if [o] is [Some at] and {!void} if [o] is [None]. *)

  (** {1:preds Predicates and comparisons} *)

  val is_void : t -> bool
  (** [is_void at] is [true] iff the name of [at] is empty. These attributes
      render nothing. See also {!void}. *)

  val equal : t -> t -> bool
  (** [equal at0 at1] is [true] if both the name and value of [at0] and
      [at1] are {!String.equal}. *)

  val compare : t -> t -> int
  (** [compare] is a total order on attributes compatible with
      {!equal}. *)

  (** {1:conv Converting and formatting} *)

  val to_pair : t -> string * string
  (** [to_pair at] is [(n, v)] the name and value of the attribute. *)

  val of_pair : string * string -> t
  (** [of_pair (n, value)] is [v n value]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf at] formats [at] on [ppf] using HTML syntax. {!is_void}
      attributes format nothing. *)

  (** {1:constructors Attribute constructors}

      See the
      {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}MDN
      HTML attribute reference}. *)

  type 'a cons = 'a -> t
  (** The type for attribute constructors with attribute values of type ['a]. *)

  val accesskey : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/accesskey}accesskey} *)

  val action : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#action}
      action} *)

  val alt : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img#alt}
      alt} *)

  val autocomplete : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete}autocomplete} *)

  val autofocus : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autofocus}autofocus} *)

  val charset : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
      charset} *)

  val checked : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#checked}checked} *)

  val class' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/class}class} *)

  val cols : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#cols}cols} *)

  val colspan : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td#colspan}
      colspan} *)

  val content : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta#content}content} *)

  val contenteditable : bool cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/contenteditable}contenteditable} *)

  val datetime : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time#datetime}datetime} *)

  val defer : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script#defer}defer} *)

  val dir : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/dir}dir} *)

  val disabled : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled}
      disabled} *)

  val draggable : bool cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/draggable}draggable} *)

  val download : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#download}
      download}. *)

  val for' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/for}
      for'} *)

  val height : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}height} *)

  val hidden : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hidden}hidden} *)

  val href : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}href} *)

  val id : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id}
      id} *)

  val lang : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang}lang} *)

  val list : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#list}list} *)

  val media : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}media} *)

  val method' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form#method}method}. *)

  val name : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}name} *)

  val placeholder : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
      placeholder} *)

  val popover : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/popover}
      popover} *)

  val popovertarget : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button#popovertarget}
      popovertarget} *)

  val popovertargetaction : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button#popovertarget}
      popovertargetaction}. *)

  val rel : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/rel}
      rel} *)

  val required : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/required}
      required} *)

  val role : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles}
      role} *)

  val rows : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#rows}rows} *)

  val rowspan : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td#rowspan}
      rowspan} *)

  val selected : t
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option#selected}selected} *)

  val spellcheck : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/spellcheck}spellcheck} *)

  val src : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}src} *)

  val style : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/style}style} *)

  val tabindex : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/tabindex}tabindex} *)

  val title : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/title}title} *)

  val type' : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
      type} *)

  val value : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}value} *)

  val wrap : string cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea#wrap}wrap} *)

  val width : int cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes}
      width} *)
end

(** HTML elements and fragments.

    See the {{!El.constructors}element constructors} and
    {{!El.section-page}minimal page generation}.  *)
module El : sig

  (** {1:fragments HTML fragments} *)

  type html
  (** The type for HTML fragments. A fragment is either:
      {ul
      {- A single HTML {{!elements}element}.}
      {- {{!section-text}Text}: character data.}
      {- A {{!section-splice}splice}: a list of fragments.}
      {- {{!unsafe_raw_data}Unsafe raw data}: character data
         output as is, without escaping.}} *)

  (** {2:elements Elements} *)

  type name = string
  (** The type for element names. *)

  val v : ?at:At.t list -> name -> html list -> html
  (** [v ?at n cs] is an element with name [n], attributes [at]
      and children [cs]. Favour {{!constructors}element constructors} whenever
      possible.
      Regarding [at]:
      {ul
      {- [at] defaults to [[]].}
      {- Except for {!At.class'} and {!At.style} attributes, [at] must not
         specify an attribute more than once. This is not checked by the
         module and what happens on multiple definitions is undefined.}
      {- For {!At.class'} attributes, multiple specifications are gathered to
         form a single, space separated, attribute value for the
         {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/class}[class] HTML attribute}.}
      {- For {!At.style} attributes, multiple specifications are gathered to
        form a single, semi-colon separated, attribute value for the
        {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/style}[style] HTML attribute}.}} *)

  (** {2:text Text} *)

  val txt : string -> html
  (** [txt d] is character data [d]. *)

  val txt_of : ('a -> string) -> 'a -> html
  (** [txt_of f v] is [txt (f v)]. Cuts a bit on delimiter orgies. *)

  val sp : html
  (** [sp] is [El.txt " "]. *)

  val nbsp : html
  (** [nbsp] is [El.txt "\u{00A0}"]. *)

  (** {2:splice Splices} *)

  val splice : ?sep:html -> html list -> html
  (** [splice ?sep hs] is the list of fragments [hs] separated by
      [sep] (if any). When added to an element's children, the list
      is spliced in the element's children. *)

  (** {2:unsafe_raw_data Unsafe raw data} *)

  val unsafe_raw : string -> html
  (** [unsafe_raw s] is the raw string [s] without escaping markup delimiters.
      [s] must be well-formed HTML otherwise invalid markup is generated.
      This can be used to:
      {ul
      {- Include foreign markup, for example markup generated by another
         mechanism.}
      {- Avoid unpleasant surprises with the {!style} element.}
      {- Let user-generated content create
         {{:https://owasp.org/www-community/attacks/xss/}XSS attacks}
         in your application.}} *)

   (** {2:voids Voids}

       Like {{!At.is_void}void attributes}, void fragments render nothing. *)

  val void : html
  (** [void] is [splice []]. See also {!is_void}. *)

  val is_void : html -> bool
  (** [is_void h] is [true] iff [h] is an empty {!val-splice}, an empty
      {!txt} or an empty {!unsafe_raw}. These fragments render nothing.
      See also {!void}. *)

  (** {1:rendering Rendering} *)

  val buffer_add : doctype:bool -> Buffer.t -> html -> unit
  (** [buffer_add ~doctype b h] adds the HTML fragment [h] to [b]. If
      [doc_type] is [true] an HTML doctype declaration is
      prepended. *)

  val to_string : doctype:bool -> html -> string
  (** [to_string] is like {!buffer_add} but returns directly a string. *)

  (** Low level representation (unstable).

      This representation may change even between minor versions
      of the library. Use at your own risk. *)
  module Low : sig
    type t =
    | El of name * At.t list * t list
    (** Element, name attributes and children. *)
    | Txt of string
    (** Character data. *)
    | Splice of t option * t list
    (** List of parts, separated by an optional separator. *)
    | Raw of string
    (** Raw output string. *)
    (** The low-level HTML fragment representation. *)

    val of_html : html -> t
    (** [of_html h] is a low-level representation for [h]. *)
  end

  (** {1:page Page}

      There's more than one way to generate a basic minimal HTML page. The
      following provides good defaults for quick minimal pages. *)

  val page :
    ?lang:string -> ?generator:string -> ?styles:string list ->
    ?scripts:string list -> ?more_head:html -> title:string -> html -> html
  (** [page ~lang ~title body] is an {!El.val-html} element with an
      {!At.lang} attribute of [lang] (if specified and non-empty)
      containing a {!El.head} element followed by [body] which must be
      a {!El.body} element.

      The children of the {!El.head} element are in order:
      {ol
      {- An {!El.meta} charset with value [utf-8], unconditional.}
      {- An {!El.meta} generator with value [generator], if specified and
         non-empty.}
      {- An {!El.meta} viewport with value
         [width=device-width, initial-scale=1], unconditional.}
      {- For each non-empty element [href] of [styles] (defaults to [[]]), an
         {!El.link} with {!At.type'} [text/css] and {!At.href} value [href].
         In order.}
      {- For each non-empty element [src] of [scripts] (defaults to [[]]),
         an {!El.script} with {!At.defer}, {!At.type'} value
         [text/javascript] and {!At.src} value [src]. In order.}
      {- [more_head] fragment (defaults to {!El.void}). Be {{!style}careful}
         if you add [style] tags with direct CSS source.}
      {- An {!El.title} with value [title] which is {!String.trim}ed.
         If the result is empty falls back to ["Untitled"].
         See also {!title_of_filepath} to derive titles from
         file paths.}} *)

  val title_of_filepath : string -> string
  (** [title_of_filepath f] is a {e non-empty} page title for filepath
      [f]. Either the basename of [f] without extension or if that
      results in ["index"] or [""] the basename of the parent
      directory without extension or if that results in [""] the value
      ["Untitled"]. Directory separators can be ['/'] or ['\\']
      regardless of the platform. *)

  (** {1:constructors Element constructors}

      See the
      {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element}MDN
      HTML element reference}. *)

  type cons = ?at:At.t list -> html list -> html
  (** The type for element constructors. This is simply {!El.v} with a
      pre-applied element name. *)

  type void_cons = ?at:At.t list -> unit -> html
  (** The type for void element constructors. This is simply {!El.v}
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

  val main : cons
  (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main}main} *)

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
      style}

      {b Warning.} If your style element contains CSS source, use
      {!unsafe_raw} to specify it. Otherwise the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/Child_combinator}
      CSS child combinator ['>']} gets escaped. *)

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
