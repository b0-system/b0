(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** World Wide Web interaction.

    Toys to interact with the World Wide Web. *)

(** {1:www WWW} *)

open B0_std

(** HTTP requests.

    HTTP requests via [curl]. *)
module Http : sig

  (** {1:meth HTTP methods and headers} *)

  type meth =
  [ `CONNECT | `DELETE | `GET | `HEAD | `OPTIONS | `Other of string
  | `PATCH | `POST | `PUT | `TRACE ]
  (** The type for HTTP methods. *)

  val meth_to_string : meth -> string
  (** [meth_to_string m] is a string representation of [m]. *)

  type headers = (string * string) list
  (** The type for HTTP headers. *)

  (** {1:requests HTTP requests} *)

  type req
  (** The type for HTTP requests. *)

  val req : ?headers:headers -> ?body:string -> uri:string -> meth -> req
  (** [req uri m ~headers ~body] is a request on [uri] with method [m],
      headers [headers] (defaults to [[]]) and body [body] (defaults to
      [""]). *)

  val req_uri : req -> string
  (** [req_uri r] is [r]'s request URI. *)

  val req_meth : req -> meth
  (** [req_meth r] is [r]'s HTTP method. *)

  val req_headers : req -> headers
  (** [req_headers r] is [r]'s headers. *)

  val req_body : req -> string
  (** [req_body r] is [r]'s body. *)

  (** {1:responses HTTP responses} *)

  type resp
  (** The type for HTTP responses. *)

  val resp_headers : resp -> headers
  (** [resp_headers r] are the HTTP response headers. *)

  val resp_status : resp -> int
  (** [resp_status r] is the HTTP response status. *)

  val resp_body : resp -> string
  (** [resp_body r] is the HTTP response body. *)

  (** {1:peforming Performing requests} *)

  type t
  (** The type for HTTP requestors. *)

  val curl :
    ?docs:string -> ?env:Cmdliner.Arg.env -> unit -> Cmd.t Cmdliner.Term.t
  (** [curl] is a cli interface for specifying the curl command
      line tool. *)

  val find_curl :
    ?search:Fpath.t list -> curl:Cmd.t -> unit -> (t, string) result

  val perform : ?follow:bool -> t -> req -> (resp, string) result
  (** [perform curl r] performs request [r] via [curl] which is looked up
      in the PATH or in the environment variable [B0_CURL].  If
      [follow] is [true] (default) HTTP redirects for GET and HEAD
      requests that return 301, 302, 303, 305 or 307 are automatically
      followed. *)
end

(** HTML generation.

    Open this module to use it.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)
module Htmlg : sig

  (** {1:attsels HTML attributes and elements} *)

  (** Attributes. *)
  module Att : sig

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
    (** [add_some n o atts] is [(v n value) :: atts] if [o] is
        [Some value] and [atts] otherwise. *)

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

    type child
    (** The type for element children. Either textual data or an element
        or a sequence thereof. *)

    val v : name -> ?a:Att.t list -> child list -> child
    (** [v n ~a cs] is an element with name [n], attributes [a]
        (defaults to [[]]) and children [cs]. It is illegal to specify
        an attribute name more than once in [a] except for
        {!Att.class'} which is treated specially: multiple
        specifications are gathered to form a single space seperated
        attribute value for the class attribute. *)

    val txt : string -> child
    (** [txt d] is character data [d]. *)

    val splice : child list -> child
    (** [splice cs] when added to a child list splices [cs] into
        the list. *)

    val raw : string -> child
    (** [raw s] is the raw string [s] without escaping markup delimiters.
        This can be used to include foreign markup. *)

    (** {1:output Output} *)

    val buffer_add : doc_type:bool -> Buffer.t -> child -> unit
    (** [buffer_add ~doc_type b c] adds child [c] (or children if [c]
        is a {!splice}) to [b].  If [doc_type] is [true] an HTML
        doctype declaration is prepended. *)

    val to_string : doc_type:bool -> child -> string
    (** [to_string] is like {!buffer_add} but returns directly a string. *)

    (** {1:predef Predefined element constructors}

        {b Convention.} Whenever an element name conflicts with an OCaml
        keyword we prime it, see for example {!object'}. *)

    type cons = ?a:Att.t list -> child list -> child
    (** The type for element constructors. This is simply {!v} with
        a pre-applied element name. *)

    type void_cons = a:Att.t list -> child
    (** The type for void element constructors. This is simply {!el}
         with a pre-applied element name and without children. *)

    val a : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a}a} *)

    val abbr : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr}
        abbr} *)

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
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link}
        link} *)

    val map : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map}
        map} *)

    val mark : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark}
        mark} *)

    val menu : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu}
        menu} *)

    val meta : void_cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meta}
        meta} *)

    val meter : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter}
        meter} *)

    val nav : cons
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav}
        nav} *)

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
    (** {{:https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby}
        ruby} *)

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
end

(** JSON text codec.

    {b Warning.} The module assumes strings are UTF-8 encoded. *)
module Json : sig

  (** {1:repr Generic JSON representation} *)

  type t =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of t list | `O of (string * t) list ]
  (** The type for generic JSON text representations. *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses JSON text from [s] according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} with the following
      limitations:
      {ul
      {- Numbers are parsed with [string_of_float] which is not
       compliant.}.
      {- Unicode escapes are left unparsed (this will not round trip
       with {!to_string}).}} *)

  val to_string : t -> string
  (** [to_string v] is [v] as JSON text, encoded according to
      {{:https://tools.ietf.org/html/rfc8259}RFC8259} *)
end

(** JSON value generation. *)
module Jsong : sig

  (** {1:gen Generation} *)

  type t
  (** The type for generated JSON values. *)

  val null : t
  (** [null] is the generated JSON null value. *)

  val bool : bool -> t
  (** [bool b] is [b] as a generated JSON boolean value. *)

  val int : int -> t
  (** [int i] is [i] as a generated JSON number. *)

  val float : float -> t
  (** [float f] is [f] as a generated JSON number. *)

  val string : string -> t
  (** [str s] is [s] as a generated JSON string value. *)

  type arr
  (** The type for generated JSON arrays. *)

  val arr : arr
  (** [arr] is an empty array. *)

  val arr_end : arr -> t
  (** [arr_end els] is arr a a generated JSON value. *)

  val el : t -> arr -> arr
  (** [el e arr] is array [arr] wit [e] added at the end. *)

  val el_if : bool -> (unit -> t) -> arr -> arr
  (** [el cond v arr] is [el (v ()) arr] if [cond] is [true] and
      [arr] otherwise. *)

  type obj
  (** The type for generated JSON objects. *)

  val obj : obj
  (** [obj] is an empty object. *)

  val obj_end : obj -> t
  (** [obj_end o] is [o] as a generated JSON value. *)

  val mem : string -> t -> obj -> obj
  (** [mem name v o] is [o] with member [name] bound to value [v]
      added. *)

  val mem_if : bool -> string -> (unit -> t) -> obj -> obj
  (** [mem_if cond name v o] is [mem name (v ()) o] if [cond] is [true]
      and [o] otherwise. *)

  (** {1:derived Derived generators} *)

  val json : Json.t -> t
  (** [of_json v] is the JSON value [v] as a generated value. *)

  val path : Fpath.t -> t
  (** [path p] is [p] as a generated JSON string value. *)

  val cmd : Cmd.t -> t
  (** [cmd c] is [c] as a generated JSON string array value. *)

  val list : ('a -> t) -> 'a list -> t
  (** [list el l] is [l] as a generated JSON array whose elements
      are generated using [el]. *)

  val strf : ('a, Format.formatter, unit, t) format4 -> 'a
  (** [strf fmt ...] is a JSON string generated value formatted according
      to [fmt]. *)

  (** {1:output Output} *)

  val buffer_add : Buffer.t -> t -> unit
  (** [buffer_add b g] adds the generated JSON value [g] to [b]. *)

  val to_string : t -> string
  (** [to_string g] is the generated JSON value [g] as a string. *)
end

(** JSON value queries. *)
module Jsonq : sig

  (** {1:query Queries} *)

  type 'a t
  (** The type for a query on a JSON value returning values of type ['a]. *)

  val null : unit t
  (** [null] queries a null JSON value. *)

  val nullable : 'a t -> 'a option t
  (** [nullable q] queries either a null JSON value or with [q]. *)

  val bool : bool t
  (** [bool] queries a boolean JSON value. *)

  val int : int t
  (** [int] queries a float JSON value and {!truncate}s it. *)

  val float : float t
  (** [float] queries a float JSON value. *)

  val string : string t
  (** [string] queries a string JSON value. *)

  val array : 'a t -> 'a list t
  (** [array q] queries the elements of a JSON array with [q]. *)

  val mem : string -> 'a t -> ('a -> 'b) t -> 'b t
  (** [mem name q o] queries a JSON object [o]'s member named
      [name] with [q]. *)

  val mem_opt : string -> 'a t -> ('a option -> 'b) t -> 'b t
  (** [mem_opt name q] queries a JSON object [o]'s optional member named
      [name] with [q]. *)

  val obj : 'a -> 'a t
  (** [obj v] queries an object and returns [v]. *)

  val json : Json.t t
  (** [json] queries any JSON value. *)

  val get : 'a -> 'a
  (** [get] is the identity function *)

  val sel : string -> 'a t -> 'a t
  (** [sel name q] is [obj get |> mem name q] *)

  val query : 'a t -> Json.t -> ('a, string) result
  (** [query q j] queries a JSON value [j] with [q]. *)
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
