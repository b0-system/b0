(*---------------------------------------------------------------------------
   Copyright (c) 2024 The webs programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sloppy URL processing.

    URL standard{e s} are in a sorry state. This module takes a sloppy
    approach to URL processing. It only breaks URLs into their components
    and classifies them.

    {b Warning.} None of the functions here perform percent encoding or
    decoding. Use {!Percent} when deemed appropriate.  *)

(** {1:urls URLs} *)

(** Sloppy authority processing. *)
module Authority : sig
  type t = string
  (** The type for [[userinfo@]HOST:PORT] authorities. *)

  type port = int
  (** The type for authority ports. *)

  type host = string
  (** The type for authority hosts. *)

  val userinfo : t -> string option
  (** [userinfo a] is anything before the lefmost ['@'] (if any) *)

  val host : t -> host
  (** [host a] is anything between the first ['@'] (if any) and the
      [':'] separating the {!port}. *)

  val port : t -> port option
  (** [port a] is the port made of suffix decimal digits before a [':']. *)
end

type scheme = string
(** The type for schemes, without the [':'] separator. *)

type path = string
(** The type for paths. *)

type query = string
(** The type for queries, without the ['?'] separator. *)

type fragment = string
(** The type for fragments, without the ['#'] seperator. *)

type t = string
(** The type for URLs. *)

val scheme : t -> scheme option
(** [scheme u] is the {!type-scheme} of [u], if any. *)

val authority : t -> Authority.t option
(** [authority u] is the {!type-authority} of [u], if any. *)

val path : t -> path option
(** [path u] is the {!type-path} of [u], if any. *)

val query : t -> query option
(** [query u] is the {!type-query} of [u], if any. *)

val fragment : t -> fragment option
(** [fragment u] is the {!type-fragment} of [u], if any. *)

(** {1:derived Derived components} *)

val target : t -> string option
(** [target u] is the contenation of {!val-path}, {!val-query} and
    {!val-fragment}, that is everything that comes after the {!val-scheme}
    and {!val-authority} in an URL. *)

(** {1:kinds Kinds} *)

type relative_kind = Scheme | Absolute_path | Relative_path | Empty (** *)
(** The type for kinds of relative references. Represents
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-4.2}this
    alternation}. *)

type kind = Absolute | Relative of relative_kind (** *)
(** The type for kinds of URLs. Represents this
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-4.1} this
    alternation}. *)

val kind : t -> kind
(** [kind u] determines the kind of [u]. It decides that [u] is
    absolute if [u] starts with a
    {{:https://www.rfc-editor.org/rfc/rfc3986.html#section-3.1}scheme}
    and [:]. *)

(** {1:ops Operations} *)

val of_url : t ->
  ?scheme:scheme option -> ?authority:Authority.t option ->
  ?path:path option -> ?query:query option -> ?fragment:fragment option ->
  unit -> t
(** [of_url u ()] is a new url whith unspecified components defaulting
    to those of [u]. If specified with [None] the given component is
    deleted. *)

val append : t -> t -> t
(** [append root u] is [u] if {!val-kind}[ u] is [Absolute]. Otherwise
    uses [root] to make it absolute according to its {!relative_kind}.
    The result is guaranteed to be absolute if [root] is, the result
    may be surprising or non-sensical if [root] isn't (FIXME can't we
    characterize that more ?). *)

val to_absolute : scheme:scheme -> root_path:path option -> t -> t
(** [to_absolute ~scheme ~root_path] transforms [u] depending on the value of
    {!val-kind}[ u]:
    {ul
    {- If [Absolute] then this is [u] itself.}
    {- If [Relative Scheme] then [u] is given the scheme [scheme].}
    {- If [Relative Absolute_path] then [u] is given the scheme [scheme].}
    {- If [Relative Relative_path] then [u] is given the scheme [scheme] and
           the path of [u] is prepended by [root_path] (if any).}
    {- If [Relative Empty] then [u] is given the scheme [scheme] and the
       path is [root_path] (if any).}} *)

(*
val path_and_rest : t -> string option
(** [path_and_query u] extract a URL path and query part from
    [u]. *)

val drop_path_and_rest : t -> string
(** [drop_path_and_rest u] is [u] without the path and query. *)
*)

(** {1:pct Percent encoding} *)

val is_likely_percent_decoded : t -> bool
(** [is_likely_percent_decoded u] tries to guess if [u] is percent
    decoded. It returns

    {ul
    {- [true] if there is a byte in [u] such that {!Char.Ascii.is_graphic}
       is [false].}
    {- [false] otherwise. In this case [u] is likely encoded or needs no
       encoding.}}

    If [u] is the result of {!Percent.encode}[ Uri] this always returns
    [false] but on foreign data the test mail fail e.g. it wrongly returns
    [false] on [http://example.org/zoom/100%].

    {b Note.} This function can likely be improved. *)

(** Percent-encoding codecs according to
    {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.1}RFC 3986}.

    {b Note.} This should not be used for URL query strings and
    {{:https://url.spec.whatwg.org/#application/x-www-form-urlencoded}
    [application/x-www-form-urlencoded]} which is slightly different (welcome
    to the Web). The {!Webs.Http.Query} module handles that. *)
module Percent : sig
  type kind =
  | Uri_component
  (**  Percent-encodes anything but
       {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.3}[unreserved]}
       and
       {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.2}sub-delims}
       URI characters. In other words only
           ['a'..'z'], ['A'..'Z'], ['0'..'9'], ['-'], ['.'], ['_'], ['~']
           and ['!'], ['$'], ['&'], ['\''], ['('], [')']
           ['*'], ['+'], [','], [';'], ['='] are not percent-encoded. *)
  | Uri
  (** Percent-encodes like [Uri_component] except it also preserves
      {{:https://www.rfc-editor.org/rfc/rfc3986#section-2.2}gen-delims}
      URI characters. In other words in addition to those characters above,
      [':'], ['/'], ['?'], ['#'], ['\['], ['\]'], ['@'] are not
      percent-encoded. *)
  (** The kind of percent encoding. *)

  val encode : kind -> string -> string
  (** [encode kind s] is the percent encoding of [s] according to [kind]. *)

  val decode : string -> string
  (** [decode s] is the percent decoding of [s]. *)

  (**/**)
  val is_char_verbatim_in_uri_component : char -> bool
  val is_char_verbatim_in_uri : char -> bool
  val is_hexdig : char -> bool
  val hexdig_to_int : char -> int
  val unsafe_hexdig_of_int : int -> char
  val encode_to_buffer : (char -> bool) -> Buffer.t -> string -> unit
  val decode_to_buffer : Buffer.t -> string -> first:int -> last:int -> unit
end

(** {1:predicates Predicates and comparisons} *)

val equal : t -> t -> bool
(** [equal] tests URLs for binary equality. *)

val compare : t -> t -> int
(** [compare] is a total order on URLs compatible with {!equal}. *)

(** {1:converting Converting} *)

val to_endpoint :
  supported_schemes:(scheme * Authority.port) list -> t ->
  ([> `Host of Authority.host * Authority.port], string) result
(** [to_endpoint ~supported_scheme url]:
    {ul
    {- [Ok (`Host (host, port))] iff [url] has a {!scheme} and it can be found
       in [supported_schemes] and [url] has an {!authority}. The [host] value is
       the {!Authority.host} and [port] the {!Authority.port} or the default
       port specified in [supported_schemes] if absent.}
    {- [Error _], if [url] has no scheme or that it can't be found in
       [supported_schems] or if [url] has no authority. The error message
       is of the form ["URL <url>: …"]}}

    Raises [Invalid_argument] if [supported_schemes] is empty. *)

(** {1:formatting Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an URL. For now this is just {!Format.pp_print_string}. *)

val pp_kind : Format.formatter -> kind -> unit
(** [pp_kind] formats an unspecified representation of kinds. *)

(** {1:scraping Scraping} *)

val list_of_text_scrape : ?root:t -> string -> t list
(** [list_of_text_scrape ?root s] roughly finds absolute and relative
    URLs in the ASCII compatible (including UTF-8) textual data [s] by
    looking in order:
    {ol
    {- For the next [href] or [src] substring then tries to parses the
       content of an HTML attribute. This may result in relative
       or absolute paths.}
    {- For the next [http] substrings in [s] and then delimits an URL
       depending on the previous characters and checks that the delimited
       URL starts with [http://] or [https://].}}

    Relative URLs are {{!append}appended} to [root] if provided. Otherwise
    they are kept as is. The result may have duplicates. *)
