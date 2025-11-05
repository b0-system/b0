(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** HTTP client.

    Good enough (?) toys to interact with the World Wide Web.  *)

open B0_std

(** HTTP datatypes. *)
module Http : sig

  type method' =
  [ `CONNECT | `DELETE | `GET | `HEAD | `OPTIONS | `Other of string
  | `PATCH | `POST | `PUT | `TRACE ]
  (** The type for HTTP methods. *)

  val method_to_string : method' -> string
  (** [method_to_string m] is an HTTP method string for [m]. *)

  type headers = (string * string) list
  (** The type for HTTP headers. List of header names (without the [:])
      tupled with their value. *)

  (** HTTP requests. *)
  module Request : sig

    type t
    (** The type for HTTP requests. *)

    val make : ?headers:headers -> ?body:string -> method' -> url:Net.Url.t -> t
    (** [make m ~url ~headers ~body] is a request on [url] with method [m],
        headers [headers] (defaults to [[]]) and body [body] (defaults to
        [""]). *)

    val url : t -> Net.Url.t
    (** [url r] is the URL of [r]. *)

    val method' : t -> method'
    (** [meth r] is the method of [r]. *)

    val headers : t -> headers
    (** [headers r] are the headers of [r]. *)

    val body : t -> string
    (** [body r] is the body of [r]. *)
  end

  (** HTTP responses. *)
  module Response : sig

    type t
    (** The type for HTTP responses. *)

    val make : ?headers:headers -> ?body:string -> int -> t
    (** [make status ~headers ~body] is a response with status [status],
        headers [headers] (defaults to [[]]) and body [body]
        (defaults to [""]). *)

    val of_string : string -> (t, string) result
    (** [of_string s] is a response from [s]. *)

    val status : t -> int
    (** [status r] is the status of [r]. *)

    val headers : t -> headers
    (** [headers r] are headers of [r] *)

    val body : t -> string
    (** [body r] is body of [r]. *)
  end
end

(** HTTP clients. *)
module Http_client : sig

  type t
  (** The type for HTTP clients. *)

  val make :
    ?insecure:bool -> ?progress:bool -> ?search:Cmd.tool_search ->
    ?cmd:Cmd.t -> unit ->
    (t, string) result
  (** [make ~search ~cmd ()] looks for [cmd] (defaults to [Cmd.tool "curl"])
      in [search] (defaults to [Os.Cmd.get ~search]). If [insecure] is
      [true] (defaults to [false]) TLS server certificates are not checked.
      If [progress] is [true] some form of progress is shown on
      [stdout] on {!request}. *)

  val request :
    t -> follow:bool -> Http.Request.t -> (Http.Response.t, string) result
  (** [request httpc ~follow r] performs request [r] via [httpr].  If [follow]
      is [true] HTTP redirects for GET and HEAD requests that return
      301, 302, 303, 305 or 307 are automatically followed. In this
      case the response has an {!x_follow_location} header with the final
      requested URL.

      The response's {!Http.Response.headers} are lowercased. *)

  val head_request_follow_location :
    ?headers:Http.headers -> t -> Net.Url.t -> (Net.Url.t, string) result
  (** [head_request_follow_location httpc url] performs a [`HEAD] request
      on [url], follows redirection and returns the final location or
      [url] if there was no redirection. If specified [headers] are
      added to the request (and redirect requests). *)

  val x_follow_location : string
  (** [x_follow_location] is ["x-follow-location"] the location
      that was finally requested on {!request} with [~follow:true]. *)

  (** {1:cli Command line interface} *)

  val curl :
    ?docs:string -> ?env:Cmdliner.Cmd.Env.info -> unit -> Cmd.t Cmdliner.Term.t
  (** [curl] is a cli interface for specifying the curl command
      line tool. *)

  val curl_fetch_args :
    ?args:Cmd.t -> progress:bool -> Net.Url.t -> Fpath.t -> Cmd.t
  (** [curl_fetch_args url file] are curl arguments to fetch the URL
      [url] and write it to [file]. If progress is [true] it is reported.
      [args] are added to the result before the URL. Redirections
      are followed. *)
end
