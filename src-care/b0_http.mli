(*---------------------------------------------------------------------------
   Copyright (c) 2016 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** World Wide Web interaction.

    Toys to interact with the World Wide Web. See also {!B0_json}. *)

(** {1:www WWW} *)

open B0_std

(** URIs. *)
module Uri : sig

  type t = string
  (** The type for URIs. *)

  val parse_scheme : string -> string option
  (** [parse_scheme s] tries to parse a scheme at the beginning of [s]. *)

  val parse_authority : string -> string option
  val parse_path_and_query : string -> string option
end

(** HTTP methods, requests and responses. *)
module Http : sig

  (** {1:meth HTTP methods and headers} *)

  type meth =
  [ `CONNECT | `DELETE | `GET | `HEAD | `OPTIONS | `Other of string
  | `PATCH | `POST | `PUT | `TRACE ]
  (** The type for HTTP methods. *)

  val meth_to_string : meth -> string
  (** [meth_to_string m] is a string representation of [m]. *)

  type headers = (string * string) list
  (** The type for HTTP headers. List of header names (without the [:])
      tupled with their value. *)

  (** {1:requests HTTP requests} *)

  type req
  (** The type for HTTP requests. *)

  val req : ?headers:headers -> ?body:string -> uri:string -> meth -> req
  (** [req uri m ~headers ~body] is a request on [uri] with method [m],
      headers [headers] (defaults to [[]]) and body [body] (defaults to
      [""]). *)

  val req_uri : req -> Uri.t
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

  val resp : ?headers:headers -> ?body:string -> int -> resp
  (** [resp status ~headers ~body] is a response with status [status],
      headers [headers] (defaults to [[]]) and body [body] (defaults to [""]) *)

  val resp_headers : resp -> headers
  (** [resp_headers r] are the HTTP response headers. *)

  val resp_status : resp -> int
  (** [resp_status r] is the HTTP response status. *)

  val resp_body : resp -> string
  (** [resp_body r] is the HTTP response body. *)
end

(** HTTP requests via [curl]. *)
module Httpr : sig

  (** {1:peforming Performing requests} *)

  type t
  (** The type for HTTP requestors. *)

  val curl :
    ?docs:string -> ?env:Cmdliner.Arg.env -> unit -> Cmd.t Cmdliner.Term.t
  (** [curl] is a cli interface for specifying the curl command
      line tool. *)

  val find_curl :
    ?search:Fpath.t list -> curl:Cmd.t -> unit -> (t, string) result

  val perform : ?follow:bool -> t -> Http.req -> (Http.resp, string) result
  (** [perform curl r] performs request [r] via [curl] which is looked up
      in the PATH or in the environment variable [B0_CURL].  If
      [follow] is [true] (default) HTTP redirects for GET and HEAD
      requests that return 301, 302, 303, 305 or 307 are automatically
      followed.

      The response's {!Http.resp_headers} are lowercased. *)
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
