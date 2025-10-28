(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Networking URLs and endpoints.

    @canonical B0_std.Net *)

module Url = B0__url

(** Network endpoints.

    See also {!Os.Socket}. *)
module Endpoint : sig
  type t =
  [ `Host of string * int (** Hostname and port. *)
  | `Sockaddr of Unix.sockaddr (** Given socket address. *)
  | `Fd of Unix.file_descr (** Direct file descriptor. *) ]
  (** The type for specifying a socket endpoint to connect or to listen
      to on. *)

  val of_string : default_port:int -> string -> (t, string) result
  (** [of_string ~default_port s] parses a socket endpoint
      specification from [s].

      The format is [ADDR[:PORT]] or [PATH] for a Unix domain socket
      (detected by the the presence of a
      {{!B0_std.Fpath.is_dir_sep_char}directory
      separator}). [default_port] port is used if no [PORT] is
      specified. *)

  val with_port_of_sockaddr : Unix.sockaddr -> t -> t
  (** [with_port_of_sockaddr saddr ep] makes [ep]'s port coincide with
      the port of [saddr] iff both have a port. Otherwise this is
      [ep] itself. This is mostly useful to adjust an endpoint whose
      port number was specified as [0] in order to get one allocated
      by [bind]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats endpoints. *)
end
