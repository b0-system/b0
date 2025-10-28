(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Url = B0__url

module Endpoint = struct
  type t =
    [ `Host of string * int
    | `Sockaddr of Unix.sockaddr
    | `Fd of Unix.file_descr ]

  let of_string ~default_port s =
    if String.contains s Filename.dir_sep.[0]
    then Ok (`Sockaddr (Unix.ADDR_UNIX s)) else
    match String.rindex_opt s ':' with
    | None -> Ok (`Host (s, default_port))
    | Some i ->
        match String.index_from_opt s i ']' with (* beware IPv6 *)
        | Some _ -> Ok (`Host (s, default_port))
        | None ->
            let h = B0__string.subrange ~last:(i - 1) s in
            let p = B0__string.subrange ~first:(i + 1) s in
            match int_of_string_opt p with
            | None -> B0__fmt.error "port %S not an integer" p
            | Some p -> Ok (`Host (h, p))

  let with_port_of_sockaddr sockaddr ep = match sockaddr with
  | Unix.ADDR_UNIX _ -> ep
  | Unix.ADDR_INET (_, port) ->
      match ep with
      | `Host (n, _) -> `Host (n, port)
      | `Sockaddr (Unix.ADDR_INET (a, _)) -> `Sockaddr (ADDR_INET (a, port))
      | ep -> ep

  let pp ppf = function
  | `Host (n, p) -> B0__fmt.host_and_port ppf (n, p)
  | `Fd _fd -> B0__fmt.pf ppf "<fd>"
  | `Sockaddr addr -> B0__fmt.sockaddr ppf addr
end
