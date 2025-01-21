(*---------------------------------------------------------------------------
   Copyright (c) 2025 The more programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

include Stdlib.Result

let product r0 r1 = match r0, r1 with
| (Error _ as r), _ | _, (Error _ as r) -> r
| Ok v0, Ok v1 -> Ok (v0, v1)

let retract = function Ok v | Error v -> v

(* Interacting with Stdlib exceptions *)

let error_to_failure = function Ok v -> v | Error e -> failwith e
let get_ok' = function Ok v -> v | Error e -> invalid_arg e

  (* Syntax *)

module Syntax = struct
  let ( let* ) v f = bind v f
  let ( and* ) a b = product a b
  let ( let+ ) v f = map f v
  let ( and+ ) a b = product a b
end
