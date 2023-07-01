(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type t = { def : B0_def.t; cmd : cmd }
and cmd = t -> B0_build.t -> Cmd.t -> Os.Exit.t

module T = struct
  type nonrec t = t
  let def_kind = "action"
  let def a = a.def
  let pp_name_str = Fmt.(code string)
end

include (B0_def.Make (T) : B0_def.S with type t := t)

let v ?doc ?meta name cmd =
  let def = define ?doc ?meta name in
  let a = { def; cmd } in add a; a

let cmd a = a.cmd

let of_cli_cmd ?meta _ = failwith "TODO"
