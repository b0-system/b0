(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

type t = { def : B0_def.t; units : B0_unit.t list; locked : bool; }

module T = struct
  type nonrec t = t
  let def_kind = "pack"
  let def p = p.def
  let pp_name_str = Fmt.(code string)
end

include (B0_def.Make (T) : B0_def.S with type t := t)

let v ?doc ?meta n ~locked units  =
  let def = define ?doc ?meta n in
  let p = { def; units; locked; } in add p; p

let units p = p.units
let locked p = p.locked

(* Formatting *)

let pp_locked ppf p =
  if locked p then Fmt.tty_string [`Fg `Red] ppf "locked " else Fmt.nop ppf ()

let pp_units ppf p =
  if units p = [] then () else
  let label = Fmt.tty_string [`Fg `Green ] in
  let pp = Fmt.(pp_locked ++ using units (list ~sep:sp B0_unit.pp_name)) in
  Fmt.field ~label "units" Fmt.id pp ppf p

let pp_synopsis ppf v =
  let pp_tag ppf u =
    let style = [`Bold] in
    Fmt.tty_string style ppf "[";
    Fmt.string ppf "p";
    Fmt.tty_string style ppf "]";
  in
  Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v



let pp ppf p =
  Fmt.pf ppf "@[<v>@[%a@]@, @[<v>%a%a@]@]"
    pp_synopsis p pp_units p B0_meta.pp_non_empty (meta p)
