(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

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

let pp ppf p =
  Fmt.pf ppf "@[<v>@[%a %a@]@, @[<v>%a%a@]@]"
    pp_name p pp_doc p
    pp_units p B0_meta.pp_non_empty (meta p)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
