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

let make ?doc ?meta n ~locked units  =
  let def = define ?doc ?meta n in
  let p = { def; units; locked; } in add p; p

let units p = p.units
let locked p = p.locked


let synopsis_and_description_of_cmark file =
  let syn_of_title t = (* Get $SYN in "$NAME $SEP $SYN" *)
    let ws = Char.Ascii.is_white and tok c = not @@ Char.Ascii.is_white c in
    let skip = String.lose_left in
    let d = t |> skip ws |> skip tok |> skip ws |> skip tok |> skip ws in
    if d <> "" then Some d else None
  in
  let descr_of_section s =
    (* XXX This is only here because of the person who wrote this. Has
       too many README's with that. This shouldn't exist. *)
    let prefix = "\x25%VERSION%%" in
    if not (String.starts_with ~prefix s) then String.trim s else
    String.trim (String.subrange ~first:(String.length prefix) s)
  in
  let contents = Os.File.read file |> Log.if_error ~use:"" in
  let convert (t, d) = syn_of_title t, descr_of_section d in
  Option.map convert (String.commonmark_first_section ~preamble:true contents)


let derive_synopsis_and_description p m =
  match B0_meta.(mem synopsis m, mem description m) with
  | true, true -> m
  | has_syn, has_descr ->
      let extracted = match in_scope_dir p (Fpath.v "README.md") with
      | None -> None
      | Some readme ->
          let exists = Os.File.exists readme |> Log.if_error ~use:false in
          if exists then synopsis_and_description_of_cmark readme else None
      in
      match extracted with
      | None -> m
      | Some (syn, d) ->
          let syn = Option.value ~default:"" syn in
          let m = if has_syn then m else B0_meta.(add synopsis syn m) in
          let m = if has_descr then m else B0_meta.(add description d m) in
          m

let find_default () = find "default"

(* Formatting *)

let pp_locked ppf p =
  if locked p then Fmt.tty' [`Fg `Red] ppf "locked " else Fmt.nop ppf ()

let pp_units ppf p =
  if units p = [] then () else
  let label = Fmt.tty' [`Fg `Green ] in
  let pp = Fmt.(pp_locked ++ using units (list ~sep:sp B0_unit.pp_name)) in
  Fmt.field ~label "units" Fmt.id pp ppf p

let pp_synopsis ppf v =
  let pp_tag ppf u =
    let style = [`Bold] in
    Fmt.tty' style ppf "[";
    Fmt.string ppf "p";
    Fmt.tty' style ppf "]";
  in
  Fmt.pf ppf "@[%a %a@]" pp_tag v pp_synopsis v

let pp ppf p =
  Fmt.pf ppf "@[<v>@[%a@]@, @[<v>%a%a@]@]"
    pp_synopsis p pp_units p B0_meta.pp_non_empty (meta p)
