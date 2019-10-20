(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std

let add_mod_names i names =
  let rec loop names = function
  | [] -> names
  | [] :: todo -> loop names todo
  | (i :: is) :: todo ->
      match i with
      | Types.Sig_module (i, _, d, _, _) ->
          let names = String.Set.add (Ident.name i) names in
          begin match d.Types.md_type with
          | Types.Mty_signature is' -> loop names (is' :: is :: todo)
          | _ -> loop names (is :: todo)
          end
      | _ -> loop names (is :: todo)
  in
  loop names [i.Cmi_format.cmi_sign]

let read f =
  try
    let i = Cmi_format.read_cmi (Fpath.to_string f) in
    let name = i.Cmi_format.cmi_name in
    let mod_names = add_mod_names i (String.Set.singleton name) in
    let digest, deps =
      let rec loop d deps = function
      | (n, Some d) :: crcs when String.equal n name -> loop d deps crcs
      | (n, Some d') :: crcs -> loop d ((n, d') :: deps) crcs
      | (n, None) :: crcs -> loop d deps crcs
      | [] -> d, deps
      in
      loop "" [] i.Cmi_format.cmi_crcs
    in
    Ok (name, digest, mod_names, deps)
  with
  | Cmi_format.Error e -> Error (Fmt.str "%a" Cmi_format.report_error e)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers

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
