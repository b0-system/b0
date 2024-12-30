(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_html

let write
    ?(lang = "") ?(generator = "") ?(styles = []) ?(scripts = [])
    ?more_head ?(title = "") m ~frag ~o
  =
  (* FIXME Ideally we would like the read to be in write.
       The write fun return a future but this has other impacts. *)
  let open Fut.Syntax in
  ignore @@ (* FIXME maybe get rid of that. *)
  let* contents = B0_memo.read m frag in
  let title =
    if title = "" then El.title_of_filepath (Fpath.to_string o) else title
  in
  let more_head = match more_head with
  | None -> ""
  | Some more_head -> El.to_string ~doctype:false more_head
  in
  let stamp = lang :: generator :: more_head :: title :: [] in
  let stamp = List.rev_append styles stamp in
  let stamp = List.rev_append scripts stamp in
  let stamp = String.concat "" stamp in
  Fut.return @@
  (B0_memo.write m ~stamp ~reads:[frag] o @@ fun () ->
   let more_head = El.unsafe_raw more_head in
   let body = El.(body [unsafe_raw contents]) in
   let page =
     El.page ~lang ~generator ~styles ~scripts ~more_head ~title body
   in
   Ok (El.to_string ~doctype:true page))
