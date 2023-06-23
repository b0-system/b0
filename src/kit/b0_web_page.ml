(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_html

let write
    ?(lang = "") ?(generator = "") ?(styles = []) ?(scripts = [])
    ?more_head ?(title = "") m ~frag ~o
  =
  (* FIXME Ideally we would like the read to be in write.
       The write fun return a future but this has other impacts. *)
  let open B0_std.Fut.Syntax in
  ignore @@ (* FIXME maybe get rid of that. *)
  let* contents = B0_memo.Memo.read m frag in
  let title = if title = "" then El.title_of_fpath o else title in
  let more_head = match more_head with
  | None -> ""
  | Some more_head -> El.to_string ~doc_type:false more_head
  in
  let stamp = lang :: generator :: more_head :: title :: [] in
  let stamp = List.rev_append styles stamp in
  let stamp = List.rev_append scripts stamp in
  let stamp = String.concat "" stamp in
  B0_std.Fut.return @@
  (B0_memo.Memo.write m ~stamp ~reads:[frag] o @@ fun () ->
   let more_head = El.raw more_head in
   let body = El.(body [raw contents]) in
   let page =
     El.basic_page ~lang ~generator ~styles ~scripts ~more_head ~title body
   in
   Ok (El.to_string ~doc_type:true page))
