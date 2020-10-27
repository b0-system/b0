(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

let tool = Tool.by_name "cmark"
let cmd ?(validate_utf_8 = true) ?(format = "html") m ~opts ~mds ~o =
  let cmark = Memo.tool m tool in
  let validate = Cmd.if' validate_utf_8 (Cmd.atom "--validate-utf8") in
  Memo.spawn m ~reads:mds ~writes:[o] ~stdout:(`File o) @@
  cmark Cmd.(atom "--to" % format %% validate %% opts %% unstamp (paths mds))

let to_html
    ?generator ?lang ?scripts ?styles ?title m ~opts ~mds ~o_frag:frag ~o
  =
  cmd m ~opts ~mds ~o:frag;
  B00_htmlg.El.write_page ?generator ?lang ?scripts ?styles ?title m ~frag ~o

let first_section ~preamble md =
  let atx_heading s (* trimmed *) =
    (* approximate https://spec.commonmark.org/0.29/#atx-headings *)
    let num, title = String.span_left (Char.equal '#') s in
    let num = String.length num and tlen = String.length title in
    if num = 0 || num > 6 then None else
    if tlen = 0 then Some (num, "") else
    if title.[0] <> ' ' then None else
    Some (num, String.trim title)
  in
  let setex_heading s (* trimmed *) =
    (* approximate https://spec.commonmark.org/0.29/#setext-headings *)
    if String.starts_with "==" s then Some 1 else
    if String.starts_with "--" s then Some 2 else
    None
  in
  let nl = "\n" in
  let prev_lines prev = String.trim (String.concat nl (List.rev prev)) in
  let rec find_content num title prev = function
  | [] -> Some (title, String.concat nl (List.rev prev))
  | l :: ls ->
      let ltrim = String.trim l in
      match atx_heading ltrim with
      | Some (n, _) when n <= num || preamble -> Some (title, prev_lines prev)
      | Some _ -> find_content num title (l :: prev) ls
      | None ->
          match setex_heading ltrim with
          | Some n when n <= num || preamble ->
              let prev = match prev with [] -> [] | _ :: prev -> prev in
              Some (title, prev_lines prev)
          | Some _ | None -> find_content num title (l :: prev) ls
  and find_heading prev = function
  | [] -> None
  | l :: ls ->
      let l = String.trim l in
      if String.is_empty l then find_heading [] ls else
      match atx_heading l with
      | Some (num, title) -> find_content num title [] ls
      | None ->
          match setex_heading l with
          | None -> find_heading (l :: prev) ls
          | Some num ->
              let title = prev_lines prev in
              find_content num title [] ls
  in
  find_heading [] (B00_lines.of_string md)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
