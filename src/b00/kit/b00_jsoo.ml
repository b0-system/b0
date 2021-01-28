(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00

type source_map = [`Inline | `File ] option

let env_vars = [ "BUILD_PATH_PREFIX_MAP" ]
let tool = Tool.by_name ~vars:env_vars "js_of_ocaml"

let build_runtime m ~opts ~jss ~o =
  let jsoo = Memo.tool m tool in
  Memo.spawn m ~reads:jss ~writes:[o] @@
  jsoo Cmd.(atom "build-runtime" % "-o" %% (unstamp @@ path o) %% opts %%
            unstamp (paths jss))

let handle_source_map ~o = function
| None -> [o], Cmd.empty
| Some `Inline -> [o], Cmd.(atom "--source-map-inline")
| Some `File -> [o; Fpath.(o -+ ".map")], Cmd.(atom "--source-map")

let compile m ~opts ~source_map ~jss ~byte ~o =
  let jsoo = Memo.tool m tool in
  let writes, source_map = handle_source_map ~o source_map in
  Memo.spawn m ~reads:(byte :: jss) ~writes @@
  jsoo Cmd.(atom "compile" % "-o" %% (unstamp @@ path o) %% opts %%
            source_map %% (unstamp @@ paths jss %% path byte))

let link m ~opts ~source_map ~jss ~o =
  let jsoo = Memo.tool m tool in
  let writes, source_map = handle_source_map ~o source_map in
  Memo.spawn m ~reads:jss ~writes @@
  jsoo Cmd.(atom "link" % "-o" %% (unstamp @@ path o) %% opts %% source_map %%
            (unstamp @@ paths jss))

let write_page
    ?(lang = "") ?(generator = "") ?(styles = [])  ?(scripts = [])
    ?(title = "") m ~o
  =
  let title = if title = "" then Fpath.basename ~no_ext:true o else title in
  let stamp = List.rev_append styles scripts in
  let stamp = String.concat "" (lang :: generator :: title :: stamp) in
  Memo.write m ~stamp o @@ fun () ->
  let open B00_htmlg in
  let body =
    let sorry = "Sorry, you need to enable JavaScript to see this page." in
    El.body El.[noscript [txt sorry]]
  in
  let page = El.basic_page ~generator ~lang ~scripts ~styles ~title body in
  Ok (El.to_string ~doc_type:true page)

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
