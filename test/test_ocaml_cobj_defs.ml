(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let () =
  Log.set_level Log.Info;
  if Array.length Sys.argv < 2
  then Fmt.epr "Specify a compilation object@." else
  let to_fpath p = Fpath.of_string p |> Result.to_failure in
  let files = List.tl (Array.to_list Sys.argv) in
  try
    let objinfo = Cmd.(arg "ocamlobjinfo" % "-no-code" % "-no-approx") in
    let files = List.map to_fpath files in
    Result.to_failure @@
    Result.bind (Os.Cmd.run_out ~trim:true
                   Cmd.(objinfo %% paths files)) @@ fun s ->
    Result.bind (B00_ocaml.Cobj.of_string s) @@ fun cobjs ->
    Ok (Fmt.pr "@[<v>%a@]@." Fmt.(list B00_ocaml.Cobj.pp) cobjs)
  with Failure  e -> Fmt.epr "%s@." e; exit 1

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
