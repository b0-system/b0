(*---------------------------------------------------------------------------
   Copyright (c) 2019 The brzo programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let () =
  Log.set_level Log.Info;
  if Array.length Sys.argv < 2
  then Fmt.epr "Specify a compilation object@." else
  let to_fpath p = Fpath.of_string p |> Result.error_to_failure in
  let files = List.tl (Array.to_list Sys.argv) in
  try
    let objinfo = Cmd.(arg "ocamlobjinfo" % "-no-code" % "-no-approx") in
    let files = List.map to_fpath files in
    Result.error_to_failure @@
    Result.bind (Os.Cmd.run_out ~trim:true
                   Cmd.(objinfo %% paths files)) @@ fun s ->
    Result.bind (B0_ocaml.Cobj.of_string s) @@ fun cobjs ->
    Ok (Fmt.pr "@[<v>%a@]@." Fmt.(list B0_ocaml.Cobj.pp) cobjs)
  with Failure  e -> Fmt.epr "%s@." e; exit 1
