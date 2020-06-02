(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax

let () = B0_def.Scope.lib "opam"

module Meta = struct
  let tag = B0_meta.Key.tag "opam" ~doc:"opam related entity"
end

(* Cmdlet *)

let cmdlet =
  let gen_file ps =
    let use = B0_cmdlet.Exit.Code 120 in
    Log.if_error ~use(* FIXME B0_driver.Exit.no_such_name *) @@
    let* ps = match ps with
    | [] -> Ok (B0_pack.list ())
    | ps -> B0_pack.get_list ps
    in
    let has_opam p = B0_meta.mem Meta.tag (B0_pack.meta p) in
    let ps = List.filter has_opam ps in
    begin match List.sort B0_pack.compare ps with
    | [] -> ()
    | ps -> Log.app (fun m -> m "@[<v>%a@]" Fmt.(list B0_pack.pp_synopsis) ps)
    end;
    Ok (B0_cmdlet.Exit.Code 0)
  in
  let gen_file cmdlet ~argv =
    let open Cmdliner in
    let packs = Arg.(value & pos_all string [] & info []
                       ~doc:"packs to consider for publishing" ~docv:"PACK")
    in
    let term = Term.(const gen_file $ packs) in
    B0_cmdlet_cli.run cmdlet ~argv term
  in
  let doc = "Interaction with opam repositories" in
  B0_cmdlet.v "opam" ~doc (`Cmd gen_file)

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
