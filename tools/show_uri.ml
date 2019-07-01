(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

let urify u = (* Detects if u is simply a file path and urifies it *)
  let file_uri p = Fmt.str "file://%s" (Fpath.to_string p) in
  try
    let p = Fpath.of_string u |> Result.to_failure in
    match Os.File.exists p |> Result.to_failure ||
          Os.Dir.exists p |> Result.to_failure
    with
    | false -> u
    | true when (Fpath.is_abs p) -> file_uri p
    | true ->
        let cwd = Os.Dir.cwd () |> Result.to_failure in
        file_uri Fpath.(cwd // p)
  with Failure _ -> u

let show_uris () background prefix browser uris =
  match B0_ui.Browser.find ~browser () with
  | Error e -> Log.err (fun m -> m "%s" e); 1
  | Ok browser ->
      let rec loop = function
      | [] -> 0
      | uri :: uris ->
          let uri = urify uri in
          match B0_ui.Browser.show ~background ~prefix browser uri with
          | Error e -> Log.err (fun m -> m "%s" e); 1
          | Ok () -> loop uris
      in
      loop uris

open Cmdliner

let uris =
  let doc = "URI to show. If URI is an existing file path \
             a corresponding file:// URI is opened."
  in
  Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"URI")

let cmd =
  let doc = "Show URIs in web browsers" in
  let man = [
    `S Manpage.s_description;
    `P "The $(mname) command show URIs specified on the command line.";
    `P "Up to sever platform and browser limitation, $(mname) tries
        to limit the creation of new tabs, reloading existing ones
        which have the same URI or are, see option $(b,--prefix),
        prefixed by the URI."; ]
  in
  let exits =
    Term.exit_info 1 ~doc:"if the URI failed to load in some way" ::
    Term.default_exits
  in
  Term.(const show_uris $ B0_ui.B0_std.cli_setup () $
        B0_ui.Browser.background ~opts:["g"; "background"] () $
        B0_ui.Browser.prefix ~opts:["p"; "prefix"] () $
        B0_ui.Browser.browser ~opts:["b"; "browser"] () $ uris),
  Term.info "show-uri" ~doc ~sdocs:Manpage.s_common_options ~man ~exits

let () = Term.(exit_status @@ eval cmd)

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
