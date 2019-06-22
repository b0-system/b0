(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_std

let cp_cmd () allow_hardlinks follow_symlinks recurse src dst =
  let r =
    Os.Path.copy
      ~allow_hardlinks ~follow_symlinks ~make_path:true ~recurse ~src dst
  in
  Log.if_error ~use:1 (Result.bind r @@ fun () -> Ok 0)

let () =
  let open Cmdliner in
  let cmd =
    let allow_hardlinks =
      let doc = "If possible use hard links instead of copying." in
      Arg.(value & flag & info ["h"; "allow-hard-links"] ~doc)
    in
    let follow_symlinks =
      let doc = "Preserve symbolic links rather than following them." in
      let p = Arg.(value & flag & info ["s"; "preserve-symbolic-links"] ~doc) in
      Term.(const (fun p -> not p) $ p)
    in
    let recurse =
      let doc = "If $(i,SRC) is a directory copy it recursively. Otherwise
                 only copies the files therein to the destination."
      in
      Arg.(value & flag & info ["r"; "recurse"] ~doc)
    in
    let src =
      let doc = "$(docv) is the source file or directory" in
      Arg.(required & pos 0 (some B0_ui.Cli.Arg.fpath) None &
           info [] ~doc ~docv:"SRC")
    in
    let dst =
      let doc = "$(docv) is the destination path; which must not exist." in
      Arg.(required & pos 1 (some B0_ui.Cli.Arg.fpath) None &
           info [] ~doc ~docv:"DST")
    in
    Term.(const cp_cmd $ B0_ui.B0_std.cli_setup () $ allow_hardlinks $
          follow_symlinks $ recurse $ src $ dst),
    Term.info "test-cp" ~sdocs:Manpage.s_common_options
  in
  Term.exit_status (Term.eval cmd)


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
