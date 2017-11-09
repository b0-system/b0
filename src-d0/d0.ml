(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Main *)

let version = "%%VERSION%%"

let doc = "Deploy b0 builds"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Cli.driver_default_exits
let man =
  [ `S Manpage.s_description;
    `P "$(mname) deploys builds obtained via $(b,b0).";
    `P "Use '$(mname)' or '$(mname) $(b,push)' to deploy the build.";
    `Noblank;
    `P "Use '$(mname) $(b,deploy)' to operate on deployments.";
    `S Manpage.s_common_options;
    `S B0_driver.Cli.s_driver_opts;
    `S Manpage.s_bugs;
    `P "See %%PKG_HOMEPAGE%% for contact information.";
    `S "REFERENCES";
    `P "The B0 manual. $(i,%%PKG_HOMEPAGE%%/doc/B0.html#manual) or
        $(b,odig doc b0),"; ]

let main =
  let cmd, _, exec = D0_push.cmd in
  cmd, Term.info "d0" ~version ~doc ~sdocs ~exits ~man, exec

let cmds =
  [ main; ]

(* Driver definition *)

let () =
  let libs = ["d0"] in
  let d = B0_driver.Driver.create ~name:"d0" ~version ~libs cmds in
  B0_driver.Driver.set d

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
