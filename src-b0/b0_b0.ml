(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Main *)

let version = "%%VERSION%%"

let doc = "Build, update and inspect b0 descriptions"
let sdocs = Manpage.s_common_options
let exits = B0_driver.Cli.driver_default_exits
let man =
  [ `S Manpage.s_description;
    `P "$(mname) takes care of $(b,B0.ml) file descriptions.";
    `P "Use '$(mname)' or '$(mname) $(b,build)' to run the build.";
    `Noblank;
    `P "Use '$(mname) $(b,reset)' to reset the build.";
    `Noblank;
    `P "Use '$(mname) $(b,key)' to operate on configuration keys.";
    `Noblank;
    `P "Use '$(mname) $(b,log)' to inspect the last build outcome operations.";
    `Noblank;
    `P "Use '$(mname) $(b,outcome)' to inspect the last build outcome info.";
    `Noblank;
    `P "Use '$(mname) $(b,help) $(i,COMMAND)' for help about $(i,COMMAND).";
    `S Manpage.s_common_options;
    `S B0_driver.Cli.s_driver_opts;
    `S Manpage.s_bugs;
    `P "See %%PKG_HOMEPAGE%% for contact information.";
    `S "REFERENCES";
    `P "The B0 manual. $(i,%%PKG_HOMEPAGE%%/doc/B0.html#manual) or
        $(b,odig doc b0),"; ]

let main =
  let cmd, _, exec = B0b_build.cmd in
  cmd, Term.info "b0" ~version ~doc ~sdocs ~exits ~man, exec

let cmds =
  [ main;
    B0b_build.cmd;
    B0b_cache.cmd;
    B0b_group.cmd;
    B0b_key.cmd;
    B0b_outcome.cmd;
    B0b_preset.cmd;
    B0b_reset.cmd;
    B0b_run.cmd;
    B0b_scheme.cmd;
    B0b_unit.cmd;
    B0b_pkg.cmd;
    B0b_log.cmd;
    B0b_variant.cmd; ]

(* Driver definition *)

let () =
  let libs = ["b0.b0"] in
  let d = B0_driver.Driver.create ~name:"b0" ~version ~libs cmds in
  B0_driver.Driver.set d

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
