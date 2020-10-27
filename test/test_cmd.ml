(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let ls p = Cmd.(atom "ls" % "-a" %% path p)
let tar file dir = Cmd.(atom "tar" % "-cvf" %% path file %% path dir)

let opam cmd = Cmd.(atom "opam" % cmd)
let opam_install pkgs = Cmd.(opam "install" %% list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(atom "ocamlc" % "-c" %% if' debug (atom "-g") %% unstamp (path file))

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(if' profile (atom "-p")) in
  let debug = Cmd.(if' debug (atom "-g")) in
  let incs = Cmd.(unstamp (paths ~slip:"-I" incs)) in
  Cmd.(atom "ocamlopt" % "-c" %% debug %% profile %% incs %% (path file))

let test_stamp () =
  let test cl cmd wit =
    assert ((cmd, wit) = Cmd.to_list_and_stamp cl)
  in
  test (ls (Fpath.v "/tmp/hey ha"))
    [ "ls"; "-a"; "/tmp/hey ha" ]
    [ "ls"; "-a"; "/tmp/hey ha" ];
  test (tar (Fpath.v "p.tar") (Fpath.v "dir"))
    [ "tar"; "-cvf"; "p.tar"; "dir" ]
    [ "tar"; "-cvf"; "p.tar"; "dir" ];
  test (opam_install ["cmdliner"; "b0"; "odoc"])
    [ "opam"; "install"; "cmdliner"; "b0"; "odoc" ]
    [ "opam"; "install"; "cmdliner"; "b0"; "odoc" ];
  test (ocamlc ~debug:false (Fpath.v "m.ml"))
    [ "ocamlc"; "-c"; "m.ml" ]
    [ "ocamlc"; "-c" ];
  test (ocamlopt ~profile:true ~debug:false
          [Fpath.v "cmdliner"; Fpath.v "xmlm";] (Fpath.v "m.ml"))
    [ "ocamlopt"; "-c"; "-p"; "-I"; "cmdliner"; "-I"; "xmlm"; "m.ml" ]
    [ "ocamlopt"; "-c"; "-p"; "m.ml" ];
  ()

let test () =
  test_stamp ();
  ()

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
