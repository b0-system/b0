(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

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
