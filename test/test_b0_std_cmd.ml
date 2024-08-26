(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing

let ls p = Cmd.(arg "ls" % "-a" %% path p)
let tar file dir = Cmd.(arg "tar" % "-cvf" %% path file %% path dir)

let opam cmd = Cmd.(arg "opam" % cmd)
let opam_install pkgs = Cmd.(opam "install" %% list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(arg "ocamlc" % "-c" %% if' debug (arg "-g") %% unstamp (path file))

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(if' profile (arg "-p")) in
  let debug = Cmd.(if' debug (arg "-g")) in
  let incs = Cmd.(unstamp (paths ~slip:"-I" incs)) in
  Cmd.(arg "ocamlopt" % "-c" %% debug %% profile %% incs %% (path file))

let test_stamp () =
  Test.test "stamps" @@ fun () ->
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

let main () =
  Test.main @@ fun () ->
  test_stamp ();
  ()

let () = if !Sys.interactive then () else exit (main ())
