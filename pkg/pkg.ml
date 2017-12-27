#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  let opams =
    [ Pkg.opam_file ~install:true "b0.opam";
      (* Pkg.opam_file ~install "b0-lib.opam" *) ]
  in
  Pkg.describe "b0" ~opams @@ fun c ->
  Ok [ Pkg.mllib ~api:[] "src-std/b0_std.mllib";
       Pkg.clib "src-std/libb0_stubs.clib";
       Pkg.mllib ~api:["B0"] "src-lib/b0.mllib";
       Pkg.mllib ~api:["B0_driver"] "src-driver/b0_driver.mllib";
       Pkg.mllib "src-care/b0_care.mllib";
       Pkg.mllib ~api:[] "src-b0/b0_b0.mllib";
       Pkg.mllib ~api:[] "src-d0/d0.mllib";
       Pkg.test "test/test_hmap";
       Pkg.bin "src-exe/bzero" ~dst:"b0";
       Pkg.bin "src-exe/dzero" ~dst:"d0"; ]
