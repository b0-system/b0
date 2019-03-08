#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "b0" @@ fun c ->
  Ok [ Pkg.mllib ~api:["B00"] "src-b00/b00.mllib";
       Pkg.mllib ~api:["B0_std"] "src-std/b0_std.mllib";
       Pkg.mllib "src-care/b0_care.mllib";
       Pkg.lib "src-std/b0_std_top_init.ml";
       Pkg.clib "src-std/libb0_stubs.clib";
       Pkg.test "test/test";
       Pkg.test "test/test_cp";
       Pkg.test "test/test_rm";
       Pkg.test "examples/build";
       Pkg.bin "tools/b00_cache" ~dst:"b00-cache";
       Pkg.bin "tools/show_uri" ~dst:"show-uri";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";]
