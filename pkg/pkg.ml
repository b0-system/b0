#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "b0" @@ fun c ->
  Ok [ Pkg.mllib "src-b00/b00.mllib" ~dst_dir:"b00";
       Pkg.mllib "src-std/b0_std.mllib" ~dst_dir:"std";
       Pkg.lib "src-std/b0_std_top_init.ml" ~dst:"std/b0_std_top_init.ml";
       Pkg.clib "src-std/libb0_stubs.clib" ~lib_dst_dir:"std";
       Pkg.mllib "src-care/b0_care.mllib" ~dst_dir:"care";
       Pkg.mllib "src-file/b0_file.mllib" ~dst_dir:"file";
       Pkg.mllib "src-defs/b0_defs.mllib" ~dst_dir:"defs";
       Pkg.mllib "src-driver/b0_driver.mllib" ~dst_dir:"driver";
       Pkg.mllib "src-b0/b0.mllib" ~dst_dir:"b0";
       Pkg.test "test/test";
       Pkg.test "test/test_cp";
       Pkg.test "test/test_rm";
       Pkg.test "test/test_memo_failures";
       Pkg.test "test/test_memo_no_write";
       Pkg.test "test/test_memo_redir";
       Pkg.test "test/test_findex";
       Pkg.test "test/test_ocaml_cobj_defs";
       Pkg.test "test/test_b0_file";
       Pkg.bin "src-b0/b0_main_run" ~dst:"b0";
       Pkg.bin "tools/b00_cache" ~dst:"b00-cache";
       Pkg.bin "tools/b00_log" ~dst:"b00-log";
       Pkg.bin "tools/b00_hash" ~dst:"b00-hash";
       Pkg.bin "tools/show_uri" ~dst:"show-uri";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/b00_manual.mld" ~dst:"odoc-pages/b00_manual.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
       Pkg.doc "doc/b0_driver_dev.mld" ~dst:"odoc-pages/b0_driver_dev.mld";
     ]
