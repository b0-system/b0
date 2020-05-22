#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "b0" @@ fun c ->
  Ok [
    (* B00 libraries *)
    Pkg.mllib "src-b00/std/b0_b00_std.mllib" ~dst_dir:"b00/std";
    Pkg.clib "src-b00/std/libb0_b00_stubs.clib" ~lib_dst_dir:"b00/std";
    Pkg.lib "src-b00/std/b0_b00_std_top_init.ml"
      ~dst:"b00/std/b0_b00_std_top_init.ml";
    Pkg.mllib "src-b00/b0_b00.mllib" ~dst_dir:"b00";
    Pkg.mllib "src-b00/kit/b0_b00_kit.mllib" ~dst_dir:"b00/kit";

    (* B0 libraries *)
    Pkg.mllib "src/b0.mllib";
    Pkg.mllib "src/kit/b0_kit.mllib" ~dst_dir:"kit";
    Pkg.mllib "src/driver/b0_driver.mllib" ~dst_dir:"driver";

    (* b0 & d0 tools *)
    Pkg.mllib "tool-b0/b0_driver_b0.mllib" ~dst_dir:"driver/b0";
    Pkg.bin "tool-b0/b0_main_run" ~dst:"b0";
(*    Pkg.mllib "tool-d0/b0_driver_d0.mllib" ~dst_dir:"driver/d0";
      Pkg.bin "tool-d0/d0_main_run" ~dst:"b0";
 *)

    (* Tools *)
    Pkg.bin "tools/b00_cache" ~dst:"b00-cache";
    Pkg.bin "tools/b00_log" ~dst:"b00-log";
    Pkg.bin "tools/b00_hash" ~dst:"b00-hash";
    Pkg.bin "tools/show_uri" ~dst:"show-uri";

    (* Tests *)
    Pkg.test "test/test";
    Pkg.test "test/test_cp";
    Pkg.test "test/test_rm";
    Pkg.test "test/test_memo_failures";
    Pkg.test "test/test_memo_no_write";
    Pkg.test "test/test_memo_redir";
    Pkg.test "test/test_memo_store";
    Pkg.test "test/test_findex";
    Pkg.test "test/test_ocaml_cobj_defs";
    Pkg.test "test/test_b0_file";

    (* Doc *)
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/b00_manual.mld" ~dst:"odoc-pages/b00_manual.mld";
    Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
    Pkg.doc "doc/driver_dev.mld" ~dst:"odoc-pages/driver_dev.mld";
  ]
