#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "b0" @@ fun c ->
  Ok [
    (* Libraries *)
    Pkg.mllib "src/std/b0_std.mllib" ~dst_dir:"std";
    Pkg.clib "src/std/libb0_stubs.clib" ~lib_dst_dir:"std";
    Pkg.lib "src/std/b0_std_top_init.ml"
      ~dst:"std/b0_std_top_init.ml";
    Pkg.mllib "src/memo/b0_memo.mllib" ~dst_dir:"memo";
    Pkg.mllib "src/file/b0_file.mllib" ~dst_dir:"file";
    Pkg.mllib "src/kit/b0_kit.mllib" ~dst_dir:"kit";

    (* b0 tool *)
    Pkg.mllib "src/tool/b0_tool.mllib" ~dst_dir:"tool";
    Pkg.bin "src/tool/b0_main_run" ~dst:"b0";

    (* Tools *)
    Pkg.bin "tools/b0_cache" ~dst:"b0-cache";
    Pkg.bin "tools/b0_log" ~dst:"b0-log";
    Pkg.bin "tools/b0_hash" ~dst:"b0-hash";
    Pkg.bin "tools/show_uri" ~dst:"show-uri";

    (* Tests *)
    Pkg.test "test/test";
    Pkg.test "test/test_b0_file";
    Pkg.test "test/test_cp";
    Pkg.test "test/test_findex";
    Pkg.test "test/test_memo_failures";
    Pkg.test "test/test_memo_no_write";
    Pkg.test "test/test_memo_redir";
    Pkg.test "test/test_memo_store";
    Pkg.test "test/test_ocaml_cobj_defs";
    Pkg.test "test/test_rm";

    (* Doc *)
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/memo_manual.mld" ~dst:"odoc-pages/memo_manual.mld";
    Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
    Pkg.doc "doc/driver_dev.mld" ~dst:"odoc-pages/driver_dev.mld";
    Pkg.doc "doc/old_manual.mld" ~dst:"odoc-pages/old_manual.mld";
    Pkg.doc "doc/unit_manual.mld" ~dst:"odoc-pages/unit_manual.mld";
    Pkg.doc "doc/cmdlet_manual.mld" ~dst:"odoc-pages/cmdlet_manual.mld";
    Pkg.doc "doc/rationale.mld" ~dst:"odoc-pages/rationale.mld";
    Pkg.doc "doc/todo.mld" ~dst:"odoc-pages/todo.mld";
    Pkg.doc "doc/opam.mld" ~dst:"odoc-pages/opam.mld";
    Pkg.doc "doc/release.mld" ~dst:"odoc-pages/release.mld";
  ]
