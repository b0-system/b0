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
    Pkg.lib "src/std/runtime.js" ~dst:"std/";
    Pkg.lib "src/std/b0_std_top_init.ml" ~dst:"std/";
    Pkg.mllib "src/memo/b0_memo.mllib" ~dst_dir:"memo";
    Pkg.mllib "src/file/b0_file.mllib" ~dst_dir:"file";
    Pkg.mllib "src/kit/b0_kit.mllib" ~dst_dir:"kit";

    (* b0 tool *)
    Pkg.mllib "src/tool/b0_tool.mllib" ~dst_dir:"tool";
    Pkg.bin "src/tool/b0_main_run" ~dst:"b0";

    (* Tools *)
    Pkg.bin "src/lowtools/b0_cache" ~dst:"b0-cache";
    Pkg.bin "src/lowtools/b0_log" ~dst:"b0-log";
    Pkg.bin "src/lowtools/b0_hash" ~dst:"b0-hash";
    Pkg.bin "src/lowtools/b0_sttyle" ~dst:"b0-sttyle";
    Pkg.bin "src/lowtools/show_url" ~dst:"show-url";

    (* Doc *)
    Pkg.doc "doc/action.mld" ~dst:"odoc-pages/action.mld";
    Pkg.doc "doc/driver.mld" ~dst:"odoc-pages/driver.mld";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld";
    Pkg.doc "doc/memo.mld" ~dst:"odoc-pages/memo.mld";
    Pkg.doc "doc/ocaml.mld" ~dst:"odoc-pages/ocaml.mld";
    Pkg.doc "doc/old_manual.mld" ~dst:"odoc-pages/old_manual.mld";
    Pkg.doc "doc/opam.mld" ~dst:"odoc-pages/opam.mld";
    Pkg.doc "doc/occasionally.mld" ~dst:"odoc-pages/occasionally.mld";
    Pkg.doc "doc/rationale.mld" ~dst:"odoc-pages/rationale.mld";
    Pkg.doc "doc/release.mld" ~dst:"odoc-pages/release.mld";
    Pkg.doc "doc/testing.mld" ~dst:"odoc-pages/testing.mld";
    Pkg.doc "doc/todo.mld" ~dst:"odoc-pages/todo.mld";
    Pkg.doc "doc/unit.mld" ~dst:"odoc-pages/unit.mld";
  ]
