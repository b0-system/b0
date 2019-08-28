#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let copy src dst =
  Log.info (fun m -> m "cp %S %S" src dst);
  OS.File.read src >>= fun content ->
  let content = strf "# 1 %S\n%s" src content in
  OS.File.write dst content

let ocaml_conditional c =
  let maj, min, _, _ = Conf.OCaml.version (Conf.OCaml.v c `Host_os) in
  let dst = "src-care/b0_ocaml_cmi.ml" in
  match (maj, min) < (4,08) with
  | true  -> copy "src-care/b0_ocaml_cmi_pre_408/b0_ocaml_cmi.ml" dst
  | false -> copy "src-care/b0_ocaml_cmi_geq_408/b0_ocaml_cmi.ml" dst

let () =
  let build = Pkg.build ~pre:ocaml_conditional () in
  Pkg.describe ~build "b0" @@ fun c ->
  Ok [ Pkg.mllib "src-b00/b00.mllib";
       Pkg.mllib "src-std/b0_std.mllib";
       Pkg.mllib "src-care/b0_care.mllib";
       Pkg.lib "src-std/b0_std_top_init.ml";
       Pkg.clib "src-std/libb0_stubs.clib";
       Pkg.test "test/test";
       Pkg.test "test/test_cp";
       Pkg.test "test/test_rm";
       Pkg.test "test/test_memo_failures";
       Pkg.test "test/test_findex";
       Pkg.bin "tools/b00_cache" ~dst:"b00-cache";
       Pkg.bin "tools/b00_log" ~dst:"b00-log";
       Pkg.bin "tools/show_uri" ~dst:"show-uri";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/b00_manual.mld" ~dst:"odoc-pages/b00_manual.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld"; ]
