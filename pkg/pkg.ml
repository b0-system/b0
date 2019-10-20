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
  let dst = "src-ocaml/b0_ocaml_cmi.ml" in
  match (maj, min) < (4,08) with
  | true  -> copy "src-ocaml/b0_ocaml_cmi_pre_408/b0_ocaml_cmi.ml" dst
  | false -> copy "src-ocaml/b0_ocaml_cmi_geq_408/b0_ocaml_cmi.ml" dst

let () =
  let build = Pkg.build ~pre:ocaml_conditional () in
  Pkg.describe ~build "b0" @@ fun c ->
  Ok [ Pkg.mllib "src-b00/b00.mllib" ~dst_dir:"b00";
       Pkg.mllib "src-std/b0_std.mllib" ~dst_dir:"std";
       Pkg.lib "src-std/b0_std_top_init.ml" ~dst:"std/b0_std_top_init.ml";
       Pkg.clib "src-std/libb0_stubs.clib" ~lib_dst_dir:"std";
       Pkg.mllib "src-care/b0_care.mllib" ~dst_dir:"care";
       Pkg.mllib "src-ocaml/b0_ocaml.mllib" ~dst_dir:"ocaml";
       Pkg.test "test/test";
       Pkg.test "test/test_cp";
       Pkg.test "test/test_rm";
       Pkg.test "test/test_memo_failures";
       Pkg.test "test/test_findex";
       Pkg.test "test/test_ocaml_cobj_defs";
       Pkg.bin "tools/b00_cache" ~dst:"b00-cache";
       Pkg.bin "tools/b00_log" ~dst:"b00-log";
       Pkg.bin "tools/show_uri" ~dst:"show-uri";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/b00_manual.mld" ~dst:"odoc-pages/b00_manual.mld";
       Pkg.doc "doc/manual.mld" ~dst:"odoc-pages/manual.mld"; ]
