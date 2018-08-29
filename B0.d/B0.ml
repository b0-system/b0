
open B0

(* Variant schemes *)

let opam = B0_opam.variant_scheme "4.06.0+rc1"
let docker = B0_docker.variant_scheme "b0-builder" B0.Variant.Scheme.nop
let ssh =
  let excludes = [B0.Fpath.v "_build"] in
  B0_ssh.variant_scheme ~excludes "b0_builds" B0.Variant.Scheme.nop

(* Libraries *)

let result = "result"
let unix = "unix"
let cmdliner = "cmdliner"

let srcs dir = (`Src_dirs [Fpath.v dir])

(* Core libraries *)

let pkg = Pkg.create "b0-lib" ~doc:"Software construction care"
let b0_std = "b0.std"
let b0 = "b0"
let b0_care = "b0.care"

let std =
  let lib_deps = [unix] in
  B0_ocaml.Unit.lib ~pkg b0_std (srcs "src-std") ~lib_deps

let lib =
  let lib_deps = [b0_std] in
  B0_ocaml.Unit.lib ~pkg b0 (srcs "src-lib") ~lib_deps

let care =
  let lib_deps = [b0] in
  B0_ocaml.Unit.lib ~pkg b0_care (srcs "src-care") ~lib_deps

(* b0 and d0 *)

let pkg = Pkg.create "b0" ~doc:"Software construction care tools"
let b0_driver = "b0.driver"
let b0_b0 = "b0.b0"

let driver =
  let lib_deps = [result; cmdliner; b0] in
  B0_ocaml.Unit.lib ~pkg b0_driver (srcs "src-driver") ~lib_deps

let driver_b0 =
  let lib_deps = [result; cmdliner; b0; b0_care; b0_driver ] in
  B0_ocaml.Unit.lib ~pkg "b0.b0" (srcs "src-b0") ~lib_deps

let exe =
  let lib_deps = [b0_driver; b0_b0] in
  let build = `Srcs [Fpath.v "src-exe/bzero.ml"] in
  B0_ocaml.Unit.exe ~pkg ~name:"b0" "b0-exe" build ~lib_deps

(* Other tools *)

let bzsize =
  let lib_deps = [result; cmdliner; b0;] in
  let build = `Srcs [Fpath.v "src-exe/bzsize.ml"] in
  B0_ocaml.Unit.exe ~pkg "bzsize" build ~lib_deps
