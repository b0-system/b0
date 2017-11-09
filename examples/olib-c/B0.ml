open B0

(* B0_ocaml: simple mixed OCaml/C library and a test.
   FIXME once we get testing support make the exe a test. *)

let srcs dir = `Src_dirs [Fpath.v dir]
let clib = "clib"
let lib = B0_ocaml.Unit.lib clib (srcs "src")
let test = B0_ocaml.Unit.exe "test" (srcs "test") ~lib_deps:[clib]
