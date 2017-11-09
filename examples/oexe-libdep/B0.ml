open B0

(* B0_ocaml: simple OCaml executable with an external library dependency *)

let hello =
  let lib_deps = ["cmdliner"] in
  B0_ocaml.Unit.exe "hello" ~lib_deps ~doc:"The hello dep binary"
