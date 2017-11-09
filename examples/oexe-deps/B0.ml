open B0

(* B0_ocaml: simple OCaml executable made of multiple dependent sources. *)

let hello =
  B0_ocaml.Unit.exe "hello" ~doc:"The hello dep binary"
