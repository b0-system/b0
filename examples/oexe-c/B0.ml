open B0

(* B0_ocaml: simple mixed OCaml/C executable. *)

let exe =
  let srcs = `Src_dirs [Fpath.v "src"] in
  B0_ocaml.Unit.exe "oexe-c" srcs
