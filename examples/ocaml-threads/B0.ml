open B0_kit.V000
open B00_std


let threads = B0_ocaml.libname "threads"
let test =
  let srcs = Fpath.[`File (v "test.ml")] in
  B0_ocaml.exe "test" ~requires:[threads] ~srcs
