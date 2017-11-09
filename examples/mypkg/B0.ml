open B0

(* OCaml package with B0_ocaml.
   FIXME B0_ocaml.Pkg should be provided, we want bells and whistles. *)

let mypkg = "mypkg"
let lib1_id = mypkg ^ ".lib1" (* FIXME B0_ocaml could provide a combinator *)
let lib2_id = mypkg ^ ".lib2"

let pkg = Pkg.create mypkg ~doc:"My example OCaml package"

let src_dir d = `Src_dirs [Fpath.v d]

let lib1 =
  B0_ocaml.Unit.lib ~pkg lib1_id (src_dir "src-lib1")

let lib2 =
  B0_ocaml.Unit.lib ~pkg lib2_id (src_dir "src-lib2") ~lib_deps:[lib1_id]

let test =
  B0_ocaml.Unit.exe ~pkg "test" (src_dir "test") ~lib_deps:[lib1_id; lib2_id]
