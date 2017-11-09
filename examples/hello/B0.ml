open B0

(* Simple B0_ocaml-less example for:

   ocamlc -c -o $BDIR/hello hello.ml
   ocamlc -o $BDIR/hello $BDIR/hello.cmo  *)

let hello b =
  let ocamlc = Build.tool b (Tool.v "ocamlc") in
  let src = Build.src b (Fpath.v "hello.ml") in
  let bin = Build.build_file b "hello" in
  let cmi = Fpath.(bin + ".cmi") in
  let cmo = Fpath.(bin + ".cmo") in
  Build.spawn b ~reads:[src] ~writes:[cmi; cmo] @@
  ocamlc Cmd.(v "-c" % "-o" % p bin % p src);
  Build.spawn b ~reads:[cmo] ~writes:[bin] @@
  ocamlc Cmd.(v "-o" % p bin % p cmo);
  ()

let hello = Unit.create "hello" hello ~doc:"Hello from scratch"
