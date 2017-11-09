open B0

(* Simple B0_ocaml-less example for:

   ocamlc -c -o $BDIR/hello hello.ml
   ocamlc -o $BDIR/hello $BDIR/hello.cmo

   but a bit more evolved. Debug builds can be requested. The OCaml
   compiler used can be specified in the configuration and found in
   the environment under different names. The executable is tagged so
   that it shows up in `b0 run list`. *)

(* Configuration *)

let group = Conf.Group.v "ocaml" ~doc:"OCaml related configuration keys."

let debug =
  let doc = "Debug mode" in
  Conf.key "debug" Conv.bool ~doc ~default:(Conf.const true) ~group

let ocamlc =
  let doc = "The OCaml byte-code compiler." in
  let tools = [Fpath.v "ocamlc.opt"; Fpath.v "ocamlc"] in
  Tool.key ~tools "ocamlc" ~doc ~group

(* Compilation of an [ml] source to a byte-code [exe_name] binary. *)

let byte_exe ?exe_name ml b =
  let exe_name = match exe_name with
  | None -> Fpath.(basename @@ rem_ext @@ ml)
  | Some n -> n
  in
  let ocamlc = Build.conf_tool b ocamlc in
  let debug = Build.conf b debug in
  let ml = Build.src b ml in
  let bin = Build.build_file b exe_name in
  let cmi = Fpath.(bin + ".cmi") in
  let cmo = Fpath.(bin + ".cmo") in
  let debug = Cmd.(on debug @@ v "-g") in
  (* We specify linking before compiling, but b0 sorts it out. *)
  Build.spawn b ~reads:[cmo] ~writes:[bin] @@
  ocamlc Cmd.(debug % "-o" % p bin % p cmo);
  Build.spawn b ~reads:[ml] ~writes:[cmi; cmo] @@
  ocamlc Cmd.(debug % "-c" % "-o" % p bin % p ml);
  (* Add metadata to allow the exe to appear in b0 run list *)
  Build.add_path_meta b bin B0_care.exe true;
  ()

let exe =
  let exe = byte_exe (Fpath.v "hello.ml") in
  Unit.create "hello" exe ~doc:"Hello from scratch with configuration keys"
