open B0

(* B0_ocaml: compile quine0.ml to an exe, run it to generate quine1.ml,
   compile it, run it to generate quine2.ml and verify it's equal
   to quine0.ml. Besides compile quine2.ml for the host OS *)

let assert_srcs b src0 src1 =
  Build.read b src0 @@ fun src0 ->
  Build.read b src1 @@ fun src1 ->
  match String.equal src0 src1 with
  | false -> Build.fail (fun m -> m "%S <> %S" src0 src1)
  | true -> ()

let build_quine b conf src =
  let quine = Fpath.(v @@ filename @@ rem_ext src) in
  let quine_tool = B0_ocaml.Build.tool ~name:quine b (Build.unit b) conf in
  B0_ocaml.Build.exe b conf ~srcs:[src] (Fpath.to_string quine);
  quine_tool

let run_quine b quine_tool ~dst =
  Build.spawn b ~writes:[dst] ~stdout:(`File dst) @@
  quine_tool Cmd.empty

let quine _ _ _ b =
  let conf = B0_ocaml.conf b in
  let q0_src = Build.src b (Fpath.v "quine0.ml") in
  let q1_src = Build.build_file b "quine1.ml" in
  let q2_src = Build.build_file b "quine2.ml" in
  let quine_tool0 = build_quine b conf q0_src in
  let () = run_quine b quine_tool0 ~dst:q1_src in
  let quine_tool1 = build_quine b conf q1_src in
  let () = run_quine b quine_tool1 ~dst:q2_src in
  B0_ocaml.Build.exe b conf ~srcs:[q2_src] "quine";
  assert_srcs b q0_src q2_src

let quine =
  B0_ocaml.Unit.exe "quine" (`Func quine) ~doc:"The quine executable by itself"
