open B0

(* Test Build.{read,copy_file} *)

let copy_file =
  let build b =
    let x = Fpath.(Build.src_dir b / "x") in
    let y = Build.build_file b "y" in
    let z = Build.build_file b "z" in
    let lambda = "lambda" in
    let assert_file b f s = Build.read b f (fun r -> assert (r = s)) in
    Build.ready b x;
    Build.copy_file b x y;
    Build.copy_file b ~linenum:1 y z;
    assert_file b x lambda;
    assert_file b y lambda;
    assert_file b z (strf "#line 1 %a\n%s" Fpath.dump y lambda);
  in
  Unit.create "copy_file" build ~doc:"Testing file copy and reads."

(* Test Build.mkdir *)

let mkdir_file =
  let build b =
    let d = Fpath.(Build.build_dir b / "a" / "b" / "c") in
    let assert_dir () =
      Build.fail_on_error_msg (OS.Dir.exists d >>| fun e -> assert e)
    in
    Build.mkdir b d assert_dir
  in
  Unit.create "mkdir" build ~doc:"Testing direction creation."
