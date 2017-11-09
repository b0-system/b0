
(* Test Build.spawn redirections *)

open B0

(* FIXME would be nice to port to windows. *)

let echo = Tool.v "echo"
let cat = Tool.v "cat"

let stdout_stderr_redirect =
  let build b =
    let echo = Build.tool b echo in
    let cat = Build.tool b cat in
    let x = Build.build_file b "x" in
    let y = Build.build_file b "y" in
    let data = "lambda" in
    let assert_file b f s = Build.read b f (fun r -> assert (r = s)) in
    Build.spawn b ~writes:[x] ~stdout:(`File x) @@
    echo Cmd.(v "-n" % data);
    Build.spawn b ~reads:[x] ~writes:[y] ~stdin:x ~stdout:(`File y) @@
    cat Cmd.(v "-");
    assert_file b x data;
    assert_file b y data;
  in
  Unit.create "stdout_redirect" build ~doc:"Test stdin/stdout redirection"

let stdout_tee =
  let build b =
    let echo = Build.tool b echo in
    let x = Build.build_file b "x" in
    Build.spawn b ~writes:[x] ~stdout:(`Tee x) @@
    echo Cmd.(v "hey");
    Build.read b x (fun d -> assert (d <> ""))
  in
  Unit.create "stdout_tee" build ~doc:"Test stderr tee redirection"

let stderr_redirect =
  let build b =
    let cat = Build.tool b cat in
    let x = Build.build_file b "x" in
    Build.spawn b ~writes:[x] ~stderr:(`File x) ~exits:[] @@
    cat Cmd.(v "-7");
    Build.read b x (fun d -> assert (d <> ""))
  in
  Unit.create "stderr_redirect" build ~doc:"Test stderr redirection"

let stderr_tee =
  let build b =
    let cat = Build.tool b cat in
    let x = Build.build_file b "x" in
    Build.spawn b ~writes:[x] ~stderr:(`Tee x) ~exits:[] @@
    cat Cmd.(v "-7");
    Build.read b x (fun d -> assert (d <> ""))
  in
  Unit.create "stderr_tee" build ~doc:"Test stderr tee redirection"
