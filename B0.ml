open B0_kit.V000

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"
let b0_std = B0_ocaml.libname "b0.std"
let b0_memo = B0_ocaml.libname "b0.memo"
let b0_file = B0_ocaml.libname "b0.file"
let b0_kit = B0_ocaml.libname "b0.kit"
let b0_tool = B0_ocaml.libname "b0.tool"

(* B0 libraries *)

let b0_std_lib =
  let srcs =
    Fpath.[`Dir_rec (v "src/std"); `X (v "src/std/b0_std_top_init.ml")]
  in
  let requires = [unix; cmdliner] in
  B0_ocaml.lib b0_std ~doc:"B0 standard needs" ~srcs ~requires

let b0_memo_lib =
  let srcs = Fpath.[`Dir (v "src/memo")] in
  let requires = [unix; cmdliner; b0_std] in
  B0_ocaml.lib b0_memo ~doc:"B0 build library" ~srcs ~requires

let b0_file_lib =
  let srcs = Fpath.[`Dir (v "src/file")] in
  let requires = [unix; cmdliner; b0_std; b0_memo] in
  B0_ocaml.lib b0_file ~doc:"B0 build file library" ~srcs ~requires

let b0_kit_lib =
  let srcs = Fpath.[`Dir (v "src/kit")] in
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file] in
  B0_ocaml.lib b0_kit ~doc:"B0 toolkit library" ~srcs ~requires

(* The b0 tool *)

let bootstrap_env boot_root =
  let env = Os.Env.current () |> Log.if_error ~use:Os.Env.empty in
  env
  |> Os.Env.add "B0_BOOTSTRAP" (Fpath.to_string boot_root)
  |> Os.Env.add "B0_DRIVER_BOOTSTRAP" (Fpath.to_string boot_root)
  |> Os.Env.to_assignments

let b0_tool_lib =
  let srcs = Fpath.[`Dir (v "src/tool"); `X (v "src/tool/b0_main_run.ml")] in
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file; b0_kit] in
  B0_ocaml.lib b0_tool ~doc:"b0 tool driver library" ~srcs ~requires

let b0 =
  let srcs = Fpath.[`File (v "src/tool/b0_main_run.ml")] in
  let requires = [b0_file; b0_tool] in
  let meta =
    let env b u =
      (* FIXME need to access the root of the build. *)
      Fut.return (bootstrap_env Fpath.(B0_build.build_dir b u / ".."))
    in
    B0_meta.empty
    |> B0_meta.add B0_unit.Action.exec_env env
  in
  B0_ocaml.exe "b0" ~doc:"The b0 tool" ~srcs ~requires ~meta

(* Low-level B0 tools *)

let tool_exe n ~doc file =
  let requires = [cmdliner; b0_std; b0_memo; b0_file; b0_kit] in
  let srcs = [`File Fpath.(v "src/lowtools" / file)] in
  B0_ocaml.exe n ~doc ~srcs ~requires

let b0_cache_tool =
  tool_exe "b0-cache" ~doc:"Operate on b0 caches" "b0_cache.ml"

let b0_hash_tool =
  tool_exe "b0-hash" ~doc:"Hash like b0" "b0_hash.ml"

let b0_log_tool =
  tool_exe "b0-log" ~doc:"Operate on b0 logs" "b0_log.ml"

let show_uri_tool =
  tool_exe "show-uri" ~doc:"Show URIs in web browsers" "show_uri.ml"

(* Tests *)

let test_src f = `File Fpath.(v "test" // f)
let test_exe ?(requires = []) ?(more_srcs = []) file ~doc =
  let file = Fpath.v file in
  let more_srcs = List.map (fun v -> test_src (Fpath.v v)) more_srcs in
  let srcs = (test_src file) :: more_srcs in
  let requires =
    b0_std :: b0_memo :: b0_file :: b0_kit :: cmdliner :: requires
  in
  B0_ocaml.exe (Fpath.basename ~no_ext:true file) ~doc ~srcs ~requires

let test_memo ?requires ?(more_srcs = []) file ~doc =
  let more_srcs = "test_memo_setup.ml" :: more_srcs in
  test_exe ?requires ~more_srcs file ~doc

let test =
  test_exe "test.ml" ~doc:"Some tests for basic modules (B0_std, etc.)"
    ~more_srcs:["test_fmt.ml"; "test_fpath.ml"; "test_cmd.ml"; "test_base64.ml"]

let test_cp = test_exe "test_cp.ml" ~doc:"Test for Os.Path.copy"
let test_rm = test_exe "test_rm.ml" ~doc:"Test for Os.Path.delete"
let test_findex = test_exe "test_findex.ml" ~doc:"Test for B0_findex"
let test_memo_failure =
  test_memo "test_memo_failures.ml" ~doc:"Tests some failures of B0_memo.Memo."

let test_memo_no_write =
  test_memo "test_memo_no_write.ml" ~doc:"???"

let test_memo_store =
  test_memo "test_memo_store.ml" ~doc:"???"

let test_memo_redir =
  test_memo "test_memo_redir.ml" ~doc:"Test memo spawn stdio redirection"

let test_ocaml_cobj_defs =
  test_exe "test_ocaml_cobj_defs.ml" ~doc:"Test B0_ocaml.Cobj.of_string"

let test_b0_file =
  let requires = [b0_memo] in
  test_exe "test_b0_file.ml" ~requires ~doc:"Test B0_file module"

(* Packs *)

let b0_pack =
  B0_pack.v "b0" ~doc:"The B0 libraries" ~locked:true @@
  [b0_std_lib; b0_memo_lib; b0_file_lib; b0_kit_lib]

let tool_pack =
  B0_pack.v "b0-tools" ~doc:"The low-level B0 tools" ~locked:false @@
  [b0_cache_tool; b0_hash_tool; b0_log_tool; show_uri_tool]

let driver_pack =
  B0_pack.v "b0-drivers" ~doc:"The B0 drivers" ~locked:false @@ [b0]

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The b0 programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/b0"
    |> add online_doc "https://erratique.ch/software/b0/doc"
    |> add licenses ["ISC"; "BSD-2-Clause"]
    |> add repo "git+https://erratique.ch/repos/b0.git"
    |> add issues "https://github.com/b0-system/b0/issues"
    |> add description_tags ["dev"; "org:erratique"; "org:b0-system"; "build"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> add B0_opam.Meta.depends [
      "ocaml", {|>= "4.08.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.0.3"|};
      "cmdliner", {|>= "1.1.0"|}; ]
    |> tag B0_opam.tag
  in
  B0_pack.v "default" ~doc:"The B0 system" ~meta ~locked:true @@
  B0_unit.list ()

(* Actions *)

let b0wl =
  B0_action.v "b0wl" ~doc:"Run built b0 in the b0wl directory" @@
  fun env args ->
  let cwd = Fpath.(B0_action.Env.scope_dir env / "b0wl") in
  let b0_dir = B0_action.Env.b0_dir env in
  let variant = "user" in (* FIXME access to current variant *)
  let bootstrap_root = B0_dir.build_dir ~b0_dir ~variant in
  let env = bootstrap_env bootstrap_root in
  let b0_exec = Fpath.(bootstrap_root / "b0-exe" / "b0") in
  Os.Exit.exec ~env ~cwd b0_exec Cmd.(arg "b0" %% args)

let strap =
  B0_action.v "strap" ~doc:"Run boot/strap" @@
  B0_action.exec_file (Fpath.v "boot/strap")
