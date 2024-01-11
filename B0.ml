open B0_kit.V000
open Result.Syntax

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
  let srcs = [ `Dir_rec ~/"src/std"; `X ~/"src/std/b0_std_top_init.ml" ] in
  let requires = [unix; cmdliner] in
  B0_ocaml.lib b0_std ~doc:"B0 standard library" ~srcs ~requires

let b0_memo_lib =
  let srcs = [`Dir ~/"src/memo"] in
  let requires = [unix; cmdliner; b0_std] in
  B0_ocaml.lib b0_memo ~doc:"B0 build library" ~srcs ~requires

let b0_file_lib =
  let srcs = [`Dir ~/"src/file"] in
  let requires = [unix; cmdliner; b0_std; b0_memo] in
  B0_ocaml.lib b0_file ~doc:"B0 build file library" ~srcs ~requires

let b0_kit_lib =
  let srcs = [`Dir ~/"src/kit"] in
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file] in
  B0_ocaml.lib b0_kit ~doc:"B0 toolkit library" ~srcs ~requires

(* The b0 tool *)

let add_bootstrap_env env boot_root =
  env
  |> Os.Env.add "B0_BOOTSTRAP" (Fpath.to_string boot_root)
  |> Os.Env.add "B0_DRIVER_BOOTSTRAP" (Fpath.to_string boot_root)

let b0_tool_lib =
  let srcs = [ `Dir ~/"src/tool"; `X ~/"src/tool/b0_main_run.ml"] in
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file; b0_kit] in
  B0_ocaml.lib b0_tool ~doc:"b0 tool driver library" ~srcs ~requires

let b0 =
  let srcs = [`File ~/"src/tool/b0_main_run.ml"] in
  let requires = [b0_file; b0_tool] in
  let build_bootstrap_exec_env =
    let env env unit =
      (* FIXME need to access the root of the build and access build os env *)
      let build = B0_env.build env in
      let env = (Os.Env.current () |> Log.if_error ~use:Os.Env.empty) in
      let boot_root = Fpath.(B0_build.build_dir build unit / "..") in
      let env = add_bootstrap_env env boot_root in
      (* If the build is locked via `b0 lock`, unlock it. *)
      let env = String.Map.remove "B0_FILE" env in
      let env = String.Map.remove "B0_DIR" env in
      Fut.return env
    in
    `Custom_env ("Bootstrap env set on the b0 build", env)
  in
  let meta =
    B0_meta.empty
    |> B0_meta.add B0_unit.exec_env build_bootstrap_exec_env
  in
  B0_ocaml.exe "b0" ~doc:"The b0 tool" ~srcs ~requires ~meta

(* Low-level B0 tools *)

let tool_exe n ~doc file =
  let requires = [cmdliner; b0_std; b0_memo; b0_file; b0_kit] in
  let srcs = [`File Fpath.(~/"src/lowtools" / file)] in
  B0_ocaml.exe n ~doc ~srcs ~requires

let b0_cache_tool =
  tool_exe "b0-cache" ~doc:"Operate on b0 caches" "b0_cache.ml"

let b0_hash_tool =
  tool_exe "b0-hash" ~doc:"Hash like b0" "b0_hash.ml"

let b0_log_tool =
  tool_exe "b0-log" ~doc:"Operate on b0 logs" "b0_log.ml"

let show_url_tool =
  tool_exe "show-url" ~doc:"Show URLs in web browsers" "show_url.ml"

(* Tests *)

let test_src f = `File Fpath.(~/"test" // f)
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
  B0_pack.make "b0" ~doc:"The B0 libraries" ~locked:true @@
  [b0_std_lib; b0_memo_lib; b0_file_lib; b0_kit_lib]

let tool_pack =
  B0_pack.make "b0-tools" ~doc:"The low-level B0 tools" ~locked:false @@
  [b0_cache_tool; b0_hash_tool; b0_log_tool; show_url_tool]

let driver_pack =
  B0_pack.make "b0-drivers" ~doc:"The B0 drivers" ~locked:false @@ [b0]

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The b0 programmers"]
    |> B0_meta.(add maintainers)
      ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/b0"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/b0/doc"
    |> B0_meta.(add licenses) ["ISC"; "BSD-2-Clause"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/b0.git"
    |> B0_meta.(add issues) "https://github.com/b0-system/b0/issues"
    |> B0_meta.(add description_tags)
      ["dev"; "org:erratique"; "org:b0-system"; "build"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.add B0_opam.depends [
      "ocaml", {|>= "4.08.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.0.3"|};
      "cmdliner", {|>= "1.1.0"|}; ]
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"The B0 system" ~meta ~locked:true @@
  B0_unit.list ()

(* Actions *)

let strap =
  B0_action.make "strap" ~doc:"Run boot/strap" @@
  B0_action.exec_file' ~/"boot/strap"

let bowl =
  let doc = "Run built b0 in the bowl directory" in
  B0_action.make "bowl" ~units:[b0] ~doc @@
  fun _ env ~args ->
  match B0_env.unit_file_exe env b0 with (* FIXME b0 error struct *)
  | Error e -> Log.err (fun m -> m "%s" e); B0_cli.Exit.some_error
  | Ok exec ->
      let bootstrap_root =
        (* FIXME b0 access to current variant *)
        let b0_dir = B0_env.b0_dir env in
        let variant = "user" in
        B0_dir.build_dir ~b0_dir ~variant
      in
      let cwd = B0_env.in_scope_dir env ~/"bowl" in
      let env = Os.Env.current () |> Log.if_error ~use:Os.Env.empty in
      let env = add_bootstrap_env env bootstrap_root |> Os.Env.to_assignments in
      (* FIXME b0 B0_env.exec_file allow to choose name of args ? *)
      Os.Exit.exec ~cwd ~env exec Cmd.(path exec %% args)

let vendor_htmlit =
  let doc = "Vendor Htmlit and expose it as B0_html" in
  B0_action.make' "vendor-htmlit" ~doc @@
  fun _ env ~args ->
  Log.app (fun m -> m "TODO");
  Os.Exit.exit (Code 0)
