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

(* b0 libraries *)

let b0_std_lib =
  let srcs = [ `Dir_rec ~/"src/std"; `X ~/"src/std/b0_std_top_init.ml" ] in
  let requires = [unix; cmdliner] in
  B0_ocaml.lib b0_std ~doc:"b0 standard library" ~srcs ~requires

let b0_memo_lib =
  let srcs = [`Dir ~/"src/memo"] in
  let requires = [unix; cmdliner; b0_std] in
  B0_ocaml.lib b0_memo ~doc:"b0 build library" ~srcs ~requires

let b0_file_lib =
  let srcs = [`Dir ~/"src/file"] in
  let requires = [unix; cmdliner; b0_std; b0_memo] in
  B0_ocaml.lib b0_file ~doc:"b0 build file library" ~srcs ~requires

let b0_kit_lib =
  let srcs = [`Dir ~/"src/kit"] in
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file] in
  B0_ocaml.lib b0_kit ~doc:"b0 toolkit library" ~srcs ~requires

(* The b0 tool *)

let bootstrap_env env unit =
  let boot_root = (* TODO b0: need to access the root of the build  *)
    Fpath.to_string (B0_env.in_unit_dir env unit ~/"..")
  in
  B0_env.build_env env
  (* If the build is locked via `b0 lock`, unlock it. *)
  |> Os.Env.remove "B0_FILE"
  |> Os.Env.remove "B0_DIR"
  |> Os.Env.add "B0_BOOTSTRAP" boot_root
  |> Os.Env.add "B0_DRIVER_BOOTSTRAP" boot_root
  |> Result.ok

let b0_tool_lib =
  let srcs = [ `Dir ~/"src/tool"; `X ~/"src/tool/b0_main_run.ml"] in
  let requires = [unix; cmdliner; b0_std; b0_memo; b0_file; b0_kit] in
  B0_ocaml.lib b0_tool ~doc:"b0 tool driver library" ~srcs ~requires

let b0 =
  let srcs = [`File ~/"src/tool/b0_main_run.ml"] in
  let requires = [b0_file; b0_tool] in
  let exec_env = `Fun ("Bootstrap env on the b0 build", bootstrap_env) in
  let meta = B0_meta.empty |> ~~ B0_unit.Action.env exec_env in
  B0_ocaml.exe "b0" ~public:true ~doc:"The b0 tool" ~srcs ~requires ~meta

(* Low-level b0 tools *)

let tool_exe ?(requires = []) n ~doc file =
  let requires = (cmdliner :: b0_std :: requires) in
  let srcs = [`File Fpath.(~/"src/lowtools" / file)] in
  B0_ocaml.exe n ~public:true ~doc ~srcs ~requires

let b0_cache_tool =
  let requires = [b0_memo; b0_file] in
  tool_exe "b0-cache" "b0_cache.ml" ~doc:"Operate on b0 caches" ~requires

let b0_hash_tool =
  let requires = [b0_file] (* for B0_cli *)  in
  tool_exe "b0-hash" "b0_hash.ml" ~doc:"Hash like b0" ~requires

let b0_log_tool =
  let requires = [b0_memo; b0_file] in
  tool_exe "b0-log" "b0_log.ml" ~doc:"Operate on b0 logs" ~requires

let b0_sttyle =
  tool_exe "b0-sttyle" "b0_sttyle.ml" ~doc:"Show ANSI escape styles"

let show_url_tool =
  tool_exe "show-url" "show_url.ml" ~doc:"Show URLs in web browsers"

(* Tests *)

let test_src f = `File Fpath.(~/"test" // f)
let test ?(run = false) ?(requires = []) ?(more_srcs = []) ?doc file =
  let file = Fpath.v file in
  let more_srcs = List.map (fun v -> test_src (Fpath.v v)) more_srcs in
  let srcs = (test_src file) :: more_srcs in
  let requires =
    b0_std :: b0_memo :: b0_file :: b0_kit :: cmdliner :: requires
  in
  let meta =
    B0_meta.empty
    |> B0_meta.tag B0_meta.test
    |> B0_meta.add B0_meta.run run
  in
  B0_ocaml.exe (Fpath.basename ~strip_ext:true file) ~meta ?doc ~srcs ~requires

let test_memo ?requires ?(more_srcs = []) ?doc file =
  let more_srcs = "test_memo_setup.ml" :: more_srcs in
  test ?requires ~more_srcs ?doc file

let test_base =
  let doc = "Basic module tests (B0_std, etc.)" in
  let more_srcs =
    ["test_fmt.ml"; "test_fpath.ml"; "test_cmd.ml"; "test_base64.ml"]
  in
  test "test.ml" ~run:true ~doc ~more_srcs

let test_cp = test "test_cp.ml" ~doc:"Test for Os.Path.copy"
let test_rm = test "test_rm.ml" ~doc:"Test for Os.Path.delete"
let test_findex = test "test_findex.ml" ~doc:"Test for B0_findex"
let test_memo_failure =
  test_memo "test_memo_failures.ml" ~doc:"Tests some failures of B0_memo.Memo."

let test_memo_no_write =
  test_memo "test_memo_no_write.ml"

let test_memo_store =
  test_memo "test_memo_store.ml"

let test_memo_redir =
  test_memo "test_memo_redir.ml" ~doc:"Test memo spawn stdio redirection"

let test_ocaml_cobj_defs =
  test "test_ocaml_cobj_defs.ml" ~doc:"Test B0_ocaml.Cobj.of_string"

let test_b0_file =
  let requires = [b0_memo] in
  test "test_b0_file.ml" ~requires ~doc:"Test B0_file module"

(* Packs *)

let b0_pack =
  B0_pack.make "b0" ~doc:"The b0 libraries" ~locked:true @@
  [b0_std_lib; b0_memo_lib; b0_file_lib; b0_kit_lib]

let tool_pack =
  B0_pack.make "b0-tools" ~doc:"The low-level b0 tools" ~locked:false @@
  [b0_cache_tool; b0_hash_tool; b0_log_tool; show_url_tool]

let driver_pack =
  B0_pack.make "b0-drivers" ~doc:"The b0 drivers" ~locked:false @@ [b0]

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The b0 programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/b0"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/b0/doc"
    |> ~~ B0_meta.licenses ["ISC"; "BSD-2-Clause"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/b0.git"
    |> ~~ B0_meta.issues "https://github.com/b0-system/b0/issues"
    |> ~~ B0_meta.description_tags
      ["dev"; "org:erratique"; "org:b0-system"; "build"]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> ~~ B0_opam.depends [
      "ocaml", {|>= "4.08.0"|};
      "ocamlfind", {|build|};
      "ocamlbuild", {|build|};
      "topkg", {|build & >= "1.0.3"|};
      "cmdliner", {|>= "1.3.0"|}; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.tag B0_release.tag
  in
  B0_pack.make "default" ~doc:"The b0 system" ~meta ~locked:true @@
  B0_unit.list ()

(* Actions *)

let strap =
  B0_unit.of_action' "strap" ~doc:"Run boot/strap" @@
  B0_unit.Action.scope_exec (Cmd.tool "boot/strap")

let bowl =
  let doc = "Run built b0 in the bowl directory" in
  B0_unit.of_action' "bowl" ~units:[b0] ~doc @@ fun env _ ~args ->
  let* b0_exe = B0_env.unit_exe_file_cmd env b0 in
  let cwd = B0_env.in_scope_dir env ~/"bowl" in
  let env = bootstrap_env env b0 |> Result.get_ok in
  let env = Os.Env.to_assignments env in
  Ok (Os.Exit.execv ~cwd ~env Cmd.(b0_exe %% args))

let vendor_htmlit =
  let doc = "Vendor Htmlit and expose it as B0_html" in
  B0_unit.of_action "vendor-htmlit" ~doc @@
  fun _ env ~args ->
  Log.app (fun m -> m "TODO");
  Ok ()
