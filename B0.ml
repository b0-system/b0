open B0_kit.V000
open B00_std

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"
let b00_std = B0_ocaml.libname "b0.b00.std"
let b00 = B0_ocaml.libname "b0.b00"
let b00_kit = B0_ocaml.libname "b0.b00.kit"
let b0 = B0_ocaml.libname "b0"
let b0_kit = B0_ocaml.libname "b0.kit"
let b0_b0 = B0_ocaml.libname "b0.b0"

(* B00 libraries *)

let b00_std_lib =
  let srcs = [`Dir_rec "src-b00/std"; `X "src-b00/std/b0_b00_std_top_init.ml"]
  in
  let requires = [unix] in
  B0_ocaml.lib b00_std ~doc:"B00 Stdlib extensions" ~srcs ~requires

let b00_lib =
  let srcs = [`Dir "src-b00"] in
  let requires = [unix; b00_std] in
  B0_ocaml.lib b00 ~doc:"B00 build API" ~srcs ~requires

let b00_kit_lib =
  let srcs = [`Dir "src-b00/kit"] in
  let requires = [unix; cmdliner; b00_std; b00] in
  B0_ocaml.lib b00_kit ~doc:"B00 toolkit API" ~srcs ~requires

(* B0 libraries *)

let b0_lib =
  let srcs = [`Dir "src"] in
  let requires = [unix; cmdliner; b00_std; b00; b00_kit] in
  B0_ocaml.lib b0 ~doc:"B0 description API" ~srcs ~requires

let b0_kit_lib =
  let srcs = [`Dir "src/kit"] in
  let requires = [unix; cmdliner; b00_std; b00; b00_kit; b0] in
  B0_ocaml.lib b0_kit ~doc:"B0 toolkit API" ~srcs ~requires

(* B0 tool *)

let bootstrap_env boot_root =
  let env = Os.Env.current () |> Log.if_error ~use:Os.Env.empty in
  env
  |> Os.Env.add "B0_BOOTSTRAP" (Fpath.to_string boot_root)
  |> Os.Env.add "B0_DRIVER_BOOTSTRAP" (Fpath.to_string boot_root)
  |> Os.Env.to_assignments

let b0_b0_lib =
  let srcs = [`Dir "tool-b0"; `X "tool-b0/b0_main_run.ml"] in
  let requires = [unix; cmdliner; b00_std; b00; b00_kit; b0; b0_kit] in
  B0_ocaml.lib b0_b0 ~doc:"b0 tool driver library" ~srcs ~requires

let b0_tool =
  let srcs = [`File "tool-b0/b0_main_run.ml"] in
  let requires = [b0; b0_b0] in
  let meta =
    let env b u =
      (* FIXME need to access the root of the build. *)
      Fut.return (bootstrap_env Fpath.(B0_build.build_dir b u / ".."))
    in
    B0_meta.empty
    |> B0_meta.add B0_unit.Action.exec_env env
  in
  B0_ocaml.exe "b0" ~name:"b0-exe" ~doc:"The b0 tool" ~srcs ~requires ~meta

(* Low-level B00 tools units *)

let b00_tool n ~doc file =
  let requires = [cmdliner; b00_std; b00; b00_kit] in
  B0_ocaml.exe n ~doc ~srcs:[`File ("tools/" ^ file)] ~requires

let b00_cache_tool =
  b00_tool "b00-cache" ~doc:"Operate on b0 caches" "b00_cache.ml"

let b00_hash_tool =
  b00_tool "b00-hash" ~doc:"Hash like b0" "b00_hash.ml"

let b00_log_tool =
  b00_tool "b00-log" ~doc:"Operate on b0 logs" "b00_log.ml"

let show_uri_tool =
  b00_tool "show-uri" ~doc:"Show URIs in web browsers" "show_uri.ml"

(* Packs *)

let b00_pack =
  let us = [b00_std_lib; b00_lib; b00_kit_lib] in
  B0_pack.v "b00" ~doc:"The B00 subsystem" ~locked:true us

let tool_pack =
  let us = [b00_cache_tool; b00_hash_tool; b00_log_tool; show_uri_tool] in
  B0_pack.v "b00-tools" ~doc:"The low-level B00 tools" us

let driver_pack =
  let us = [b0_tool] in
  B0_pack.v "b0-drivers" ~doc:"The B0 drivers" us

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The B0 programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/b0"
    |> add online_doc "https://erratique.ch/software/b0/doc"
    |> add licenses ["ISC"; "BSD2"]
    |> add repo "git+https://erratique.ch/repos/b0.git"
    |> add issues "https://github.com/b0-system/b0/issues"
    |> add description_tags ["dev"; "org:erratique"; "org:b0-system"; "build"]
    |> tag B0_opam.tag
  in
  let us = B0_unit.list () in
  B0_pack.v "default" ~doc:"The B0 system" ~meta ~locked:true us

(* Cmdlets *)

let b0wl =
  let open Result.Syntax in
  let cmd _clet ~argv =
    Log.if_error ~use:B00_cli.Exit.some_error @@
    let* cwd = Os.Dir.cwd () in
    (* FIXME assumes invoked at the root. Need to sort out cmdlet cwd and
       get current variant. *)
    let bootstrap_root =  Fpath.(cwd // v "_b0/b/user") in
    let b0_exec = Fpath.(bootstrap_root / "b0-exe" / "b0") in
    let env = bootstrap_env bootstrap_root in
    let b0wl = Fpath.(cwd // Fpath.v "b0wl") in
    let cmd = Cmd.(arg "b0" %% args (List.tl argv)) in
    Ok (Os.Exit.exec ~env ~cwd:b0wl b0_exec cmd)
  in
  B0_cmdlet.v ~doc:"Run built b0 in the b0wl directory" "b0wl" (`Cmd cmd)

let strap =
  let open Result.Syntax in
  let cmd clet ~argv =
    Log.if_error ~use:B00_cli.Exit.some_error @@
    let* cwd = Os.Dir.cwd () in
    (* FIXME assumes invoked at the root. Need to sort out cmdlet cwd
       let root_dir = B0_cmdlet.get_meta B0_meta.scope_dir clet in *)
    let strap = Fpath.(cwd / "b00t" / "strap") in
    Ok (Os.Exit.exec strap Cmd.(arg "strap" %% args (List.tl argv)))
  in
  B0_cmdlet.v ~doc:"Run b00t/strap" "strap" (`Cmd cmd)
