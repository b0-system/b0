open B0_kit.V000

(* OCaml library names *)

let unix = B0_ocaml.lib "unix"
let cmdliner = B0_ocaml.lib "cmdliner"
let b00_std = B0_ocaml.lib "b0.b00.std"
let b00 = B0_ocaml.lib "b0.b00"
let b00_kit = B0_ocaml.lib "b0.b00.kit"
let b0 = B0_ocaml.lib "b0"
let b0_kit = B0_ocaml.lib "b0.kit"
let b0_driver = B0_ocaml.lib "b0.driver"
let b0_driver_b0 = B0_ocaml.lib "b0.driver.b0"

(* B00 libraries *)

let b00_std_lib =
  let requires = [unix] in
  let srcs = [`D_rec "src-b00/std"; `X "src-b00/std/b0_b00_std_top_init.ml" ] in
  B0_ocaml.Unit.lib b00_std ~doc:"B00 Stdlib extensions" ~requires ~srcs

let b00_lib =
  let requires = [unix; b00_std] in
  let srcs = [`D "src-b00"] in
  B0_ocaml.Unit.lib b00 ~doc:"B00 build abstraction" ~requires ~srcs

let b00_kit_lib =
  let requires = [unix; cmdliner; b00_std; b00] in
  let srcs = [`D "src-b00/kit"] in
  B0_ocaml.Unit.lib b00_kit ~doc:"B00 toolkit" ~requires ~srcs

(* B0 libraries *)

let b0_lib =
  let requires = [unix; b00_std; b00; b00_kit] in
  let srcs = [`D "src"] in
  B0_ocaml.Unit.lib b0 ~doc:"B0 description API" ~requires ~srcs

let b0_kit_lib =
  let requires = [unix; cmdliner; b00_std; b00; b00_kit; b0] in
  let srcs = [`D "src/kit"] in
  B0_ocaml.Unit.lib b0_kit ~doc:"B0 toolkit" ~requires ~srcs

let b0_driver_lib =
  let requires = [unix; cmdliner; b00_std; b00; b00_kit; b0] in
  let srcs = [`D "src/driver"] in
  B0_ocaml.Unit.lib b0_driver ~doc:"B0 driver API" ~requires ~srcs

(* B0 driver and tool *)

let b0_driver_b0_lib =
  let requires =
    [unix; cmdliner; b00_std; b00; b00_kit; b0; b0_kit; b0_driver]
  in
  let srcs = [`D "tool-b0"; `X "tool-b0/b0_main_run.ml"] in
  B0_ocaml.Unit.lib b0_driver_b0 ~doc:"b0 tool driver library" ~requires ~srcs

let b0_tool =
  let requires = [b0_driver; b0_driver_b0] in
  let srcs = [`F "tool-b0/b0_main_run.ml"] in
  B0_ocaml.Unit.exe "b0" ~name:"b0-tool" ~doc:"b0 tool" ~requires ~srcs

(* Low-level B00 tools units *)

let b00_tool =
  let requires = [cmdliner; b00_std; b00; b00_kit; b0_driver] in
  fun n ~doc f -> B0_ocaml.Unit.exe n ~doc ~requires ~srcs:[`F ("tools/" ^ f)]

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
  let us = [ b00_cache_tool; b00_hash_tool; b00_log_tool; show_uri_tool ] in
  B0_pack.v "b00-tools" ~doc:"The low-level B00 tools" us

let driver_pack =
  let us = [ b0_tool ] in
  B0_pack.v "b0-drivers" ~doc:"The B0 drivers" us

let default =
  let us = B0_unit.list () in
  let meta = B0_meta.v @@ B0_meta.[
      authors, ["The B0 programmers"];
      maintainers, ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"];
      homepage, "https://erratique.ch/software/b0";
      online_doc, "https://erratique.ch/software/b0/doc";
      licenses, ["ISC"; "BSD2"];
      repo, "git+https://erratique.ch/repos/b0.git";
      issues, "https://github.com/b0-system/b0/issues"]
  in
  B0_pack.v "b0" ~doc:"The B0 system" ~meta ~locked:true us
