
open B0_std

(* Units *)

let b00 =
  B0_unit.v "b00" ~doc:"The B00 build library" B0_unit.nop

let care =
  B0_unit.v "care" ~doc:"B0 & B00 convenience APIs" B0_unit.nop

let defs =
  B0_unit.v "defs" ~doc:"B0 description API" B0_unit.nop

let driver =
  B0_unit.v "driver" ~doc:"B0 driver API" B0_unit.nop

let b0 =
  B0_unit.v "b0" ~doc:"The b0 tool" B0_unit.nop

let tools =
  B0_unit.v "tools" ~doc:"B0 & B00 convenience tools" B0_unit.nop

(* Packs *)

let b00_pack =
  let units = [std; b00] in
  B0_pack.v "b00" ~doc:"The B00 subsystem" ~locked:true units

let default =
  let units = [std; b00; care; defs; driver; tools] in
  let meta =
    let open B0_meta in
    add authors ["The B0 programmers"] @@
    add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"] @@
    add homepage "https://erratique.ch/software/b0" @@
    add online_doc "https://erratique.ch/software/b0/doc" @@
    add licenses ["ISC"; "BSD2"] @@
    add repo "git+https://erratique.ch/repos/b0.git" @@
    add issues "https://github.com/b0-system/b0/issues" @@
    add doc_tags ["dev"; "org:erratique"; "org:b0-system"; "build"] @@
    empty
  in
  B0_pack.v "default" ~doc:"The B0 system" ~meta ~locked:true units
