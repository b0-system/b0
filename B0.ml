
open B00_std

(* Units *)

let b00_std =
  B0_unit.v "b00-std" ~doc:"B00 stdlib extensions" B0_unit.nop

let b00 =
  B0_unit.v "b00" ~doc:"B00 build abstraction" B0_unit.nop

let b00_kit =
  B0_unit.v "b00-kit" ~doc:"B00 toolkit" B0_unit.nop

let b0 =
  B0_unit.v "b0" ~doc:"B0 description API" B0_unit.nop

let b0_kit =
  B0_unit.v "b0-kit" ~doc:"B0 toolkit" B0_unit.nop

let driver =
  B0_unit.v "driver" ~doc:"B0 driver API" B0_unit.nop

let driver_b0 =
  B0_unit.v "driver-b0" ~doc:"The b0 tool" B0_unit.nop

let tools =
  B0_unit.v "tools" ~doc:"B00 convenience tools" B0_unit.nop

(* Packs *)

let b00_pack =
  let units = [b00_std; b00; b00_kit] in
  B0_pack.v "b00" ~doc:"The B00 subsystem" ~locked:true units

let default =
  let units = [b00_std; b00; b00_kit; b0; b0_kit; driver; driver_b0; tools] in
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
