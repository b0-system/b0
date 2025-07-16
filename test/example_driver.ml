(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open B0_std
open Cmdliner

let my_driver conf = Fmt.pr "Running!@."; Os.Exit.ok

let driver =
  let name = "mydriver" and version = "%%VERSION%%" in
  let self = B0_ocaml.libname name in
  let libs = [self (* other needed libraries can be added here *) ] in
  B0_driver.make ~name ~version ~libs

let main () =
  let cmd =
    let doc = "My driver" in
    let exits = B0_driver.Exit.infos in
    let man = [ `S Manpage.s_description; `P "$(cmd) does not much." ] in
    let name = B0_driver.name driver and version = B0_driver.version driver in
    Cmd.make (Cmd.info name ~version ~doc ~exits ~man) @@
    B0_driver.with_b0_file ~driver (Term.const my_driver)
  in
  Cmd.eval_value cmd

let () = B0_driver.set ~driver ~main
