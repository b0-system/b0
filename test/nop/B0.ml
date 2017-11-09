
(* Nop unit *)

open B0

let nop =
  Unit.create "nop" (fun b -> ()) ~doc:"No op unit"
