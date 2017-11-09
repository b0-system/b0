open B0

(* B0_opam: Variant schemes *)

let s0 = B0_opam.variant_scheme "4.03.0"
let s1 = B0_opam.variant_scheme ~build_switch:"4.03.0" "4.06.0"
