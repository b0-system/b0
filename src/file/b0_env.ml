(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ocamldep invocation in boot/strap which only runs on [.ml] files
   misses that. *)
type bootstrap_dep = B0_build.t

include B0_unit.Env
