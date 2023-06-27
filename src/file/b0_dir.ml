(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let build_dir ~b0_dir ~variant = Fpath.(b0_dir / "b" / variant)
let shared_build_dir ~build_dir = Fpath.(build_dir / "_shared")
let store_dir ~build_dir = Fpath.(build_dir / "_store")
let unit_build_dir ~build_dir ~name = Fpath.(build_dir / name)
let scratch_dir ~b0_dir = Fpath.(b0_dir / "_scratch")
