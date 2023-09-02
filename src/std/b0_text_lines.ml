(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let file_error ?(file = Fpath.dash) e = Fmt.error "%a:%s" Fpath.pp file e
