(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module V000 = struct
  module Type = B0_std.Type
  module Fmt = B0_std.Fmt
  module Result = B0_std.Result
  module Char = B0_std.Char
  module String = B0_std.String
  module List = B0_std.List
  module Fpath = B0_std.Fpath
  module Hash = B0_std.Hash
  module Mtime = B0_std.Mtime
  module Cmd = B0_std.Cmd
  module Fut = B0_std.Fut
  module Os = B0_std.Os
  module Log = B0_std.Log
  module Bval = B0_std.Bval
  let ( ~/ ) = Fpath.v
  let ( ~~ ) = B0_meta.add
end
