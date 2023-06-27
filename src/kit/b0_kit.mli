(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** APIs to use in B0 files. *)

(** Version 000. *)
module V000 : sig

  (* This is B0_std, do we gain anything in having module aliases
     here ? *)

  module Type = B0_std.Type
  module Tty = B0_std.Tty
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
  module Random_queue = B0_std.Random_queue
end
