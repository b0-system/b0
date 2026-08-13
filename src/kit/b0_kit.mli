(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** APIs to use in b0 files. *)

(** Version 000. *)
module V000 : sig

  (* This is B0_std, do we gain anything in having module aliases
     here ? *)

  module Type = B0_std.Type
  module Fmt = B0_std.Fmt
  module Result = B0_std.Result
  module Char = B0_std.Char
  module String = B0_std.String
  module List = B0_std.List
  module Filepath = B0_std.Filepath
  module Mtime = B0_std.Mtime
  module Net = B0_std.Net
  module Cmd = B0_std.Cmd
  module Fut = B0_std.Fut
  module Os = B0_std.Os
  module Log = B0_std.Log
  module Bval = B0_std.Bval

  val ( ~/ ) : string -> Filepath.t
  (** [~/path] is [Filepath.v path] *)

  val ( ~~ ) : 'a B0_meta.key -> 'a -> B0_meta.t -> B0_meta.t
  (** [~~ k v m] is {!B0_meta.add}[ k v m].  *)
end
