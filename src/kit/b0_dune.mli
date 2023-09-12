(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://dune.build/}[dune]} support. *)

(** {1:action [.dune] action} *)

val action : B0_action.t
(** [action] is the [.dune] action.

    See [b0 -- .dune --help]. *)
