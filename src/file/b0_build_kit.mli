(*---------------------------------------------------------------------------
   Copyright (c) 2025 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Build tools. *)

val vcs_describe_scope : B0_build.t -> string option
(** [vcs_describe_scope] uses {!B0_vcs_repo.describe} to get
    a version string for the scope directory of [b].

    This is [None] if no VCS is found in the scope in directory.
    Otherwise any error makes the build memo fail. *)
