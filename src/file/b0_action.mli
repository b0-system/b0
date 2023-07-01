(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Actions

    Actions are user defined custom procedures. They can involve
    a build or not.

    They can be used to run build artefacts, test suites,
    script or generic software life-cycle procedures. *)

open B0_std

type t
(** The type for actions. *)

type cmd = t -> B0_build.t -> Cmd.t -> Os.Exit.t
(** The type for action implementations.

    A function that given a context and command line arguments
    eventually specifies a way to exit. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> cmd -> t
(** [v name cmd] is an action named [name] implemented
    with [cmd]. *)

val of_cli_cmd :
  ?meta:B0_meta.t -> (B0_build.t -> Os.Exit.t) Cmdliner.Cmd.t -> t
(** [of_cli_cmd cmd] is an action from the Cmdliner command [cmd]. *)

val cmd : t -> cmd
(** [cmd a] is the command of the action. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t (** @inline *)
