(*---------------------------------------------------------------------------
   Copyright (c) 2023 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Execution environments.

    This is notably what actions get access to.

    {b Note.} At some point we need a build/host distinction here. *)

open B0_std

type t
(** The type for execution environments. *)

val make :
  b0_dir:Fpath.t -> build:B0_build.t -> cwd:Fpath.t -> root_dir:Fpath.t ->
  scope_dir:Fpath.t -> t
(** [make ~cwd ~scope_dir ~root_dir ~action] is an execution context
    with given parameters. See corresponding accessors for semantics. *)

(** {1:dirs Directories}

    See also the {{!rel}relative file resolutions}. *)

val b0_dir : t -> Fpath.t
(** [b0_dir env] is the {{!page-manual.root_dir}b0 directory}. *)

val cwd : t -> Fpath.t
(** [cwd env] is the absolute path to the current working directory. *)

val root_dir : t -> Fpath.t
(** [root_dir env] is the {{!page-manual.root_dir}root directory}. *)

val scope_dir : t -> Fpath.t
(** [scope_dir env] is the absolute path to the directory of the B0 file
    in which the action is defined or the {{!root_dir}root
    directory} if the action is defined the global scope. *)

val scratch_dir : t -> Fpath.t
(** [scratch_dir env] is a shared scratch directory for actions in
    [b0_dir]. The directory must be created it may not exist, it's
    content may be destroyed at any time and actions are in charge of
    inventing a naming scheme to avoid collisions. *)

val unit_dir : t -> B0_unit.t -> Fpath.t
(** [unit_dir env u] is the build directory if [u] in the
    build [build env]. That is {!B0_build.build_dir} [(build env) u]. *)

(** {1:rel Relative file resolution} *)

val in_root_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(root_dir env // p)]. *)

val in_scope_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(scope_dir env // p)]. *)

val in_scratch_dir : t -> Fpath.t -> Fpath.t
(** [in_scope_dir env p] is [Fpath.(scratch_dir env // p)]. *)

val in_unit_dir : t -> B0_unit.t -> Fpath.t -> Fpath.t
(** [in_unit_dir env u p] is [Fpath.(unit_build_dir env // p)]. *)

(** {1:build Build} *)

val build : t -> B0_build.t
(** [build env] is the current build (if any). *)

(** {1:tool Tool lookup} *)

val get_tool : ?no_build:bool -> t -> Fpath.t -> (Fpath.t, string) result
val get_cmd : ?no_build:bool -> t -> Cmd.t -> (Cmd.t, string) result
val unit_file_exe : t -> B0_unit.t -> (Fpath.t, string) result
val unit_cmd : t -> B0_unit.t -> (Cmd.t, string) result
