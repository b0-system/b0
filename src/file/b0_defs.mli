(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Type bootstrap for {!B0_build.t}, {!B0_env.t} and {!B0_unit.t}. *)

(**/**)
open B0_std

type b0_unit_set
type b0_unit

module Build_def : sig
  type t = { u : build_ctx; b : build_state }
  and build_ctx = { current : b0_unit option; m : B0_memo.t; }
  and build_state =
    { root_dir : Fpath.t;
      b0_dir : Fpath.t;
      build_dir : Fpath.t;
      shared_dir : Fpath.t;
      store : B0_store.t;
      must_build : b0_unit_set;
      may_build : b0_unit_set;
      mutable requested : b0_unit String.Map.t;
      mutable waiting : b0_unit Random_queue.t; }
end


type b0_build = Build_def.t
type build_proc = b0_build -> unit Fut.t


module Unit : B0_def.S
  with type t = b0_unit
   and type Set.t = b0_unit_set

val unit_build_proc : b0_unit -> build_proc


type b0_env =
  { b0_dir : Fpath.t;
    build : b0_build;
    built_tools : b0_unit String.Map.t Lazy.t;
    cwd : Fpath.t;
    root_dir : Fpath.t;
    scope_dir : Fpath.t;
    build_env : Os.Env.t;
    driver_env : Os.Env.t; }


val exe_file : Fpath.t Fut.t B0_meta.key
val tool_name : string B0_meta.key
val tool_name_map : Unit.Set.t -> Unit.t B0_std.String.Map.t
val tool_name_index : Unit.t list B0_std.String.Map.t ref
val tool_is_user_accessible : Unit.t -> bool
val unit_is_public : Unit.t -> bool
val unit_make : ?doc:string -> ?meta:B0_meta.t -> string -> build_proc -> Unit.t
(**/**)
