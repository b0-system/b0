(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build units.

    A build unit is a named build procedure with metadata associated
    to it. Build units are the smallest unit of build in B0. *)

open B00_std

(** {1:proc Build procedures} *)

type build
(** The type for builds, see {!B0_build}. *)

type proc = build -> unit Fut.t
(** The type for unit build procedures. Note that when the future
    determines the build may not be finished. *)

val proc_nop : proc
(** [proc_nop] does nothing. *)

(** {1:action Unit actions} *)

(** {1:units Units} *)

type action = build -> t -> args:string list -> Os.Exit.t Fut.t
(** The type for unit outcome actions. This associates a default
    action to the built unit. [args] are command line argument passed
    on the command line.

    For example for executables a natural action is to [execv] them
    directly or via their runtime. For non-executable files it can be
    to (re)load them in their corresponding viewer application, etc.

    {b TODO.} This is not a final design, {{!page-todo.unit_action}see
    unit actions}. *)

and t
(** The type for build units. *)

val v : ?doc:string -> ?meta:B0_meta.t -> ?action:action -> string -> proc -> t
(** [v n proc ~doc ~meta ~action] is a build unit named [n] with build
    procedure [proc], synopsis [doc] and metada [meta]. *)

val proc : t -> proc
(** [proc u] are the unit's build procedure. *)

val action : t -> action option
(** [action] is the unit's action. *)

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t

(**/**)
module Build : sig
  type build_unit = t
  type t = build
  val memo : t -> B00.Memo.t
  val must_build : t -> Set.t
  val may_build : t -> Set.t
  val require : t -> build_unit -> unit
  val current : t -> build_unit
  val current_meta : t -> B0_meta.t
  val current_root_dir : t -> Fpath.t
  val current_build_dir : t -> Fpath.t
  val shared_build_dir : t -> Fpath.t
  val root_dir : t -> build_unit -> Fpath.t
  val build_dir : t -> build_unit -> Fpath.t
  val create :
    root_dir:Fpath.t -> b0_dir:Fpath.t -> variant:string -> B00.Memo.t ->
    may_build:Set.t -> must_build:Set.t -> t

  val store : t -> B00.Store.t
  val get : t -> 'a B00.Store.key -> 'a Fut.t
  val self : t B00.Store.key
  val run : t -> (unit, unit) result
end
(**/**)

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
