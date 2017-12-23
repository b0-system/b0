(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Builds

    See {!B0.Build}. *)

open B0_result

(* FIXME make that abstract *)

type stats =
  { mutable cpu_dur : B0_time.cpu;
    mutable cmd_stamp_count : int; (* uncached *)
    mutable cmd_stamp_dur : B0_time.span;
    mutable file_stamp_count : int; (* uncached *)
    mutable file_stamp_dur : B0_time.span;
    mutable total_dur : B0_time.span; }

type written_file = (* Information about a written file in the build. *)
  { w_age : int; (* Age in which the path was last linked/written. *)
    w_file_stamp : B0_stamp.t; (* File stamp *)
    w_cmd_write_stamp : B0_hash.t; (* Cmd stamp (key in the cache). *) }

type source_file = (* Information about a source (non-written) file. *)
  B0_stamp.t

type outcome =
  { o_age : int;
    o_stats : stats;
    o_prev_stats : stats;
    o_written_files : written_file B0_fpath.map;
    o_source_files : source_file B0_fpath.map;
    o_fpath_meta : (string * string) list B0_fpath.map;
    o_conf : (string * string) list;
    o_ops : B0_op.t list;
    o_units : B0_unit.marshalable list; }

type ctrl
val ctrl : ?tty_cap:B0_tty.cap -> ?max_spawn:int -> unit -> ctrl

type cache = B0_cache.t

type t
type run
type build_unit = B0_unit.t * (t -> unit)

val create :
  ?prev_outcome:outcome -> cache -> ctrl -> B0_env.t -> B0_conf.t ->
  B0_fpath_meta.Meta_map.t -> dir:B0_fpath.t -> universe:build_unit list ->
  build_unit list -> t result

val start : t -> unit
val finish : t -> unit result

val build_dir : t -> B0_fpath.t
val build_file : t -> string -> B0_fpath.t
val unit_build_dir : t -> B0_unit.t -> B0_fpath.t
val units : t -> B0_unit.t list
val src_dir : t -> B0_fpath.t
val dir : t -> B0_fpath.t
val trash : t -> B0_fpath.t
val unit : t -> B0_unit.t
val outcome : t -> outcome
val stored_conf : t -> B0_conf.t
val conf : t -> 'a B0_conf.key -> 'a

val tool : t -> B0_tool.t -> B0_cmd.t -> run
val conf_tool : t -> B0_tool.t B0_conf.key -> B0_cmd.t -> run

val find_path_meta : t -> B0_fpath.t -> 'a B0_fpath_meta.Meta.key -> 'a option
val add_path_meta :
  ?force:bool -> t -> B0_fpath.t -> 'a B0_fpath_meta.Meta.key -> 'a -> unit

val cache : t -> cache

(* Build operations *)

type 'a fiber = ('a -> unit) -> unit
type stdo = [ `Ui | `File of B0_fpath.t | `Tee of B0_fpath.t ]

val ready : t -> B0_fpath.t -> unit
val src : t -> B0_fpath.t -> B0_fpath.t
val spawn :
  t -> ?reads:B0_fpath.t list -> ?writes:B0_fpath.t list ->
  ?success:int list -> ?env:B0_os.Env.t -> ?cwd:B0_fpath.t ->
  ?stdin:B0_fpath.t -> ?stdout:stdo -> ?stderr:stdo -> run -> unit

val copy_file : ?linenum:int -> t -> B0_fpath.t -> B0_fpath.t -> unit
val read : t -> B0_fpath.t -> string fiber
val write :
  t -> ?reads:B0_fpath.t list -> B0_fpath.t -> (unit -> string) -> unit

val mkdir : t -> B0_fpath.t -> unit fiber
val await_units : t -> B0_unit.t list -> unit fiber

val fail :
  ((('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a) -> unit) -> 'b

val fail_on_error_msg : 'a result -> 'a

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
