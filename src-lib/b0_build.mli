(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Builds

    See {!B0.Build}. *)

open B0_result

type file_kind = Root | Built
type file_info = { f_kind : file_kind; }

type ctrl
val ctrl : ?tty_cap:B0_tty.cap -> ?max_spawn:int -> unit -> ctrl

type t
type run
type build_unit = B0_unit.t * (t -> unit)

val create :
  B0_cache.t -> ctrl -> B0_env.t -> B0_conf.t -> B0_meta.Fpath.Map.t ->
  dir:B0_fpath.t -> universe:build_unit list -> build_unit list -> t result

val start : t -> unit
val finish : t -> unit result

val env : t -> B0_env.t
val build_dir : t -> B0_fpath.t
val build_file : t -> string -> B0_fpath.t
val unit_build_dir : t -> B0_unit.t -> B0_fpath.t
val units : t -> B0_unit.t list
val src_dir : t -> B0_fpath.t
val dir : t -> B0_fpath.t
val trash : t -> B0_fpath.t
val unit : t -> B0_unit.t
val stored_conf : t -> B0_conf.t
val conf : t -> 'a B0_conf.key -> 'a

val cache : t -> B0_cache.t
val cpu_dur : t -> B0_time.cpu
val total_dur : t -> B0_time.span
val finished : t -> bool
val conf_used : t -> B0_conf.t
val file_info : t -> file_info B0_fpath.Map.t
val fpath_meta : t -> B0_meta.Fpath.Map.t
val ops : t -> B0_op.t list

val tool : t -> B0_tool.t -> B0_cmd.t -> run
val conf_tool : t -> B0_tool.t B0_conf.key -> B0_cmd.t -> run

val find_path_meta : t -> B0_fpath.t -> 'a B0_meta.Fpath.key -> 'a option
val add_path_meta :
  ?force:bool -> t -> B0_fpath.t -> 'a B0_meta.Fpath.key -> 'a -> unit

(* Build operations *)

type 'a fiber = ('a -> unit) -> unit
type stdo = [ `Ui | `File of B0_fpath.t | `Tee of B0_fpath.t ]

val ready : t -> B0_fpath.t -> unit
val src : t -> B0_fpath.t -> B0_fpath.t
val spawn :
  t -> ?reads:B0_fpath.t list -> ?writes:B0_fpath.t list ->
  ?exits:int list -> ?env:B0_os.Env.t -> ?cwd:B0_fpath.t ->
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
   Copyright (c) 2017 The b0 programmers

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
