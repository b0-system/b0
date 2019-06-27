(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Serialize and format {!B00} values. *)

open B0_std
open B00

module File_cache : sig
  val pp_feedback : File_cache.feedback Fmt.t
  (** [pp_feedback] formats file cache feedback. *)
end

module Guard : sig
  val pp_feedback : Guard.feedback Fmt.t
  (** [pp_feedback] formats guard feedback. *)
end

module Exec : sig
  val pp_feedback : Exec.feedback Fmt.t
  (** [pp_feedback] formats executor feedback. *)
end

(** {!B00.Op} converters *)
module Op : sig

  (** {1:op_kind Operations kinds} *)

  (** Tool spawns. *)
  module Spawn : sig

    (** {1:fmt Formatters} *)

    val pp_success_exits : int list Fmt.t
    (** [pp_success_exits] formats the success exits. *)

    val pp_cmd : Op.Spawn.t Fmt.t
    (** [pp_cmd] formats the command issued by the spawn, including
        redirections. *)

    val pp_stdo_ui : truncate:bool -> Op.Spawn.t Fmt.t
    (** [pp_stdo_ui] formats the standard output ui of the spawn. If
        [truncate] is [true] truncates long outputs. *)

    val pp_result : (Os.Cmd.status, string) result Fmt.t
    (** [pp_result] formats the command status of the spawn. *)

    val pp : Op.Spawn.t Fmt.t
    (** [pp] formats a spawn. *)
  end

  (** File reads. *)
  module Read : sig

    (** {1:formatting Formatters} *)

    val pp_result : (string, string) result Fmt.t
    (** [pp_result] formats the read result. *)

    val pp : Op.Read.t Fmt.t
    (** [pp] formats a read. *)
  end

  (** File writes. *)
  module Write : sig

    (** {1:fmt Formatters} *)

    val pp_result : (unit, string) result Fmt.t
    (** [pp_result] formats a write result. *)

    val pp : Op.Write.t Fmt.t
    (** [pp] formats a write. *)
  end

  (** File copy *)
  module Copy : sig

    (** {1:fmt Formatters} *)

    val pp_result : (unit, string) result Fmt.t
    (** [pp_result] formats a write result. *)

    val pp : Op.Copy.t Fmt.t
    (** [pp] formats a write. *)
  end

  (** Directory creation. *)
  module Mkdir : sig

    (** {1:fmt Formatters} *)

    val pp_result : (bool, string) result Fmt.t
    (** [pp_result] formats directory creation results. *)

    val pp : Op.Mkdir.t Fmt.t
    (** [pp] formats directory creations. *)
  end

  module Wait_files : sig
  end

  (** {1:fmt Formatters} *)

  val pp_status : Op.status Fmt.t
  (** [pp_status] formats build operation statuses. *)

  val pp : Op.t Fmt.t
  (** [pp] formats a build operation. *)

  val pp_short : Op.t Fmt.t
  (** [pp_short] formats a build operation on a single line. *)

  val pp_did_not_write : (Op.t * Fpath.t list) Fmt.t
  (** [pp_did_not_write] formats a build operation and the files
      it failed to write. *)

  val pp_spawn_status_fail : Op.t Fmt.t
  (** [pp_spawn_status_fail] formats a spawn operation failure due to
       exit result. *)

  (** {1:bin_serial Binary serialization} *)

  val to_string : Op.t list -> string
  (** [to_string ops] is a binary encoding of [ops]. *)

  val of_string : ?file:Fpath.t -> string -> (Op.t list, string) result
  (** [of_string ops] is a binary decoding of a {!to_string} encoding.
      [file] is a filename to report errors (defaults to {!Os.File.dash}). *)
end

module Memo : sig
  val pp_feedback : Memo.feedback Fmt.t
  (** [pp_feedback] formats file cache feedback. *)

  val stdo_feedback :
    [B00.Memo.feedback | B00.File_cache.feedback | B00.Exec.feedback] Fmt.t
end

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers

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
