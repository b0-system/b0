(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Serialize and format {!B000} and {!B00} values. *)

open B0_std
open B00

module File_cache : sig
  val pp_feedback : B000.File_cache.feedback Fmt.t
  (** [pp_feedback] formats file cache feedback. *)
end

module Guard : sig
  val pp_feedback : B000.Guard.feedback Fmt.t
  (** [pp_feedback] formats guard feedback. *)
end

module Exec : sig
  val pp_feedback : B000.Exec.feedback Fmt.t
  (** [pp_feedback] formats executor feedback. *)
end

(** {!B00.Op} converters *)
module Op : sig

  (** {1:fmt Formatters} *)

  val pp_spawn_stdo_ui : B000.Op.Spawn.t Fmt.t
  (** [pp_spawn_stdo_ui] formats the standard output ui of the spawn. *)

  val pp_spawn_result : (Os.Cmd.status, string) result Fmt.t
  (** [pp_spawn_result] formats the command status of the spawn. *)

  val pp_file_read : Fpath.t Fmt.t
  (** [pp_file_read] formats a read file. *)

  val pp_file_write : Fpath.t Fmt.t
  (** [pp_file_write] formats a written file. *)

  val pp_hash : Hash.t Fmt.t
  (** [pp_hash] formats an operation hash. *)

  val pp_short : B000.Op.t Fmt.t
  (** [pp_short] formats a build operation on a single line. *)

  val pp_short_with_ui : B000.Op.t Fmt.t
  (** [pp_short_with_stdo_ui] formats like {!pp_short} but also
      a feedback UI if the operation has one. *)

  val pp : B000.Op.t Fmt.t
  (** [pp] formats a build operation. *)

  val pp_failed : op_howto:B000.Op.t Fmt.t -> B000.Op.t Fmt.t
  (** [pp] formats an operation failure. [op_howto] formats how
      to get more information about the failing operartion. *)

  (** {1:bin_serial Binary serialization} *)

  val enc : B000.Op.t Binc.enc
  (** [enc] binary encodes an operation. *)

  val dec : B000.Op.t Binc.dec
  (** [dec] binary decodes an operation. *)
end

module Memo : sig
  (*
  val pp_feedback : Memo.feedback Fmt.t
  (** [pp_feedback] formats file cache feedback. *)
*)

  val pp_leveled_feedback :
    ?sep:unit Fmt.t ->
    ?op_howto:B000.Op.t Fmt.t -> show_op_ui:Log.level -> show_op:Log.level ->
    level:Log.level ->
    [B00.Memo.feedback | B000.File_cache.feedback | B000.Exec.feedback] Fmt.t
  (** [pp_leveled_feedback ~sep ~op_howto ~show_spawn_ui ~show_success ~level
      ppf] formats memo feedback on [ppf] followed by [sep] iff something
      is printed (defaults to {!Fmt.flush_nl}).
      {ul
      {- {!Log.Quiet} formats nothing}
      {- {!Log.Error} and {!Log.Warning} only report build operation failures}
      {- {!Log.Debug} report all operations with all the information.}}
      besides for operations that execute without failure:
      {ul
      {- [show_op_ui] is the level at which any executed operation with a
         feedback UI is logged with {!B00_conv.Op.pp_short_and_ui}}
      {- [show_op] is the level at which any executed operation gets
         logged with {!B00_conv.Op.pp_short_and_ui}}}
      The formatter [op_howto] should format a way to got more information
      about an operation, default to {!nop}. *)

  val pp_never_ready : op_howto:Fpath.t Fmt.t -> Fpath.Set.t Fmt.t
  (** [pp_never_reads ~op_howto] formats a failure indicating
      the given set of files never became ready.

      [op_howto] is prefixed before each file and should be a command
      fragment to get information about which operation needed the file. *)
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
