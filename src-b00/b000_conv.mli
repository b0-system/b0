(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Serialize and format {!B000} values. *)

open B0_std
open B000

(** {!B000.Op} converters *)
module Op : sig

  (** {1:fmt Formatters} *)

  val pp_spawn_stdo_ui : Op.Spawn.t Fmt.t
  (** [pp_spawn_stdo_ui] formats the standard output ui of the spawn. *)

  val pp_spawn_result : (Os.Cmd.status, string) result Fmt.t
  (** [pp_spawn_result] formats the command status of the spawn. *)

  val pp_file_read : Fpath.t Fmt.t
  (** [pp_file_read] formats a read file. *)

  val pp_file_write : Fpath.t Fmt.t
  (** [pp_file_write] formats a written file. *)

  val pp_hash : Hash.t Fmt.t
  (** [pp_hash] formats an operation hash. *)

  (** {1:ops Operation formatters} *)

  val pp_short : Op.t Fmt.t
  (** [pp_short] formats a build operation on a single line. *)

  val pp_short_with_ui : Op.t Fmt.t
  (** [pp_short_with_stdo_ui] formats like {!pp_short} but also
      a feedback UI if the operation has one. *)

  val pp : Op.t Fmt.t
  (** [pp] formats a build operation with details. *)

  val pp_failed : op_howto:Op.t Fmt.t -> Op.t Fmt.t
  (** [pp] formats an operation failure. [op_howto] formats how
      to get more information about the failing operartion. *)

  (** {1:bin_serial Binary serialization} *)

  val enc : Op.t Binc.enc
  (** [enc] binary encodes an operation. *)

  val dec : Op.t Binc.dec
  (** [dec] binary decodes an operation. *)
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
