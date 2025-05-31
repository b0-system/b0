(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Serialize and format {!B0_zero} values. *)

open B0_std
open B0_zero

(** {!B0_zero.Op} converters *)
module Op : sig

  (** {1:to_string Stringifiers} *)

  val status_to_string : Op.status -> string
  (** [status_to_string s] is [s] as a string. *)

  val notify_kind_to_string : Op.Notify.kind -> string
  (** [notify_kind_to_string k] is [k] as a string. *)

  (** {1:fmt Formatters} *)

  val pp_file_read : Fpath.t Fmt.t
  (** [pp_file_read] formats a read file. *)

  val pp_file_write : Fpath.t Fmt.t
  (** [pp_file_write] formats a written file. *)

  val pp_line : Op.t Fmt.t
  (** [pp_line] formats a build operation on a single line. *)

  val pp_line_and_ui : Op.t Fmt.t
  (** [pp_line_and_ui] formats like {!pp_line} but also a potential
      feedback spawn UI and/or failure error messages. *)

  val pp_ui : sep:unit Fmt.t -> op_howto:Op.t Fmt.t -> Op.t Fmt.t
  (** [pp_ui] formats notifications, failed operations and spawn operations
      with a UI. In case something is printed [sep] is added at the end.
      [op_howto] should be command fragment to obtain more information
      about operation failures. *)

  val pp : Op.t Fmt.t
  (** [pp] formats a build operation with full details. *)

  val bincode : Op.t B0_bincode.t
  (** [bincode] binary codes an operation. *)

  (** {1:errors Errors} *)

  val pp_aggregate_error :
    ?sep:unit Fmt.t -> ?read_howto:Fpath.t Fmt.t ->
    ?write_howto:Fpath.t Fmt.t -> unit ->
    B0_zero.Op.aggregate_error Fmt.t
    (** [pp_aggregate_error ~read_howto ~write_howto] formats a memo
        error followed by [sep] iff somethings is printed (defaults
        to {!B0_std.Fmt.flush_nl}). The errors are formatted as follows:
        {ul
        {- {!B0_zero.Op.Failures} formats {!B0_std.Fmt.nop}.}
        {- {!B0_zero.Op.Never_became_ready} formats each file
         prefixing it with [read_howto].}
        {- {!B0_zero.Op.Cycle}, formats the operations of the cycle.
         by writing the files that form the cycle and prefixing
         them with [write_howto].}} *)

  val pp_build_correctness_error :
    pp_op:Op.t Fmt.t -> B0_zero.Op.build_correctness_error Fmt.t
end
