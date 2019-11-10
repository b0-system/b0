(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Serialize and format {!B000} values. *)

open B0_std
open B000

(** {!B000.Op} converters *)
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

  val bincode : Op.t Bincode.t
  (** [bincode] binary codes an operation. *)

  (** {1:errors Aggregate errors} *)

  val pp_aggregate_error :
    ?sep:unit Fmt.t -> ?read_howto:Fpath.t Fmt.t ->
    ?write_howto:Fpath.t Fmt.t -> unit ->  B000.Op.aggregate_error Fmt.t
    (** [pp_aggregate_error ~read_howto ~write_howto] formats a memo
        error followed by [sep] iff somethings is printed (defaults
        to {!Fmt.flush_nl}). The errors are formatted as follows:
        {ul
        {- {!B000.Op.Failures} formats {!Fmt.nop}.}
        {- {!B000.Op.Never_became_ready} formats each file
         prefixing it with [read_howto].}
        {- {!B000.Op.Cycle}, formats the operations of the cycle.
         by writing the files that form the cycle and prefixing
         them with {!write_howto}.}} *)
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
