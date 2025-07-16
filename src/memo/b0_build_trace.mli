(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Trace build operations.

    Converts build operations to trace formats. *)

(** {1:trace Tracing} *)

(** JSON Trace Event Format.

    Outputs build operations in JSON
    {{:https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview}
    Trace Event Format}. Can be read back in Google Chrome
    in [chrome://tracing/]. What about {{:https://perf-html.io/}perf.html} ? *)
module Trace_event : sig

  (** {1:trace_event Trace Events} *)

  val of_ops : B0_zero.Op.t list -> B0_json.Jsong.t
end

(** JSON compilation database.

    Output build operation spawns in
    {{:http://clang.llvm.org/docs/JSONCompilationDatabase.html} JSON
    compilation database} format (see also
    {{:https://sarcasm.github.io/notes/dev/compilation-database.html}here}).

    The format is a bit underpowered (see also the discusion in the
    merlin issue tracker
    {{:https://github.com/ocaml/merlin/issues/737}here}) and doesn't
    exactly match b0's build model here are a few notes on the
    treatment:
    {ol
    {- The [file] field. b0's operations do not identify a "main source"
       processed by a step. For now we use the first element of
       {!B0_zero.Op.reads}
       TODO maybe we should spell out this convention in {!B0_memo}.}
    {- The [output] field. b0's operations support multiple writes.
       If multiple files are written we repeat the command for
       each write. We add an numbered [id] field to indicate that this is
       the same command.}
    {- No specification of the environment, we add it under an [env]
       key as an array of strings.}
    {- We never generate [command], we only use [arguments].}} *)
module Compilation_database : sig

  (** {1:compilation_database JSON compilation database} *)

  val of_ops : B0_zero.Op.t list -> B0_json.Jsong.t
end
