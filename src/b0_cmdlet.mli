(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Cmdlets.

    Cmdlets are used to define custom software life-cycle procedures.
    Examples range from invoking linters on your sources to perform
    post build checks or actions.

    {b TODO.}
    {ul
    {- Definition root/scopes retrieval and cwd}
    {- Visibility control.}} *)

open B00_std

(** {1:cmdlets Cmdlets} *)

type t
(** The type for cmdlets. *)

type cmd = [ `Cmd of t -> argv:string list -> Os.Exit.t ]
(** The type for cmdlet commands. [argv] has the arguments for the
    cmdlet and the name of the cmdlet is in [argv.(0)], it can be given
    as is to your command line parsing technology, see for example
    {!Cli}. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> cmd -> t
(** [v n cmd ~doc ~meta] is a cmdlet named [n] implemented by [cmd]
    with doc string [doc] and metadata [meta]. *)

val cmd : t -> cmd
(** [cmd c] is the command of the cmdlet. *)

(** {1:cli Command line interaction} *)

(** Command line interaction. *)
module Cli : sig
  open Cmdliner

  (** Use {!B00_cli.Exit} as a basis for exit codes. See {!B00_cli} and
      {!B0_cli} for arg definitions.

      {b FIXME}. This looks daunting but it's not make simple examples. *)

  val info :
    ?man_xrefs:Manpage.xref list -> ?man:Manpage.block list ->
    ?envs:Term.env_info list -> ?exits:Term.exit_info list ->
    ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
    t -> Term.info
  (** [info c] derives a cmdliner term info for the cmdlet [c]. In
      particular it uses {!B0_cmdlet.name} for the name and
      {!B0_cmdlet.doc c} for [doc]. [exits] defaults to
      {!B00_cli.Exit.infos}.*)

  val run :
    ?info:Cmdliner.Term.info -> t -> argv:string list -> Os.Exit.t Term.t ->
    Os.Exit.t
  (** [run cmdlet ~argv t ~info] is [B00_cli.Exit.of_eval_result @@ Term.eval
      ~argv (t, info)]. [info] defaults to [info c]. *)

  val run_cmds :
    t -> argv:string list -> (Os.Exit.t Term.t * Term.info) ->
    (Os.Exit.t Term.t * Term.info) list -> Os.Exit.t
  (** [run_choice cmdlet ~argv main cmds] is [B00_cli.Exit.of_eval_result @@
      Term.eval_choice ~argv main cmds] *)
end

(** {1:b0_def B0 definition API} *)

include B0_def.S with type t := t

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
