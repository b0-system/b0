(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Cmdliner cmdlet cli parsing *)

open Cmdliner

val info :
  ?man_xrefs:Manpage.xref list -> ?man:Manpage.block list ->
  ?envs:Term.env_info list -> ?exits:Term.exit_info list ->
  ?sdocs:string -> ?docs:string -> ?doc:string -> ?version:string ->
  B0_cmdlet.t -> Cmdliner.Term.info
(** [info c] derives a cmdliner term info for the cmdlet [c]. In
    particular it uses {!B0_cmdlet.name} for the name and
    {!B0cCmdlet.doc c} for [doc]. *)

val exit : B0_cmdlet.Exit.t Cmdliner.Term.result -> B0_cmdlet.Exit.t
(** [exit r] turns a cmdliner evaluation result [r] into a cmdlet exit
    code. *)

val run :
  B0_cmdlet.t -> argv:string array -> B0_cmdlet.Exit.t Term.t ->
  B0_cmdlet.Exit.t
(** [run cmdlet ~argv t] is [exit @@ Term.eval ~argv (t, info c)]. *)

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
