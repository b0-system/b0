(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Cmdlets.

    Cmdlets are used to define custom software life-cycle procedures.
    Examples range from invoking linters on your sources to perform
    post build checks.

    {b TODO.}
    {ul
    {- Definition root/scopes retreival.}
    {- Visibility control.}} *)

open B00_std

(** {1:cmdlets Cmdlets} *)

type t
(** The type for cmdlets. *)

(** The type for cmdlet exits. *)
module Exit : sig

  (* FIXME better exec *)
  type t =
  | Code of int
  | Exec of Fpath.t * Cmd.t

  val ok : t
  (** [ok] is the zero exit code. *)

  val some_error : t
  (** [some_error] indicates an indiscriminate error reported on stdout. *)
end

type cmd = [ `Cmd of t -> argv:string array -> Exit.t ]
(** The type for cmdlet commands. [argv] has the arguments for the
    cmdlet and the name of the cmdlet is in [argv.(0)], it can be given
    as is to your command line parsing technology, see for example
    {!B0_cmdlet_cli}. *)

val v : ?doc:string -> ?meta:B0_meta.t -> string -> cmd -> t
(** [v n cmd ~doc ~meta] is a cmdlet named [n] implemented by [cmd]
    with doc string [doc] and metadata [meta]. *)

val cmd : t -> cmd
(** [cmd c] is the command of the cmdlet. *)

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
