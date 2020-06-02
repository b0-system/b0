(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** [b0] driver common definitions *)

open B00_std

val driver : B0_driver.t
(** [driver] is the driver definition. *)

(** {!B0_def} generic support.

    Generic implementation of a few standard commands we need for
    B0 defintions. FIXME this could likely be moved to a B0_driver_ui in
    B0 care. *)
module Def : sig

  val list :
    (module B0_def.S) -> B0_driver.Conf.t -> B00_ui.Cli.out_details ->
    string list -> B0_driver.Exit.t
  (** [list (module Def) c details ns] lists definition [Def] named
      [ns] with details [details]. If [ns] is empty all definitions
      are listed. *)

  val edit :
    (module B0_def.S) -> B0_driver.Conf.t -> string list ->
    B0_driver.Exit.t
  (** [edit (module Def) c ns] edits the B0 files which define [Def]s
      named [ns]. If [ns] is empty all the B0 files that have
      definitions of kind [Def] are edited. *)

  val get_meta_key :
    (module B0_def.S) -> B0_driver.Conf.t -> B00_ui.Cli.out_details ->
    string -> string list -> B0_driver.Exit.t
  (** [get (module Def) k ns] gets key [k] in the metadata of
      definitions named [ns] with details [details]. If [ns] is empty
      all definitions are listed. *)
end

module Cli : sig
  val man_see_manual : Cmdliner.Manpage.block
end

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
