(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://github.com/commonmark/cmark}[cmark]} support. *)

open B0_std

(** {1:cmark Cmark} *)

val tool : B0_memo.Tool.t
(** [tool] is the [cmark] tool. *)

val cmd :
  ?validate_utf_8:bool -> ?format:string -> B0_memo.t -> opts:Cmd.t ->
  mds:Fpath.t list -> o:Fpath.t -> unit
(** [cmd m ~format ~validate_utf_8 ~args ~mds ~o] writes to file [o] the
    result of processing the CommonMark files [mds].
    {ul
    {- [format] is the output format. It defaults to ["html"] which
       outputs a fragment not a full page page (this the [--to]
       option).}
    {- [validate_utf_8] is the [--validate-utf8] option it defaults
       to [true].}
    {- [args] are additional command line arguments you may want to
       pass to [cmark].}} *)

(** {1:conv Convenience} *)

val to_html :
  ?generator:string -> ?lang:string -> ?scripts:string list ->
  ?styles:string list -> ?title:string -> B0_memo.t -> opts:Cmd.t ->
  mds:Fpath.t list -> o_frag:Fpath.t -> o:Fpath.t -> unit
(** [to_html m ~opts ~mds ~o_frag o] compiles the concatenation of
    [mds] to an HTML fragment [o_frag] and then to an HTML page [o] by
    invoking {!cmd} with [opts] and {!B0_web_page.write}; for
    the documentation of optional arguments see the later. *)
