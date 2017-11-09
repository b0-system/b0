(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Driver compilation action *)

open B0

(** {1:act Driver compilation action} *)

type action
(** The type for compilation actions. *)

val action :
  B0d_driver_cli.t -> B0d_root_descr.t -> driver_dir:Fpath.t ->
  driver_name:string -> driver_libs:string list -> bin:Fpath.t ->
  action result
(** [action cli descr ~driver_dir ~driver_name ~driver_libs bin]
    is a compilation action for description [descr] to generate the
    binary [bin]. *)

val action_stamp : action -> string result
(** [action_stamp a] is a validity stamp for the driver compilation action. *)

val compile : action -> unit result
(** [compile a] performs the compilation action [a]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
