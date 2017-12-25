(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** OS Interaction.

    Abridged [bos]. See {!B0.OS}. *)

open B0_result

module Env : sig
 val find : ?empty_is_absent:bool -> string -> string option
 val get : ?empty_is_absent:bool -> string -> absent:string -> string

 val value :
   ?empty_is_absent:bool -> string -> 'a B0_conv.t -> absent:'a -> 'a result

 val get_value :
   ?log:B0_log.level -> ?empty_is_absent:bool -> string -> 'a B0_conv.t ->
   absent:'a -> 'a

 type t = string B0_string.map
 val empty : t
 val current : unit -> t result
 val override : t -> by:t -> t
 val assignments : unit -> string list result
 val of_assignments : ?init:t -> string list -> t result
 val to_assignments : t -> string list
end

module File : sig
  val null : B0_fpath.t
  val dash : B0_fpath.t
  val is_dash : B0_fpath.t -> bool

  val exists : B0_fpath.t -> bool result
  val must_exist : B0_fpath.t -> B0_fpath.t result
  val delete : ?must_exist:bool -> B0_fpath.t -> unit result

  val link : force:bool -> target:B0_fpath.t -> B0_fpath.t -> unit result
  val is_executable : B0_fpath.t -> bool

  val with_ic : B0_fpath.t -> (in_channel -> 'a -> 'b) -> 'a -> 'b result
  val read : B0_fpath.t -> string result
  val read_fd : B0_fpath.t -> Unix.file_descr -> string result

  val with_oc :
      ?mode:int -> B0_fpath.t ->
      (out_channel -> 'a -> (('c, 'd) Pervasives.result as 'b)) -> 'a ->
      'b result

  val write : ?mode:int -> B0_fpath.t -> string -> unit result
  val open_tmp :
    ?flags:Unix.open_flag list -> ?mode:int -> B0_fpath.t ->
    (B0_fpath.t * Unix.file_descr) result

  val with_tmp_oc :
    ?flags:Unix.open_flag list -> ?mode:int -> B0_fpath.t ->
    (B0_fpath.t -> out_channel -> 'a -> 'b) -> 'a -> 'b result
end

module Dir : sig
  val exists : B0_fpath.t -> bool result
  val must_exist : B0_fpath.t -> B0_fpath.t result
  val create : ?path:bool -> ?mode:int -> B0_fpath.t -> bool result
  val delete : ?must_exist:bool -> contents:bool -> B0_fpath.t -> unit result
  val contents :
    ?dotfiles:bool -> ?rel:bool -> B0_fpath.t -> B0_fpath.t list result

  val files :
    ?dotfiles:bool -> ?rel:bool -> B0_fpath.t -> B0_fpath.t list result

  val dirs :
    ?dotfiles:bool -> ?rel:bool -> B0_fpath.t -> B0_fpath.t list result

  val current : unit -> B0_fpath.t result
  val set_current : B0_fpath.t -> unit result
  val with_current : B0_fpath.t -> ('a -> 'b) -> 'a -> 'b result

  val default_tmp : unit -> B0_fpath.t
  val set_default_tmp : B0_fpath.t -> unit
end

module Cmd : sig

  (* Tool existence and search *)

  val exe_is_path : string -> bool
  val find_tool :
    ?search:B0_fpath.t list -> B0_cmd.t -> B0_fpath.t option result

  val get_tool : ?search:B0_fpath.t list -> B0_cmd.t -> B0_fpath.t result
  val exists : ?search:B0_fpath.t list -> B0_cmd.t -> bool result
  val must_exist : ?search:B0_fpath.t list -> B0_cmd.t -> B0_cmd.t result
  val resolve : ?search:B0_fpath.t list -> B0_cmd.t -> B0_cmd.t result
  val search_path_dirs : ?sep:string -> string -> B0_fpath.t list result

  (* Process completion statuses *)

  type status = [`Exited of int | `Signaled of int ]
  val pp_status : status B0_fmt.t
  val pp_cmd_status : (B0_cmd.t * status) B0_fmt.t

  (* Process standard inputs and outputs *)

  type stdi
  type stdo

  val in_string : string -> stdi
  val in_file : B0_fpath.t -> stdi
  val in_fd : close:bool -> Unix.file_descr -> stdi
  val in_stdin : stdi
  val in_null : stdi

  val out_file : B0_fpath.t -> stdo
  val out_fd : close:bool -> Unix.file_descr -> stdo
  val out_stdout : stdo
  val out_stderr : stdo
  val out_null : stdo

  (* Blocking command execution *)

  val run_status :
    ?env:string list -> ?cwd:B0_fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
    ?stderr:stdo -> B0_cmd.t -> status result

  val run_status_out :
    ?trim:bool -> ?env:string list -> ?cwd:B0_fpath.t -> ?stdin:stdi ->
    ?stderr:[`Stdo of stdo | `Out] -> B0_cmd.t -> (status * string) result

  val run :
    ?env:string list -> ?cwd:B0_fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
    ?stderr:stdo -> B0_cmd.t -> unit result

  val run_out :
    ?trim:bool -> ?env:string list -> ?cwd:B0_fpath.t -> ?stdin:stdi ->
    ?stderr:[`Stdo of stdo | `Out] -> B0_cmd.t -> string result

  (* Non-blocking command execution *)

  type pid = int
  val pid_to_int : pid -> int

  val spawn_low :
    env:string array -> cwd:string -> stdin:stdi -> stdout:stdo ->
    stderr:stdo -> B0_cmd.t -> pid result

  val spawn :
    ?env:string list -> ?cwd:B0_fpath.t -> ?stdin:stdi -> ?stdout:stdo ->
    ?stderr:stdo -> B0_cmd.t -> pid result


  val collect : ?block:bool -> pid -> status option result

  (* Executing files *)

  val execv :
    ?env:string list -> ?cwd:B0_fpath.t -> B0_fpath.t -> B0_cmd.t -> unit result
end

(* B0 internals *)

module B0 : sig
  val rm_rf : B0_fpath.t -> Cmd.pid result
  val trash_path : B0_fpath.t -> in_dir:B0_fpath.t -> unit result
end


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
