(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** OS Interaction.

    Abridged [bos]. See {!B0.OS}. *)

open B0_result

module Env : sig
 val var : string -> string option
 val opt_var : string -> absent:string -> string

 type t = string B0_string.map
 val empty : t
 val current : unit -> t result
 val override : t -> by:t -> t
 val of_assignments : ?init:t -> string list -> t result
 val to_assignments : t -> string list
end

module Path : sig
  val trash : B0_fpath.t -> in_dir:B0_fpath.t -> unit result
end

module File : sig
  val null : B0_fpath.t
  val dash : B0_fpath.t
  val is_dash : B0_fpath.t -> bool

  val exists : B0_fpath.t -> bool result
  val must_exist : B0_fpath.t -> B0_fpath.t result
  val delete : ?must_exist:bool -> B0_fpath.t -> unit result

  val link : force:bool -> target:B0_fpath.t -> B0_fpath.t -> unit result

  val with_ic : B0_fpath.t -> (in_channel -> 'a -> 'b) -> 'a -> 'b result
  val read : B0_fpath.t -> string result

  val with_oc :
      ?mode:int -> B0_fpath.t ->
      (out_channel -> 'a -> (('c, 'd) Pervasives.result as 'b)) -> 'a ->
      'b result

  val write : ?mode:int -> B0_fpath.t -> string -> unit result
  val open_tmp_path :
    ?mode:int -> B0_fpath.t -> (B0_fpath.t * Unix.file_descr) result

  val with_tmp_oc :
      ?mode:int -> B0_fpath.t -> (B0_fpath.t -> out_channel -> 'a -> 'b) ->
      'a -> 'b result
    (** [with_tmp_oc mode dir pat f v] is a new temporary file in
        [dir] (defaults to {!Dir.default_tmp}) named according to
        [pat] and atomically created and opened with permission [mode]
        (defaults to [0o600] only readable and writable by the
        user). Returns [Ok (f file oc v)] with [file] the file path
        and [oc] an output channel to write the file. After the
        function returns (normally or via an exception), [oc] is
        closed and [file] is deleted. *)
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
  val path_dirs : ?sep:string -> string -> string list
  val which_lookup_needed : string -> bool
  val which_file : dirs:string list -> string -> string option
  val which_raw : string -> string option
  val which : B0_cmd.t -> B0_fpath.t option result

  val exists : B0_cmd.t -> bool result
  val must_exist : B0_cmd.t -> B0_cmd.t result


  type status = [`Exited of int | `Signaled of int ]
  val pp_status : status B0_fmt.t
  type run_status = B0_cmd.t * status

  val run : ?err:B0_fpath.t -> B0_cmd.t -> unit result
  val run_status : ?err:B0_fpath.t -> B0_cmd.t -> status result

  val success : ('a * run_status) result -> 'a result

  type run_out
  val out_string : ?trim:bool -> run_out -> (string * run_status) result
  val out_file : B0_fpath.t -> run_out -> (unit * run_status) result
  val out_stdout : run_out -> (unit * run_status) result

  val to_string : ?trim:bool -> run_out -> string result
  val to_file : B0_fpath.t -> run_out -> unit result
  val to_null : run_out -> unit result

  val run_out : ?err:B0_fpath.t -> B0_cmd.t -> run_out

  type spawn_pid = int
  type spawn_stdio =
    [ `Fd of Unix.file_descr * bool (* close *)
    | `File of B0_fpath.t ]

  val spawn :
    string array -> cwd:B0_fpath.t ->
    stdin:spawn_stdio -> stdout:spawn_stdio -> stderr:spawn_stdio ->
    B0_cmd.t -> spawn_pid result

  val collect : block:bool -> spawn_pid -> (spawn_pid * status) option result

  val rm_rf : B0_fpath.t -> spawn_pid result

  (* exec *)

  val execv_raw : string -> string array -> unit result
  val execve_raw : string -> string array -> env:string array -> unit result
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
