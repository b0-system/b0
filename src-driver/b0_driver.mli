(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** B0 file drivers.

    B0 file drivers access the definitions of B0 files.
    See {{!page-manual.defining_drivers}this manual section} for an
    overview and a minimal example. *)

open B0_std

(** {1:commonalities Commonalities} *)

(** Driver exit codes. *)
module Exit : sig

  type t =
  | Code of int
  | Exec of Fpath.t * Cmd.t (** *)
  (** The type for exits. Either an exit code or a command to [execv]. *)

  val code : t -> int
  (** [code e] is the exit code of [e]. Raises [Invalid_argument] if
      [e] is {!Exec}. *)

  val b0_file_error : t
  (** [b0_file_error] indicates an error with the B0 file. *)

  val no_b0_file : t
  (** [no_b0_file] indicates no B0 file could be found. *)

  val no_such_name : t
  (** [no_such_name] indicates a named entity was not found. *)

  val ok : t
  (** [ok] is the zero exit code. *)

  val some_error : t
  (** [some_error] indicates an indiscriminate error reported on stdout. *)

  (** Cmdliner documentation. *)
  module Info : sig
    val base_cmd : Cmdliner.Term.exit_info list
    (** [base_cmd] documents the exit code of {!B0_driver.Exit}. *)
  end
end

(** Driver environment variables. *)
module Env : sig

  val b0_dir : string
  (** [b0_dir] is the variable used to specify the b0 directory. *)

  val b0_file : string
  (** [b0_file] is the variable used to specify the b0 file to use. *)

  val cache_dir : string
  (** [cache_dir] is the variable used to specify the b0 cache directory. *)

  val color : string
  (** [color] is the variable used to specify tty styling. *)

  val code : string
  (** [code] is the variable used to specify the code to which the driver
      compiles to. *)

  val hash_fun : string
  (** [hash_fun] is the variable used to specify the cache hash function. *)

  val jobs : string
  (** [jobs] is the variable used to specify the maximal number of spawns. *)

  val verbosity : string
  (** [verbosity] is the variable used to specify log verbosity. *)
end


(** Driver configuration. *)
module Conf : sig

  (** {1:filename Default file names} *)

  val b0_file_name : string
  (** [b0_file_name] is ["B0.ml"] the default B0 file name. *)

  val drivers_dir_name : string
  (** [driver_dir_name] is [".drivers"] the default directory
      for drivers in the b0 directory. *)

  (** {1:conf Configurations} *)

  type t
  (** The type for configurations. *)

  val v :
    b0_dir:Fpath.t -> b0_file:Fpath.t option -> cache_dir:Fpath.t ->
    cwd:Fpath.t -> code:B0_ocaml.Cobj.code option -> hash_fun:(module Hash.T) ->
    jobs:int -> log_level:Log.level -> no_pager:bool -> tty_cap:Tty.cap ->
    unit -> t
  (** [v] constructs a configuration with given attributes. See the
      accessors for semantics. *)

  val b0_file : t -> Fpath.t option
  (** [b0_file] is the absolute path to the B0 file (if any). *)

  val b0_dir : t -> Fpath.t
  (** [b0_dir] is the absolute path to the b0 directory. *)

  val cache_dir : t -> Fpath.t
  (** [cache_dir] is the absolute path to the cache directory. *)

  val cwd : t -> Fpath.t
  (** [cwd] is the absolute path to the current working directory. *)

  val code : t -> B0_ocaml.Cobj.code option
  (** [code] is the code to which the driver is compiled. *)

  val hash_fun : t -> (module Hash.T)
  (** [hash_fun] is the hash function to use for build caching. *)

  val jobs : t -> int
  (** [jobs] is the maximal number of spawns allowed. *)

  val log_level : t -> Log.level
  (** [log_level] is the desired log level. *)

  val memo : t -> (B00.Memo.t, string) result
  (** [memo] is the memoizer for the configuration. *)

  val no_pager : t -> bool
  (** [no_pager] indicates no paging is desired on stdout. *)

  val tty_cap : t -> Tty.cap
  (** [tty_cap] is the terminal capability to assume for outputs. *)

  (** {1:derived Derived data} *)

  val get_b0_file : t -> (Fpath.t, string) result
  (** [get_b0_file] provides an error message if [b0_file] is [None]. *)

  (** {1:setup Setup} *)

  val setup_with_cli :
    b0_dir:Fpath.t option -> b0_file:Fpath.t option ->
    cache_dir:Fpath.t option -> code:B0_ocaml.Cobj.code option ->
    hash_fun:(module Hash.T) option -> jobs:int option ->
    log_level:Log.level option -> no_pager:bool ->
    tty_cap:Tty.cap option option -> unit -> (t, string) result
  (** [setup_with_cli] determines and setups a configuration with the
      given values. These are expected to have been determined by
      environment variables and command line arugments. *)
end

(** Cli interaction. *)
module Cli : sig
  val conf : Conf.t Cmdliner.Term.t
end

(** {1:driver Drivers} *)

type main = unit -> Exit.t Cmdliner.Term.result
(** The type for driver main functions. A function that returns
    a Cmdliner evaluation result. This evaluation result is used
    by {!run} to handle program termination. Note that the driver
    may exit prematurely with {!Exit.b0_file_error} if there's
    a fatal error in the B0 file. *)

type t
(** The type for drivers. *)

val create : name:string -> version:string -> libs:B0_ocaml_lib.Name.t list -> t
(** [create ~name ~version] is a new driver named [name] which has
    version [version] and uses library [libs] to link the B0 file. Note
    that these libraries are not added during the compilation phase.

    {b Note.} The b0 libraries are automatically added to [libs]
    there's no need to mention them. Just mention your driver library
    and its dependencies in order. *)

val name : t -> string
(** [name d] is the name of [d]. *)

val version : t -> string
(** [version d] is the version of [d]. *)

val libs : t -> B0_ocaml_lib.Name.t list
(** [libs d] are the libraries that need to be added for linking. *)

val set : driver:t -> main:main -> unit
(** [set d] sets the driver to [d] and its main function to [main].
    Use {!run} to run the driver. *)

val run : has_b0_file:bool -> unit
(** [run ~has_b0_file] runs the driver set by {!set}. [has_b0_file]
    must be [true] if the B0 file is linked in, this is typically done
    by the B0 file expanded source invocation. Raises
    [Invalid_argument] if no driver is set. *)

(** {1:require Require the B0 file} *)

val with_b0_file :
  driver:t -> (Conf.t -> Exit.t) Cmdliner.Term.t -> Exit.t Cmdliner.Term.t
(** [with_b0_file ~driver cmd] wraps [cmd] to make sure it runs with
    the B0 file compiled and linked in as specified by [driver]. *)

val has_b0_file : unit -> bool
(** [has_b0_file ()] is [true] if {!run} is called with [has_b0_file]. *)

(** {1:compile Compilation} *)

(** Driver compilation.

    Driver compilation is automatically handled by {!with_b0_file}.
    But a few driving bits are exposed here. *)
module Compile : sig
  val build_dir : Conf.t -> driver:t -> Fpath.t
  (** [build_dir c ~driver] is a build directory for driver [driver]
      in configuration [c]. *)

  val build_log : Conf.t -> driver:t -> Fpath.t
  (** [build_log c ~driver] is a build log file for driver [driver]
      in configuration [c]. *)

  val exe : Conf.t -> driver:t -> Fpath.t
  (** [exe c ~driver] is the driver executable for driver [driver]
      in configuration [c]. *)

  val compile :
    Conf.t -> driver:t -> B0_file_src.t -> (Fpath.t, string) result
  (** [compile c ~driver b0_file] compiles [b0_file] with driver [driver]
      in configuration [c]. If all is well the executable file path is
      returned. *)
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
