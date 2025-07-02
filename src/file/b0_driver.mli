(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** b0 file drivers.

    b0 file drivers access the definitions of b0 files.
    See {{!page-driver}this manual} for an
    overview and a minimal example. *)

open B0_std

(** {1:commonalities Commonalities} *)

(** Driver exit codes. *)
module Exit : sig

  (** See also {!B0_cli.Exit} *)

  val build_error : Os.Exit.t
  (** [build_error] indicates a build error. *)

  val b0_file_error : Os.Exit.t
  (** [b0_file_error] indicates a b0 file error. *)

  val deploy_error : Os.Exit.t
  (** [deploy_error] indicates a deploy error. *)

  val no_b0_file : Os.Exit.t
  (** [no_b0_file] indicates no b0 file could be found. *)

  val infos : Cmdliner.Cmd.Exit.info list
  (** [infos] has the infos of {!Cmdliner.Cmd.Exit.defaults},
      {!B0_cli.Exit.ok}, {!B0_cli.Exit.some_error} and those above.  *)
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
  (** [b0_file_name] is ["B0.ml"] the default b0 file name. *)

  val drivers_dir_name : string
  (** [driver_dir_name] is [".drivers"] the default directory
      for drivers in the b0 directory. *)

  (** {1:conf Configurations} *)

  type t
  (** The type for configurations. *)

  val make :
    b0_dir:Fpath.t -> b0_file:Fpath.t option -> cache_dir:Fpath.t ->
    cwd:Fpath.t -> code:B0_ocaml.Code.t option -> env:Os.Env.t ->
    hash_fun:(module B0_hash.T) -> jobs:int -> log_level:Log.level ->
    no_pager:bool -> fmt_styler:Fmt.styler -> unit -> t
  (** [make] constructs a configuration with given attributes. See the
      accessors for semantics. *)

  val b0_file : t -> Fpath.t option
  (** [b0_file] is the absolute path to the b0 file (if any). *)

  val b0_dir : t -> Fpath.t
  (** [b0_dir] is the absolute path to the b0 directory. *)

  val cache_dir : t -> Fpath.t
  (** [cache_dir] is the absolute path to the cache directory. *)

  val cwd : t -> Fpath.t
  (** [cwd] is the absolute path to the current working directory. *)

  val code : t -> B0_ocaml.Code.t option
  (** [code] is the code to which the driver is compiled. *)

  val env : t -> Os.Env.t
  (** [env] is the process environment of the driver. *)

  val hash_fun : t -> (module B0_hash.T)
  (** [hash_fun] is the hash function to use for build caching. *)

  val jobs : t -> int
  (** [jobs] is the maximal number of spawns allowed. *)

  val log_level : t -> Log.level
  (** [log_level] is the desired log level. *)

  val memo : t -> (B0_memo.t, string) result
  (** [memo] is the memoizer for the configuration. *)

  val no_pager : t -> bool
  (** [no_pager] indicates no paging is desired on stdout. *)

  val fmt_styler : t -> Fmt.styler
  (** [fmt_styler] is the styler to assume for outputs. *)

  (** {1:derived Derived data} *)

  val get_b0_file : t -> (Fpath.t, string) result
  (** [get_b0_file] provides an error message if [b0_file] is [None]. *)

  (** {1:setup Setup} *)

  val setup_with_cli :
    b0_dir:Fpath.t option -> b0_file:Fpath.t option ->
    cache_dir:Fpath.t option -> code:B0_ocaml.Code.t option ->
    hash_fun:(module B0_hash.T) option -> jobs:int option ->
    log_level:Log.level option -> no_pager:bool ->
    color:Fmt.styler option option -> unit -> (t, string) result
  (** [setup_with_cli] determines and setups a configuration with the
      given values. These are expected to have been determined by
      environment variables and command line arugments. *)
end

(** Cli interaction. *)
module Cli : sig
  val log_level : B0_std.Log.level option Cmdliner.Term.t
  val color : B0_std.Fmt.styler option option Cmdliner.Term.t
  val no_pager : bool Cmdliner.Term.t
  val conf : Conf.t Cmdliner.Term.t
end

(** {1:driver Drivers} *)

type main =
  unit -> (Os.Exit.t Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result
(** The type for driver main functions. A function that returns
    a Cmdliner evaluation result. This evaluation result is used
    by {!run} to handle program termination. Note that the driver
    may exit prematurely with {!Exit.b0_file_error} if there's
    a fatal error in the b0 file. *)

type t
(** The type for drivers. *)

val make :
  name:string -> version:string -> libs:B0_ocaml.Libname.t list -> t
(** [make ~name ~version] is a new driver named [name] which has
    version [version] and uses library [libs] to link the b0 file. Note
    that these libraries are not added during the compilation phase.

    {b Note.} The b0 libraries are automatically added to [libs]
    there's no need to mention them. Just mention your driver library
    and its dependencies in order. *)

val name : t -> string
(** [name d] is the name of [d]. *)

val version : t -> string
(** [version d] is the version of [d]. *)

val libs : t -> B0_ocaml.Libname.t list
(** [libs d] are the libraries that need to be added for linking. *)

val set : driver:t -> main:main -> unit
(** [set d] sets the driver to [d] and its main function to [main].
    Use {!run} to run the driver. *)

val run : has_b0_file:bool -> unit
(** [run ~has_b0_file] runs the driver set by {!set}. [has_b0_file]
    must be [true] if the b0 file is linked in, this is typically done
    by the b0 file expanded source invocation. Raises
    [Invalid_argument] if no driver is set. *)

(** {1:require Require the b0 file} *)

val with_b0_file :
  driver:t -> (Conf.t -> Os.Exit.t) Cmdliner.Term.t -> Os.Exit.t Cmdliner.Term.t
(** [with_b0_file ~driver cmd] wraps [cmd] to make sure it runs with
    the b0 file compiled and linked in as specified by [driver]. *)

val with_b0_file_if_any :
  driver:t -> (Conf.t -> Os.Exit.t) Cmdliner.Term.t -> Os.Exit.t Cmdliner.Term.t
(** [with_b0_file_if_any ~driver cmd] is like {!with_b0_file} however
    it doesn't error if the b0 file does not exist or if it fails to
    compile (a warning is generated in this case). The driver code can
    use {!B0_driver.has_b0_file} to see if one is linked and
    {!B0_driver.has_failed_b0_file} to check if there was an error in linking
    it. *)

val has_b0_file : unit -> bool
(** [has_b0_file ()] is [true] if {!run} is called with [has_b0_file]. *)

val has_failed_b0_file : unit -> bool
(** [has_failed_b0_file ()] is [true] for {!with_b0_file_if_any} when
    there is a b0 file but it failed to compile. *)

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
    Conf.t -> driver:t -> feedback:bool -> B0_file.t -> (Fpath.t, string) result
  (** [compile c ~driver b0_file] compiles [b0_file] with driver [driver]
      in configuration [c]. If all is well the executable file path is
      returned. [feedback] indicates whether errors are reported
      interactively. *)
end
