(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Driver support for [B0.{b0,ml}] description files.

    See {!Driver} for more details. *)
open B0

(** {1 Driver} *)

(** b0 directory operations *)
module B0_dir : sig

  (** {1:dir B0 directory} *)

  type t
  (** The type for values representing a B0 directory. The directory
      itself may not exist. *)

  val v : b0_dir:Fpath.t -> t
  (** [v ~b0_dir] is a b0 directory located at [b0_dir] which might
      not exist. *)

  val exists : t -> bool
  (** [exists d] is [true] iff [(dir d)] exists. *)

  val must_exist : t -> unit result
  (** [must_exist d] is [Ok ()] if [(dir d)] exists and an
      error otherwise. *)

  val dir : t -> Fpath.t
  (** [dir d] is [d]'s directory. *)

  val variant_dir : t -> Fpath.t
  (** [variant_dir d] is the directory where variants are located. *)

  (** {1:defaults Defaults}

      Setting defaults fails if the directory doesn't exist. *)

  val default_variant_name : t -> string option
  (** [default_variant_name d] is [d]'s default variant name (if any). *)

  val set_default_variant_name : t -> string option -> unit result
  (** [set_default_variant_name d n] sets the default variant name to
      [n] (if any). *)

  val default_variant_scheme_name : t -> string option
  (** [default_variant_scheme_name d] is [d]'s default variant scheme
      name (if any). *)

  val set_default_variant_scheme_name : t -> string option -> unit result
  (** [set_default_variant_scheme_name d n] sets the default variant name
      to [n] (if any). *)
end

(** Drivers.

    A driver is an executable that provides a service on a
    {{!B0.root}root description}. Some commands of the driver may need
    the description to run and others may not.

    For driver commands that need the description. Running the driver
    compiles and links the root description with libraries
    implementing the driver into a single executable called a {e
    driver instance}. This executable is then run to provide the
    driver's service on the root description. If no description can be
    found the driver's executable is run with an indication that this
    is the case.

    {b TODO} A simple, complete example. *)
module Driver : sig

  (** {1 Driver setup} *)

  type exec = [ `Driver | `Instance ]
  (** The type for driver executions. [`Driver] is the driver executable.
      [`Instance] is the driver library linked with the root description. *)

  type setup
  (** The type for information given to driver commands about the execution. *)

  val b0_dir : setup -> B0_dir.t
  (** [b0_dir] is the absolute path to the b0 directory. *)

  val color : setup -> Tty.cap option
  (** [color s] is the requested terminal capability.
      {b Note.} {!B0.Tty} is already setup accordingly. *)

  val verbosity : setup -> Log.level
  (** [verbosity s] is the requested log verbosity.
      {b Note.} {!B0.Log} is already setup accoringly. *)

  val exec : setup -> exec
  (** [exec] is the kind of execution. Note that if a command
      requests an [`Instance] execution, this will be [`Driver] if no
      root description was found. *)

  (** {1 Drivers} *)

  type cmd = (setup -> int) Cmdliner.Term.t * Cmdliner.Term.info * exec
  (** The type for driver command specifications.
      {ul
      {- The [exec] value indicates if a driver needs an instance to run
       or not. Note that even if the command requests [`Instance]
       and there is no build description it will be executed with [`Driver].}
      {- The term is given a driver setup and should return an exit
         code. When the term is evaluated
         {{!B0.Tty.set_styling_cap}tty}, the {{!B0.Log.set_level}log
         level} and the current working directory are setup. The latter
         is set to either the directory from which the driver binary was
         launched by the user or the directory specified via the [-C]
         option. Remaining files specified on the cli and used by the
         term should be interpreted relative to the current directory.}} *)

  type t
  (** The type for drivers. *)

  val create :
    name:string -> version:string -> libs:string list -> cmd list -> t
  (** [create ~name ~version ~libs cmds main] is a driver named [name]
      versioned with [version] that implements its service using the
      libraries [libs] sorted in dependency order (recursive
      dependencies are not looked up) and the given [cmds]. The
      [B0_driver] library and its recursive dependencies should not be
      mentioned in [libs].

      The given [cmd] terms are wrapped with additional options for
      configuring and setting up the driver instance. These options are
      documented in the {!Cli.s_driver_opts} section of the commands
      manual. [cmd]s that requested [`Instance] execution will be run as
      [`Driver] if no root description is found. *)

  val set : t -> unit
  (** [set d] sets the driver to [d]. This must be set by one of the driver's
      libraries mentioned in {!create}.

      @raise Invalid_argument if a driver is already set. *)

  val driver_main : unit -> unit
  (** [driver_main ()] runs the driver executable's main function. This must
      be called by the driver executable.

      @raise Invalid_argument if no driver was set. *)

  val instance_main : unit -> unit
  (** [instance_main ()] runs the driver instance's main
      function. This gets called by the compiled driver instance. *)
end

(** [Cmdliner] support for drivers.


    {b TODO} What we put here we need to support in the long term so
    needs a good balancing act.  Make the obvious desires easy to do
    (e.g. get a grip on the build outcome of the current variant).
    {ul
    {- Some cleaning and unification here should happen with the
    definitions in [b0]'s driver {!B0b_cli}.}
    {- Not sure it's worth exposing the arguments that are parsed
       by driver setup (e.g. {!root}).}} *)
module Cli : sig

  (** {1:bdirs Default dirs} *)

  val default_cache_dir : Fpath.t
  (** {b TODO} get rid of this. *)

  (** {1:exits Exits and error handling} *)

  val exit_no_description : int
  (** exit code when no description is found. *)

  val exit_driver_setup_err : int
  (** exit code when driver setup errors. *)

  val exit_some_error : int
  (** exit code when some undiscriminate error reported on stdout occurs. *)

  val driver_default_exits : Cmdliner.Term.exit_info list
  (** [driver_default_exits] describe the above errors to Cmdliner. *)

  val handle_error : int result -> int
  (** [handle_error (Ok c)] is [c]. In case of error the message
      is logged on stderr and {!exit_some_error} is returned. *)

  val no_description_found : unit -> int
  (** [no_description_found ()] logs an error that no description was
      found and returns {!exit_no_description}. *)

  (** {1:conv Argument converters} *)

  val path_arg : Fpath.t Cmdliner.Arg.conv
  (** [path_arg] is a cmdliner converter for {!Fpath} values. This not
      not check for path existence, only that the path is valid. *)

  (** {1:copts Common arguments} *)

  val root : Fpath.t option Cmdliner.Term.t
  (** [root] specifies a root directory to lookup the description from. *)

  val cwd : Fpath.t option Cmdliner.Term.t
  (** [cwd] specifies a directory to immedialy change to before
      interpreting anything in the program. In particular relative
      files on the cli should be interpreted according to this
      path. *)

  val b0_dir : Fpath.t option Cmdliner.Term.t
  (** [b0_dir] specifies the b0 directory to use. *)

  val color : Tty.cap option Cmdliner.Term.t
  (** [color] specifies the end-user output mode. *)

  val verbosity : Log.level Cmdliner.Term.t
  (** [verbosity] specifies the log verbosity. *)

  (** {1:mcopts More common arguments} *)

  val cache_dir : Fpath.t option Cmdliner.Term.t
  val cache_index : Fpath.t option Cmdliner.Term.t

  val variant_env : string
  (** [variant_env] is the environment variable used to specify
      the default environment. *)

  val variant : string option Cmdliner.Term.t
  (** [variant] specifies the variant name to act upon. *)

  val variant_scheme_env : string
  val variant_scheme : string option Cmdliner.Term.t

  (** {1:out_fmt Output format arguments} *)

  type out_fmt = [ `Normal | `Short | `Long ]
  (** The type for output format specification. Use to ask for less
      or more data. *)

  val out_fmt : [ `Normal | `Short | `Long ] Cmdliner.Term.t
  (** [out_fmt] specifies the amount of information requested. *)

  (** {1:args Arguments} *)

  val file_kind : [ `All | `Built | `Roots ] Cmdliner.Term.t
  (** [file_kind] specifies the kind of files that need to be looked up. *)

  val ctrl : Build.ctrl Cmdliner.Term.t
  (** [ctrl] has options control the build. *)

  (** {1:driver Driver setup arguments}

      Along with {{!copts}these arguments}, these are automatically added to
      driver commands. *)

  val s_driver_opts : string
  val driver_dir : Fpath.t option Cmdliner.Term.t
  (** [driver_dir] is an option to specify the directory in which
      driver instances should be built. *)

  val driver_only : bool Cmdliner.Term.t
  (** [driver_only] is an option to specify that only the driver
      instance should be built and the program should stop. *)

  val driver_force : bool Cmdliner.Term.t
  (** [driver_force] is an option to specify that the driver instance
      should be recompiled unconditionaly. *)

  val driver_trust : bool Cmdliner.Term.t
  (** [driver_trust] is an option to specify that the driver instance
      should be trusted to be up-to-date. *)

  val driver_ocamlc : string option Cmdliner.Term.t
  (** [driver_ocamlc] is the executable to use as the OCaml byte-code
      compiler for the instance. *)

  val driver_ocamlopt : string option Cmdliner.Term.t
  (** [driver_ocamlopt] is the executable to use as the OCaml native-code
      compiler for the instance. *)

  val driver_compile_kind : [`Byte | `Native | `Auto] option Cmdliner.Term.t
  (** [driver_compile_kind] is an option to specify which kind of compilation
      should be used for compiling the instance. *)

  val driver_compile : string list Cmdliner.Term.t
  (** [driver_compile] are options to be added to the driver instance
      compilation command. *)

  val driver_link : string list Cmdliner.Term.t
  (** [driver_link] are options to be added to the driver link command. *)

  (** {1:man Man} *)

  val common_man : Cmdliner.Manpage.block
  (** [common_man] is a manual that should be included in any driver
      command. It has the driver setup options in a dedicated section
      and a reference to the b0 manual. *)
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
