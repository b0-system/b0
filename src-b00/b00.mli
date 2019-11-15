(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Build kernel *)

(** {1:b00 B00} *)

open B0_std

(** Build environment.

    Build environments control tool lookup and the environment
    of tool spawns. *)
module Env : sig

  (** {1:lookup Tool lookup} *)

  type tool_lookup = Cmd.tool -> (Fpath.t, string) result
  (** The type for tool lookups. Given a command line tool
      {{!type:B0_std.Cmd.tool}specification} returns a file path to
      the tool executable or an error message mentioning the tool if
      it cannot be found. *)

  val env_tool_lookup :
    ?sep:string -> ?var:string -> Os.Env.t -> tool_lookup
  (** [env_tool_lookup ~sep ~var env] is a tool lookup that gets the
      value of the [var] variable in [env] treats it as a [sep]
      separated {{!B0_std.Fpath.list_of_search_path}search path} and
      uses the result to lookup with {!B0_std.Os.Cmd.must_find}.
      [var] defaults to [PATH] and [sep] to
      {!B0_std.Fpath.search_path_sep}. *)

  (** {1:env Environment} *)

  type t
  (** The type for build environments. *)

  val v : ?lookup:tool_lookup -> ?forced_env:Os.Env.t -> Os.Env.t -> t
  (** [v ~lookup ~forced_env env] is a build environment with:
      {ul
      {- [lookup] used to find build tools. Defaults to
         [env_tool_lookup env].}
      {- [forced_env] is environment forced on any tool despite
         what it declared to access, defaults to {!Os.Env.empty}}
      {- [env] the environment read by the tools' declared environment
         variables.}} *)

  val env : t -> Os.Env.t
  (** [env e] is [e]'s available spawn environment. *)

  val forced_env : t -> Os.Env.t
  (** [forced_env e] is [e]'s forced spawn environment. *)

  val tool : t -> Cmd.tool -> (Fpath.t, string) result
  (** [tool e t] looks up tool [t] in [e]. *)
end

(** Command line tools.

    A tool is specified either by name, to be looked up via an
    unspecified mecanism, or by a file path to an executable file. It
    declares the environment variables it accesses in the process
    environment and whether and how it supports response files.

    By default declared environment variables are assumed to influence
    the tool's output and are part of the stamp used to memoize tool
    spawns. If an environment variable is accessed by the tool but
    does not influence its output it should be declared as
    unstamped. Variables specifying the location of
    {{!tmp_vars}temporary file directories} are good examples of
    unstamped variables.

    {b Portability.} In order to maximize portability no [.exe]
    suffix should be added to executable names on Windows, the
    search procedure will add the suffix during the tool search
    procedure if absent. *)
module Tool : sig

  (** {1:env Environment variables} *)

  type env_vars = string list
  (** The type for lists of environment variable names. *)

  val tmp_vars : env_vars
  (** [tmp_vars] is [["TMPDIR"; "TEMP"; "TMP"]]. *)

  (** {1:resp Response files} *)

  type response_file
  (** The type for response file specification. *)

  val response_file_of :
    (Cmd.t -> string) -> (Fpath.t -> Cmd.t) -> response_file
  (** [response_file_of to_file cli] is a response file specification
      that uses [to_file cmd] to convert the command line [cmd] to a
      response file content and [cli f] a command line fragment to be
      given to the tool so that it treats file [f] as a response
      file. *)

  val args0 : response_file
  (** [args0] is response file support for tools that reads null byte
      ([0x00]) terminated arguments response files via an [-args0
      FILE] command line synopsis. *)

  (** {1:tools Tools} *)

  type t
  (** The type for tools. *)

  val v :
    ?response_file:response_file -> ?unstamped_vars:env_vars ->
    ?vars:env_vars -> Cmd.tool -> t
  (** [v ~response_file ~unstamped_vars ~vars cmd] is a tool specified
      by [cmd]. [vars] are the stamped variables accessed by the
      tool (defaults to [[]]). [unstamped_vars] are the unstamped
      variables accessed by the tool (defaults to {!tmp_vars}).
      [response_file] defines the reponse file support for the tool
      (if any). *)

  val by_name :
    ?response_file:response_file -> ?unstamped_vars:env_vars ->
    ?vars:env_vars -> string -> t
  (** [by_name] is like {!v} but reference the tool directly via a name.

      @raise Invalid_argument if {!B0_std.Fpath.is_seg} [name] is [false]. *)

  val name : t -> Cmd.tool
  (** [name t] is [t]'s tool name. If this is a relative file path
      with a single segment the tool is meant to be searched via an
      external mecanism. *)

  val vars : t -> env_vars
  (** [vars t] are the stamped environment variables accessed by [t]. *)

  val unstamped_vars : t -> env_vars
  (** [unstamped_vars t] are the unstamped environment variables
      accessed by [t]. *)

  val response_file : t -> response_file option
  (** [response_file t] is [t]'s response file specification (if any). *)

  val read_env : t -> Os.Env.t -> Os.Env.t * Os.Env.t
  (** [read_env t env] is (all, stamped) with [all] the
      environment with the variables of [env] that are in [vars t]
      and [unstamped_vars t] and [stamped] those of [vars t] only. *)
end

(** Build memoizer.

    A memoizer ties together and environment, an operation cache, a guard
    and an executor. *)
module Memo : sig

  (** {1:memo Memoizer} *)

  type feedback =
  [ `Miss_tool of Tool.t * string
  | `Op_complete of B000.Op.t ]
  (** The type for memoizer feedback. *)

  type t
  (** The type for memoizers. This ties together an environment, a
      guard, an operation cache and an executor. *)

  val create :
    ?clock:Time.counter -> ?cpu_clock:Time.cpu_counter ->
    feedback:(feedback -> unit) -> cwd:Fpath.t -> Env.t -> B000.Guard.t ->
    B000.Reviver.t -> B000.Exec.t -> t

  val memo :
    ?hash_fun:(module Hash.T) -> ?env:Os.Env.t -> ?cwd:Fpath.t ->
    ?cache_dir:Fpath.t -> ?trash_dir:Fpath.t -> ?jobs:int ->
    ?feedback:([feedback | B000.Exec.feedback] -> unit) -> unit ->
    (t, string) result
  (** [memo] is a simpler {!create}
      {ul
      {- [hash_fun] defaults to {!B0_std.Hash.Xxh_64}.}
      {- [jobs] defaults to {!B0_std.Os.Cpu.logical_count}.}
      {- [env] defaults to {!B0_std.Os.Env.current}}
      {- [cwd] defaults to {!B0_std.Os.Dir.cwd}}
      {- [cache_dir] defaults to [Fpath.(cwd / "_b0" / ".cache")]}
      {- [trash_dir] defaults to [Fpath.(cwd / "_b0" / ".trash")]}
      {- [feedback] defaults to a nop.}} *)

  val clock : t -> Time.counter
  (** [clock m] is [m]'s clock. *)

  val cpu_clock : t -> Time.cpu_counter
  (** [cpu_clock m] is [m]'s cpu clock. *)

  val env : t -> Env.t
  (** [env m] is [m]'s environment. *)

  val reviver : t -> B000.Reviver.t
  (** [reviver m] is [m]'s reviver. *)

  val guard : t -> B000.Guard.t
  (** [guard m] is [m]'s guard. *)

  val exec : t -> B000.Exec.t
  (** [exec m] is [m]'s executors. *)

  val trash : t -> B000.Trash.t
  (** [trash m] is [m]'s trash. *)

  val has_failures : t -> bool
  (** [has_failures m] is [true] iff at least one operation (or fiber)
      has failed. *)

  val hash_string : t -> string -> Hash.t
  (** [hash_string m s] is {!B000.Reviver.hash_string}[ (reviver m) s]. *)

  val hash_file : t -> Fpath.t -> (Hash.t, string) result
  (** [hash_file m f] is {!B000.Reviver.hash_file}[ (reviver m) f].
      Note that these file hashes operations are memoized. *)

  val stir : block:bool -> t -> unit
  (** [stir ~block m] runs the memoizer a bit. If [block] is [true]
      blocks until the memoizer is stuck with no operation and fibers to
      execute. *)

  val status : t -> (unit, B000.Op.aggregate_error) result
  (** [status m] looks for aggregate errors in [m] in [ops m], see
      {!B000.Op.aggregate_error} for details.

      Usually called after a blocking {!stir} to check everything
      executed as expected. The function itself has no effect more
      operations can be on [m] afterwards. If you are only interested
      in checking if a failure occured in the memo {!has_failures} is
      faster. *)

  val delete_trash : block:bool -> t -> (unit, string) result
  (** [delete_trash ~block m] is {!B000.Trash.delete}[ ~block (trash m)]. *)

  val ops : t -> B000.Op.t list
  (** [ops m] is the list of operations that were submitted to the
      memoizer *)

  (** {1:fibers Futures and fibers} *)

  type 'a fiber = ('a -> unit) -> unit
  (** The type for memoizer fibers returning values of type ['a]. A
      fiber [f k] represent a thread of execution that eventually
      kontinues [k] with its result value when it reaches an end.

      Fibers should always be run on a Memo via {!spawn_fiber} or as
      the continuation of a build operation. A fiber can fail either
      explictly via {!fail} or because an uncaught exception
      occurs. In both these cases a [`Fail] {!notify} operation gets
      added to the memo to witness the fiber failure.  The status of
      this operation is like any [Fail] notify: {!B000.Op.Failed}. *)

  val spawn_fiber : t -> unit fiber
  (** [run m k] calls [k ()] asynchronously and handles any fiber
      {!fail}ure. This also catches non-asynchronous uncaught
      exceptions and turns them into [`Fail] notification
      operations. *)

  val fail : t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [fail m fmt ...] fails the fiber via a {!notify} operation. *)

  val fail_if_error : t -> ('a, string) result -> 'a
  (** [fail_if_error m r] is [v] if [r] is [Ok v] and [fail m "%s" e] if
      [r] is [Error _]. *)

  (** Future values.

      A future is an undetermined value that becomes determined
      at an an arbitrary point in the future. The future acts as
      a placeholder for the value while it is undetermined. *)
  module Fut : sig

    (** {1:fut Future values} *)

    type memo = t
    (** See {!Memo.t} *)

    type 'a undet
    (** The type for undetermined future information. Forget about this,
        it cannot be acted upon. *)

    type 'a state =
    | Det of 'a (** The future is determined with the given value. *)
    | Undet of 'a undet (** The future is undetermined. *)
    | Never (** The future will never determine. *)
    (** The type for future state. When the state is [Det _] or [Never] we
        say the future is {e set}. *)

    type 'a t
    (** The type for futures with values of type ['a]. *)

    val create : memo -> 'a t * ('a option -> unit)
    (** [create m] is [(f, set)] with [f] the future value and
        [set] the function to [set] it. The latter can be called only
        once, [Invalid_argument] is raised otherwise. If called with
        [None] the future value becomes [Never]. *)

    val ret : memo -> 'a -> 'a t
    (** [ret m v] is [v] as a determined future value. *)

    val state : 'a t -> 'a state
    (** [state f] is the state of [f]'s. *)

    val value : 'a t -> 'a option
    (** [value f] is [f]'s value, if any. *)

    val await : 'a t -> 'a fiber
    (** [await f k] waits for [f] to be determined and continues with [k v]
        with [v] the value of the future. If the future never determines
        [k] is not invoked. Use {!await_set} to witness never determining
        futures. *)

    val await_set : 'a t -> 'a option fiber
    (** [await_set f k] waits for [f] to be set and continues with
        [k None] if [state f] is [Never] and [k (Some v)] if
        [state f] is [Det v]. *)

    val of_fiber : memo -> 'a fiber -> 'a t
    (** [of_fiber m f] runs fiber [f] and sets the resulting future when it
        returns. If [f] raises then the future is set to [Never]. *)
  end

  (** {1:group Operation groups} *)

  val group : t -> string
  (** [group m] is [m]'s group. *)

  val with_group : t -> string -> t
  (** [group m g] is [m] but operations performed on [m] have group [g]. *)

  (** {1:feedback Feedback} *)

  val notify :
    ?k:(unit -> unit) -> t -> [ `Fail | `Warn | `Start | `End | `Info ] ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [notify m kind msg] is a notification [msg] of kind [kind]. Note that
      a [`Fail] notification will entail a {!finish_error}, see also {!fail}
      and {!fail_if_error}. *)

  (** {1:files Files and directories} *)

  val file_ready : t -> Fpath.t -> unit
  (** [ready m p] declares path [p] to be ready, that is exists and is
      up-to-date in [b]. This is typically used with source files
      and files external to the build (e.g. installed libraries). *)

  val read : t -> ?unready_read:bool -> Fpath.t -> string fiber
  (** [read m file k] reads the contents of file [file] as [s] when it
      becomes ready and continues with [k s]. *)

  val write :
    t -> ?stamp:string -> ?unready_reads:Fpath.t list -> ?reads:Fpath.t list ->
    ?mode:int -> ?unready_write:bool -> ?k:(unit -> unit) -> Fpath.t ->
    (unit -> (string, string) result) -> unit
  (** [write m ~reads file w] writes [file] with data [w ()] and mode
      [mode] (defaults to [0o644]) when [reads] are ready. [w]'s
      result must only depend on [reads] and [stamp] (defaults to
      [""]). *)

  val copy :
    t -> ?mode:int -> ?linenum:int -> ?unready_src:bool -> src:Fpath.t ->
    ?unready_dst:bool -> Fpath.t -> unit
  (** [copy m ~mode ?linenum ~src dst] copies file [src] to [dst] with
      mode [mode] (defaults to [0o644]) when [src] is ready. If [linenum]
      is specified, the following line number directive is prependend
      in [dst] to the contents of [src]:
{[
#line $(linenum) "$(src)"
]} *)

  val mkdir : t -> ?mode:int -> Fpath.t -> unit fiber
  (** [mkdir m dir p] creates the directory path [p] with [mode]
      [mode] (defaults to [0o755]) and continues with [k ()] whne
      [dir] is available. The behaviour with respect to file
      permission matches {!Os.Dir.create}. *)

  val delete : t -> Fpath.t -> unit fiber
  (** [delete m p] deletes (trashes in fact) path [p] and continues
      with [k ()] when the path [p] is free to use. *)

  val wait_files : t -> Fpath.t list -> unit fiber
  (** [wait_files m files k] continues with [k ()] when [files] become
      ready. {b FIXME} Unclear whether we really want this. *)

  (** {1:spawn Memoizing tool spawns} *)

  type tool
  (** The type for memoized tools. *)

  type cmd
  (** The type for memoized tool invocations. *)

  val tool : t -> Tool.t -> (Cmd.t -> cmd)
  (** [tool m t] is tool [t] memoized. Use the resulting function
      to spawn the tool with the given arguments. *)

  val tool_opt : t -> Tool.t -> (Cmd.t -> cmd) option
  (** [tool_opt m t] is like {!tool}, except [None] is returned
      if the tool cannot be found. *)

  val spawn :
    t -> ?stamp:string -> ?unready_reads:Fpath.t list ->
    ?reads:Fpath.t list -> ?unready_writes:Fpath.t list ->
    ?writes:Fpath.t list -> ?env:Os.Env.t -> ?cwd:Fpath.t ->
    ?stdin:Fpath.t -> ?stdout:B000.Op.Spawn.stdo ->
    ?stderr:B000.Op.Spawn.stdo -> ?success_exits:B000.Op.Spawn.success_exits ->
    ?post_exec:(B000.Op.t -> unit) -> ?k:(int -> unit) -> cmd -> unit
  (** [spawn m ~reads ~writes ~env ~cwd ~stdin ~stdout ~stderr
      ~success_exits cmd] spawns [cmd] once [reads] files are ready
      and makes files [writes] ready if the spawn succeeds and the
      file exists. The rest of the arguments are:
      {ul
      {- [stdin] reads input from the given file. If unspecified reads
         from the standard input of the program running the build.  {b
         Warning.} The file is not automatically added to [reads],
         this allows for example to use {!B0_std.Os.File.null}.}
      {- [stdout] and [stderr], the redirections for the standard
         outputs of the command, see {!stdo}. Path to files are
         created if needed. {b Warning.} File redirections
         are not automatically added to [writes]; this allows for example
         to use {!B0_std.Os.File.null}.}
      {- [success_exits] the exit codes that determine if the build operation
         is successful (defaults to [0], use [[]] to always succeed)}
      {- [env], environment variables added to the build environment.
         This overrides environment variables read by the tool in the
         build environment except for forced one. It also allows to
         specify environment that may not be mentioned by the running
         tool's {{!Tool.v}environment specification}.}
      {- [cwd] the current working directory. Default is {!cwd}. In
         general it's better to avoid using relative file paths and
         tweaking the [cwd]. Construct your paths using the absolute
         {{!dirs}directory functions} and make your invocations
         independent from the [cwd].}
      {- [post_exec], if specified is called with the build operation
         after it has been executed or revived. If it was executed
         this is called before the operation gets recorded. It can
         be used to define the [reads] and [writes] of the operation
         if they are difficult to find out before hand. {b Do not}
         access [m] in that function.}
      {- [k], if specified a fiber invoked once the spawn has succesfully
         executed with the exit code.}
      {- [stamp] is used for caching if two spawns diff only in their
         stamp they will cache to different keys. This can be used to
         memoize tool whose outputs may not entirely depend on the environment,
         the cli stamp and the the content of read files.}}

      {b Note.} If the tool spawn acts on a sort of "main" file
      (e.g. a source file) it should be specified as the first element
      of [reads], this is interpreted specially by certain build
      tracer. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers

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
