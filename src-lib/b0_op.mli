(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build operations.*)

open B0_result

(** {1:op Operations} *)

type id = int
(** The type for build operation identifiers. *)

type t
(** The type for build operations. *)

val id : t -> id
(** [id o] is the identifier of operation [o]. *)

val unit_id : t -> B0_unit.id
(** [unit_id o] is the identifier of the unit in which the operation was
    submitted. *)

val aim : t -> B0_conf.build_aim
(** [aim o] is the build target for which the operation was submitted. *)

val reads : t -> B0_fpath.set
(** [reads o] are the file paths read by operation. *)

val writes : t -> B0_fpath.set
(** [writes o] writes are the file paths written by [o]. *)

val creation_time : t -> B0_time.span
(** [creation_time o] is [o]'s monotonic creation time. *)

val exec_start_time : t -> B0_time.span
(** [exec_start_time o] is [o]'s monotonic operating system starting
    execution time. This is different from {!B0_mtime.zero} once the
    operation has been submitted to the OS. *)

val set_exec_start_time : t -> B0_time.span -> unit
(** [set_submit_time o t] sets [o]'s submit time to [t]. *)

val exec_end_time : t -> B0_time.span
(** [exec_end_time o] is [o]'s monotonic time when the operation's has
    been processed by the operating system. This is different from
    {!B0_mtime.zero} once the operation has been completed by the OS
    and collected and processed into [Executed] status by the build
    program. *)

val set_exec_end_time : t -> B0_time.span -> unit
(** [set_exec_end_time o t] sets [o]'s return time to [t]. *)

val cached : t -> bool
(** [cached o] is [true] if the result of the operation was looked up
    in the cache. *)

val set_cached : t -> bool -> unit
(** [set_cached o c] sets {!cached} to [c] for [o]. *)

type status =
| Guarded
| Ready
| Executed
| Finished
(** The type for operation statuses.
    {ul
    {- [Guarded] the operation is guarded from execution (initial state).}
    {- [Ready] the operation is ready to be submitted for execution. This
       does not mean that the operation has been submitted to the OS yet,
       see {!seq}.}
    {- [Executed] is [true] iff [o] has been executed by the OS (but it may not
       have succeded), see the individual operation results.}
    {- [Finished] the effects of the operation have been applied.}} *)

val status : t -> status
(** [status o] is [o]'s status. *)

val set_status : t -> status -> unit
(** [set_status o s] sets the status of [o] to [s]. *)

val pp_status : status B0_fmt.t
(** [pp_status] is a formatter for statuses. *)

val stamp : t -> B0_stamp.t
(** [stamp o] is [o]'s stamp. This should be {!B0_stamp.zero} until
    the operation is ready, since the {!reads} contents should be recorded
    by the stamp. *)

val set_stamp : t -> B0_stamp.t -> unit
(** [set_stamp o s] sets [o]'s stamp to [s]. *)

(** {1:spawn Process spawns} *)

type spawn_pid = int
(** The type for OS specific process identifiers. *)

type spawn_stdo = [ `Ui | `File of B0_fpath.t | `Tee of B0_fpath.t ]
(** The type for spawn standard output redirections. *)

type spawn_stdo_ui =
  [ `Tmp_file of B0_fpath.t | `Stdo of string result | `None ]
(** The type for spawn standard output [`Ui] redirection result. When
    submitted this becomes [`Tmp_file], once read back this is [`Stdo]. *)

type spawn_env = string array
(** The type for spawn process environments. *)

type spawn_success_codes = int list option
(** The list of exit codes that indicates success. If this is [None]
    only zero is success. If the list is empty this any exit code. *)

type spawn
(** The type for process spawn operations. *)

val spawn :
  B0_unit.t -> B0_conf.build_aim -> B0_time.span -> reads:B0_fpath.set ->
  writes:B0_fpath.set -> success:spawn_success_codes ->
  stdin:B0_fpath.t option -> stdout:spawn_stdo -> stderr:spawn_stdo ->
  cwd:B0_fpath.t -> spawn_env -> B0_cmd.t -> t
(** [spawn u t reads writes success stdin stdout stderr cwd env cmd],
    spawns command [cmd] in environment [env] and current working
    directory [cwd] reading standard input from [stdin] and writing
    standard output and error to [stderr] and [stderr], using file
    [stdo_ui] for [`Ui] output redirections. [u] is the unit in which
    the operation executes, [reads] are file read by the operation and
    [writes] are file written by the operation. *)

val spawn_cmd : spawn -> B0_cmd.t
(** [spawn_cmd s] is [s]'s invocation. *)

val spawn_env : spawn -> spawn_env
(** [spawn_env s] is the environment in which [s] was run. *)

val spawn_cwd : spawn -> B0_fpath.t
(** [spawn_cwd s] is the current working directory in which [s] was run. *)

val spawn_stdin : spawn -> B0_fpath.t option
(** [spawn_stdin s] is file where stdin was read from for [s] (if any). *)

val spawn_stdout : spawn -> spawn_stdo
(** [spawn_stdout s] is destination to which stdout was written for [s]. *)

val spawn_stderr : spawn -> spawn_stdo
(** [spawn_stderr s] is destination to which stderr was written for [s]. *)

val spawn_success_codes : spawn -> int list option
(** [spawn_success_codes s] is the list of exit codes denoting success
    for the oparation. *)

val spawn_stdo_ui : spawn -> spawn_stdo_ui
(** [spawn_stdo_ui s] is the [`Ui] redirection result of [s]. *)

val set_spawn_stdo_ui : spawn -> spawn_stdo_ui -> unit
(** [set_spawn_stdo_ui s st] sets the [`Ui] redirection result to
    [st]. *)

val spawn_result : spawn -> (spawn_pid * B0_os.Cmd.status) result
(** [spawn_result s] is [s]'s OS completion result or an error. *)

val set_spawn_result : spawn -> (spawn_pid * B0_os.Cmd.status) result -> unit
(** [set_spawn_result s st] sets [s] result to [st]. *)

(** {1:read File reads} *)

type read
(** The type for file read operations. *)

val read : B0_unit.t -> B0_conf.build_aim -> B0_time.span -> B0_fpath.t -> t
(** [read f] reads the contents of file [f] in memory. *)

val read_file : read -> B0_fpath.t
(** [read_file r] is [r]'s read file. *)

val read_result : read -> string result
(** [read_result r] is [r]'s result. Either the read data or an
    error. *)

val set_read_result : read -> string result -> unit
(** [set_read_result r st] sets [r]'s result to [st]. *)

val get_read_data : read -> string
(** [get_read_data r] is the data read by [r]. @raise Invalid_argument
    if [read_result] is [Error _]. *)

(** {1:write File writes} *)

type write
(** The type for (atomic) file write operations. *)

val write :
  B0_unit.t -> B0_conf.build_aim -> B0_time.span -> reads:B0_fpath.set ->
  B0_fpath.t -> t
(** [write u f], when submitted, starts to atomically set the
    contents of [f] to [write_data w]. *)

val write_file : write -> B0_fpath.t
(** [write_file w] is [w]'s written file. *)

val write_data : write -> string
(** [write_data w] is [w]'s written data. *)

val set_write_data : write -> string -> unit
(** [write_data w] sets [w]'s written data to [w]. *)

val write_result : write -> unit result
(** [write_result w] is [w]'s result. Either unit or an error. *)

val set_write_result : write -> unit result -> unit
(** [set_write_result w st] sets [w]'s result to [st]. *)

(** {1:copyfile File copies} *)

type copy_file
(** The type for file copies. *)

val copy_file :
  ?linenum:int -> B0_unit.t -> B0_conf.build_aim -> B0_time.span ->
  B0_fpath.t -> B0_fpath.t -> t
(** [copy_file ?linenum src dst], when submitted, copies [src] to
    [dst].  If [linenum] is [true] the following line number directive
    is prepended to the contents of [src] in [dst]:
{[
#line $(linenum) "$(src)"
]}
*)

val copy_file_src : copy_file -> B0_fpath.t
(** [copy_file_src c] is [c]'s source file. *)

val copy_file_dst : copy_file -> B0_fpath.t
(** [copy_file_dst c] is [c]'s destination file. *)

val copy_file_linenum : copy_file -> int option
(** [copy_file_linenum c]  is [c]'s line number directive value (if any). *)

val copy_file_result : copy_file -> unit result
(** [copy_file_result c] is [c]'s result. Either the unit or an error. *)

val set_copy_file_result : copy_file -> unit result -> unit
(** [set_copy_file_result c st] sets [c]'s result to [st]. *)

(** {1:delete File deletions} *)

type delete
(** The type for file deletions. *)

val delete :
  B0_unit.t -> B0_conf.build_aim -> B0_time.span -> B0_fpath.t -> t
(** [delete u t p] starts to delete path [p]. *)

val delete_file : delete -> B0_fpath.t
(** [delete_file d] is [d]'s deleted file. *)

val delete_result : delete -> unit result
(** [delete_result d] is [d]'s result. *)

val set_delete_result : delete -> unit result -> unit
(** [set_delete_result d st] sets [d]'s result to [st]. *)

(** {1:mkdir Directory creation *)

type mkdir
(** The type for directory creation operations. *)

val mkdir :
  B0_unit.t -> B0_conf.build_aim -> B0_time.span -> B0_fpath.t -> t
(** [mkdir u t] start to create directory path [d]. *)

val mkdir_dir : mkdir -> B0_fpath.t
(** [mkdir_dir mk] is [mk]'s created directory. *)

val mkdir_result : mkdir -> unit result
(** [mkdir_result mk] is [mk]'s result. *)

val set_mkdir_result : mkdir -> unit result -> unit
(** [set_mkdir_result d st] sets [mk]'s result to [st]. *)

(** {1:unit Unit sychronisation} *)

type sync
(** The type for unit synchronisation. *)

val sync :
  B0_unit.t -> B0_conf.build_aim -> B0_time.span -> B0_unit.Idset.t -> t
(** [sync u us] synchronises on the successful termination of [us]. *)

val sync_units : sync -> B0_unit.Idset.t
(** [sync_units s] is the units on which [s] synchronizes. *)

(** {1:kind Operation kinds} *)

type kind =
| Spawn of spawn
| Read of read
| Write of write
| Copy_file of copy_file
| Delete of delete
| Mkdir of mkdir
| Sync of sync (** *)
(** The type for asynchronous operation kinds. *)

val kind_to_string : kind -> string
(** [kind_to_string k] is a short string for the kind [k]. *)

val kind : t -> kind
(** [kind o] is [o]'s kind. *)

val get_spawn : t -> spawn
(** [get_spawn o] is [o]'s spawn operation. @raise Invalid_argument
    if [o] is not a spawn operation. *)

val get_read : t -> read
(** [get_read o] is [o]'s read operation. @raise Invalid_argument
    if [o] is not a read operation. *)

val get_write : t -> write
(** [get_write o] is [o]'s write operation. @raise Invalid_argument
    if [o] is not a write operation. *)

val get_copy_file : t -> copy_file
(** [get_copy_file o] is [o]'s file copy operation. @raise Invalid_argument
    if [o] is not a file copy operation. *)

val get_delete : t -> delete
(** [get_delete o] is [o]'s delete operation. @raise Invalid_argument
    if [o] is not a delete operation. *)

val get_mkdir : t -> mkdir
(** [get_mkdir o] is [o]'s mkdire operation. @raise Invalid_argument
    if [o] is not a mkdir operation. *)

val get_sync : t -> sync
(** [get_sync o] is [o]'s sync operation. @raise Invalid_argument
    if [o] is not a sync operation. *)

(** {1:preds Predicates} *)

val cycle : t -> t -> (B0_fpath.t * B0_fpath.t) option
(** [cycle o0 o1] is [Some (r0, r1)] with [r0] a file read by [o0]
    and written by [o1] and [r1] a file read by [o1] and written by
    [o0] or [None] if there no such two files. *)

val equal : t -> t -> bool
(** [equal o0 o1] is [true] iff [o0] and [o1] are the same operation. *)

val compare : t -> t -> int
(** [compare o0 o1] is a total order on operation compatible with {!equal}. *)

val compare_exec_start_time : t -> t -> int
(** [compare o0 o1] is a total order on operations according to their
    {!exec_start_time}. *)

(** {1:pretty Pretty printing} *)

val pp : t B0_fmt.t
val dump : t B0_fmt.t
val pp_spawn_fail : t B0_fmt.t
val pp_log_line : t B0_fmt.t
val pp_long : t B0_fmt.t

(** {1:setmap Sets and maps} *)

type set

module Set : sig
  include Set.S with type elt := t
                 and type t = set
end

type +'a map

module Map : sig
  include Map.S with type key := t
                 and type 'a t := 'a map
  val dom : 'a map -> set
  val of_list : (t * 'a) list -> 'a map
  type 'a t = 'a map
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers

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
