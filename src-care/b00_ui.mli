(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** User interaction fragments.

    User interaction fragments for devising build tools. *)

(** {1:ui User interaction fragments} *)

open B0_std
open Cmdliner

(** {!Cmdliner} fragments. *)
module Cli : sig

  (** {1:out_fmt Specifying output formats} *)

  type out_fmt = [ `Normal | `Short | `Long ]
  (** The type for specifying output format details. *)

  val out_fmt :
    ?docs:string -> ?short_opts:string list -> ?long_opts:string list ->
    unit -> out_fmt Term.t
  (** [out_fmt ~short_opts ~long_opts ()] are mutually exclusive options
      to specify short and long output format, without options this is
      [`Normal]. [short_opts] defaults to [["s"; "short"]] and
      [long_opts] default to [["l"; "long"]]. [docs] is the manual section
      in which options are documtend. *)
end

(** {!B000.File_cache} interaction. *)
module File_cache : sig

  (** {1:high-level High-level commands.}

      These commands act on a cache directory. They avoid to create
      it via {!B00.File_cache.create} if it doesn't exists and mostly
      return [Ok ()] in these cases. *)

  val delete :
    dir:Fpath.t -> [ `All | `Keys of B000.File_cache.key list ] ->
    (unit, string) result
  (** [delete dir keys] deletes [keys] in [dirs] if an explicit key
      does not exist in [dir] a {!Log.warn} is issued. If [`All] is
      specified [dir] is deleted and recreated. *)

  val gc : dir:Fpath.t -> (unit, string) result
  (** [gc dir] deletes unused keys via {!B00.File_cache.delete_unused}. *)

  val size : dir:Fpath.t -> (unit, string) result
  (** [size dir] shows statistics about the file cache on stdout
      via {!B00.File_cache.Stats.pp}. *)

  val trim :
    dir:Fpath.t -> max_byte_size:int -> pct:int -> (unit, string) result
    (** [trim dir ~max_byte_size ~pct] trims the cache using
        {!B00.File_cache.trim_size}. *)

  (** {1:cli Cli fragments} *)

  val key_arg : B000.File_cache.key Cmdliner.Arg.conv
  (** [key_arg] is an argument converter for cache keys. *)

  val keys_none_is_all :
    ?pos_right:int -> unit ->
    [ `All | `Keys of B000.File_cache.key list ] Cmdliner.Term.t
   (** [keys_none_is_all ~pos_right ()] are the keys at the right
       of position [pos_right] (defaults is all positional arguments).
       If none is specified this is [`All]. *)
end

(** {!B000.Op} interaction. *)
module Op : sig

  val is_selected :
    reads:Fpath.t list -> writes:Fpath.t list -> ids:B000.Op.id list ->
    hashes:Hash.t list -> groups:string list -> B000.Op.t -> bool
  (** [is_selected ~reads ~writes ~ids ~hashes ~groups o] is [true]
      iff [o] reads a file in [reads] or writes a file in [writes]
      or has its id in [ids], or has its hash in [hashes] or has
      is [group] in [groups] or if all these lists are empty. *)

  val read_write_indexes :
    B000.Op.t list -> B000.Op.Set.t Fpath.Map.t * B000.Op.Set.t Fpath.Map.t
  (** [read_write_indexes ops] is [reads, writes] with [reads] mapping
      file path to operations that reads them and [writes] mapping file
      paths to operations that write them. *)

  val find_needs :
    ?acc:B000.Op.Set.t -> recursive:bool -> writes:B000.Op.Set.t Fpath.Map.t ->
    B000.Op.Set.t -> B000.Op.Set.t
  (** [find_needs ~recursive ~writes ~acc ops] add to [acc] (defaults
      to {!B00.Op.Set.empty}) the set of operations in the write index
      [writes] that need to be executed for the set of operations
      [ops] to be able to proceed. If [recursive] is [false] only direct
      dependencies are reported. *)

  val find_enables :
    ?acc:B000.Op.Set.t -> recursive:bool ->
    reads:B000.Op.Set.t Fpath.Map.t -> B000.Op.Set.t -> B000.Op.Set.t
    (** [find_enables ~recursive ~writes ~acc ops] add to [acc]
      (defaults to {!B00.Op.Set.empty}) the set of operations in the
      read index [reads] that are enabled by the set of operations
      [ops]. If [recursive] is [false] only direct dependencies are
        reported. *)

  val filter :
    revived:bool option ->
    statuses:[ `Aborted | `Done | `Failed | `Waiting ] list ->
    kinds:[ `Copy | `Delete | `Mkdir | `Notify | `Read | `Spawn | `Wait_files
          | `Write ] list ->
    B000.Op.t -> bool
  (** [filter ~revived ~statuses ~kinds] is an operation filter for the
      given filters. *)

  val order :
    by:[`Create | `Dur | `Wait | `Start] -> B000.Op.t list -> B000.Op.t list
  (** [order ~by ops] orders [ops] by [by] time. *)

  val select :
    reads:Fpath.t list -> writes:Fpath.t list -> ids:B000.Op.id list ->
    hashes:Hash.t list -> groups:string list -> needs:bool -> enables:bool ->
    recursive:bool -> revived:bool option ->
    statuses:[`Aborted | `Done | `Failed | `Waiting ] list ->
    kinds:[ `Copy | `Delete | `Notify | `Mkdir | `Read | `Spawn | `Wait_files
          | `Write ] list ->
    order_by:[ `Create | `Dur | `Wait | `Start ] -> B000.Op.t list ->
    B000.Op.t list

  val select_cli :
    ?docs:string -> unit -> (B000.Op.t list -> B000.Op.t list) Cmdliner.Term.t

  val select_man : Cmdliner.Manpage.block list
end

(** {!B00.Memo} interaction. *)
module Memo : sig

  (** {1:feedback Memo feedback} *)

  val pp_leveled_feedback :
    ?sep:unit Fmt.t -> ?op_howto:B000.Op.t Fmt.t -> show_op:Log.level ->
    show_ui:Log.level -> level:Log.level ->
    [B00.Memo.feedback | B000.File_cache.feedback | B000.Exec.feedback] Fmt.t
  (** [pp_leveled_feedback ~sep ~op_howto ~show_spawn_ui ~show_success ~level
      ppf] formats memo feedback on [ppf] followed by [sep] iff something
      is printed (defaults to {!Fmt.flush_nl}).
      {ul
      {- {!Log.Quiet} formats nothing}
      {- {!Log.Debug} report all operations with {!B000_conv.Op.pp_short_ui}.}}
      {ul
      {- [show_ui] is the level at which any completed operation
         gets logged with {!B000_conv.Op.pp_ui}.}
      {- [show_op] is the level at which any completed operation gets
         logged with {!B000_conv.Op.pp_short_ui}}}
      The formatter [op_howto] should format a way to got more information
      about an operation, default to {!nop}. *)

  val pp_error :
    ?sep:unit Fmt.t -> ?read_howto:Fpath.t Fmt.t ->
    ?write_howto:Fpath.t Fmt.t -> unit ->  B00.Memo.error Fmt.t
  (** [pp_error ~read_howto ~write_howto] formats a memo
      error followed by [sep] iff somethings is printed (defaults
      to {!Fmt.flush_nl}). The errors are formatted as follows:
      {ul
      {- {!B00.Memo.Failures} formats {!Fmt.nop}.}
      {- {!B00.Memo.Never_became_ready} formats each file
         prefixing it with [op_reading_howto].}
      {- {!B00.Memo.Cycle}, formats the operations of the cycle.}} *)

  (** {1:dirs_files Specifying directories and files} *)

  val b0_dir_name : string
  (** [b0_dir_name] is ["_b0"] the default b0 directory name. *)

  val cache_dir_name : string
  (** [cache_dir_name] is [".cache"] the default cache directory name
      in the [b0] directory. *)

  val trash_dir_name : string
  (** [trash_dir_name] is [".trash"] the default trash directoy name
      in the [b0] directory. *)

  val log_file_name : string
  (** [log_file_name] is [".log"] the default log file name in
      the [b0] directory. *)

  val b0_dir_env : string
  (** [b0_dir_env] is ["B0_DIR"]. *)

  val cache_dir_env : string
  (** [b0_dir_env] is ["B0_CACHE_DIR"]. *)

  val log_file_env : string
  (** [b0_dir_env] is ["B0_LOG_FILE"]. *)

  val b0_dir :
    ?docs:string -> ?doc:string -> ?doc_none:string -> ?env:Cmdliner.Arg.env ->
    unit -> Fpath.t option Term.t
  (** [b0_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 directory.
      {ul
      {- [docs] is where the option is documented, defaults to
         {!Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!b0_dir_env}.}} *)

  val cache_dir :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Arg.env -> unit -> Fpath.t option Term.t
  (** [cache_dir ~doc_none ~docs ~doc ~env] is a cli interface for specifying
      a b0 cache directory.
      {ul
      {- [opts] are the cli options to specify it.}
      {- [docs] is where the option is documented, defaults to
         {!Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!cache_dir_env}.}} *)

  val log_file :
    ?opts:string list -> ?docs:string -> ?doc:string -> ?doc_none:string ->
    ?env:Cmdliner.Arg.env -> unit -> Fpath.t option Term.t
  (** [log_file ~doc_none ~docs ~doc ~env] is a cli interface for
      specifing a b0 log file.
      {ul
      {- [opts] are the cli options to specify it.}
      {- [docs] is where the option is documented, defaults to
         {!Manpage.s_common_options}}
      {- [doc] is a doc string.}
      {- [doc_none] describes how the value is determined if the term is
         evaluates to [None].}
      {- [env] is a variable that can be used to override the default
         value, defaults to {!cache_dir_env}.}} *)

  val get_b0_dir :
    cwd:Fpath.t -> root:Fpath.t -> b0_dir:Fpath.t option -> Fpath.t
  (** [get_b0_dir ~cwd ~root ~b0_dir] determines a b0 directory. If
      [b0_dir] is [Some d] then this is [Fpath.(cwd // d)]. If [None]
      then this is [Fpath.(root / b0_dir_name)]. *)

  val get_cache_dir :
    cwd:Fpath.t -> b0_dir:Fpath.t -> cache_dir:Fpath.t option -> Fpath.t
  (** [get_cache_dir ~cwd ~b0_dir ~cache_dir] determines a cache directory.
      If [cache_dir] is [Some d] then this is [Fpath.(cwd // d)]. If [None]
      then this is [Fpath.(b0_dir / cache_dir)]. *)

  val get_trash_dir :
    cwd:Fpath.t -> b0_dir:Fpath.t -> trash_dir:Fpath.t option -> Fpath.t
  (** [get_trash_dir ~cwd ~b0_dir ~trash_dir] dtermiens a trash directory.
      If [trash_dir] is [Some d] then this is [Fpath.(cwd // d]. If
      [None] then this is [Fpath.(b0_dir /trash_dir)]. *)

  val get_log_file :
    cwd:Fpath.t -> b0_dir:Fpath.t -> log_file:Fpath.t option -> Fpath.t
  (** [get_log_file ~cwd ~b0_dir ~log_file] determines a log file.
      If [log_file] is [Some f] then this is [Fpath.(cwd // f)]. If [None]
      then this is [Fpath.(b0_dir /log_file)]. *)

  (** {1:build Build parameters} *)

  val jobs : ?docs:string -> ?env:Arg.env -> unit -> int option Term.t
  (** [jobs] is a cli interface for specifying the maximal number of
      commands to spawn concurrently. *)

  val find_jobs : jobs:int option -> unit -> int
  (** [fin_jobs jobs] determines a maximal number of spans. This is
      either, in order, [jobs] or {!B0_machine.logical_cpu_count} or
      [1]. *)

  (** {1:build_log Build log} *)

  module Log : sig

    type info
    val write_file : Fpath.t -> B00.Memo.t -> (unit, string) result
    val read_file : Fpath.t -> (info * B000.Op.t list, string) result

    type out_fmt = [`Long | `Normal | `Short | `Trace_event | `Stats ]
    val out : out_fmt -> (out_fmt * (info * B000.Op.t list -> unit))
    val out_fmt_cli :
      ?docs:string -> unit ->
      (out_fmt * (info * B000.Op.t list -> unit)) Cmdliner.Term.t

    val pp_stats : (info * B000.Op.t list) Fmt.t
    (** [pp_stats] formats statistics about the memoizer. *)
  end
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
