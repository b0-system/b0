(*---------------------------------------------------------------------------
   Copyright (c) 2024 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Memo log.

    A log has all the build operations, the hashed
    file paths and a few global timings. *)

open B0_std

(** {1:logs Logs} *)

type t
(** The type for {!B0_memo} logs. *)

val of_memo : B0_memo.t -> t
(** [of_memo m] is a log for memo [m]. *)

val hash_fun : t -> string
(** [hash_fun] is the identifier of the hash function that was used. *)

val file_hashes : t -> Hash.t Fpath.Map.t
(** [file_hashes l] has all the files that were hashed through the memo. *)

val hash_dur : t -> Mtime.Span.t
(** [hash_dur l] is the time span spent hashing. *)

val total_dur : t -> Mtime.Span.t
(** [total_dur l] is the time spanning from {!B0_memo.make} to {!of_memo}. *)

val cpu_dur : t -> Os.Cpu.Time.Span.t
(** [cpu_dur l] is the CPU time spanning from {!B0_memo.make} to {!of_memo}. *)

val jobs : t -> int
(** [jobs l] is the maximal number of concurent spawns given to
    the build operation executor. *)

val ops : t -> B0_zero.Op.t list
(** [ops l] are the operations of the log. *)

(** {1:io IO} *)

val bincode : t B0_bincode.t
(** [bincode] is a binary codec for logs. *)

val write : Fpath.t -> t -> (unit, string) result
(** [write f l] writes log [l] to file [f]. *)

val read : Fpath.t -> (t, string) result
(** [read f] read a log from file [f]. *)
