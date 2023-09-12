(*---------------------------------------------------------------------------
   Copyright (c) 2018 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Lazy immutable stores.

    These stores provide access to immutable, lazily determined, typed
    key-value bindings.

    The value of a key in a store is defined either:
    {ul
    {- Explicitly when the store is {{!B0_store.create}created}.}
    {- Lazily on the first key {{!B0_store.get}access} via a key determination
       function
       specified at {{!B0_store.val-key}key creation time}.}}
    Once determined the value of a key in the store never changes.

    {b XXX.} Maybe move that at the B0 level. *)

open B0_std

(** {1:stores Stores} *)

type 'a key
(** The type for keys binding values of type ['a]. *)

type binding = B : 'a key * 'a -> binding (** *)
(** The type for store bindings. A key and its value. *)

type t
(** The type for stores. *)

val make : B0_memo.t -> dir:Fpath.t -> binding list -> t
(** [make memo ~dir bs] is a store with predefined bindings [bs].
    If a key is mentioned more than once in [bs] the last binding
    takes over. The store uses [memo] to determine other keys as
    {{!get}needed}.  [dir] is a scratch directory used by key determination
    functions to write memoized file outputs. *)

val memo : t -> B0_memo.t
(** [memo s] is [s]'s memo as given on {!create}. *)

val dir : t -> Fpath.t
(** [dir s] is the scratch directory of [s]. Key determination functions
    using this directory to write files should do so using nice file name
    prefixes (e.g. lowercased module or lib names) to avoid name
    clashes. *)

val key : ?mark:string -> (t -> B0_memo.t -> 'a Fut.t) -> 'a key
(** [key ~mark det] is a new key whose value is determined on
    {{!get}access} by the future:
{[
det s (Memo.with_mark mark (B0_store.memo s))
]}
    [mark] defaults to [""]. *)

val get : t -> 'a key -> 'a Fut.t
(** [get s k] is a future that dermines with the value of [k] in [s]. *)

(**/**)
val set : t -> 'a key -> 'a -> unit
(** [set s k v] sets value [k] to [v] in [s]. {b Warning.} In general
     this should not be used but it may be useful to initialize the
     store. In particular this will raise [Invalid_argument] if [k] is
     already set in [s]. *)
(**/**)
