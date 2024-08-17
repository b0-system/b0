(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Name scopes.

    Scopes are used to track and scope the name of b0 definitions created
    by libraries and b0 files.

    {b TODO.}
    {ul
    {- Distinguish a private module or at least make sure which
       functions can be used by users.}
    {- This API is annoying, we likely want to surface the
       scope subtypes that are used internally.}} *)

open B0_std

(** {1:names Names} *)

type name = string
(** The type for scope names. These are non empty strings without
    a dot. *)

type qualified_name = string
(** The type for qualified scope names. There are {!sep} separated {!name}.
    Library scopes are prefixed by a dot. *)

val is_name_valid : name -> bool
(** [is_name_valid name] is [true] iff [name] contains no {!sep} and is
    not empty. *)

val pp_name : string Fmt.t
(** [pp_name] formats scope or qualified scope names. *)

val sep : string
(** [sep] is ["."] the separator for qualified names. *)

(** {1:scopes Scopes} *)

type t
(** The type for scopes. *)

val file : t -> Fpath.t option
(** [file s] is the file in which the scope is opened. *)

val dir : t -> Fpath.t option
(** [dir s] is the directory of [file s] (if any). *)

val is_root : t -> bool
(** [is_root s] is [true] if [s] is the file root scope. *)

val path : t -> string list
(** [path s] are the segments of the qualified name of [s].
    The segments of the root scope is [[]]. Library scopes
    have an initial [""] segment. [String.concat sep (path s)] is
    the qualified name of [s]. *)

val qualify_name : t -> kind:string -> string -> qualified_name
(** [qualify_name s ~kind name] qualifies name [name] for an entity of
    kind [kind] (used for error reporting) in scope [s]. Raises an
    error if {!is_name_valid}[ name] is [false]. However if [s] is a
    library scope then [name] can be empty. *)

val make_unique_qualified_name :
  t -> defs:'a String.Map.t -> kind:string -> string -> qualified_name
(** [make_unique_qualified_name] is like {!qualify_name}
    but it raises a duplicate definition error if the resulting qualified
    name is already defined in [defs]. *)

(** {1:scoping Scoping} *)

val current_scope_prefix : unit -> string


val current : unit -> t option
(** [current ()] is the current scope (if any). *)

val current_is_root : unit -> bool
(** [is_root ()] is [true] if we are in the root scope. *)

val qualify_name_in_current : kind:string -> name -> qualified_name
(** [qualify_name_in_current name] qualifies [name] in the current scope.
    This is [name] if there is no current scope. *)

val current_make_unique_qualified_name :
  defs:'a B0_std.String.Map.t ->
  kind:string -> name -> qualified_name * t
(** [current_make_unique_qualified_name ~defs ~kind name] creates
    a unique qualified name for [name] (as checked against [def]'s domain)
    in the current scope. This raises if there is no scope, if the scopes
    are sealed or if [name] is already defined in the current scope
    or if [name] is invalid. *)

val close : unit -> unit
(** [close ()] closes the last opened scope. *)

(** {2:library Library scopes} *)

val open_lib : module':string -> name -> unit
(** [open_lib ~module':__MODULE__ l] opens a scope for library [l].
    Must be called before making any static definition in a
    library. Library scopes don't nest, no other scope can be opened
    before a {!close}. [module'] is used to help users to identify the
    module to lookup for documentation. This is typically used at
    the top of your module and the end of your module has a {!close}.
 {[
 let () = B0_scope.open_lib ~module':__MODULE__ "mydeflib"
 …
 let () = B0_scope.close ()
]}
*)

(** {2:b0_file b0 file scopes}

    {b Note.} This is used by the implementation of the driver API, if
    you are fiddling with this you are likely doing something
    wrong. *)

val name_list : unit -> (qualified_name * Fpath.t) list
(** [name_list ()] is the list of file scopes names tupled with the
    file that defines it. This function can only be called once
    definitions are {!seal}ed otherwise it raises
    [Invalid_argument]. *)

val open_root : Fpath.t -> unit
(** [open_root file] initializes b0 file scoping and opens a root scope
    for the root b0 file at the {e absolute} file path [file].

    Only file scope can be opened from now on.

    This installs a {!Printexc.set_uncaught_exception_handler} to
    handle uncaught and {{!section-error}definition errors}. If that happens
    the error is logged and the program {!Stdlib.exit}s with
    {!B0_driver.Exit.b0_file_error}. *)

val open_file : name -> Fpath.t -> unit
(** [open' name] opens a scope named [name] (unqualified) to add the
    definitions of the {e absolute} file path [file].

    {b Warning.} Scope unicity is not checked by the module this is
    expected to be handled by the client.*)

(** {1:error Erroring} *)

(*
exception Error of string
(** Exception thrown if a definition error occurs. This can be due to
    {ul
    {- Duplicate name.}
    {- Malformed name.}}
    The argument is an error message to print as is. The
    backtrace should point to the redefinition (it is unfortunately
    difficult to keep track of the previous definition). *)
*)

val raise_error : ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [raise_error fmt …] can be used if an unrecoverable b0 file user
    error occurs in a scope (e.g. duplicate definition). The given
    error message is printed and the program exits with
    {!B0_driver.Exit.b0_file_error}. *)

val raise_no_scope_error : kind:string -> name:string -> 'a
(** [raise_no_scope_error ~kind ~name] raises an error
    if an entity named [name] of kind [kind] is created without an open
    scope.

    {b Note.} This is borderline [Invalid_argument] though. *)

val raise_invalid_name_error : kind:string -> name:string -> 'a
(** [raise_invalid_name_error ~kind ~name] raises an error
    for an invalid name [name] (as per {!is_name_valid}) for an entity
    of kind [kind]. *)

val raise_duplicate_error : kind:string -> name:string -> 'a
(** [raise_duplicate_error ~kind ~name] raises an error for
    a dupliciate name [name] for an entity of kind [kind]. *)

(** {1:sealing Sealing}

    Scope sealing allows to make sure no definitions are unscoped
    or happen dynamically. *)

exception After_seal of string
(** Exception that can be used if definitions are made after {!seal}
    was invoked. The argument is an error message to print. The
    backtrace should point to the location of the illegal
    definition. See {!raise_after_seal} *)

val seal : unit -> unit
(** [seal ()] prevents further scope definitions from being made. This
    function is called at the end of the root b0 file. Since it comes
    last during linking the library scopes have already been established. *)

val sealed : unit -> bool
(** [sealed ()] is [true] if [seal] has been called. *)

val raise_after_seal : ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [raise_after_seal fmt …] can be used to bail out with an {!After_seal}
    exceptions if something happens after seal that should not have in
    a scope the given error message is printed and the program exits
    with {!B0_driver.Exit.b0_file_error}. *)

val raise_create_after_seal : kind:string -> name:string -> 'a
(** [raise_create_after_seal ~kind ~name] raises an {!After_seal}
    error if a entity named [name] of kind [kind] was created after
    scope sealing. *)
