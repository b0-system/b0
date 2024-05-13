(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let exit_b0_file_error = 121 (* See B0_driver.Exit.b0_file_error *)

(* Names *)

type name = string
type qualified_name = string
let pp_name = Fmt.code

let sep = "." (* Note this has to be one byte. *)
let lib_root = sep

let is_name_valid name =
  name <> "" && match String.index_opt name sep.[0] with
  | None -> true | Some _ -> false

(*
let check_scope_name name = match String.index_opt name sep.[0] with
| None -> name
| Some _ -> Fmt.invalid_arg "Illegal scope name %S: cannot contain %s" name sep
*)

(* Errors *)

exception Error of string
let raise_error fmt = Fmt.kstr (fun s -> raise (Error s)) fmt

let raise_no_scope_error ~kind ~name =
  raise_error "No open file or library scope to define %s %a" kind pp_name name

(* Scopes *)

type file =
  { qname : qualified_name; (* The fully qualified name scope. *)
    file : Fpath.t;
    dir : Fpath.t }

type lib =
  { module' : string;
    qname : qualified_name; }

type t = (* XXX add a case for Root ? *)
| Lib of lib
| File of file * file list (* parents, from nearest to root *)

let file = function File (file, _) -> Some file.file | Lib _ -> None
let dir = function File (file, _) -> Some file.dir | Lib _ -> None

let is_root = function File ({ qname = ""}, _) -> true | _ -> false
let path = function
| Lib lib -> String.split_on_char '.' lib.qname
| File ({ qname }, _) ->
    if qname = "" then [] else String.split_on_char '.' qname

let raise_invalid_name_error ~kind ~name = match name with
| "" -> raise_error "The empty string is not a valid %s name" kind
| name ->
    raise_error
      "%a is not a valid %s name, dots are not allowed" pp_name name kind

let raise_duplicate_error ~kind ~name =
  raise_error "%s %a already defined in scope."
    (String.Ascii.capitalize kind) pp_name name

let qualify_name scope ~kind name = match scope with
| File ({ qname; _ }, _) ->
    if not (is_name_valid name) then raise_invalid_name_error ~kind ~name else
    if qname <> "" then String.concat sep [qname; name] else name
  | Lib lib ->
      (* Allow to use lib scope name without [sep] as a name *)
      if name = "" then lib.qname else
      if not (is_name_valid name)
      then raise_invalid_name_error ~kind ~name
      else String.concat sep [lib.qname; name]

let make_unique_qualified_name scope ~defs ~kind name =
  let qname = qualify_name scope ~kind name in
  if String.Map.mem qname defs then raise_duplicate_error ~kind ~name else
  qname

(* Scope state *)

let current = ref None
let sealed = ref false
let list = ref []

let check_no_scope () = match !current with
| None -> ()
| Some Lib lib -> Fmt.invalid_arg "Unclosed library scope %s" lib.qname
| Some File (file, _) ->
    Fmt.invalid_arg "Unclosed file scope %s %a" file.qname Fpath.pp file.file

(* Sealing *)

exception After_seal of string

let seal () =
  (* XXX it would be nice to remove the set_uncaught_exception_handler
     added by [open_root]. Doable once we require OCaml 4.11 see
     https://github.com/ocaml/ocaml/issues/9248 *)
  check_no_scope ();
  sealed := true

let sealed () = !sealed
let raise_after_seal fmt = Fmt.kstr (fun s -> raise (After_seal s)) fmt

let raise_create_after_seal ~kind ~name =
  raise_after_seal
    "%s %a illegaly created after B0 file initialization."
    (String.Ascii.capitalize kind) pp_name name

(* Scoping *)

let current_scope_prefix () = match !current with
| None -> ""
| Some Lib lib -> lib.qname
| Some File (file, _) -> file.qname

let qualify_name_in_current ~kind name = match !current with
| None -> name
| Some scope -> qualify_name scope ~kind name

let close () = match !current with
| None -> invalid_arg "No scope to close"
| Some Lib _ -> current := None
| Some File (file, parents) ->
    list := (file.qname, file.file) :: !list;
    let parent = match parents with
    | [] -> None | file :: parents -> Some (File (file, parents))
    in
    current := parent

let open_lib ~module' lib =
  (* XXX why don't we check for dots here ? *)
  if lib = "" then Fmt.invalid_arg "Library scope name cannot be empty" else
  let qname = String.concat "" [lib_root; lib] in
  check_no_scope ();
  current := Some (Lib { module'; qname })

(* File scopes *)

let root_scope file = File ({ qname = ""; file; dir = Fpath.parent file}, [])
let current_is_root () = match !current with
| Some scope -> is_root scope | _ -> false

let name_list () =
  if sealed () then List.rev !list else
  invalid_arg "B0_scope.seal () has not been called yet."

let pp_uncaught_exn ppf (exn, bt) =
  let current_location use_bt =
    let location_in_backtrace file bt = match Printexc.backtrace_slots bt with
    | None -> None
    | Some slots ->
        (* find earliest slot that has [file] *)
        let rec loop file found slots i max = match i > max with
        | true -> found
        | false ->
            match Printexc.Slot.location slots.(i) with
            | None -> loop file found slots (i + 1) max
            | Some loc ->
                match String.equal loc.Printexc.filename file with
                | false -> loop file found slots (i + 1) max
                | true -> loop file (Some loc) slots (i + 1) max
        in
        loop (Fpath.to_string file) None slots 0 (Array.length slots - 1)
    in
    match !current with
    | None -> invalid_arg "no current scope"
    | Some Lib lib -> Fmt.str "Library %a:" pp_name lib.qname
    | Some File (file, _) ->
        let loc = match use_bt with
        | None -> "line 1"
        | Some bt ->
            match location_in_backtrace file.file bt with
            | None -> "line 1"
            | Some loc ->
                Fmt.str "line %d, characters %d-%d"
                  loc.Printexc.line_number
                  loc.Printexc.start_char
                  loc.Printexc.end_char
        in
        Fmt.str "File %S, %s:" (Fpath.to_string file.file) loc
  in
  let pp_error_label ppf () = Fmt.tty [`Fg `Red; `Bold] ppf "Error" in
  let pp_error ppf (err, bt) =
    Fmt.pf ppf "@[<v>%s@,%a: %s@]"
      (current_location (Some bt)) pp_error_label () err
  in
  let pp_uncaught ppf (exn, bt) =
    Fmt.pf ppf
      "@[<v>%s@,%a: B0 file raised an uncaught exception.@, @[<v>%a@]@]"
      (current_location None) pp_error_label () Fmt.exn_backtrace (exn, bt)
  in
  match exn with
  | Error err -> pp_error ppf (err, bt)
  | exn -> pp_uncaught ppf (exn, bt)

let open_root file =
  let catch_exn exn bt =
    Fmt.epr "@[%a@]@." pp_uncaught_exn (exn, bt);
    Stdlib.exit exit_b0_file_error
  in
  current := Some (root_scope file);
  (* XXX we want style ! But we didn't setup the driver config yet :-(
     We should look Sys.argv for --color and the B0_COLOR env var. Forcing
     for now, this will be set again later by B0_driver.Cli.conf. *)
  Fmt.set_tty_cap ~cap:`Ansi ();
  Printexc.record_backtrace true;
  Printexc.set_uncaught_exception_handler catch_exn

let open_file name file = match !current with
| Some File ({ qname; _ } as parent, parents) ->
    let qname = if qname <> "" then String.concat sep [qname; name] else name in
    let new' = { qname; file; dir = Fpath.parent file } in
    current := Some (File (new', parent :: parents))
| _ ->
    Fmt.invalid_arg
      "No root scope found to open scope %s for file %a" name Fpath.pp file

(* Current *)


let current () = !current

let current_make_unique_qualified_name ~defs ~kind name = match current () with
| Some scope ->
    let qname = make_unique_qualified_name scope ~defs ~kind name in
    qname, scope
| None when sealed () ->
    raise_create_after_seal ~kind ~name
| None ->
    raise_no_scope_error ~kind ~name
