(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

let exit_b0_file_error = 121 (* See B0_driver.Exit.b0_file_error *)
let pp_error_str ppf () = Fmt.tty' [`Fg `Red; `Bold] ppf "Error"

(* Scopes *)

exception Err of string

module Scope = struct

  let sep = "."
  let lib_root = sep

  (* Names *)

  type name = string
  let pp_name = Fmt.(code string)

  (* Scopes *)

  type t = Nil | Lib of string | File of (string * Fpath.t * Fpath.t) list
  let current = ref Nil

  let list = ref []
  let sealed = ref false

  let name_list () =
    if !sealed then List.rev !list else
    invalid_arg "B0_def.Scope.seal () has not been called yet."

  let close () = match !current with
  | File ((pre, file, _) :: ss) ->
      list := (pre, file) :: !list;
      current := (match ss with [] -> Nil | ss -> (File ss));
  | Lib _ -> current := Nil
  | Nil -> invalid_arg "No scope to close"
  | File [] -> assert false

  let check_no_scope () = match !current with
  | Nil -> ()
  | Lib n -> Fmt.invalid_arg "Unclosed library scope %s" n
  | File ((pre, file, _) :: _) ->
      Fmt.invalid_arg "Unclosed file scope %s %a" pre Fpath.pp file
  | File [] -> assert false

  let open_lib lib =
    if lib = "" then Fmt.invalid_arg "Empty library scope name" else
    check_no_scope ();
    current := Lib (String.concat "" [lib_root; lib])

  let file = function
  | Nil | Lib _ | File [] -> None | File ((_, f, _) :: _) -> Some f

  let dir = function
  | Nil | Lib _ | File [] -> None | File ((_, _, d) :: _) -> Some d

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

  let current_to_loc_str use_bt = match !current with
  | Lib lib -> Fmt.str "Library %a:" Fmt.(code string) lib
  | File ((name, file, _) :: _) ->
      let loc = match use_bt with
      | None -> "line 1"
      | Some bt ->
          match location_in_backtrace file bt with
          | None -> "line 1"
          | Some loc ->
              Fmt.str "line %d, characters %d-%d"
                loc.Printexc.line_number
                loc.Printexc.start_char
                loc.Printexc.end_char
      in
      Fmt.str "File %S, %s:" (Fpath.to_string file) loc
  | Nil | File [] -> invalid_arg "no current scope"

  let err_error err bt =
    Fmt.str "@[<v>%s@,%a: %s@]"
      (current_to_loc_str (Some bt)) pp_error_str () err

  let err_uncaught exn bt =
    Fmt.str "@[<v>%s@,%a: B0 file raised an uncaught exception.@, @[<v>%a@]@]"
      (current_to_loc_str None) pp_error_str () Fmt.exn_backtrace (exn, bt)

  let open_root file =
    let catch_exn exn bt =
      let err = match exn with
      | Err err -> err_error err bt
      | exn -> err_uncaught exn bt
      in
      Fmt.epr "@[%s@]@." err;
      exit exit_b0_file_error
    in
    let setup_fmt () =
      (* XXX we want style ! But we didn't setup the driver config yet :-(
         We should look Sys.argv for --color and the B0_COLOR env var. Forcing
         for now, this will be set again later by B0_driver.Cli.conf. *)
      Fmt.set_tty_cap ~cap:`Ansi ()
    in
    current := File (["", file, Fpath.parent file]);
    setup_fmt ();
    Printexc.record_backtrace true;
    Printexc.set_uncaught_exception_handler catch_exn

  let open_file name file = match !current with
  | File ((pre, _, _) :: _ as ss) ->
      let pre = if pre <> "" then String.concat sep [pre; name] else name in
      current := File ((pre, file, Fpath.parent file) :: ss)
  | _ -> invalid_arg "Illegal scope context, no root"

  let is_root () = match !current with
  | File (["", _, _]) -> true | _ -> false

  let qualify_name n = match !current with
  | Lib prefix ->
      (* Allow to use lib scope name without [sep] as a name *)
      if n = "" then prefix else String.concat sep [prefix; n]
  | File (("", _, _) :: _) | Nil -> n
  | File ((prefix, _, _) :: _) -> String.concat sep [prefix; n]
  | File [] -> assert false

  let seal () =
    (* XXX it would be nice to remove the set_uncaught_exception_handler
       added by init. See https://github.com/ocaml/ocaml/issues/9248 *)
    check_no_scope ();
    sealed := true

  let current () = !current
  exception After_seal of string
end

(* Names *)

type t =
  { scope : Scope.t;
    name : string;
    basename : string;
    doc : string;
    meta : B0_meta.t }

type def = t
let scope d = d.scope
let file d = Scope.file d.scope
let scope_dir d = Scope.dir d.scope
let name d = d.name
let basename d = d.basename
let doc d = d.doc
let meta d = d.meta

(* Defining values *)

module type VALUE = sig
  type t
  val def_kind : string
  val def : t -> def
  val pp_name_str : string Fmt.t
end

module type S = sig
  type t
  val define : ?doc:string -> ?meta:B0_meta.t -> string -> def
  val def_kind : string
  val def : t -> def
  val name : t -> string
  val basename : t -> string
  val doc : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val meta : t -> B0_meta.t
  val mem_meta : 'a B0_meta.key -> t -> bool
  val has_tag : bool B0_meta.key -> t -> bool
  val find_meta : 'a B0_meta.key -> t -> 'a option
  val find_or_default_meta : 'a B0_meta.key -> t -> 'a
  val get_meta : 'a B0_meta.key -> t -> ('a, string) result
  val add : t -> unit
  val fold : (t -> 'a -> 'a) -> 'a -> 'a
  val list : unit -> t list
  val find : string -> t option
  val get : string -> t
  val get_or_suggest : string -> (t, t list) result
  val get_or_hint : string -> (t, string) result
  val get_list_or_hint :
    all_if_empty:bool -> string list -> (t list, string) result

  val scope_path : t -> string list
  val in_root_scope : t -> bool
  val in_current_scope : t -> bool
  val scope_dir : t -> Fpath.t option
  val scope_dir' : t -> (Fpath.t, string) result
  val in_scope_dir : t -> Fpath.t -> Fpath.t option
  val in_scope_dir' : t -> Fpath.t -> (Fpath.t, string) result

  val pp_name_str : string Fmt.t
  val pp_name : t Fmt.t
  val pp_doc : t Fmt.t
  val pp_synopsis : t Fmt.t
  val pp : t Fmt.t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Make (V : VALUE) = struct
  type t = V.t
  let def_kind = V.def_kind
  let def = V.def
  let name v = name (V.def v)
  let basename v = basename (V.def v)
  let doc v = doc (V.def v)
  let scope v = scope (V.def v)
  let equal v0 v1 = String.equal (name v0) (name v1)
  let compare v0 v1 = String.compare (name v0) (name v1)

  let meta v = meta (V.def v)
  let mem_meta k v = B0_meta.mem k (meta v)
  let has_tag k v = B0_meta.has_tag k (meta v)
  let find_meta k v = B0_meta.find k (meta v)
  let find_or_default_meta k v = B0_meta.find_or_default k (meta v)
  let get_meta k v = match find_meta k v with
  | Some v -> Ok v
  | None ->
      Fmt.error "%s %a does not define metadata %a"
        (String.Ascii.capitalize V.def_kind)
        V.pp_name_str (name v) B0_meta.Key.pp_name k

  let pp_name_str = V.pp_name_str
  let pp_name = Fmt.using name pp_name_str
  let pp_doc = Fmt.using doc (Fmt.tty' [])
  let pp_synopsis ppf v = Fmt.pf ppf "%a  %a" pp_name v pp_doc v
  let pp ppf v =
    let pp_non_empty ppf m = match B0_meta.is_empty m with
    | true -> () | false -> Fmt.pf ppf "@, %a" B0_meta.pp m in
    Fmt.pf ppf "@[<v>@[%a@]%a@]" pp_synopsis v pp_non_empty (meta v)

  let defs = ref String.Map.empty
  let add v = defs := String.Map.add (name v) v !defs

  let is_name name = String.for_all (fun c -> c <> '.') name
  let illegal_name_error name =
    Fmt.str "%a is not a legal %s name, dots are not allowed."
      Fmt.(code string) name V.def_kind

  let seal_error name =
    Fmt.str "%s %a illegaly created after B0 file initialization."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str name

  let duplicate_error name =
    Fmt.str "%s %a already defined in scope."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str name

  let err_undefined name =
    Fmt.str "%s %a undefined in scope."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str name

  let qualify_name name =
    if not (is_name name) then raise (Err (illegal_name_error name)) else
    Scope.qualify_name name

  let define ?(doc = "undocumented") ?(meta = B0_meta.empty) n =
    match !Scope.sealed with
    | true -> raise (Scope.After_seal (seal_error n))
    | false ->
        (* XXX with Printexc.get_callstack and a bit of munging
           we could maybe get the exact definition point. *)
        let scope = Scope.current () in
        let name = qualify_name n in
        match String.Map.mem name !defs with
        | true -> raise (Err (duplicate_error n))
        | false -> { scope; name; basename = n; doc; meta }

  let scoped_find name = String.Map.find_opt (Scope.qualify_name name) !defs
  let find = scoped_find
  let get name = match scoped_find name with
  | Some v -> v | None -> raise (Err (err_undefined name))

  let get_or_suggest name = match scoped_find name with
  | Some v -> Ok v
  | None ->
      let add_sugg k v acc =
        if String.edit_distance k name <= 2 then v :: acc else acc
      in
      Error (List.rev (String.Map.fold add_sugg !defs []))

  let get_or_hint candidate = match get_or_suggest candidate with
  | Ok _ as v -> v
  | Error suggs ->
      let kind ppf () = Fmt.pf ppf "%s" def_kind in
      let hint = Fmt.did_you_mean in
      let pp = Fmt.unknown' ~kind V.pp_name_str ~hint in
      Fmt.error "@[%a@]" pp (candidate, List.map name suggs)

  let fold f acc = match Scope.is_root () with
  | true ->
      let add _ v acc = f v acc in
      String.Map.fold add !defs acc
  | false ->
      let prefix = Scope.qualify_name "" in
      let add k v acc = if String.starts_with ~prefix k then f v acc else acc in
      String.Map.fold add !defs acc

  let list () = List.rev (fold List.cons [])

  let get_list_or_hint ~all_if_empty names =
    if all_if_empty && names = [] then Ok (List.sort compare (list ())) else
    let rec loop vs es = function
    | [] ->
        if es <> []
        then Error (String.concat "\n" (List.rev es))
        else Ok (List.rev vs)
    | n :: ns ->
        match get_or_hint n with
        | Ok v -> loop (v :: vs) es ns
        | Error e -> loop vs (e :: es) ns
    in
    loop [] [] names

  let scope_path v = match scope v with
  | Scope.File (["", _, _]) -> []
  | Scope.File ((pre, _, _) :: _) -> String.split_on_char '.' pre
  | Scope.Lib lib -> String.split_on_char '.' lib
  | Scope.File []
  | Scope.Nil -> assert false

  let in_root_scope v = match scope v with
  | Scope.File (["", _, _]) -> true | _ -> false

  let in_current_scope v =
    let prefix = Scope.qualify_name "" in
    String.starts_with ~prefix (name v)

  let scope_dir v = scope_dir (def v)
  let scope_dir' v = match scope_dir v with
  | None -> Fmt.error "%s %a has no scope directory." def_kind pp_name v
  | Some dir -> Ok dir

  let in_scope_dir v path = match scope_dir v with
  | None -> None | Some dir -> Some Fpath.(dir // path)

  let in_scope_dir' v path = match in_scope_dir v path with
  | Some v -> Ok v
  | None ->
      Fmt.error "%s %a has no scope directory, cannot lookup %a in it."
        def_kind pp_name v Fpath.pp path



  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

type value = V : (module S with type t = 'a) * 'a -> value
