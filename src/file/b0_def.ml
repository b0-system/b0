(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* Names *)

type t =
  { scope : B0_scope.t;
    name : string;
    basename : string;
    doc : string;
    meta : B0_meta.t }

type def = t
let scope d = d.scope
let file d = B0_scope.file d.scope
let scope_dir d = B0_scope.dir d.scope
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
  val mangle_basename : string -> string
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
  let mangle_basename s =
    try
      for i = 0 to String.length s - 1
      do if s.[i] = B0_scope.sep.[0] then raise Exit; done;
      s
    with
    | Exit -> String.map (function '.' -> '-' | c -> c) s

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
  let pp_doc = Fmt.using doc (Fmt.st [])
  let pp_synopsis ppf v = Fmt.pf ppf "%a  %a" pp_name v pp_doc v
  let pp ppf v =
    let pp_non_empty ppf m = match B0_meta.is_empty m with
    | true -> () | false -> Fmt.pf ppf "@, %a" B0_meta.pp m in
    Fmt.pf ppf "@[<v>@[%a@]%a@]" pp_synopsis v pp_non_empty (meta v)

  let defs = ref String.Map.empty
  let add v = defs := String.Map.add (name v) v !defs

  let raise_error_undefined name =
    B0_scope.raise_error "%s %a undefined in scope."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str name

  let define ?(doc = "undocumented") ?(meta = B0_meta.empty) name =
    (* XXX with Printexc.get_callstack and a bit of munging
       we could maybe get the exact definition point. *)
    let kind = V.def_kind in
    let qname, scope =
      B0_scope.current_make_unique_qualified_name ~defs:!defs ~kind name
    in
    { scope; name = qname; basename = name; doc; meta }

  let scoped_find name =
    String.Map.find_opt
      (B0_scope.qualify_name_in_current ~kind:V.def_kind name) !defs

  let find = scoped_find
  let get name = match scoped_find name with
  | Some v -> v | None -> raise_error_undefined name

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

  let fold f acc = match B0_scope.current_is_root () with
  | true ->
      let add _ v acc = f v acc in
      String.Map.fold add !defs acc
  | false ->
      let prefix = B0_scope.current_scope_prefix () in
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

  let scope_path v = B0_scope.path (scope v)
  let in_root_scope v = B0_scope.is_root (scope v)

  let in_current_scope v =
    let prefix = B0_scope.current_scope_prefix () in
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
