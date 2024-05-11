(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std

(* Keys *)

module Key = struct
  type t = V : 'a typed -> t
  and 'a typed =
    { default : 'a option;
      doc : string;
      id : 'a Type.Id.t;
      name : string;
      pp_value : 'a Fmt.t;
      scope : B0_scope.t;
      untyped : t; }

  let[@inline] uid k = Type.Id.uid k.id

  let defs = ref String.Map.empty
  let add k = defs := String.Map.add k.name k.untyped !defs

  (* Typed keys *)

  let kind = "key"

  let make ?(doc = "undocumented") ?default name ~pp_value =
    let id = Type.Id.make () in
    let name, scope =
      B0_scope.current_make_unique_qualified_name ~defs:!defs ~kind name
    in
    let rec k = { default; doc; id; name; pp_value; scope; untyped }
    and untyped = V k in
    add k; k

  let make_tag ?doc name = make ?doc name ~default:false ~pp_value:Fmt.bool

  let name k = k.name
  let default k = k.default
  let get_default k = Option.get k.default
  let doc k = k.doc
  let pp_value k = k.pp_value

  (* Existential keys *)

  let equal (V k0) (V k1) = Int.equal (uid k0) (uid k1)
  let compare (V k0) (V k1) = Int.compare (uid k0) (uid k1)
  let compare_by_name (V k0) (V k1) = String.compare (name k0) (name k1)
  let pp_name_str = Fmt.tty' [`Fg `Yellow]
  let pp_name ppf k = pp_name_str ppf k.name
  let pp ppf (V k) = pp_name_str ppf k.name

  (* Lookup keys by name *)

  let find n = match String.Map.find n !defs with
  | exception Not_found -> None | k -> Some k

  let get n = match find n with
  | Some v -> v | None -> Fmt.invalid_arg "No meta key named %s" n

  let get_or_suggest n = match find n with
  | Some v -> Ok v
  | None ->
      let add_sugg k v acc =
        if String.edit_distance k n <= 2 then v :: acc else acc
      in
      Error (List.rev (String.Map.fold add_sugg !defs []))

  let get_or_hint n = match get_or_suggest n with
  | Ok _ as v -> v
  | Error suggs ->
      let kind = Fmt.any "meta key" and hint = Fmt.did_you_mean in
      let pp = Fmt.unknown' ~kind pp_name_str ~hint in
      let name (V k) = name k in
      Fmt.error "@[%a@]" pp (n, List.map name suggs)

  let fold f acc = match B0_scope.current_is_root () with
  | true ->
      let add _ v acc = f v acc in
      String.Map.fold add !defs acc
  | false ->
      let prefix = B0_scope.current_scope_prefix () in
      let add k v acc = if String.starts_with ~prefix k then f v acc else acc in
      String.Map.fold add !defs acc

  let list () = List.sort compare_by_name (fold List.cons [])

  let get_list_or_hint ~all_if_empty names =
    if all_if_empty && names = [] then Ok (list ()) else
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
end

type 'a key = 'a Key.typed

let err_no_default k =
  Fmt.invalid_arg "Key %a has no default value" Key.pp_name k

let err_no_such_key_name k =
  Fmt.invalid_arg "Key %a not found in map" Key.pp_name_str k

let err_no_such_key k =
  Fmt.invalid_arg "Key %a not found in map" Key.pp_name k

(* Metadadta *)

type binding = B : 'a key * 'a -> binding
module M = Map.Make (Int)
type t = binding M.t

let empty = M.empty

(* Predicates *)

let is_empty = M.is_empty
let mem k m = M.mem (Key.uid k) m
let has_tag : bool key -> t -> bool =
fun k m -> match Key.default k with
| None -> err_no_default k
| Some default ->
    match M.find_opt (Key.uid k) m with
    | None -> default
    | Some (B (k', v)) ->
        match Type.Id.provably_equal k.Key.id k'.Key.id with
        | Some Type.Equal -> v
        | None -> assert false

(* Adding and removing *)

let add k v m = M.add (Key.uid k) (B (k, v)) m
let tag k m = add k true m
let add_some k o m = match o with None -> m | Some v -> add k v m
let add_some_or_default k o m = match k.Key.default with
| None -> err_no_default k
| Some default -> add k (match o with None -> default | Some v -> v) m

let add_if_undef k v m =
  let update = function None -> Some (B (k, v)) | Some _ as b -> b in
  M.update (Key.uid k) update m

let override m ~by =
  let override _ _ by = Some by in
  M.union override m by

let remove k m = M.remove (Key.uid k) m

(* Lookup *)

let find : type a. a key -> t -> a option =
fun k m -> match M.find_opt (Key.uid k) m with
| None -> None
| Some (B (k', v)) ->
    match Type.Id.provably_equal k.Key.id k'.Key.id with
    | Some Type.Equal -> Some v
    | None -> assert false

let find_or_default : type a. a key -> t -> a =
fun k m -> match k.Key.default with
| None -> err_no_default k
| Some default ->
    match M.find_opt (Key.uid k) m with
    | None -> default
    | Some (B (k', v)) ->
        match Type.Id.provably_equal k.Key.id k'.Key.id with
        | Some Type.Equal -> v
        | None -> assert false

let get k m = match find k m with
| Some v -> v
| None -> err_no_such_key k

(* Bindings *)

let find_binding k m = M.find_opt (Key.uid k) m
let find_binding_by_name n m = match Key.find n with
| None -> None | Some (Key.V k) -> M.find_opt (Key.uid k) m

let get_binding k m = match find_binding k m with
| None -> err_no_such_key k | Some v -> v

let get_binding_by_name n m = match find_binding_by_name n m with
| None -> err_no_such_key_name n | Some v -> v

let pp_binding ppf (B (k, v)) =
  Fmt.field k.Key.name Fmt.id k.Key.pp_value ppf v

(* Traversing *)

let fold f m acc = M.fold (fun _ b acc -> f b acc) m acc

(* Formatting *)

let pp ppf m =
  (* The circumvolution here is to print in key name order. *)
  let add_binding _ (B (k, v) as b) acc = String.Map.add (Key.name k) b acc in
  let bindings = M.fold add_binding m String.Map.empty in
  (Fmt.vbox @@ Fmt.iter_bindings String.Map.iter (Fmt.using snd pp_binding))
    ppf bindings

let pp_non_empty ppf m = if M.is_empty m then () else (Fmt.cut ppf (); pp ppf m)

(* Standard keys *)

let string_list = Fmt.(list ~sep:sp string)
let string_list_key k ~doc = Key.make k ~doc ~pp_value:string_list
let string_key ?default k ~doc = Key.make k ?default ~doc ~pp_value:Fmt.string

(* End-user information. *)

let () = B0_scope.open_lib ~module':__MODULE__ "meta"

let authors = string_list_key "authors" ~doc:"Author list"
let description = string_key "description" ~doc:"Description"
let description_tags = string_list_key "descr-tags" ~doc:"Description tags"
let homepage = string_key "homepage" ~doc:"Homepage URI"
let issues = string_key "issues" ~doc:"Issue tracker URI"

type spdxid = string
let licenses = string_list_key "licenses" ~doc:"License list (SPDX ids)"
let maintainers = string_list_key "maintainers" ~doc:"Maintainer list"
let online_doc = string_key "online-doc" ~doc:"Online documentation URI"
let repo = string_key "repo" ~doc:"VCS source repository URI"
let synopsis =
  string_key "synopsis" ~default:"Undocumented" ~doc:"One line synopsis"

(* Entity tags *)

let bench = Key.make_tag "bench" ~doc:"Benchmarking entity"
let build = Key.make_tag "build" ~doc:"A build system entity"
let deprecated = Key.make_tag "deprecated" ~doc:"Deprecated entity"
let dev = Key.make_tag "dev" ~doc:"Development entity"
let doc = Key.make_tag "doc" ~doc:"Documentation entity"
let exe = Key.make_tag "exe" ~doc:"Executable entity"
let test = Key.make_tag "test" ~doc:"Testing entity"
let lib = Key.make_tag "lib" ~doc:"Library entity"
let public = Key.make_tag "public" ~doc:"Public entity"
let run =
  Key.make_tag "run" ~doc:"Entity should be part of a run in a given context."

let warning =
  string_key "warning" ~doc:"A warning shown when the entity is used"

let () = B0_scope.close ()
