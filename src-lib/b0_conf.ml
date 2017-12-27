(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

type build_aim = [ `Build_os | `Host_os ]

let group_tty_color = `Blue
let key_tty_color = `Magenta
let preset_tty_color = `Cyan

module rec Group : sig
  type t
  val v : ?loc:B0_def.loc -> ?doc:string -> string -> t
  val none : t
  val keys : t -> Map.Key.t list
  val add_key : t -> Map.Key.t -> unit

  include B0_def.S with type t := t
end = struct
  type group = { def : B0_def.t; mutable keys : Map.Key.t list;}

  module Group = struct
    type t = group
    let def_kind = "key group"
    let def_get g = g.def
    let def_namespaced = true
    let def_name_tty_color = group_tty_color
    let def_pp_info ppf g =
      let sep = B0_fmt.unit ",@ " in
      let pp_keys ppf g =
        let keys = List.sort Map.Key.compare_by_name g.keys in
        B0_fmt.(box @@ list ~sep Map.Key.pp_name) ppf keys
      in
      B0_fmt.pf ppf "@,%a" (B0_fmt.field "keys" pp_keys) g
  end

  include B0_def.Make (Group)

  let v ?loc ?doc n =
    let def = def ?loc ?doc n in
    let g = { def; keys = [] } in
    def_add g; g

  let none = v "none" ~loc:B0_def.Loc.b0 ~doc:"Keys without group."
  let keys g = g.keys
  let add_key g k = g.keys <- k :: g.keys
end

and Key_info : sig
  type 'a value =
  | Const of 'a
  | Discover of
      [`Store of bool ] *
      (B0_env.t -> B0_env.build_aim -> 'a Map.key -> Map.t -> 'a result)

  val pp_value : 'a B0_fmt.t -> 'a value B0_fmt.t

  type 'a t = { group : Group.t; default : 'a value; }
  include B0_hmap.KEY_INFO with type 'a t := 'a t
end = struct
  type 'a value = (* FIXME maybe move that into its own module *)
  | Const of 'a
  | Discover of
      [`Store of bool ] *
      (B0_env.t -> B0_env.build_aim -> 'a Map.key -> Map.t -> 'a result)

  let pp_value pp_v ppf = function
  | Const v -> pp_v ppf v
  | Discover _ -> B0_tty.pp_str [`Fg `Green] ppf "discovered"

  type 'a t = { group : Group.t; default : 'a value; }
  let key_kind = "key"
  let key_namespaced = true
  let key_name_tty_color = key_tty_color
  let pp _ _ _ =
    (* if we try to use Group.pp_name to print the info fields the module rec
       fails, we redefine the printers Key.pp_info[_ext] in the Key module
       below *)
    ()
end

and Map : B0_hmap.S
with type 'a Key.info = 'a Key_info.t = B0_hmap.Make (Key_info) ()

(* Configuration *)

include (Map : B0_hmap.MAP
         with type t = Map.t and type 'a key = 'a Map.key)

(* Persistence *)

let codec = B0_codec.v ~id:"conf"
let load f = B0_codec.read codec f >>| fun l -> decode l
let save c f =
  let l, errs = encode c in
  B0_codec.write codec f l >>| fun () -> errs

(* Key value definition. *)

type 'a value = 'a Key_info.value
let const v = Key_info.Const v
let discover ?(store = true) f = Key_info.Discover (`Store store, f)

let value env aim k c d = match d with
| Key_info.Const v -> Ok v
| Key_info.Discover (_, f) -> f env aim k c

let value_const d = match d with
| Key_info.Const v -> Some v
| Key_info.Discover (_, f) -> None

let value_store = function
| Key_info.Const v -> false
| Key_info.Discover (`Store store, _) -> store

let pp_value = Key_info.pp_value

(* Keys *)

module Key = struct
  include Map.Key

  let v ?loc ?doc ?(group = Group.none) name conv ~default =
    let info = Key_info.{ group; default } in
    let k = Map.Key.v ?loc ?doc name conv info in
    Group.add_key group (of_typed k);
    k

  let default k = (info k).Key_info.default
  let group (V k) = (info k).Key_info.group

  let pp_info_ext ext ppf (V kt as k) =
    let fields ppf v =
      B0_fmt.pf ppf "@,%a@,%a%a"
        B0_fmt.(field "group" @@ Group.pp_name) (info kt).Key_info.group
        B0_fmt.(field "default" @@ pp_value (B0_conv.print (conv kt)))
        (info kt).Key_info.default
        ext k
    in
    pp_info_ext fields ppf k

  let pp_info = pp_info_ext B0_fmt.nop
end

let key = Key.v
let get_default env aim k c = value env aim k c (Key.default k)
let get_effective env aim k c =
  let dot_to_uscore = function '.' -> '_' | c -> c in
  let var_name k = B0_string.map dot_to_uscore @@ B0_string.uppercase_ascii k in
  (* FIXME this is the place where B0_HOST_C and B0_BUILD_C should also be
     consulted. I think these details should maybe be abstracted in Env.
     So that e.g. library users can use their own prefix. *)
  let var = "B0_C_" ^ (var_name (Key.(name (of_typed k)))) in
  let env_vars = B0_env.env env aim in
  match B0_string.Map.find var env_vars with
  | d -> B0_conv.parse (Key.conv k) d >>| fun v -> `Env, v
  | exception Not_found ->
      match find k c (* FIXME aim ? *) with
      | Some v -> Ok (`Conf, v)
      | None -> get_default env aim k c >>| fun v -> `Default, v

let keys c = Map.fold (fun (Map.B (k, v)) ks -> (Key.of_typed k :: ks)) c []

(* Presets *)

module Preset = struct

  (* Key value definition. *)

  type conf = t
  let const = const
  let discover = discover

  type binding = B : 'a key * 'a value -> binding

  type def = { doc : string; binding : binding; }

  let _def ?(doc = "Undocumented.") k v = { binding = B (k, v); doc }
  let def_key { doc; binding = B (k, _) } = Key.of_typed k
  let def_doc d = d.doc
  let def_binding d = d.binding
  let def_compare d0 d1 =
    let B (k0, _) = d0.binding in
    let B (k1, _) = d1.binding in
    Key.compare (Key.of_typed k0) (Key.of_typed k1)

  let pp_def ppf d =
    let B (k, v) = d.binding in
    let pp_value = Key_info.pp_value (B0_conv.print (Key.conv k)) in
    B0_fmt.pf ppf "@[%a: @[%a@]@]"
      Key.pp_name (Key.of_typed k) pp_value v

  (* Presets base definitions *)

  type preset =
    { def : B0_def.t;
      defs : def list; (* key definitions for the preset *) }

  module Preset = struct
    type t = preset
    let def_kind = "preset"
    let def_get p = p.def
    let def_namespaced = true
    let def_name_tty_color = preset_tty_color
    let def_pp_info ppf p = B0_fmt.cut ppf (); (B0_fmt.list pp_def) ppf p.defs
  end

  include B0_def.Make (Preset)

  let v ?loc ?doc name defs =
    let def = def ?loc ?doc name in
    let p = { def; defs } in
    def_add p; p

  let def = _def (* the include defines def for naming *)

  (* Presets *)

  let defs p = p.defs

  let key_name k = Key.name (Key.of_typed k)

  let find_def n p =
    let is_name { doc; binding = B (k, _) } = String.equal n (key_name k) in
    match List.find is_name p.defs with
    | exception Not_found -> None
    | def -> Some def

  let get_or_suggest_def n p = match find_def n p with
  | Some def -> Ok def
  | None ->
      let def_key_name { doc; binding = B (k, _) } = key_name k in
      let names = List.rev_map def_key_name p.defs in
      Error (B0_string.suggest names n)

  let keys p =
    let add_key ks { doc; binding = B (k, _) } = (Key.of_typed k) :: ks in
    List.rev (List.fold_left add_key [] p.defs)

  type def_error = string * [`Msg of string]

  let add env aim p c =
    let add_def (c, errs) def =
      let B (k, v) = def_binding def in
      match value env aim k c v with
      | Ok v -> (add k v c, errs)
      | Error e -> (c, (key_name k, e) :: errs)
    in
    List.fold_left add_def (c, []) (defs p)

  let rem p c =
    let rem_def acc def =
      let B (k, _) = def_binding def in
      rem k c
    in
    List.fold_left rem_def c (defs p)

  (* FIXME concat presets *)
end

let of_preset env aim p = Preset.add env aim p empty

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
