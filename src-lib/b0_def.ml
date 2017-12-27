(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Definition locations *)

let ns = ref "" (* current namespace prefix *)

module Loc = struct

  type t = None_ | File of B0_fpath.t | Lib of string

  let none = None_
  let b0 = Lib "b0"
  let lib l = Lib l
  let file f = File f
  let is_none s = (s = None_)
  let get_file = function File f -> f | _ -> assert false
  let find_file = function File f -> Some f | _ -> assert false
  let equal s0 s1 = (s0 = s1)
  let compare s0 s1 = Pervasives.compare s0 s1
  let pp ppf = function
  | None_ -> B0_fmt.tty_str [`Fg `Red] ppf "unknown"
  | File f -> B0_fpath.pp ppf f
  | Lib l -> B0_fmt.tty_str [`Fg `Green] ppf l

  (* Current root, location and namespacing *)

  let ns_of_sub root sub = match root with
  | None -> ""
  | Some root ->
      match sub with
      | None -> ""
      | Some sub ->
          let root = B0_fpath.to_string @@ B0_fpath.to_dir_path root in
          let subr = B0_fpath.to_string sub in
          match B0_string.is_prefix ~affix:root subr with
          | false -> ""
          | true ->
              let first = B0_string.length root in
              let last = B0_string.length subr - 2 (* rem slash *) in
              let sep_to_dot c = match Char.equal c B0_fpath.dir_sep_char with
              | true -> '.'
              | false -> c
              in
              B0_string.(map sep_to_dot @@ with_index_range ~first ~last subr)

  let root = ref None
  let get_root () = !root
  let set_root r = root := r

  let sub_root = ref None
  let get_sub_root () = !sub_root
  let set_sub_root sub = sub_root := sub; ns := ns_of_sub !root sub

  let current = ref None_
  let set_current s = current := s
  let get_current () = !current
end

type loc = Loc.t

(* Names *)

module Name = struct

  (* Namespaces *)

  let space () = !ns
  let spaced n = match !ns with | "" -> n | ns -> Printf.sprintf "%s.%s" ns n

  (* Renames *)

  exception Panic

  let rename l ~kind ~exists ~name ~fst ~snd =
    let log ppf () =
      Format.fprintf ppf "@[<v>%a: %s '%s' already defined in:@,%a@,@]"
        Loc.pp snd kind name Loc.pp fst
    in
    match B0_string.unique ~exists name with
    | Ok n -> B0_log.msg l (fun m -> m "%aRenamed to '%s'." log () n); n
    | Error (`Msg e) ->
        B0_log.err (fun m -> m "%aPanic: %s, failed to rename." log () e);
        raise Panic
end

(* Named definitions *)

let def_err_none =
  "Definition location cannot be Loc.none as the current location is Loc.none"

let def_err_must_none src =
  B0_string.strf
    "Definition location '%a' must be Loc.none as the current location is %a"
    Loc.pp src Loc.pp (Loc.get_current ())

type def = { name : string; loc : Loc.t; doc : string; }
type t = def

let nil = { name = "nil"; loc = Loc.none; doc = "" }
let name d = d.name
let loc d = d.loc
let doc d = d.doc

let v ?(loc = Loc.none) ?(doc = "Unknown.") name =
  let curr = Loc.get_current () in
  match Loc.is_none curr with
  | true ->
      if Loc.is_none loc then invalid_arg def_err_none else { name; loc; doc }
  | false ->
      if not (Loc.is_none loc) then invalid_arg (def_err_must_none loc) else
      { name; loc = curr; doc }

let equal d0 d1 = B0_string.equal d0.name d1.name
let compare_by_name d0 d1 = B0_string.compare d0.name d1.name

(* Named values *)

module type DEFINED = sig
  type t
  val def_kind : string
  val def_get : t -> def
  val def_namespaced : bool
  val def_name_tty_color : B0_tty.color
  val def_pp_info : t B0_fmt.t
end

module type S = sig
  type t
  val value_kind : string
  val name : t -> string
  val loc : t -> Loc.t
  val doc : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val compare_by_name : t -> t -> int
  val find : string -> t option
  val get : string -> t
  val get_or_suggest : string -> (t, string list) Pervasives.result
  val list : unit -> t list
  val pp_name_str : string B0_fmt.t
  val pp_name : t B0_fmt.t
  val pp_synopsis : t B0_fmt.t
  val pp_info : t B0_fmt.t
  val pp_info_ext : t B0_fmt.t -> t B0_fmt.t
end

module type S_DEF = sig
  include S
  val def : ?loc:Loc.t -> ?doc:string -> string -> def
  val def_add : t -> unit
  val def_rem : t -> unit
end

module Make (V : DEFINED) = struct

  let err_unknown n =
    let kind = B0_string.capitalize_ascii V.def_kind in
    B0_string.strf "%s '%s' unknown." kind n

  type t = V.t

  let value_kind = V.def_kind
  let name v = (V.def_get v).name
  let loc v = (V.def_get v).loc
  let doc v = (V.def_get v).doc
  let equal v0 v1 = equal (V.def_get v0) (V.def_get v1)
  let compare_by_name v0 v1 = compare_by_name (V.def_get v0) (V.def_get v1)
  let compare = compare_by_name

  let vals = ref B0_string.Map.empty
  let val_add v = vals := B0_string.Map.add (name v) v !vals
  let val_rem name = vals := B0_string.Map.remove name !vals
  let val_exists name = B0_string.Map.mem name !vals

  let undef def = val_rem def.name
  let def ?loc:l ?doc name =
    let name = match V.def_namespaced with
    | true -> Name.spaced name
    | false -> name
    in
    let def = v ?loc:l ?doc name in
    match B0_string.Map.find name !vals with
    | exception Not_found -> def
    | fst ->
        let fst = loc fst in
        let snd = def.loc in
        let kind = V.def_kind in
        let exists = val_exists in
        let name = Name.rename B0_log.Warning ~kind ~exists ~name ~fst ~snd in
        { def with name }

  let def_add v = val_add v
  let def_rem v = val_rem (name v)

  let list () = List.rev @@ B0_string.Map.fold (fun _ v vs -> v :: vs) !vals []

  let find n = match B0_string.Map.find n !vals with
  | v -> Some v
  | exception Not_found -> None

  let get n = match B0_string.Map.find n !vals with
  | v -> v
  | exception Not_found -> invalid_arg (err_unknown n)

  let get_or_suggest n = match B0_string.Map.find n !vals with
  | v -> Ok v
  | exception Not_found ->
      Error (B0_string.suggest (List.rev_map name (list ())) n)

  (* Pretty-printing *)

  let pp_doc ppf v = B0_fmt.text ppf (doc v)

  let pp_name_str ppf s = B0_fmt.tty_str [`Fg V.def_name_tty_color] ppf s
  let pp_name ppf v = pp_name_str ppf (name v)
  let pp_synopsis = B0_fmt.synopsis ~name:pp_name ~doc:pp_doc

  let pp_info_ext ext =
    let src ppf v = B0_fmt.field "loc" Loc.pp ppf (loc v)in
    let info ppf v =
      B0_fmt.pf ppf "%a%a@,%a" V.def_pp_info v ext v src v
    in
    B0_fmt.info ~name:pp_name ~doc:pp_doc info

  let pp_info = pp_info_ext B0_fmt.nop
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
