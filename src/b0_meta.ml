(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

(* Keys *)

module Key = struct
  type t = V : 'a typed -> t
  and 'a typed =
    { uid : int;
      tid : 'a Tid.t;
      name : string;
      doc : string;
      pp_value : 'a Fmt.t;
      untyped : t; }

  let by_name = ref String.Map.empty
  let ensure_unique n =
    let err_too_many n = Fmt.str
        "@[<v>Too many attempts to rename B0_meta.key %s to make it unique@,\
         There is a bug in the program.@]" n
    in
    if not (String.Map.mem n !by_name) then n else
    let rec loop n i =
      if i > 100 then err_too_many n else
      let r = Fmt.str "%s~%d" n i in
      if String.Map.mem r !by_name then loop n (i + 1) else r
    in
    loop n 1

  (* Typed keys *)

  let uid = let id = ref (-1) in fun () -> incr id; !id
  let v ?(doc = "undocumented") ~pp_value name =
    let uid = uid () and tid = Tid.create () and name = ensure_unique name in
    let rec k = { uid; tid; name; doc; pp_value; untyped } and untyped = V k in
    by_name := String.Map.add name untyped !by_name; k

  let pp_tag = Fmt.any "true"
  let tag ?doc name = v ?doc ~pp_value:pp_tag name

  let name k = k.name
  let doc k = k.doc
  let pp_value k = k.pp_value

  (* Existential keys *)

  let equal (V k0) (V k1) = k0.uid = k1.uid
  let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
  let pp_name_str = Fmt.tty_string [`Fg `Yellow]
  let pp_name ppf k = pp_name_str ppf k.name
  let pp ppf (V k) = pp_name_str ppf k.name

  (* Lookup keys by name *)

  let find n = match String.Map.find n !by_name with
  | exception Not_found -> None | k -> Some k

  let get n = match find n with
  | Some v -> v | None -> Fmt.invalid_arg "No key named %s" n

  let get_or_suggest n = match find n with
  | Some v -> Ok v
  | None ->
      let add_sugg k v acc =
        if String.edit_distance k n <= 2 then v :: acc else acc
      in
      Error (List.rev (String.Map.fold add_sugg !by_name []))

  let get_or_hint n = match get_or_suggest n with
  | Ok _ as v -> v
  | Error suggs ->
      let kind = Fmt.any "metadata key" and hint = Fmt.did_you_mean in
      let pp = Fmt.unknown' ~kind pp_name_str ~hint in
      let name (V k) = name k in
      Fmt.error "@[%a@]" pp (n, List.map name suggs)
end

type 'a key = 'a Key.typed

(* Bindings *)

type binding = B : 'a key * 'a -> binding
let pp_binding ppf (B (k, v)) = Fmt.field k.Key.name Fmt.id k.Key.pp_value ppf v

type bindings =
| [] : bindings
| ( :: ) : ('a key * 'a) * bindings -> bindings

(* Metadata *)

module M = Map.Make (Key)
type t = binding M.t

let v bs =
  let rec loop acc = function
  | [] -> acc
  | (k, v) :: bs -> loop (M.add k.Key.untyped (B (k, v)) acc) bs
  in
  loop M.empty bs

let empty = M.empty
let is_empty = M.is_empty
let mem k m = M.mem k.Key.untyped m
let add k v m = M.add k.Key.untyped (B (k, v)) m
let add_if_some k o m = match o with None -> m | Some v -> add k v m
let tag k m = add k () m
let rem k m = M.remove k.Key.untyped m
let find : type a. a key -> t -> a option =
  fun k m -> match M.find k.Key.untyped m with
  | exception Not_found -> None
  | B (k', v) ->
      match Tid.equal k.Key.tid k'.Key.tid with
      | None -> None
      | Some Tid.Eq -> Some v

let find_binding k m = match M.find k.Key.untyped m with
| exception Not_found -> None | b -> Some b

let find_binding_by_name n m = match Key.find n with
| None -> None
| Some k -> match M.find k m with exception Not_found -> None | b -> Some b

let err_no_such_key n = Fmt.invalid_arg "Key %s not found in map" n

let get k m = match find k m with
| Some v -> v | None -> err_no_such_key k.Key.name

let get_binding k m = match find_binding k m with
| Some v -> v | None -> err_no_such_key k.Key.name

let get_binding_by_name n m = match find_binding_by_name n m with
| None -> err_no_such_key n | Some v -> v

let fold f m acc = M.fold (fun _ b acc -> f b acc) m acc

(* Formatting *)

let pp = Fmt.vbox @@ Fmt.iter_bindings M.iter (Fmt.using snd pp_binding)
let pp_non_empty ppf m = match M.is_empty m with
| true -> () | false -> Fmt.cut ppf (); pp ppf m

(* Standard keys *)

let str_list = Fmt.(list ~sep:sp string)
let str_list_key k ~doc = Key.v k ~doc ~pp_value:str_list
let str_key k ~doc = Key.v k ~doc ~pp_value:Fmt.string

(* End-user information. *)

let authors = str_list_key "authors" ~doc:"Author list"
let description = str_key "description" ~doc:"Description"
let description_tags = str_list_key "descr-tags" ~doc:"Description tags"
let homepage = str_key "homepage" ~doc:"Homepage URI"
let issues = str_key "issues" ~doc:"Issue tracker URI"
let licenses = str_list_key "licenses" ~doc:"License list (SPDX ids)"
let maintainers = str_list_key "maintainers" ~doc:"Maintainer list"
let online_doc = str_key "online-doc" ~doc:"Online documentation URI"
let repo = str_key "repo" ~doc:"VCS source repository URI"
let synopsis = str_key "synopsis" ~doc:"one line synopsis"

(* Entity tags *)

let bench = Key.tag "bench" ~doc:"Benchmarking entity"
let build = Key.tag "build" ~doc:"A build system entity"
let dev = Key.tag "dev" ~doc:"Development entity"
let doc = Key.tag "doc" ~doc:"Documentation entity"
let exe = Key.tag "exe" ~doc:"Executable entity"
let test = Key.tag "test" ~doc:"Testing entity"
let lib = Key.tag "lib" ~doc:"Library entity"

(* Entity properties *)

let exe_name =
  let doc = "Executable name without platform specific extension" in
  str_key "exe-name" ~doc

let exe_file = (* FIXME *)
  let doc = "Absolute file path to a built executable." in
  let pp_value = Fmt.any "<built value>" in
  Key.v "exe-file" ~doc ~pp_value

(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers

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
