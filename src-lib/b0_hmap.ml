(*---------------------------------------------------------------------------
   Copyright (c) 2017 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

module type KEY = sig
  type 'a typed
  type 'a info
  type t = V : 'a typed -> t

  val v :
    ?loc:B0_def.loc -> ?doc:string -> string -> 'a B0_conv.t -> 'a info ->
    'a typed

  val conv : 'a typed -> 'a B0_conv.t
  val info : 'a typed -> 'a info
  val of_typed : 'a typed -> t

  include B0_def.S with type t:= t
end

module type KEY_INFO = sig
  type 'a t
  val key_kind : string
  val key_namespaced : bool
  val key_name_tty_color : B0_tty.color
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type MAP = sig
  type 'a key
  type t

  val empty : t
  val is_empty : t -> bool
  val mem : 'a key -> t -> bool
  val add : 'a key -> 'a -> t -> t
  val add_tag : bool key -> t -> t
  val singleton : 'a key -> 'a -> t
  val rem : 'a key -> t -> t
  val find : 'a key -> t -> 'a option
  val get : 'a key -> t -> 'a
  val get_or_suggest : 'a key -> t -> ('a, string list) Pervasives.result
  val flag : ?absent:bool -> bool key -> t -> bool

  type binding = B : 'a key * 'a -> binding
  val iter : (binding -> unit) -> t -> unit
  val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (binding -> bool) -> t -> bool
  val exists : (binding -> bool) -> t -> bool
  val filter : (binding -> bool) -> t -> t
  val cardinal : t -> int
  val any_binding : t -> binding option
  val get_any_binding : t -> binding

  val pp : t B0_fmt.t

  type encode_error = string * [ `Msg of string ]
  val encode : t -> (string * string) list * encode_error list

  type decode_error = string * [ `Msg of string | `Unknown ]
  val decode : (string * string) list -> t * decode_error list
end

module type S = sig
  module Key : KEY
  type 'a key = 'a Key.typed
  include MAP with type 'a key := 'a key
end

module Make (Key_info : KEY_INFO) () : S with type 'a Key.info = 'a Key_info.t =
struct

  module Key = struct

    (* N.B. Unique key names are mandated for serialization and done
       via B0_def.t values. This allows to retrieve keys accross
       program runs and program source changes. Key comparisons in a
       program run are still done on uniquely generated integers but
       those are senstitive to program source changes and thus cannot
       be used in serialization. *)

    type 'a info = 'a Key_info.t
    type t = V : 'a typed -> t
    and 'a typed =
      { def : B0_def.t;
        uid : int;
        tid : 'a B0_tid.t;
        conv : 'a B0_conv.t;
        info : 'a info;
        ekey : t }

    module Key = struct
      type ekey = t
      type t = ekey
      let def_kind = Key_info.key_kind
      let def_get (V k) = k.def
      let def_namespaced = Key_info.key_namespaced
      let def_name_tty_color = Key_info.key_name_tty_color
      let def_pp_info ppf (V k) =
        Key_info.pp (B0_conv.print k.conv) ppf k.info
    end

    include (B0_def.Make (Key) : B0_def.S_DEF with type t := t)

    (* All the following key creation functions are not thread-safe *)

    let uid =
      let id = ref (-1) in
      fun () -> incr id; !id

    let v (type a) ?loc ?doc n conv info =
      let def = def ?loc ?doc n in
      let uid = uid () in
      let tid = B0_tid.create () in
      let rec k = { def; uid; tid; conv; info; ekey = V k } in
      def_add k.ekey; k

    let conv k = k.conv
    let info k = k.info
    let of_typed k = k.ekey

    let equal (V k0) (V k1) =
      (Pervasives.compare : int -> int -> int) k0.uid k1.uid = 0

    let compare (V k0) (V k1) =
      (Pervasives.compare : int -> int -> int) k0.uid k1.uid
  end

  type 'a key = 'a Key.typed

  (* Maps *)

  module M = Map.Make (Key)

  type binding = B : 'a key * 'a -> binding
  type t = binding M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let mem k m = M.mem k.Key.ekey m
  let add k v m = M.add k.Key.ekey (B (k, v)) m
  let add_tag k m = M.add k.Key.ekey (B (k, true)) m
  let singleton k v = M.singleton k.Key.ekey (B (k, v))
  let rem k m = M.remove k.Key.ekey m

  let find : type a. a key -> t -> a option =
   fun k m -> match M.find k.Key.ekey m with
   | exception Not_found -> None
   | B (k', v) ->
       match B0_tid.equal k.Key.tid k'.Key.tid with
       | None -> None
       | Some B0_tid.Eq -> Some v

  let key_name k = Key.name (Key.of_typed k)

  let get k m = match find k m with
  | Some v -> v
  | None ->
      invalid_arg (B0_string.strf "Key '%s' not found in map" (key_name k))

  let get_or_suggest k m = match find k m with
  | Some v -> Ok v
  | None ->
      let keys = M.fold (fun _ (B (k, v)) acc -> (key_name k) :: acc) m [] in
      Error (B0_string.suggest keys (key_name k))

  let flag ?(absent = false) k m = match find k m with
  | None -> absent
  | Some v -> v

  let iter f m = M.iter (fun _ b -> f b) m
  let fold f m acc = M.fold (fun _ b acc -> f b acc) m acc
  let for_all p m = M.for_all (fun _ b -> p b) m
  let exists p m = M.exists (fun _ b -> p b) m
  let filter p m = M.filter (fun _ b -> p b) m
  let cardinal m = M.cardinal m
  let any_binding m = try Some (snd (M.choose m)) with Not_found -> None
  let get_any_binding m = try snd (M.choose m) with
  | Not_found -> invalid_arg "empty map"

  let pp_binding ppf (B (k, v)) =
    B0_fmt.pf ppf "@[%a: @[%a@]@]"
      Key.pp_name k.Key.ekey (B0_conv.print @@ Key.conv k) v

  let pp = B0_fmt.vbox @@ B0_fmt.iter iter pp_binding

  (* Serialization *)

  type encode_error = string * [ `Msg of string ]

  let encode m =
    let add (B (k, v)) (acc, fails) = match B0_conv.encode k.Key.conv v with
    | Error err -> acc, (key_name k, err) :: fails
    | Ok s -> (key_name k, s) :: acc, fails
    in
    fold add m ([], [])

  type decode_error = string * [ `Msg of string | `Unknown ]

  let decode bs =
    let rec loop m fails = function
    | [] -> m, fails
    | (kn, bytes) :: bs ->
        match Key.find kn with
        | None -> loop m ((kn, `Unknown) :: fails) bs
        | Some (Key.V k) ->
            match B0_conv.decode k.Key.conv bytes with
            | Error err -> loop m (((kn, err) :> decode_error) :: fails) bs
            | Ok v ->  loop (add k v m) fails bs
    in
    loop empty [] bs
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
