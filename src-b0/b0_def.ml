(*---------------------------------------------------------------------------
   Copyright (c) 2020 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std

let exit_b0_file_error = 121 (* See B0_driver.Exit.b0_file_error *)
let pp_error_str ppf () = Fmt.tty_string [`Fg `Red; `Bold] ppf "Error"

(* Scopes *)

exception Err of string

module Scope = struct
  type t = Nil | Lib of string | File of (string * Fpath.t * Fpath.t) list
  let current = ref Nil
  let sealed = ref false

  (* Library scopes *)

  let lib lib = current := Lib (String.concat "." ["lib"; lib; ""])

  (* File scopes *)

  let file = function
  | Nil | Lib _ | File [] -> None
  | File ((_, f, _) :: _) -> Some f

  let dir = function
  | Nil | Lib _ | File [] -> None
  | File ((_, _, d) :: _) -> Some d

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

  let root file  =
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
      Fmt.set_tty_styling_cap `Ansi
    in
    current := File (["", file, Fpath.parent file]);
    setup_fmt ();
    Printexc.record_backtrace true;
    Printexc.set_uncaught_exception_handler catch_exn

  let is_root () = match !current with
  | File (["", _, _]) -> true | _ -> false

  let open' name file = match !current with
  | File ((pre, _, _) :: _ as ss) ->
      let pre = String.concat "" [pre; name; "."] in
      current := File ((pre, file, Fpath.parent file) :: ss)
  | _ -> invalid_arg "illegal scope context, no root"

  let close () = match !current with
  | File (s :: ss) -> current := File ss
  | _ -> invalid_arg "no scope to close"

  let qualify_name n =
    let prefix = match !current with
    | Lib n -> n | File ((pre, _, _) :: _) -> pre
    | File [] | Nil -> invalid_arg "no scope"
    in
    String.concat "" [prefix; n]

  let current () = !current

  let seal () =
    (* XXX it would be nice to remove the set_uncaught_exception_handler
       added by init. See https://github.com/ocaml/ocaml/issues/9248 *)
    sealed := true

  exception After_seal of string
end

(* Names *)

type t = { scope : Scope.t; name : string; doc : string; meta : B0_meta.t }

type def = t
let scope d = d.scope
let file d = Scope.file d.scope
let dir d = Scope.dir d.scope
let name d = d.name
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
  val doc : t -> string
  val meta : t -> B0_meta.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val add : t -> unit
  val list : unit -> t list
  val find : string -> t option
  val get : string -> t
  val get_or_suggest : string -> (t, t list) result
  val get_all : string list -> (t list, string) result
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
  let doc v = doc (V.def v)
  let meta v = meta (V.def v)
  let scope v = scope (V.def v)
  let equal v0 v1 = String.equal (name v0) (name v1)
  let compare v0 v1 = String.compare (name v0) (name v1)

  let defs = ref String.Map.empty
  let add v = defs := String.Map.add (name v) v !defs

  let is_name n = String.for_all (fun c -> c <> '.') n
  let illegal_name_error n =
    Fmt.str "%a is not a legal %s name, dots are not allowed."
      Fmt.(code string) n V.def_kind

  let seal_error n =
    Fmt.str "%s %a illegaly created after B0 file initialization."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str n

  let duplicate_error n =
    Fmt.str "%s %a already defined in scope."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str n

  let err_undefined n =
    Fmt.str "%s %a undefined in scope."
      (String.Ascii.capitalize V.def_kind) V.pp_name_str n

  let qualify_name n =
    if not (is_name n) then raise (Err (illegal_name_error n)) else
    Scope.qualify_name n

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
        | false -> { scope; name; doc; meta }

  let scoped_find n = match String.Map.find (Scope.qualify_name n) !defs with
  | exception Not_found -> None
  | v -> Some v

  let find = scoped_find
  let get n = match scoped_find n with
  | Some v -> v | None -> raise (Err (err_undefined n))

  let get_or_suggest n = match scoped_find n with
  | Some v -> Ok v
  | None ->
      let add_sugg k v acc =
        if String.edit_distance k n <= 2 then v :: acc else acc
      in
      Error (List.rev (String.Map.fold add_sugg !defs []))

  let list () = match Scope.is_root () with
  | true ->
      let add _ v vs = v :: vs in
      String.Map.fold add !defs []
  | false ->
      let pre = Scope.qualify_name "" in
      let add k v vs = if String.is_prefix ~affix:pre k then v :: vs else vs in
      String.Map.fold add !defs []

  let get_all = function
  | [] -> Ok (list ())
  | ns ->
      let rec loop vs es = function
      | [] ->
          if es <> []
          then Error (String.concat "\n" (List.rev es))
          else Ok (List.rev vs)
      | n :: ns ->
          match get_or_suggest n with
          | Ok v -> loop (v :: vs) es ns
          | Error suggs ->
              let kind ppf () = Fmt.pf ppf "%s" def_kind in
              let hint = Fmt.did_you_mean in
              let pp = Fmt.unknown' ~kind V.pp_name_str ~hint in
              let e = Fmt.str "@[%a@]" pp (n, List.map name suggs) in
              loop vs (e :: es) ns
      in
      loop [] [] ns

  let pp_name_str = V.pp_name_str
  let pp_name = Fmt.using name pp_name_str
  let pp_doc = Fmt.using doc (Fmt.tty_string [(* Hear DKM *) `Faint])
  let pp_synopsis ppf v = Fmt.pf ppf "%a %a" pp_name v pp_doc v
  let pp ppf v =
    let pp_non_empty ppf m = match B0_meta.is_empty m with
    | true -> () | false -> Fmt.pf ppf "@, %a" B0_meta.pp m in
    Fmt.pf ppf "@[<v>@[%a %a@]%a@]" pp_name v pp_doc v pp_non_empty (meta v)

  module T = struct type nonrec t = t let compare = compare end
  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

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
