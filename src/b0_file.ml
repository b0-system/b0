(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0 programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open B00_std.Result.Syntax
open B00_serialk_text

(* Syntactic metadata *)

type smeta = Tloc.t
let smeta ~loc = loc
let loc m = m

let pp_loc = Tloc.pp_ocaml
let loc_err_fmt ffmt m fmt =
  ffmt ("@[<v>%a:@,@[%a: " ^^ fmt ^^ "@]@]")
    pp_loc (loc m) (Fmt.tty_string [`Fg `Red; `Bold ]) "Error"

let loc_errf m fmt = loc_err_fmt Fmt.str m fmt
let loc_error m fmt = loc_err_fmt Fmt.error m fmt

(* B0 files *)

type b0_boot = (string * smeta) list
type b0_include = (string * smeta) * (Fpath.t * smeta)
type require = B00_ocaml.Lib.Name.t * smeta
type mod_use = Fpath.t * smeta
type t =
  { file : Fpath.t;
    cwd : Fpath.t; (* Fpath.parent of [file] *)
    b0_boots : b0_boot list;
    b0_includes : b0_include list;
    requires : require list;
    mod_uses : mod_use list;
    ocaml_unit : string * smeta; }

let file f = f.file
let cwd f = f.cwd
let b0_includes f = f.b0_includes
let b0_boots f = f.b0_boots
let requires f = f.requires
let mod_uses f = f.mod_uses
let ocaml_unit f = f.ocaml_unit

let pp_dump ppf s =
  let pp_fst pp = Fmt.using fst pp in
  let pp_strings = Fmt.(list ~sep:sp (pp_fst Fmt.string)) in
  let pp_boots = Fmt.vbox @@ Fmt.list (Fmt.box pp_strings) in
  let pp_includes =
    let pp_include ppf ((n, _), (p, _)) =
      Fmt.pf ppf "@[%s %a@]" n Fpath.pp_quoted p
    in
    Fmt.(vbox @@ list pp_include)
  in
  let pp_reqs = Fmt.(list ~sep:sp (pp_fst B00_ocaml.Lib.Name.pp)) in
  let pp_mod_uses = Fmt.(list (pp_fst Fpath.pp_unquoted)) in
  Fmt.record
    [ Fmt.field "file" file Fpath.pp_quoted;
      Fmt.field "b0-boots" b0_boots pp_boots;
      Fmt.field "b0-includes" b0_includes pp_includes;
      Fmt.field "requires" requires pp_reqs;
      Fmt.field "mod_uses" mod_uses pp_mod_uses;
      Fmt.field "ocaml_unit" ocaml_unit (Fmt.box @@ pp_fst Fmt.lines)]
    ppf s

let pp_locs ppf s =
  let pp_loc ppf (_, smeta) = Fmt.pf ppf "%a:" pp_loc (loc smeta) in
  let pp_loc_pair ppf (a, b) = pp_loc ppf a; Fmt.cut ppf (); pp_loc ppf b in
  let pp_list pp_v ppf = function
  | [] -> () | l -> Fmt.list pp_v ppf l; Fmt.cut ppf ()
  in
  Fmt.pf ppf "@[<v>%a%a%a%a%a@]"
    (pp_list (pp_list pp_loc)) (b0_boots s)
    (pp_list pp_loc_pair) (b0_includes s)
    (pp_list pp_loc) (requires s)
    (pp_list pp_loc) (mod_uses s)
    pp_loc (ocaml_unit s)

(* Parsing *)

let directives = ["@@@B0.boot"; "@@@B0.include"; "#requires"; "#mod_use"]
let pp_directive = Fmt.code Fmt.string

let is_dir_letter c =
  (0x61 <= c && c <= 0x7A) || c = 0x5F || (0x41 <= c && c <= 0x5A) ||
  c = 0x2E || c = 0x40 || (0x30 <= c && c <= 0x39)

let err_eoi msg d ~sbyte ~sline =
  Tdec.err_to_here d ~sbyte ~sline "Unexpected end of input: %s" msg

let err_eoi_string = err_eoi "unclosed string"
let err_eoi_esc = err_eoi "truncated escape"
let err_eoi_dir = err_eoi "unclosed directive"
let err_exp_dir_arg d = Tdec.err_here d "Expected directive argument"
let err_illegal_uchar d b = Tdec.err_here d "Illegal character U+%04X" b

let err_exp_eodir d ~sbyte ~sline =
  Tdec.err_to_here d ~sbyte ~sline "Expected end of directive (']')"

let curr_char d = (* TODO better escaping (this is for error reports) *)
  Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

let err_esc_illegal d ~sbyte ~sline pre =
  Tdec.err_to_here d ~sbyte ~sline "%s%s: illegal escape" pre (curr_char d)

let err_unsupported_directive sharp dir_loc dir =
  let hint = Fmt.must_be in
  let unknown = Fmt.(unknown' ~kind:(any "directive") pp_directive ~hint) in
  Tdec.err dir_loc (Fmt.str "@[%a@]" unknown (sharp ^ dir, directives))

let dec_byte d = match Tdec.byte d with
| c when 0x00 <= c && c <= 0x08 || 0x0E <= c && c <= 0x1F || c = 0x7F ->
    err_illegal_uchar d c
| c -> c
[@@ ocaml.inline]

let rec skip_ws d = match dec_byte d with
| 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip_ws d
| _ -> ()

let parse_esc d =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  match (Tdec.accept_byte d; dec_byte d) with
  | 0x22 -> Tdec.accept_byte d; Tdec.tok_add_char d '"'
  | 0x5C -> Tdec.accept_byte d; Tdec.tok_add_char d '\\'
  | 0x0A | 0x0D -> (* continuation line *) skip_ws d
  | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
  | _ -> err_esc_illegal d ~sbyte ~sline "\\"

let parse_string d = match (skip_ws d; dec_byte d) with
| 0x22 ->
    let rec loop d ~sbyte ~sline = match dec_byte d with
    | 0x22 ->
        let loc = Tdec.loc_to_here d ~sbyte ~sline in
        let arg = Tdec.tok_pop d in
        Tdec.accept_byte d; (arg, smeta ~loc)
    | 0x5C -> parse_esc d; loop d ~sbyte ~sline
    | 0xFFFF -> err_eoi_string d ~sbyte ~sline
    | _ -> Tdec.tok_accept_byte d; loop d ~sbyte ~sline
    in
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    Tdec.accept_byte d; loop ~sbyte ~sline d
| c -> err_exp_dir_arg d

let string_to parse (arg, smeta) =
  match parse arg with Ok v -> v, smeta | Error e -> Tdec.err (loc smeta) e

let parse_fpath d = string_to Fpath.of_string (parse_string d)

let rec parse_directive_name d ~sbyte ~sline = match dec_byte d with
| c when is_dir_letter c ->
    Tdec.tok_accept_byte d; parse_directive_name d ~sbyte ~sline
| c ->
    let ebyte = Tdec.pos d - 1 and eline = Tdec.line d in
    let loc = Tdec.loc d ~sbyte ~ebyte ~sline ~eline in
    Tdec.tok_pop d, loc

let rec parse_annot_arg_list d acc p_arg ~sbyte ~sline =
  match (skip_ws d; dec_byte d) with
  | 0x5D (* ] *) -> Tdec.accept_byte d; List.rev acc
  | 0xFFFF -> err_eoi_dir d ~sbyte ~sline
  | _ -> parse_annot_arg_list d (p_arg d :: acc) p_arg ~sbyte ~sline

let parse_boot_directive d ~sbyte ~sline =
  parse_annot_arg_list d [] parse_string ~sbyte ~sline

let parse_include_directive d ~sbyte ~sline =
  let parse_scope_name (s, smeta as scope) =
    if s = "" then Tdec.err (loc smeta) "Scope name cannot be empty." else
    if not (String.exists (Char.equal '.') s) then scope else
    Tdec.err (loc smeta)
      (Fmt.str "Scope name %a contains a dot." Fmt.(code string) s)
  in
  let arg = parse_string d in
  match skip_ws d; dec_byte d with
  | 0x5D (* ] *) ->
      Tdec.accept_byte d;
      let (p, smeta as file) = string_to Fpath.of_string arg in
      let scope = Fpath.(basename @@ parent p), smeta in
      parse_scope_name scope, file
  | 0xFFFF -> err_eoi_dir d ~sbyte ~sline
  | _ ->
      let file = string_to Fpath.of_string (parse_string d) in
      match skip_ws d; dec_byte d with
      | 0x5D (* ] *) -> Tdec.accept_byte d; (parse_scope_name arg, file)
      | 0xFFFF -> err_eoi_dir d ~sbyte ~sline
      | _ -> err_exp_eodir d ~sbyte ~sline

let parse_require_directive d ~sbyte ~sline =
  string_to B00_ocaml.Lib.Name.of_string (parse_string d)

let parse_mod_use_directive d ~sbyte ~sline =
  string_to Fpath.of_string (parse_string d)

let parse_preamble d =
  let rec loop boots incs reqs mus d = match skip_ws d; dec_byte d with
  | 0x23 (* # *) ->
      let sbyte = Tdec.pos d and sline = Tdec.line d in
      Tdec.accept_byte d;
      begin match parse_directive_name d ~sbyte ~sline with
      | "require", _ ->
          let r = parse_require_directive d ~sbyte ~sline in
          loop boots incs (r :: reqs) mus d
      | "mod_use", _ ->
          let m = parse_mod_use_directive d ~sbyte ~sline in
          loop boots incs reqs (m :: mus) d
      | dir, dir_loc ->
          err_unsupported_directive "#" dir_loc dir
      end
  | 0x5B (* [ *) ->
      let sbyte = Tdec.pos d and sline = Tdec.line d in
      Tdec.accept_byte d;
      begin match parse_directive_name d ~sbyte ~sline with
      | "@@@B0.boot", _ ->
          let b = parse_boot_directive d ~sbyte ~sline in
          loop (b :: boots) incs reqs mus d
      | "@@@B0.include", _ ->
          let i = parse_include_directive d ~sbyte ~sline in
          loop boots (i :: incs) reqs mus d
      | dir, dir_loc ->
          (* FIXME warn on @@@B0.* do not error. *)
          err_unsupported_directive "" dir_loc dir
      end
  | _ ->
      List.rev boots, List.rev incs, List.rev reqs, List.rev mus
  in
  loop [] [] [] [] d

let of_string ~file src =
  try
    let d = Tdec.create ~file:(Fpath.to_string file) src in
    let b0_boots, b0_includes, requires, mod_uses = parse_preamble d in
    let rest = String.subrange ~first:(Tdec.pos d) src in
    let ocaml_unit = rest, smeta ~loc:(Tdec.loc_here d) in
    let cwd = Fpath.parent file in
    Ok { file; cwd; b0_boots; b0_includes; requires; ocaml_unit; mod_uses }
  with Tdec.Err (loc, e) -> loc_error loc "%a" (Fmt.vbox Fmt.lines) e

(* Expansion *)

type expanded =
  { expanded_file_manifest : Fpath.t list;
    expanded_b0_boots : b0_boot list;
    expanded_b0_includes : b0_include list;
    expanded_requires : require list;
    expanded_src : string; }

let expanded_file_manifest e = e.expanded_file_manifest
let expanded_b0_boots e = e.expanded_b0_boots
let expanded_b0_includes e = e.expanded_b0_includes
let expanded_requires e = e.expanded_requires
let expanded_src e = e.expanded_src

let get_include_src b0_file (p, smeta) =
  let src =
    let* file = Os.Path.realpath Fpath.((cwd b0_file) // p) in
    let* src = Os.File.read file in
    Ok (file, src)
  in
  match src with
  | Ok (file, src) -> of_string ~file src |> Result.to_failure
  | Error e ->
      (* We could do a bit better with e here *)
      loc_err_fmt Fmt.failwith_notrace smeta "%s" e

let get_mod_use_srcs b0_file manif (p, smeta) =
  let file_impl = Fpath.((cwd b0_file) // p) in
  let file_intf = Fpath.(file_impl -+ ".mli") in
  let res =
    (* XXX maybe we should rather add non realpathed paths to files
        e.g. if people play with symlinks. Sort that out. *)
    let* file_impl = Os.Path.realpath file_impl in
    let* src_impl = Os.File.read file_impl in
    let manif = file_impl :: manif in
    let* exists = Os.File.exists file_intf in
    if not exists then Ok (manif, None, (file_impl, src_impl)) else
    let* file_intf = Os.Path.realpath file_intf in
    let* src_intf = Os.File.read file_intf in
    Ok (file_intf :: manif, Some (file_intf, src_intf), (file_impl, src_impl))
  in
  match res with
  | Ok (files, intf, impl) -> files, intf, impl
  | Error e -> loc_err_fmt Fmt.failwith_notrace smeta "%s" e

let nil_loc = "#1 \"-\""
let lineno meta =
  let l = loc meta in
  Fmt.str "#%d %S" (fst (Tloc.sline l)) (Tloc.file l)

let w acc l = l :: acc [@@ocaml.inline]

let w_src b (file, src) =
  let b = w b (Fmt.str "#1 %S" (Fpath.to_string file)) in
  let b = w b src in b

let w_mod_use b b0_file manif (p, _ as mod_use) =
  let manif, intf, impl = get_mod_use_srcs b0_file manif mod_use in
  let mod_name = B00_ocaml.Mod.Name.of_mangled_filename (Fpath.basename p) in
  let b = match intf with
  | None -> w b (Fmt.str "module %s = struct" mod_name)
  | Some intf ->
    let b = w b (Fmt.str "module %s : sig" mod_name) in
    let b = w_src b intf in
    let b = w b "end = struct" in
    b
  in
  let b = w_src b impl in
  let b = w b "end" in
  let b = w b nil_loc in
  b, manif

let rec w_mod_uses b b0_file manif =
  let rec loop b file files = function
  | [] -> b, files
  | mod_use :: mod_uses ->
      let b, manif = w_mod_use b file manif mod_use in
      loop b file manif mod_uses
  in
  loop b b0_file manif (mod_uses b0_file)

let w_ocaml_unit b (src, src_meta) =
  let b = w b (lineno src_meta) in
  let b = w b src in b

let rec w_include b b0_file id npre manif boots incs reqs ((n, nm), inc_file) =
  let b0_file = get_include_src b0_file inc_file in
  let b = w b nil_loc in
  let b = w b (Fmt.str "module Inc_%03d : sig end = struct" id) in
  let b =
    let f = Fpath.to_string (file b0_file) in
    w b (Fmt.str "let () = B0_def.Scope.open' %S (B00_std.Fpath.v %S)" n f)
  in
  let b, id, manif, boots, incs, reqs =
    let id = id + 1 in
    let file = file b0_file in
    let npre = if npre = "" then n else String.concat "." [npre; n] in
    let incs = ((npre, nm), (file, snd inc_file)) :: incs in
    w_includes b b0_file id npre manif boots incs reqs
  in
  let b, manif = w_mod_uses b b0_file manif in
  let b = w_ocaml_unit b (ocaml_unit b0_file) in
  let b = w b nil_loc in
  let b = w b "let () = B0_def.Scope.close ()" in
  let b = w b "end" in
  b, id, manif, boots, incs, reqs

and w_includes b b0_file id npre manif boots incs reqs =
  let check_scope_unique (n, smeta) nmap = match String.Map.find n nmap with
  | exception Not_found -> String.Map.add n smeta nmap
  | smeta' ->
      loc_err_fmt Fmt.failwith
        smeta "@[<v>Scope name %a already defined.@,\
               Previous definition:@,%a:@]" Fmt.(code string) n pp_loc smeta'
  in
  let rec loop b0_file id nmap npre b files boots incs reqs = function
  | (name, _ as i) :: todo ->
      let nmap = check_scope_unique name nmap in
      let b, id, files, boots, incs, reqs =
        w_include b b0_file id npre files boots incs reqs i
      in
      loop b0_file id nmap npre b files boots incs reqs todo
  | [] ->
      b, id, files, boots, incs, reqs
  in
  let manif = file b0_file :: manif in
  let boots = List.rev_append (b0_boots b0_file) boots in
  let reqs = List.rev_append (requires b0_file) reqs in
  loop b0_file id String.Map.empty npre b manif boots incs reqs
    (b0_includes b0_file)

let expand b0_file =
  try
    let b = w [] nil_loc in
    let r = Fpath.to_string (file b0_file) in
    let b = w b (Fmt.str "let () = B0_def.Scope.root (B00_std.Fpath.v %S)" r) in
    let b, _, manif, boots, incs, reqs = w_includes b b0_file 0 "" [] [] [][] in
    let b, mani = w_mod_uses b b0_file manif in
    let b = w_ocaml_unit b (ocaml_unit b0_file) in
    let b = w b nil_loc in
    let b = w b "let () = B0_def.Scope.seal ()" in
    let b = w b "let () = B0_driver.run ~has_b0_file:true" in
    Ok { expanded_file_manifest = List.rev manif;
         expanded_b0_boots = List.rev boots;
         expanded_b0_includes = List.rev incs;
         expanded_requires = List.rev reqs;
         expanded_src = String.concat "\n" (List.rev b) }
  with Failure e -> Error e

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
