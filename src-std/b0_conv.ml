(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open B0_result

(* Converters *)

type 'a codec = (string -> 'a result) * ('a -> string result)
let codec c = c

type 'a text = (string -> 'a result) * (Format.formatter -> 'a -> unit)
let text t = t

type 'a t =
  { parse : (string -> 'a result);
    print : (Format.formatter -> 'a -> unit);
    decode : (string -> 'a result);
    encode : ('a -> string result);
    docv : string; }

let v ?(docv = "VALUE") ?codec (parse, print) =
  let decode, encode = match codec with
  | Some (decode, encode) -> decode, encode
  | None ->
      let decode = parse in
      let encode v = Ok (Format.asprintf "%a" print v) in
      decode, encode
  in
  { parse; print; decode; encode; docv }

let with_docv c docv = { c with docv }
let parse c = c.parse
let print c = c.print
let decode c = c.decode
let encode c = c.encode
let docv c = c.docv

(* Predefined converters *)

let bool =
  let parse s = try Ok (bool_of_string s) with
  | Invalid_argument _ -> R.error_msgf "Can't parse boolean value from %S" s
  in
  let print = Format.pp_print_bool in
  let decode = function
  | "0" -> Ok false
  | "1" -> Ok true
  | s -> R.error_msgf "Can't decode boolean value from %S" s
  in
  let encode = function false -> Ok "0" | true -> Ok "1" in
  let docv = "BOOL"  in
  { parse; print; decode; encode; docv }

let char =
  let parse s = match String.length s = 1 with
  | true -> Ok s.[0]
  | false -> R.error_msgf "Expected a single character, got %S" s
  in
  let print = Format.pp_print_char in
  let decode = parse in
  let encode c = Ok (Printf.sprintf "%c" c) in
  let docv = "CHAR" in
  { parse; print; decode; encode; docv }

let int =
  let parse s = try Ok (int_of_string s) with
  | Failure _ -> R.error_msgf "Can't parse integer value from %S" s
  in
  let print = Format.pp_print_int in
  let decode = parse in
  let encode b = Ok (string_of_int b) in
  let docv = "INT" in
  { parse; print; decode; encode; docv }

let int32 =
  let parse s = try Ok (Int32.of_string s) with
  | Failure _ -> R.error_msgf "Can't parse int32 value from %S" s
  in
  let print ppf i = Format.fprintf ppf "%ld" i in
  let decode = parse in
  let encode i = Ok (Printf.sprintf "%ld" i) in
  let docv = "INT32" in
  { parse; print; decode; encode; docv }

let int64 =
  let parse s = try Ok (Int64.of_string s) with
  | Failure _ -> R.error_msgf "Can't parse int64 value from %S" s
  in
  let print ppf i = Format.fprintf ppf "%Ld" i in
  let decode = parse in
  let encode i = Ok (Printf.sprintf "%Ld" i) in
  let docv = "INT64" in
  { parse; print; decode; encode; docv }

let float =
  let parse s = try Ok (float_of_string s) with
  | Failure _ -> R.error_msgf "Can't parse float value from %S" s
  in
  let print = Format.pp_print_float in
  let decode = parse in
  let encode f = Ok (Printf.sprintf "%H" f) in
  let docv = "FLOAT" in
  { parse; print; decode; encode; docv }

let string =
  let parse s = Ok s in
  let print = Format.pp_print_string in
  let decode = parse in
  let encode s = Ok s in
  let docv = "STRING" in
  { parse; print; decode; encode; docv }

let string_non_empty =
  let parse = function
  | "" -> R.error_msgf "The string is empty."
  | s -> Ok s
  in
  let print = Format.pp_print_string in
  let decode = parse in
  let encode s = Ok s in
  let docv = "STRING" in
  { parse; print; decode; encode; docv }

let fpath =
  let parse = B0_fpath.of_string in
  let print = B0_fpath.pp in
  let decode = B0_fpath.of_string in
  let encode s = Ok (B0_fpath.to_string s) in
  let docv = "PATH" in
  { parse; print; decode; encode; docv }

let file = with_docv fpath "FILE"
let dir = with_docv fpath "DIR"

let tool =
  let parse s = B0_fpath.of_string s >>| fun t -> B0_cmd.(v @@ p t) in
  let print = B0_cmd.pp in
  let decode = B0_cmd.of_string in
  let encode t = Ok (B0_cmd.to_string t) in
  let docv = "TOOL" in
  { parse; print; decode; encode; docv; }

let cmd =
  let parse = B0_cmd.of_string in
  let print = B0_cmd.pp in
  let decode = B0_cmd.of_string in
  let encode t = Ok (B0_cmd.to_string t) in
  let docv = "CMDLINE" in
  { parse; print; decode; encode; docv; }

let enum ?(docv = "ENUM") alts =
  let incomplete () = invalid_arg "Incomplete enumeration for the type." in
  let inv = List.map (fun (s, v) -> (v, s)) alts in
  let parse s = try Ok (List.assoc s alts) with
  | Not_found ->
      R.error_msgf "Can't parse %S, must be one of %s"
        s (String.concat ", " (List.map fst alts))
  in
  let print ppf v = try Format.pp_print_string ppf (List.assoc v inv) with
  | Not_found -> incomplete ()
  in
  let decode = parse in
  let encode v = try Ok (List.assoc v inv) with Not_found -> incomplete () in
  let docv = docv in
  { parse; print; decode; encode; docv }

let err_to_exn f v = match f v with Ok v -> v | Error (`Msg m) -> failwith m

let list ?(sep = ",") c =
  let splits ~sep s =
    try
      let els_rev = List.rev (B0_string.cuts ~sep s) in
      Ok (List.rev_map (err_to_exn c.parse) els_rev)
    with Failure m -> R.error_msgf "%S: %s" s m
  in
  let parse s = splits ~sep s in
  let print p l = Format.(pp_print_list ~pp_sep:pp_print_space c.print) p l in
  (* The codec is a bit retired. *)
  let decode s = splits ~sep:"\x00" s in
  let encode l =
    try
      let els = List.rev_map (err_to_exn c.encode) (List.rev l) in
      Ok (String.concat "\x00" els)
    with Failure m -> R.error_msg m
  in
  let docv = Printf.sprintf "%s%s..." c.docv sep in
  { parse; print; decode; encode; docv }

let pair ?(sep = ",") fst snd =
  let split ~sep s = match B0_string.cut ~sep s with
  | None -> R.error_msgf "Could not split at %S in %S" sep s
  | Some (fv, sv) ->
      try Ok (err_to_exn fst.parse fv, err_to_exn snd.parse sv)
      with Failure m -> R.error_msgf "%S: %s" s m
  in
  let parse s = split ~sep s in
  let print ppf (f, s) =
    Format.fprintf ppf "@[%a %a@]" fst.print f snd.print s
  in
  (* The codec is a bit retired. *)
  let decode s = split ~sep:"\x00" s in
  let encode (f, s) =
    try
      Ok (String.concat "\x00" [err_to_exn fst.encode f;
                                err_to_exn snd.encode s])
    with Failure m -> R.error_msg m
  in
  let docv = Printf.sprintf "%s%s%s" fst.docv sep snd.docv in
  { parse; print; decode; encode; docv }

let option ?(none = "") c =
  let parse s = match s = none with
  | true -> Ok None
  | false -> match c.parse s with Ok v -> Ok (Some v) | Error _ as e -> e
  in
  let print ppf = function
  | Some v -> c.print ppf v
  | None -> Format.pp_print_string ppf none
  in
  let decode s =
    let err s c = R.error_msgf "Could not decode %s option from %S" c.docv s in
    match s with
    | "" -> err s c
    | "0" -> Ok None
    | s ->
        if s.[0] <> '1' then err s c else
        c.decode (B0_string.with_index_range ~first:1 s) >>| fun v -> Some v
  in
  let encode = function
  | None -> Ok "0"
  | Some v -> c.encode v >>| fun e -> ("1" ^ e)
  in
  let docv = c.docv in
  { parse; print; decode; encode; docv }

(*---------------------------------------------------------------------------
   Copyright (c) 2017 b0

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
